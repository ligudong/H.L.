# Load necessary packages and functions
source("script/00_loadPackages.R")  # Load required packages
source("script/loadFunctions.R")    # Load custom functions for data processing
source("script/loadFormulas.R")     # Load formulas used in modeling

# Load data directly using the custom get_data function
data <- get_data()

# Filter data for rice crops (Rice(LR) and Rice(SR&ER))
data_rice <- data %>% filter(crop %in% c("Rice(LR)", "Rice(SR&ER)"))

# Handle missing values by removing rows with NAs
data <- drop_na(data)

# Define the model names, including both the base model and the modified model
model_names <- c(
  "base2",         # Base model
  "no_jiaohu2"     # Modified model (no interaction)
)

# Define the updated get_plot function that supports multiple models
get_plot <- function(adata) {
  # Print basic information about the "Caod" variable
  message("Processing crop data")
  message("Caod summary: ", summary(adata$Caod))
  
  # Remove rows with NA values for modeling
  data_modeling <- na.omit(adata)
  
  # Check the valid range for "Caod" values
  Caod_range <- range(adata$Caod, na.rm = TRUE)
  
  # If Caod range is invalid (e.g., both min and max are the same or not finite), skip the plotting
  if (!is.finite(Caod_range[1]) || !is.finite(Caod_range[2]) || Caod_range[1] == Caod_range[2]) {
    warning("Invalid Caod range, skipping plot")
    return(NULL)
  }
  
  # Define and fit multiple models
  models <- lst(
    fml_base_O3, fml_no_jiaohu2  # Base model and modified model
  ) %>%
    map(~ feols(.x, data_modeling, cluster = ~city, weights = ~fraction, nthreads = 0, lean = T), .progress = T) %>%
    set_names(model_names)
  
  # Calculate predictions for multiple models
  Caod_hist <- seq(Caod_range[1], Caod_range[2], 0.001)
  Caod_mid <- round(quantile(adata$Caod, 0.5, na.rm = TRUE), 2)  # Median Caod value
  
  # Generate predicted values for each model
  Caod <- map_dfr(models, function(amodel) {
    relpred(
      amodel,
      tibble(Caod = Caod_hist),
      tibble(Caod = rep(Caod_mid, length(Caod_hist)))
    ) %>%
      mutate(Caod_x = Caod_hist)  # Add Caod_hist as x-axis for plotting
  }, .id = "model") %>%
    mutate(across(c(fit, lwr, upr), ~ expm1(.x) * 1e2))  # Apply exponential transformation to predictions
  
  # Set an offset for positioning labels and adjust plot range
  Caod_offset <- (Caod_range[2] - Caod_range[1]) * 0.01
  
  # Create labels for plotting by applying the model predictions at the maximum Caod value
  Caod_lab <- map_dfr(models, function(amodel) {
    relpred(
      amodel,
      tibble(Caod = rep(Caod_range[2], 10)),
      tibble(Caod = rep(Caod_mid, length(rep(Caod_range[2], 10))))
    ) %>%
      mutate(Caod_x = rep(Caod_range[2], 10))
  }, .id = "model") %>%
    group_by(model) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    mutate(across(c(fit, lwr, upr), ~ expm1(.x) * 1e2)) %>%
    mutate(Caod_x = Caod_x + Caod_offset)  # Adjust the position of labels
  
  # Define colors for the models
  model_colors <- c(
    "base_O3" = "#1f77b4",       # Blue color for the base model
    "no_jiaohu2" = "#ff7f0e"     # Orange color for the modified model
  )
  
  # Generate the plot for Caod with confidence intervals and labels
  p1 <- Caod %>%
    mutate(model = fct_relevel(model, rev(model_names))) %>%
    ggplot(aes(x = Caod_x, y = fit, color = model)) +
    
    # Plot confidence intervals for the "base" model
    geom_ribbon(aes(x = Caod_x, ymin = lwr, ymax = upr, fill = model),
                inherit.aes = F,  
                alpha = 0.2,      # Transparency of the confidence intervals
                show.legend = F,  
                size = 0.01,       # Border width of the confidence interval
                data = . %>% filter(model %in% c("base"))) + 
    
    # Plot solid lines for both models
    geom_line(data = . %>% filter(model == "base"), size = 1.5, linetype = "solid", alpha = 1, show.legend = TRUE) +  
    geom_line(data = . %>% filter(model == "no_jiaohu"), size = 1.5, linetype = "solid", alpha = 1, show.legend = TRUE) +  
    
    # Set colors for the models
    scale_color_manual(values = model_colors) +
    
    # Add labels and customize the plot appearance
    geom_text_repel(aes(label = model),
                    data = Caod_lab,
                    show.legend = F, 
                    min.segment.length = 0, 
                    box.padding = 1,
                    direction = "y",  # Vertical label alignment
                    nudge_x = 0.02,   # Adjust x position of the label
                    nudge_y = 0.02,   # Adjust y position of the label
                    xlim = c(Caod_range[2] + Caod_offset, Caod_range[2] * 1.1),  
                    arrow = arrow, 
                    max.overlaps = 1e2,
                    family = "Roboto Condensed", 
                    size = 5, 
                    seed = 2022) +
    
    # Set axis labels and theme
    ylab("Percentage Change in SIF") +
    xlab("cAOD") +
    theme_half_open(18, font_family = "Roboto Condensed") +
    background_grid() +
    theme(
      ggside.panel.scale = 0.2,
      legend.title = element_blank(),
      legend.position = c(1.2, 0.5),
      panel.grid = element_blank(),
      axis.text.x = element_text(size = 50),  # Adjust font size for X-axis
      axis.text.y = element_text(size = 50)   # Adjust font size for Y-axis
    ) +
    
    # Plot the Faod histogram on the side
    geom_xsidehistogram(aes(x = Faod),
                        data = adata,
                        bins = 80, fill = "#D3D3D3", color = "black", size = 0.2,
                        inherit.aes = F) +
    
    ggside(x.pos = "bottom", collapse = "x") +
    scale_xsidey_continuous(labels = NULL, breaks = NULL) +
    theme(ggside.panel.scale = 0.5) +  # Adjust the height ratio for the histogram
    scale_x_continuous(limits = c(Caod_range[1], Caod_range[2] + Caod_offset), expand = expansion(mult = c(0, 0.15))) +
    scale_xsidey_continuous(labels = NULL, breaks = NULL) +
    labs(subtitle = "Crop Response Plot")
  
  return(p1)  # Return the plot object
}

# Generate the plot data
plots <- get_plot(data)

# Create the 'figures' directory if it doesn't exist
if (!dir.exists("figures")) {
  dir.create("figures")
}

print(plots)

# Save the plot as a high-resolution TIFF file
ggsave(
  "sfig/response_climate_caod_Rice.tif", 
  plots, 
  width = 6, 
  height = 4.5, 
  dpi = 300,          # Set resolution to 300 DPI for printing
  device = "tiff"     # Specify the output file type as TIFF
)
