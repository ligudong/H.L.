# Load necessary packages and user-defined functions
source("script/00_loadPackages.R")
source("scrip/loadFunctions.R")
source("scrip/loadFormulas.R")

# Load pre-processed data
data <- get_data()

# Remove missing values
data <- drop_na(data)

# Filter to include only Wheat crop (can switch to Maize or Rice)
data <- data %>% filter(crop == "Wheat")
# data <- data %>% filter(crop == "Maize")
# data_rice <- data %>% filter(crop %in% c("Rice(LR)", "Rice(SR&ER)"))

# Define model names to label outputs
model_names <- c(
  "base_O3",     # Baseline model with O3
  "no_jiaohu2"   # Model with additional AOT40–O3 interaction
)

# Function to generate crop-specific response plots for O3
get_plot <- function(adata) {
  message("Processing crop-specific data")
  message("O3 Summary: ", summary(adata$O3))
  
  # Drop missing values
  data_modeling <- na.omit(adata)
  
  # Check O3 value range
  O3_range <- range(adata$O3, na.rm = TRUE)
  if (!is.finite(O3_range[1]) || !is.finite(O3_range[2]) || O3_range[1] == O3_range[2]) {
    warning("Invalid O3 range; skipping plot")
    return(NULL)
  }
  
  # Fit models
  models <- lst(
    fml_base_O3, fml_no_jiaohu2
  ) %>%
    map(~ feols(.x, data_modeling, cluster = ~city, weights = ~fraction, nthreads = 0, lean = TRUE), .progress = TRUE) %>%
    set_names(model_names)
  
  # Predict responses across O3 values
  O3_hist <- seq(O3_range[1], O3_range[2], 1)
  O3_mid <- round(quantile(adata$O3, 0.5, na.rm = TRUE), 2)
  
  O3 <- map_dfr(models, function(amodel) {
    relpred(
      amodel,
      tibble(O3 = O3_hist),
      tibble(O3 = rep(O3_mid, length(O3_hist)))
    ) %>% mutate(O3_x = O3_hist)
  }, .id = "model") %>%
    mutate(across(c(fit, lwr, upr), ~ expm1(.x) * 1e2))  # Convert log-diff to percentage
  
  # Prepare label positions slightly offset to the right
  O3_offset <- (O3_range[2] - O3_range[1]) * 0.01
  
  O3_lab <- map_dfr(models, function(amodel) {
    relpred(
      amodel,
      tibble(O3 = rep(O3_range[2], 10)),
      tibble(O3 = rep(O3_mid, 10))
    ) %>%
      mutate(O3_x = rep(O3_range[2], 10))
  }, .id = "model") %>%
    group_by(model) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    mutate(across(c(fit, lwr, upr), ~ expm1(.x) * 1e2)) %>%
    mutate(O3_x = O3_x + O3_offset)
  
  # Define model colors
  model_colors <- c(
    "base_O3" = "#1f77b4",
    "no_jiaohu2" = "#ff7f0e"
  )
  
  # Plot results
  p1 <- O3 %>%
    mutate(model = fct_relevel(model, rev(model_names))) %>%
    ggplot(aes(x = O3_x, y = fit, color = model)) +
    
    # Confidence ribbon only for base model
    geom_ribbon(aes(x = O3_x, ymin = lwr, ymax = upr, fill = model),
                inherit.aes = FALSE,
                alpha = 0.2,
                show.legend = FALSE,
                size = 0.01,
                data = . %>% filter(model %in% c("base"))) +
    
    # Solid lines for each model
    geom_line(data = . %>% filter(model == "base_O3"), size = 1.5, linetype = "solid", alpha = 1) +
    geom_line(data = . %>% filter(model == "no_jiaohu2"), size = 1.5, linetype = "solid", alpha = 1) +
    
    scale_color_manual(values = model_colors) +
    
    # Add text labels at the end of lines
    geom_text_repel(aes(label = model),
                    data = O3_lab,
                    show.legend = FALSE,
                    min.segment.length = 0,
                    box.padding = 1,
                    direction = "y",
                    nudge_x = 0.02,
                    nudge_y = 0.02,
                    xlim = c(O3_range[2] + O3_offset, O3_range[2] * 1.1),
                    arrow = arrow,
                    max.overlaps = 1e2,
                    family = "Roboto Condensed",
                    size = 5,
                    seed = 2022) +
    
    ylab("Percentage Change in SIF") +
    xlab("O₃") +
    theme_half_open(18, font_family = "Roboto Condensed") +
    background_grid() +
    theme(
      ggside.panel.scale = 0.2,
      legend.title = element_blank(),
      legend.position = c(1.2, 0.5),
      panel.grid = element_blank()
    ) +
    
    # Histogram on x-side showing O3 distribution
    geom_xsidehistogram(aes(x = O3),
                        data = adata,
                        bins = 80, fill = "#D3D3D3", color = "black", size = 0.2,
                        inherit.aes = FALSE) +
    
    ggside(x.pos = "bottom", collapse = "x") +
    scale_xsidey_continuous(labels = NULL, breaks = NULL) +
    theme(ggside.panel.scale = 0.5) +
    
    scale_x_continuous(limits = c(O3_range[1], O3_range[2] + O3_offset),
                       expand = expansion(mult = c(0, 0.15))) +
    
    labs(subtitle = "Crop response to O₃ exposure")
  
  return(p1)
}

# Run plotting function on filtered data
plots <- get_plot(data)

# Create output directory if missing
if (!dir.exists("figures")) {
  dir.create("figures")
}

# Show plot in console
print(plots)

# Save plot as high-resolution TIFF image
ggsave(
  "figures/sfig/response_climate_AOT40_Wheat.tif",
  plots,
  width = 6,
  height = 4.5,
  dpi = 300,
  device = "tiff"
)
