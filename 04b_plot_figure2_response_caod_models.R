source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")
source("scrip/loadFormulas_Maize.R")

# Load preprocessed dataset
data <- get_data()
data <- data %>% filter(crop == "Maize")
#data_rice <- data %>% filter(crop %in% c("Rice(LR)", "Rice(SR&ER)"))
#data <- data %>% filter(crop == "Wheat")
data <- drop_na(data)  # Remove missing values

# Define model names for comparison
model_names <- c(
  "base",     # Baseline model
  "base2"     # Model including interaction term (e.g., with ozone)
)

# Define plotting function using CaOD as the predictor
get_plot <- function(adata) {
  message("Processing crop data...")
  message("CaOD summary: ", summary(adata$Caod))
  
  # Filter missing values for modeling
  data_modeling <- na.omit(adata)
  
  # Check CaOD range validity
  Caod_range <- range(adata$Caod, na.rm = TRUE)
  if (!is.finite(Caod_range[1]) || !is.finite(Caod_range[2]) || Caod_range[1] == Caod_range[2]) {
    warning("Invalid CaOD range; skipping plot.")
    return(NULL)
  }
  
  # Fit models using fixed-effects regression
  models <- lst(fml_base, fml_base2) %>%
    map(~ feols(.x, data_modeling, cluster = ~city, weights = ~fraction, nthreads = 0, lean = TRUE), .progress = TRUE) %>%
    set_names(model_names)
  
  # Create prediction grid across observed CaOD values
  Caod_hist <- seq(Caod_range[1], Caod_range[2], 0.001)
  Caod_mid <- round(quantile(adata$Caod, 0.5, na.rm = TRUE), 2)
  
  # Predict relative impact for each model
  Caod <- map_dfr(models, function(amodel) {
    relpred(
      amodel,
      tibble(Caod = Caod_hist),
      tibble(Caod = rep(Caod_mid, length(Caod_hist)))
    ) %>%
      mutate(Caod_x = Caod_hist)
  }, .id = "model") %>%
    mutate(across(c(fit, lwr, upr), ~ expm1(.x) * 100))  # Convert to % change
  
  Caod_offset <- (Caod_range[2] - Caod_range[1]) * 0.01
  
  # Prepare model labels at the right edge of each curve
  Caod_lab <- map_dfr(models, function(amodel) {
    relpred(
      amodel,
      tibble(Caod = rep(Caod_range[2], 10)),
      tibble(Caod = rep(Caod_mid, 10))
    ) %>%
      mutate(Caod_x = rep(Caod_range[2], 10))
  }, .id = "model") %>%
    group_by(model) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    mutate(across(c(fit, lwr, upr), ~ expm1(.x) * 100)) %>%
    mutate(Caod_x = Caod_x + Caod_offset)
  
  # Define custom color palette
  model_colors <- c(
    "base" = "#1f77b4",   # Blue
    "base2" = "#ff7f0e"   # Orange
  )
  
  # Create the main ggplot object
  p1 <- Caod %>%
    mutate(model = fct_relevel(model, rev(model_names))) %>%
    ggplot(aes(x = Caod_x, y = fit, color = model)) +
    
    # Add 95% confidence ribbon for the baseline model
    geom_ribbon(aes(x = Caod_x, ymin = lwr, ymax = upr, fill = model),
                inherit.aes = FALSE,
                alpha = 0.2,
                show.legend = FALSE,
                size = 0.01,
                data = . %>% filter(model %in% c("base"))) +
    
    # Add lines for both models
    geom_line(data = . %>% filter(model == "base"), size = 1.5, alpha = 1, linetype = "solid") +
    geom_line(data = . %>% filter(model == "base2"), size = 1.5, alpha = 1, linetype = "solid") +
    
    # Set model line colors
    scale_color_manual(values = model_colors) +
    
    # Add labels to the right of the plot
    geom_text_repel(aes(label = model),
                    data = Caod_lab,
                    show.legend = FALSE,
                    min.segment.length = 0,
                    box.padding = 1,
                    direction = "y",
                    nudge_x = 0.02,
                    nudge_y = 0.02,
                    xlim = c(Caod_range[2] + Caod_offset, Caod_range[2] * 1.1),
                    arrow = arrow,
                    max.overlaps = 1e2,
                    family = "Roboto Condensed",
                    size = 5,
                    seed = 2022) +
    
    # Axis and theme settings
    ylab("Percentage Change in SIF") +
    xlab("cAOD") +
    theme_half_open(18, font_family = "Roboto Condensed") +
    background_grid() +
    theme(
      ggside.panel.scale = 0.2,
      legend.title = element_blank(),
      legend.position = c(1.2, 0.5),
      panel.grid = element_blank(),
      axis.text.x = element_text(size = 50),
      axis.text.y = element_text(size = 50)
    ) +
    
    # Add marginal histogram showing CaOD distribution
    geom_xsidehistogram(aes(x = Caod),
                        data = adata,
                        bins = 80,
                        fill = "#D3D3D3",
                        color = "black",
                        size = 0.2,
                        inherit.aes = FALSE) +
    
    ggside(x.pos = "bottom", collapse = "x") +
    scale_xsidey_continuous(labels = NULL, breaks = NULL) +
    theme(ggside.panel.scale = 0.5) +
    scale_x_continuous(
      limits = c(Caod_range[1], Caod_range[2] + Caod_offset),
      expand = expansion(mult = c(0, 0.15))
    ) +
    scale_xsidey_continuous(labels = NULL, breaks = NULL) +
    labs(subtitle = "Crop response curves")
  
  return(p1)
}

# Generate plot object
plots <- get_plot(data)
print(plots)

# Create output folder if missing
if (!dir.exists("figures")) {
  dir.create("figures")
}

# Save figure as high-resolution TIFF
ggsave(
  "figures/crop/p2/response_climate_caod_Maize.tif", 
  plots, 
  width = 6, 
  height = 4.5, 
  dpi = 300,       # 300 DPI for print quality
  device = "tiff"
)
