# Load required packages and user-defined functions
source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")
source("scrip/loadFormulas.R")

# Load preprocessed dataset
data <- get_data()

data <- drop_na(data)
data <- data %>% filter(crop == "Maize")
#data_rice <- data %>% filter(crop %in% c("Rice(LR)", "Rice(SR&ER)"))
#data <- data %>% filter(crop == "Wheat")


# Define a list of model names for comparison
model_names <- c(
  "base",         # baseline model
  "no_jiaohu",    # without interaction between fAOD and ozone
  "linear",       # model using linear soil moisture
  "noozone",      # model without ozone terms
  "noCaod",       # model excluding coarse AOD
  "noFaod"        # model excluding fine AOD
)

# Define plotting function that fits and compares multiple models
get_plot <- function(adata) {
  message("Processing crop data")
  message("fAOD summary: ", summary(adata$Faod))
  
  # Remove NA values
  data_modeling <- na.omit(adata)
  
  # Validate fAOD range before fitting
  Faod_range <- range(adata$Faod, na.rm = TRUE)
  if (!is.finite(Faod_range[1]) || !is.finite(Faod_range[2]) || Faod_range[1] == Faod_range[2]) {
    warning("Invalid fAOD range; skipping plot.")
    return(NULL)
  }
  
  # Fit all model variants using fixed effects and sampling weights
  models <- lst(
    fml_base, fml_no_jiaohu, fml_base_lin, fml_no_O3, fml_no_Caod, fml_no_Faod
  ) %>%
    map(~ feols(.x, data_modeling, cluster = ~city, weights = ~fraction, nthreads = 0, lean = TRUE), .progress = TRUE) %>%
    set_names(model_names)
  
  # Create prediction grid over observed fAOD range
  Faod_hist <- seq(Faod_range[1], Faod_range[2], 0.001)
  Faod_mid <- round(quantile(adata$Faod, 0.5, na.rm = TRUE), 2)
  
  # Predict relative impact for each model
  Faod <- map_dfr(models, function(amodel) {
    relpred(
      amodel,
      tibble(Faod = Faod_hist),
      tibble(Faod = rep(Faod_mid, length(Faod_hist)))
    ) %>%
      mutate(Faod_x = Faod_hist)
  }, .id = "model") %>%
    mutate(across(c(fit, lwr, upr), ~ expm1(.x) * 100))  # Convert to % change
  
  # Prepare labels for each curve
  Faod_offset <- (Faod_range[2] - Faod_range[1]) * 0.01
  Faod_lab <- map_dfr(models, function(amodel) {
    relpred(
      amodel,
      tibble(Faod = rep(Faod_range[2], 10)),
      tibble(Faod = rep(Faod_mid, 10))
    ) %>%
      mutate(Faod_x = rep(Faod_range[2], 10))
  }, .id = "model") %>%
    group_by(model) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    mutate(across(c(fit, lwr, upr), ~ expm1(.x) * 100)) %>%
    mutate(Faod_x = Faod_x + Faod_offset)
  
  # Define color palette for model lines
  model_colors <- c(
    "base" = "#1f77b4",
    "no_jiaohu" = "#ff7f0e",
    "linear" = "#2ca02c",
    "noozone" = "#d62728",
    "noCaod" = "#9467bd",
    "noFaod" = "#8c564b"
  )
  
  # Create main plot with ribbon and line comparisons
  p1 <- Faod %>%
    mutate(model = fct_relevel(model, rev(model_names))) %>%
    ggplot(aes(x = Faod_x, y = fit, color = model)) +
    geom_ribbon(aes(x = Faod_x, ymin = lwr, ymax = upr, fill = model),
                inherit.aes = FALSE,  
                alpha = 0.2,
                show.legend = FALSE,
                size = 0.01,
                data = . %>% filter(model %in% c("base"))) +
    geom_line(data = . %>% filter(model == "base"), size = 1.5, alpha = 1) +
    geom_line(data = . %>% filter(model == "no_jiaohu"), size = 1.5, alpha = 1) +
    geom_line(data = . %>% filter(model %in% c("linear", "noozone", "noCaod", "noFaod")), 
              size = 1.5, alpha = 0.2) +
    scale_color_manual(values = model_colors) +
    geom_text_repel(aes(label = model),
                    data = Faod_lab,
                    show.legend = FALSE,
                    min.segment.length = 0,
                    box.padding = 1,
                    direction = "y",
                    nudge_x = 0.02,
                    nudge_y = 0.02,
                    xlim = c(Faod_range[2] + Faod_offset, Faod_range[2] * 1.1),
                    arrow = arrow,
                    max.overlaps = 1e2,
                    family = "Roboto Condensed",
                    size = 5,
                    seed = 2022) +
    ylab("Percentage Change in SIF") +
    xlab("fAOD") +
    theme_half_open(18, font_family = "Roboto Condensed") +
    background_grid() +
    theme(
      ggside.panel.scale = 0.2,
      legend.title = element_blank(),
      legend.position = c(1.2, 0.5),
      panel.grid = element_blank()
    ) +
    # Add marginal histogram for fAOD distribution
    geom_xsidehistogram(aes(x = Faod),
                        data = adata,
                        bins = 80,
                        fill = "#D3D3D3",
                        color = "black",
                        size = 0.2,
                        inherit.aes = FALSE) +
    ggside(x.pos = "bottom", collapse = "x") +
    scale_xsidey_continuous(labels = NULL, breaks = NULL) +
    theme(ggside.panel.scale = 0.5) +
    scale_x_continuous(limits = c(Faod_range[1], Faod_range[2] + Faod_offset),
                       expand = expansion(mult = c(0, 0.15))) +
    labs(subtitle = "Crop response curves")
  
  return(p1)
}

# Generate plots
plots <- get_plot(data)
print(plots)

# Create output directory if it doesn't exist
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)

# Save plots to disk (PDF and TIFF formats)
ggsave("figures/crop/p2/response_climate_faod_Maize.tif", plots, width = 6, height = 4.5, dpi = 300, device = "tiff")
