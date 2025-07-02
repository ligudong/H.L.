
source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")

# Define the common plotting function
plot_combined_surface <- function(faod_file, o3_file, output_file, zlim = c(-12, 12)) {
  # Load fAOD and ozone response values
  Faod <- read.csv(faod_file)
  ozone <- read.csv(o3_file)
  
  # Create all combinations of Faod_level and peak_level
  combined_data <- expand.grid(Faod_level = Faod$Faod_level,
                               peak_level = ozone$peak_level)
  
  # Merge with respective values
  combined_data <- combined_data %>%
    left_join(Faod, by = "Faod_level") %>%
    left_join(ozone[, c("peak_level", "value")], by = "peak_level") %>%
    rename(value1 = value.x, value2 = value.y)
  
  # Calculate the combined effect
  combined_data <- combined_data %>%
    mutate(combined_value = value1 + value2) %>%
    select(Faod_level, peak_level, combined_value)
  
  # Define diverging color scale: blue (negative) to red (positive)
  colors_blue_to_red <- c(
    "#061178", "#10239E", "#1D39C4", "#4465EB", "#6682F5", "#8BA2FF", "#ADC6FF", "#D6E4FF", "#F0F5FF",
    "#FFE4D6", "#FFD2B9", "#FFB186", "#FF8F50", "#FF762A", "#F55C08", "#D94F03", "#AD2102", "#871400"
  )
  
  # Create custom break positions for centered 0
  color_positions <- rescale(c(
    seq(zlim[1], 0, length.out = 9),
    seq(1, zlim[2], length.out = 9)
  ), from = zlim)
  
  # Plot
  p <- ggplot(combined_data, aes(x = Faod_level, y = peak_level, z = combined_value)) +
    geom_tile(aes(fill = combined_value)) +
    geom_contour(color = "grey70", size = 0.5) +
    geom_contour(breaks = 0, color = "black", size = 1) +
    scale_fill_gradientn(colors = colors_blue_to_red,
                         values = color_positions,
                         limits = zlim) +
    labs(x = "FaOD Level", y = "Ozone Peak Level", fill = "Combined Impact (%)") +
    theme_minimal()
  
  # Save output
  ggsave(output_file, plot = p, width = 8, height = 6, dpi = 300, units = "in")
}

# Apply for each crop with appropriate limits
plot_combined_surface(
  faod_file = "E:/figures/crop/P6/Faod_Maize_data.csv",
  o3_file   = "E:/figures/crop/P6/O3_Maize_data.csv",
  output_file = "figures/figure2c/combined_effect_surface_maize.tif",
  zlim = c(-50, 30)
)

plot_combined_surface(
  faod_file = "E:/figures/crop/P6/Faod_Rice_data.csv",
  o3_file   = "E:/figures/crop/P6/O3_Rice_data.csv",
  output_file = "figures/figure2c/combined_effect_surface_rice.tif",
  zlim = c(-50, 30)
)

plot_combined_surface(
  faod_file = "E:/figures/Faod_Wheat_data.csv",
  o3_file   = "E:/figures/O3_Wheat_data.csv",
  output_file = "figures/figure4/combined_effect_surface_wheat.tif",
  zlim = c(-50, 30)
)
