# Load the necessary packages and functions
source("script/00_loadPackages.R")
source("script/loadFunctions.R")
source("script/loadFormulas.R")

# Load the data
data <- get_data()  # This function retrieves the data

# Handle NA values by dropping rows with NA
data <- drop_na(data)

# Filter the data for specific crop types (Rice)
data_rice <- data %>% filter(crop %in% c("Rice(LR)", "Rice(SR&ER)"))

# Interaction Analysis Section ----------------------------------------------------------------------------------
ozone <- qread("data/impacts_ozone_AOT40.qs", nthreads = qn)  # Load ozone impact data

# Replace AOT40 with ozone variable in the data
ozone <-
  ozone %>%
  filter(year %in% 2001:2022) %>%
  mutate(ozone_results = map(ozone_results, function(adata) {
    # Reshape data from long to wide format
    temp <- adata %>%
      pivot_wider(names_from = peak_level, values_from = contains("%")) %>%
      rast(type = "xyz", crs = "epsg:4326")  # Convert to raster format
    
    fraction <- temp[["fraction"]]  # Extract fraction
    temp <- temp["%"]  # Extract the percentage data
    
    # Calculate province-level statistics using exact extraction
    province_level <-
      exact_extract(temp,
                    province,
                    "weighted_mean",
                    weights = fraction,
                    progress = F
      ) %>%
      bind_cols(province, .) %>%
      st_drop_geometry() %>%
      pivot_longer(
        contains("%"),
        names_prefix = "weighted_mean.",
        names_to = c("name", "peak_level"),
        names_sep = "_"
      ) %>%
      pivot_wider()
    
    # Calculate region-level statistics using exact extraction
    region_level <- exact_extract(temp,
                                  region,
                                  "weighted_mean",
                                  weights = fraction,
                                  progress = F
    ) %>%
      bind_cols(region, .) %>%
      st_drop_geometry() %>%
      pivot_longer(
        contains("%"),
        names_prefix = "weighted_mean.",
        names_to = c("name", "peak_level"),
        names_sep = "_"
      ) %>%
      pivot_wider()
    
    lst(province_level, region_level)  # Return results for both levels
  }, .progress = T))

# Save the processed ozone data to disk
saveRDS(ozone, "data/impacts_ozone_summarised_faod.rds")

# Read the processed ozone data back from disk
ozone <- read_rds("data/impacts_ozone_summarised_faod.rds")

# Plotting function for ozone impact across different crops
ozone_plot <- function(acrop, plot_legend, subtitle) {
  # Plot at the province level
  p1 <- ozone %>%
    filter(crop_parent == acrop) %>%
    unnest_wider(ozone_results) %>%
    select(crop, year, crop_parent, province_level) %>%
    unnest() %>%
    group_by(crop_parent, 省, peak_level) %>%
    summarise(across(contains("%"), fmean), .groups = "drop") %>%
    filter(peak_level == 100) %>%
    inner_join(province_plot, c("省" = "name")) %>%
    ggplot() +
    geom_sf(
      aes(fill = `50%` * 1e2, geometry = geometry),
      size = 0.2,
      show.legend = plot_legend,
      color = NA
    ) +
    geom_sf(
      aes(color = ""),
      data = region_plot,
      fill = NA,
      size = 0.8,
      show.legend = plot_legend
    ) +
    scale_fill_gradientn(
      name = TeX("Percentage change in SIF"),
      limits = c(-1, 3),
      oob = squish,
      colours = c(
        "#1D39C4", "#8BA2FF", "#91D5FF", "#BAE7FE", "white", "#FFCCD1", 
        "#FF929D", "#E65E67", "#B8292F", "#800D00"
      ),
      na.value = "grey90",
      guide = guide_colorbar(
        title.position = "top",
        barwidth = 15,
        label.position = "bottom",
        order = 1
      )
    ) +
    xlab(NULL) +
    ylab(NULL) +
    theme_ipsum_rc(
      panel_spacing = grid::unit(0, "lines"),
      plot_margin = margin(0, 0, 0, 0),
      grid = F,
      axis = F,
      base_size = 15,
      axis_title_size = 15,
      strip_text_size = 20,
    ) +
    theme(
      legend.position = c(0.2, 0.1),
      legend.direction = "horizontal",
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      plot.tag = element_text(size = 50),
      legend.title.align = 0.5
    ) +
    scale_colour_manual(
      values = NA,
      guide = guide_legend("No data", order = 2)
    ) +
    labs(tag = subtitle) +
    ggnewscale::new_scale_color() +
    geom_sf(
      aes(color = factor(region, order)),
      inherit.aes = F,
      show.legend = F,
      data = region_plot %>% st_centroid() %>% filter(!is.na(region)),
      stroke = 2,
      fill = "white",
      shape = 21,
      size = 2
    ) +
    scale_color_manual(
      values = MetBrewer::met.brewer("Juarez", n = 7),
      guide = NULL
    )
  
  # Plot at the region level
  p2 <- ozone %>%
    filter(crop_parent == acrop) %>%
    unnest_wider(ozone_results) %>%
    select(crop, year, crop_parent, region_level) %>%
    unnest() %>%
    drop_na() %>%
    group_by(crop_parent, region, peak_level) %>%
    summarise(across(contains("%"), fmean), .groups = "drop") %>%
    mutate(
      peak_level = as.integer(peak_level),
      region = factor(region, order),
      across(contains("%"), ~ .x * 1e2)
    ) %>%
    filter(peak_level >= 50, peak_level <= 150) %>%
    ggplot(aes(
      x = peak_level,
      y = `50%`,
      color = region
    )) +
    facet_manual(
      vars(region),
      design = design,
      scales = "free_y",
      strip = strip_themed(text_x = map(
        MetBrewer::met.brewer("Juarez", n = 7) %>% append("black", 3),
        ~ element_text(color = .x)
      ))
    ) +
    geom_vline(xintercept = 100, linetype = "dashed") +
    geom_interval(
      aes(ymin = `5%`, ymax = `95%`),
      alpha = 0.5,
      size = 2,
      show.legend = FALSE
    ) +
    geom_line(show.legend = FALSE) +
    geom_point(show.legend = FALSE, color = "black") +
    geom_text(
      data = . %>% filter(peak_level == 100),   # Label only the peak=100 points
      aes(label = sprintf("%.2f", `50%`)),       # Display 2 decimal places
      hjust = -0.05,                              # Slightly shift the label to the right
      vjust = -0.2,                                 # Keep vertical alignment
      size = 6,                                  # Font size
      color = "black",                           # Font color
      show.legend = FALSE
    ) +
    scale_color_manual(values = MetBrewer::met.brewer("Juarez", n = 7) %>% append("black", 3)) +
    theme_half_open(16, font_family = "Roboto Condensed") +
    theme(
      panel.grid = element_blank(),
      strip.background = element_blank(),
      axis.title = element_text(size = 25),
      axis.text.x = element_text(size = 12)
    ) +
    xlab(TeX("Annual O$_{3}$ ($\\mu g$ $m^{-3}$)")) +
    ylab("Percentage change in SIF") +
    scale_x_continuous(
      limits = c(70, 120),
      breaks = seq(70, 120, 10)
    )
  
  return(lst(p1, p2))  # Return both plots as a list
}

# Generate plots for each crop (Maize, Rice, Wheat)
ap <- pmap(list(unique(ozone$crop_parent), c(F, T, F), letters[1:3]), ozone_plot)

# Modify specific plots (for Wheat, in this case)
ap[[2]]$p1 <- ap[[2]]$p1 +
  geom_text_npc(
    aes(npcx = 0.28, npcy = 0.04, label = "Gain"),
    family = "Roboto Condensed",
    size = 10,
    color = WrensBookshelf::WB_brewer("BabyWrenAndTheGreatGift", direction = -1)[9]
  ) +
  geom_text_npc(
    aes(npcx = 0.04, npcy = 0.04, label = "Loss"),
    family = "Roboto Condensed",
    size = 10,
    color = WrensBookshelf::WB_brewer("BabyWrenAndTheGreatGift", direction = -1)[1]
  )

# Combine the two plots into one and save the result
patch <- map(
  c(ap),
  ~ .x$p1 + inset_element(
    .x$p2,
    left = 0,
    bottom = 0,
    right = 1,
    top = 1
  )
) %>%
  wrap_plots(ncol = 2, byrow = F)

# Print the final combined plot
print(patch)

# Save the plot as a high-resolution PDF
ggsave(
  "figures/hist_impact_ozone_map1.pdf",
  patch,
  width = 2.1,
  height = 3,
  scale = 9
)

# Non-interaction Section -------------------------------------------------------------------------------------
ozone <- qread("data/impacts_ozone_AOT40_nojiaohu.qs", nthreads = qn)  # Load ozone data without interaction
# Non-interaction Section -------------------------------------------------------------------------------------
ozone <- qread("data/impacts_ozone_AOT40_nojiaohu.qs", nthreads = qn)  # Load ozone data without interaction

# Replace AOT40 variable with ozone in the data
ozone <-
  ozone %>%
  filter(year %in% 2001:2022) %>%
  mutate(ozone_results = map(ozone_results, function(adata) {
    # Reshape the data from long to wide format
    temp <- adata %>%
      pivot_wider(names_from = peak_level, values_from = contains("%")) %>%
      rast(type = "xyz", crs = "epsg:4326")  # Convert to raster format
    
    fraction <- temp[["fraction"]]  # Extract the fraction data
    temp <- temp["%"]  # Extract the percentage data
    
    # Calculate province-level statistics using exact extraction
    province_level <-
      exact_extract(temp,
                    province,
                    "weighted_mean",
                    weights = fraction,
                    progress = F
      ) %>%
      bind_cols(province, .) %>%
      st_drop_geometry() %>%
      pivot_longer(
        contains("%"),
        names_prefix = "weighted_mean.",
        names_to = c("name", "peak_level"),
        names_sep = "_"
      ) %>%
      pivot_wider()
    
    # Calculate region-level statistics using exact extraction
    region_level <- exact_extract(temp,
                                  region,
                                  "weighted_mean",
                                  weights = fraction,
                                  progress = F
    ) %>%
      bind_cols(region, .) %>%
      st_drop_geometry() %>%
      pivot_longer(
        contains("%"),
        names_prefix = "weighted_mean.",
        names_to = c("name", "peak_level"),
        names_sep = "_"
      ) %>%
      pivot_wider()
    
    lst(province_level, region_level)  # Return both level results
  }, .progress = T))

# Save the processed non-interaction ozone data
saveRDS(ozone, "data/impacts_ozone_summarised_nojiaohu.rds")

# Read the processed ozone data back
ozone <- read_rds("data/impacts_ozone_summarised_nojiaohu.rds")

# Function to generate the plots for the non-interaction data
ozone_plot <- function(acrop, plot_legend, subtitle) {
  # Plot at the province level
  p1 <- ozone %>%
    filter(crop_parent == acrop) %>%
    unnest_wider(ozone_results) %>%
    select(crop, year, crop_parent, province_level) %>%
    unnest() %>%
    group_by(crop_parent, 省, peak_level) %>%
    summarise(across(contains("%"), fmean), .groups = "drop") %>%
    filter(peak_level == 100) %>%
    inner_join(province_plot, c("省" = "name")) %>%
    ggplot() +
    geom_sf(
      aes(fill = `50%` * 1e2, geometry = geometry),
      size = 0.2,
      show.legend = plot_legend,
      color = NA
    ) +
    geom_sf(
      aes(color = ""),
      data = region_plot,
      fill = NA,
      size = 0.8,
      show.legend = plot_legend
    ) +
    scale_fill_gradientn(
      name = TeX("Percentage change in SIF"),
      limits = c(-1, 3),
      oob = squish,
      colours = c(
        "#1D39C4", "#8BA2FF", "#91D5FF", "#BAE7FE", "white", "#FFCCD1", 
        "#FF929D", "#E65E67", "#B8292F", "#800D00"
      ),
      na.value = "grey90",
      guide = guide_colorbar(
        title.position = "top",
        barwidth = 15,
        label.position = "bottom",
        order = 1
      )
    ) +
    xlab(NULL) +
    ylab(NULL) +
    theme_ipsum_rc(
      panel_spacing = grid::unit(0, "lines"),
      plot_margin = margin(0, 0, 0, 0),
      grid = F,
      axis = F,
      base_size = 15,
      axis_title_size = 15,
      strip_text_size = 20,
    ) +
    theme(
      legend.position = c(0.2, 0.1),
      legend.direction = "horizontal",
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      plot.tag = element_text(size = 50),
      legend.title.align = 0.5
    ) +
    scale_colour_manual(
      values = NA,
      guide = guide_legend("No data", order = 2)
    ) +
    labs(tag = subtitle) +
    ggnewscale::new_scale_color() +
    geom_sf(
      aes(color = factor(region, order)),
      inherit.aes = F,
      show.legend = F,
      data = region_plot %>% st_centroid() %>% filter(!is.na(region)),
      stroke = 2,
      fill = "white",
      shape = 21,
      size = 2
    ) +
    scale_color_manual(
      values = MetBrewer::met.brewer("Juarez", n = 7),
      guide = NULL
    )
  
  # Plot at the region level
  p2 <- ozone %>%
    filter(crop_parent == acrop) %>%
    unnest_wider(ozone_results) %>%
    select(crop, year, crop_parent, region_level) %>%
    unnest() %>%
    drop_na() %>%
    group_by(crop_parent, region, peak_level) %>%
    summarise(across(contains("%"), fmean), .groups = "drop") %>%
    mutate(
      peak_level = as.integer(peak_level),
      region = factor(region, order),
      across(contains("%"), ~ .x * 1e2)
    ) %>%
    filter(peak_level >= 50, peak_level <= 150) %>%
    ggplot(aes(
      x = peak_level,
      y = `50%`,
      color = region
    )) +
    facet_manual(
      vars(region),
      design = design,
      scales = "free_y",
      strip = strip_themed(text_x = map(
        MetBrewer::met.brewer("Juarez", n = 7) %>% append("black", 3),
        ~ element_text(color = .x)
      ))
    ) +
    geom_vline(xintercept = 100, linetype = "dashed") +
    geom_interval(
      aes(ymin = `5%`, ymax = `95%`),
      alpha = 0.5,
      size = 2,
      show.legend = FALSE
    ) +
    geom_line(show.legend = FALSE) +
    geom_point(show.legend = FALSE, color = "black") +
    geom_text(
      data = . %>% filter(peak_level == 100),   # Label only the peak=100 points
      aes(label = sprintf("%.2f", `50%`)),       # Display 2 decimal places
      hjust = -0.05,                              # Slightly shift the label to the right
      vjust = -0.2,                                 # Keep vertical alignment
      size = 6,                                  # Font size
      color = "black",                           # Font color
      show.legend = FALSE
    ) +
    scale_color_manual(values = MetBrewer::met.brewer("Juarez", n = 7) %>% append("black", 3)) +
    theme_half_open(16, font_family = "Roboto Condensed") +
    theme(
      panel.grid = element_blank(),
      strip.background = element_blank(),
      axis.title = element_text(size = 25),
      axis.text.x = element_text(size = 12)
    ) +
    xlab(TeX("Annual O$_{3}$ ($\\mu g$ $m^{-3}$)")) +
    ylab("Percentage change in SIF") +
    scale_x_continuous(
      limits = c(70, 120),
      breaks = seq(70, 120, 10)
    )
  
  return(lst(p1, p2))  # Return both plots as a list
}

# Generate plots for each crop (Maize, Rice, Wheat)
ap <- pmap(list(unique(ozone$crop_parent), c(F, T, F), letters[1:3]), ozone_plot)

# Modify specific plots (for Wheat, in this case)
ap[[2]]$p1 <- ap[[2]]$p1 +
  geom_text_npc(
    aes(npcx = 0.28, npcy = 0.04, label = "Gain"),
    family = "Roboto Condensed",
    size = 10,
    color = WrensBookshelf::WB_brewer("BabyWrenAndTheGreatGift", direction = -1)[9]
  ) +
  geom_text_npc(
    aes(npcx = 0.04, npcy = 0.04, label = "Loss"),
    family = "Roboto Condensed",
    size = 10,
    color = WrensBookshelf::WB_brewer("BabyWrenAndTheGreatGift", direction = -1)[1]
  )

# Combine the two plots into one and save the result
patch <- map(
  c(ap),
  ~ .x$p1 + inset_element(
    .x$p2,
    left = 0,
    bottom = 0,
    right = 1,
    top = 1
  )
) %>%
  wrap_plots(ncol = 2, byrow = F)

# Print the final combined plot
print(patch)

# Save the plot as a high-resolution PDF
ggsave(
  "figures/sfig/hist_impact_ozone_map_nojiaohu.pdf",
  patch,
  width = 2.1,
  height = 3,
  scale = 9
)

