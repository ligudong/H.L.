# Load required packages and functions
source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")

# Set O₃ raster folder path and year range
tiff_folder <- "...data/raw/O3"
years <- 2001:2023

# Load Chinese provincial boundaries and assign to regions
province_plot <- st_read("https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json") %>%
  mutate(
    region = fcase(
      name %in% c("辽宁省", "吉林省", "黑龙江省"), "Northeast China",
      name %in% c("上海市", "江苏省", "浙江省", "安徽省", "福建省", "江西省", "山东省", "台湾省"), "East China",
      name %in% c("北京市", "天津市", "河北省", "山西省", "内蒙古自治区"), "North China",
      name %in% c("河南省", "湖北省", "湖南省"), "Central China",
      name %in% c("广东省", "广西壮族自治区", "海南省", "香港特别行政区", "澳门特别行政区"), "South China",
      name %in% c("四川省", "贵州省", "云南省", "西藏自治区", "重庆市"), "Southwest China",
      name %in% c("陕西省", "甘肃省", "青海省", "宁夏回族自治区", "新疆维吾尔自治区"), "Northwest China"
    )
  )

# Merge provinces into regional geometry
region_plot <- province_plot %>%
  st_make_valid() %>%
  group_by(region) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# Initialize result table
all_region_data <- data.frame()

# Loop through each year to extract region-wise O₃ mean
for (year in years) {
  tiff_path <- file.path(tiff_folder, paste0("o3_", year, ".tif"))
  if (!file.exists(tiff_path)) {
    warning(paste("Missing file:", tiff_path))
    next
  }
  
  O3_raster <- rast(tiff_path)
  mean_values <- exact_extract(O3_raster, region_plot, "mean")
  
  year_df <- data.frame(
    region = region_plot$region,
    year = year,
    mean_O3 = mean_values
  )
  
  all_region_data <- bind_rows(all_region_data, year_df)
}

# Remove NA
all_region_data <- na.omit(all_region_data)

# National average
china_data <- all_region_data %>%
  group_by(year) %>%
  summarise(mean_O3 = mean(mean_O3, na.rm = TRUE), .groups = "drop") %>%
  mutate(region = "China")

all_region_data <- bind_rows(all_region_data, china_data)
region_data_list <- split(all_region_data, all_region_data$region)

# Output folder
output_path <- "E:/figures/crop/p1/colorbar/O3/"

# Custom O₃ color palette
custom_colors <- c("#ffe0e9", "#ffb1c9", "#ff81ac", "#e8817d", "#be5958")
value_range <- c(40, 80)  # Adjust if needed

# Generate and save colorbars
for (region in names(region_data_list)) {
  region_data <- region_data_list[[region]]
  if (nrow(region_data) == 0) next
  
  p_o3_color_bar <- ggplot(region_data, aes(x = year, y = 1, fill = mean_O3)) +
    geom_tile(height = 0.1, width = 1) +
    scale_fill_gradientn(
      colors = custom_colors,
      limits = value_range,
      breaks = seq(value_range[1], value_range[2], by = 10),
      labels = seq(value_range[1], value_range[2], by = 10),
      name = "O₃ (ppb)"
    ) +
    labs(x = "Year", y = NULL, title = paste(region, "O₃ Color Bar")) +
    scale_y_continuous(expand = c(0, 0), limits = c(0.95, 1.05)) +
    scale_x_continuous(breaks = seq(2001, 2023, by = 3), limits = c(2001, 2023)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      panel.grid = element_blank(),
      panel.background = element_blank(),
      plot.margin = margin(5, 5, 5, 5)
    )
  
  ggsave(
    filename = paste0(output_path, "O3_Color_Bar_", gsub(" ", "_", region), "_2005_2022.tif"),
    plot = p_o3_color_bar,
    width = 10, height = 2, dpi = 300
  )
}
