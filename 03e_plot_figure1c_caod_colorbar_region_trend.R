source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")

# Load province boundary data and classify into regions --------------------------
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

# Aggregate province boundaries to region-level geometries
region_plot <- province_plot %>%
  st_make_valid() %>%
  group_by(region) %>%
  summarise(geometry = st_union(geometry))

# Define path to CAOD TIFF files and year range --------------------------
tiff_folder <- "E:/naturefood/data/re caiyang/Caod"
years <- 2001:2023

# Initialize empty data frame to store results
all_region_data <- data.frame()

# Loop through each year to read TIFFs and compute region means ----------
for (year in years) {
  tiff_path <- file.path(tiff_folder, paste0("south_", year, ".tif"))
  if (!file.exists(tiff_path)) {
    warning(paste("File does not exist:", tiff_path))
    next
  }
  Caod_raster <- rast(tiff_path)
  year_mean_values <- exact_extract(Caod_raster, region_plot, "mean")
  year_mean_df <- data.frame(region = region_plot$region, year = year, mean_Caod = year_mean_values)
  all_region_data <- bind_rows(all_region_data, year_mean_df)
}

# Clean and restructure --------------------------------------------------
all_region_data <- na.omit(all_region_data)

# Compute national average ("China")
china_data <- all_region_data %>%
  group_by(year) %>%
  summarise(mean_Caod = mean(mean_Caod, na.rm = TRUE)) %>%
  mutate(region = "China")

# Combine national average with all region data
all_region_data <- bind_rows(all_region_data, china_data)

# Reshape to wide format for inspection (optional)
wide_region_data <- all_region_data %>%
  pivot_wider(names_from = region, values_from = mean_Caod)

# Subset by region ------------------------------------------------------
region_data_list <- split(all_region_data, all_region_data$region)

# Output path for color bars
output_path <- "E:/figures/crop/p1/颜色条/"

# Generate color bar plots for each region -------------------------------
for (region in names(region_data_list)) {
  region_data <- region_data_list[[region]]
  if (nrow(region_data) == 0) {
    warning(paste("No data available for", region))
    next
  }
  p_Caod_color_bar <- ggplot(region_data, aes(x = year, y = 1, fill = mean_Caod)) +
    geom_tile(height = 0.1, width = 1) +
    scale_fill_gradientn(
      colors = c("#ffe6b2", "#ffcf58", "#ffa742", "#ff822d", "#c6473e", "#742614"),
      limits = c(0.03, 0.15),
      breaks = seq(0, 0.15, by = 0.02),
      labels = seq(0, 0.15, by = 0.02),
      name = "CAOD Value"
    ) +
    labs(
      x = "Year",
      y = NULL,
      title = paste(region, "CAOD Color Bar")
    ) +
    scale_y_continuous(expand = c(0, 0), limits = c(0.95, 1.05)) +
    scale_x_continuous(breaks = seq(2001, 2023, by = 3), limits = c(2001, 2023)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12, vjust = 1),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      panel.grid = element_blank(),
      panel.background = element_blank(),
      plot.margin = margin(5, 5, 5, 5)
    )
  
  print(p_Caod_color_bar)
  
  output_file <- paste0(output_path, "Caod_Color_Bar_", gsub(" ", "_", region), "_2001_2023.tif")
  ggsave(output_file, plot = p_Caod_color_bar, width = 10, height = 2, dpi = 300, device = "tiff")
}
