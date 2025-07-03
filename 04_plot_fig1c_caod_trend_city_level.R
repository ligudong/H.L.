source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")

# Define region order for consistent plotting
order <- c(
  "North China", "Northeast China", "East China", "South China",
  "Central China", "Southwest China", "Northwest China"
)

# Load province boundaries and classify into regions
province_plot <- st_read("https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json") %>%
  mutate(region = fcase(
    name %in% c("辽宁省", "吉林省", "黑龙江省"), "Northeast China",
    name %in% c("上海市", "江苏省", "浙江省", "安徽省", "福建省", "江西省", "山东省", "台湾省"), "East China",
    name %in% c("北京市", "天津市", "河北省", "山西省", "内蒙古自治区"), "North China",
    name %in% c("河南省", "湖北省", "湖南省"), "Central China",
    name %in% c("广东省", "广西壮族自治区", "海南省", "香港特别行政区", "澳门特别行政区"), "South China",
    name %in% c("四川省", "贵州省", "云南省", "西藏自治区", "重庆市"), "Southwest China",
    name %in% c("陕西省", "甘肃省", "青海省", "宁夏回族自治区", "新疆维吾尔自治区"), "Northwest China"
  ))

# Aggregate provinces to regional boundaries
region_plot <- province_plot %>%
  st_make_valid() %>%
  group_by(region) %>%
  summarise(geometry = st_union(geometry))

# Load city boundaries
city <- province_plot %>%
  st_drop_geometry() %>%
  filter(adcode != "100000_JD") %>%
  transmute(
    adcode = fifelse(adcode %in% c("110000", "120000", "310000", "500000", "460000", "710000", "810000", "820000"), adcode, str_c(adcode, "_full")),
    data = map(adcode, ~ st_read(str_c("https://geo.datav.aliyun.com/areas_v3/bound/", .x, ".json")))
  ) %>%
  as_tibble() %>%
  bind_rows(
    . %>% filter(str_detect(adcode, "full")) %>% unnest(),
    . %>% filter(!str_detect(adcode, "full")) %>% unnest()
  ) %>%
  st_as_sf(sf_column_name = "geometry")

# Define data path and year range
tiff_folder <- "...data/raw/cAOD"
years <- 2001:2023

# Load CAOD TIFF stack
caod_files <- list.files(tiff_folder, pattern = "\\.tif$", full.names = TRUE)
caod_stack <- rast(caod_files)
names(caod_stack) <- as.character(years)

# Calculate trend (slope) for each pixel
caod_trend <- app(caod_stack, fun = function(x) {
  if (all(is.na(x))) return(NA)
  coef(lm(x ~ years, na.action = na.exclude))[2]
})

# Project trend raster to match city CRS
caod_trend <- project(caod_trend, crs(city))

# Extract average trend by city
extracted_trend <- exact_extract(caod_trend, city, "mean")
extracted <- bind_cols(city, extracted_trend) %>%
  rename(Caod_trend = `...12`) %>%
  mutate(area = units::drop_units(st_area(.)) / 1e6)

# Define custom color palette for CAOD trend visualization
custom_colors <- c("#ffe3bf", "#fcc785", "#f5af71", "#f49660", "#e47042", "#c94d33", "#9e2709")
color_values <- c(-0.01, -0.0083, -0.0066, -0.005, -0.0033, -0.0016, 0)

# Plot CAOD trend at city level
plot <- ggplot(extracted) +
  geom_sf(aes(fill = Caod_trend), color = NA) +
  geom_sf(data = region_plot, fill = NA, color = "black", size = 1.2) +
  scale_fill_gradientn(
    name = "CAOD Trend",
    colours = custom_colors,
    values = scales::rescale(color_values, to = c(0, 1)),
    limits = c(-0.01, 0),
    oob = scales::squish,
    na.value = "grey90",
    guide = guide_colorbar(title.position = "top", barwidth = 10, label.position = "bottom")
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.2, 0.2),
    legend.direction = "horizontal",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

# Save final plot
ggsave(
  filename = "E:/figures/crop/p1/figure1c_caod_trend_city_level.tif",
  plot = plot,
  width = 10, height = 8, units = "in", dpi = 600, device = "tiff"
)

# Optional: convert to raster and export
# ---- Calculate p-values from regression ----
caod_trend_p <- app(caod_stack, fun = function(x) {
  if (all(is.na(x)) || sum(!is.na(x)) < 2) return(c(NA, NA))
  model <- tryCatch(lm(x ~ years, na.action = na.exclude), error = function(e) return(NULL))
  if (is.null(model)) return(c(NA, NA))
  c(coef(model)[2], summary(model)$coefficients[2, 4])
})

caod_p_value <- project(caod_trend_p[[2]], crs(city))
extracted_p <- exact_extract(caod_p_value, city, "mean")

extracted <- bind_cols(city, extracted_p) %>%
  rename(caod_p_value = `...12`) %>%
  st_make_valid() %>%
  st_transform(crs = 4326)

# ---- Summary statistics and plot ----
p_value_summary <- extracted %>%
  st_drop_geometry() %>%
  summarise(
    `0 - 0.01` = sum(caod_p_value <= 0.01, na.rm = TRUE),
    `0 - 0.05` = sum(caod_p_value <= 0.05, na.rm = TRUE),
    `0 - 0.1`  = sum(caod_p_value <= 0.1, na.rm = TRUE),
    `> 0.1`    = sum(caod_p_value > 0.1, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "p_value_category", values_to = "count") %>%
  mutate(p_value_category = factor(p_value_category, levels = c("0 - 0.01", "0 - 0.05", "0 - 0.1", "> 0.1")))

custom_colors <- c("0 - 0.01" = "#FF0000", "0 - 0.05" = "#FF5555", "0 - 0.1" = "#FF9999", "> 0.1" = "#D3D3D3")

p <- ggplot(p_value_summary, aes(x = p_value_category, y = count, fill = p_value_category)) +
  geom_col(width = 0.7, color = "white") +
  scale_fill_manual(values = custom_colors) +
  labs(
    title = "Distribution of p-values for CAOD Trend",
    x = "p-value range",
    y = "Number of cities",
    fill = "p-value range"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "top",
    panel.grid.major = element_blank()
  )

ggsave("E:/figures/crop/p1/figure1c_caod_p_value_distribution.tif", plot = p,
       width = 10, height = 3, units = "in", dpi = 600)
