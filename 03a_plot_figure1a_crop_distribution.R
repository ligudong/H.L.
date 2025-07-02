source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")

# ----------------------------------------
# Step 1: Load crop mask raster files
# ----------------------------------------

rice  <- raster("E:/data/inputs/data_archive/作物/mask_Rice.tif")
wheat <- raster("E:/data/inputs/data_archive/作物/Mask_Wheat.tif")
maize <- raster("E:/data/inputs/data_archive/作物/Mask_Maize.tif")

# Convert raster to data frames (with coordinates)
rice_df  <- as.data.frame(rice,  xy = TRUE)
wheat_df <- as.data.frame(wheat, xy = TRUE)
maize_df <- as.data.frame(maize, xy = TRUE)

# Rename third column to indicate crop type
names(rice_df)[3]  <- "Rice"
names(wheat_df)[3] <- "Wheat"
names(maize_df)[3] <- "Maize"

# Merge all data into one long-format dataframe
combined_df <- rice_df %>%
  full_join(wheat_df, by = c("x", "y")) %>%
  full_join(maize_df, by = c("x", "y"))

combined_long <- combined_df %>%
  pivot_longer(cols = c(Rice, Wheat, Maize),
               names_to = "Crop", values_to = "Value") %>%
  filter(!is.na(Value))  # Remove NA values

# ----------------------------------------
# Step 2: Load province boundaries and generate regions
# ----------------------------------------

# Regional order for later reference
region_order <- c("North China", "Northeast China", "East China", "South China",
                  "Central China", "Southwest China", "Northwest China")

# Load Chinese provincial boundaries and classify into major regions
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

# Aggregate provincial geometries into regions
region_plot <- province_plot %>%
  st_make_valid() %>%
  group_by(region) %>%
  summarise(geometry = st_union(geometry))

# ----------------------------------------
# Step 3: Plotting
# ----------------------------------------

crop_plot <- ggplot() +
  geom_sf(data = region_plot, aes(fill = region), color = "black", alpha = 0.3) +  # Region background
  geom_tile(data = combined_long, aes(x = x, y = y, fill = Crop)) +               # Crop overlay
  scale_fill_manual(
    values = c(
      "Rice"  = "#0050B350",
      "Maize" = "#FFA94050",
      "Wheat" = "#D4880680"
    ),
    na.value = "#D6E4FF"
  ) +
  labs(title = "Crop distribution in China (2019)", fill = "Crop Type") +
  theme_minimal() +
  theme(
    plot.background    = element_rect(fill = "transparent", color = NA),
    panel.background   = element_rect(fill = "transparent", color = NA),
    legend.position    = "bottom",
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text          = element_blank(),
    axis.ticks         = element_blank(),
    axis.title         = element_blank(),
    axis.line          = element_blank()
  )

# Display plot
print(crop_plot)

# ----------------------------------------
# Step 4: Export as TIFF
# ----------------------------------------

ggsave(
  filename = ".../figures/p1/03a_plot_figure1a_crop_distribution_2019.tif",
  plot     = crop_plot,
  device   = "tiff",
  dpi      = 300,
  width    = 8,
  height   = 6,
  bg       = "transparent",
  compression = "lzw"
)
