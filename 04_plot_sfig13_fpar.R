source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")

# Define a vector for region ordering, to be used later for sorting
order <- c(
  "North China", "Northeast China", "East China", "South China",
  "Central China", "Southwest China", "Northwest China"
)

# Read and prepare the provincial boundary data of China from an open data source
province_plot <- st_read("https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json") %>%
  st_make_valid() %>%  # Ensure valid geometries
  mutate(region = fcase(  # Assign regions based on province names
    name %in% c("辽宁省", "吉林省", "黑龙江省"), "Northeast China",
    name %in% c("上海市", "江苏省", "浙江省", "安徽省", "福建省", "江西省", "山东省", "台湾省"), "East China",
    name %in% c("北京市", "天津市", "河北省", "山西省", "内蒙古自治区"), "North China",
    name %in% c("河南省", "湖北省", "湖南省"), "Central China",
    name %in% c("广东省", "广西壮族自治区", "海南省", "香港特别行政区", "澳门特别行政区"), "South China",
    name %in% c("四川省", "贵州省", "云南省", "西藏自治区", "重庆市"), "Southwest China",
    name %in% c("陕西省", "甘肃省", "青海省", "宁夏回族自治区", "新疆维吾尔自治区"), "Northwest China"
  ))

# Merge provincial boundaries to create region-level geometries
region_plot <-
  province_plot %>%
  st_make_valid() %>%
  group_by(region) %>%
  summarise(geometry = st_union(geometry))  # Merge geometries by region

# Create a city-level boundary table by processing the province data
city <-
  province_plot %>%
  st_drop_geometry() %>%
  filter(adcode != "100000_JD") %>%
  transmute(
    adcode = fifelse(adcode %in% c("110000", "120000", "310000", "500000", "460000", "710000", "810000", "820000"), adcode, str_c(adcode, "_full")),
    data = map(adcode, ~ st_read(str_c("https://geo.datav.aliyun.com/areas_v3/bound/", .x, ".json")))  # Fetch city boundaries
  ) %>%
  as_tibble()

# Combine the city data into an sf object for spatial analysis
city <-
  bind_rows(
    city %>% filter(str_detect(adcode, "full")) %>% unnest(),
    city %>% filter(!str_detect(adcode, "full")) %>% unnest()
  ) %>%
  st_as_sf(sf_column_name = "geometry")  # Convert to sf object

# Read the data from a .qs file and process it
data <- qread("data/part_tidied.qs", nthreads = qn) %>%
  mutate(
    crop = fifelse(str_detect(crop, "Rice"), "Rice", crop)  # Simplify crop names
  )

# Calculate trend (linear regression slope) for each point, ignoring missing data
yeartrend <- data %>%
  group_by(x, y, crop) %>%
  summarise(across(
    c(contains("GOSIF"),  fraction, Faod, Caod, fpar),
    ~ tryCatch(coef(lm(. ~ year, na.action = na.exclude))[2], error = function(e) NA),  # Handle errors and return NA
    .names = "trend_{.col}"  # Add prefix to the new columns
  ), .groups = "drop") %>%
  pivot_wider(names_from = crop, values_from = starts_with("trend_")) %>%
  as_tibble()  # Convert to tibble for further processing

# For Maize - Process trend data for Maize (with proper error handling)
# Check if the trend for Maize exists
if (!"trend_fpar_Maize" %in% names(yeartrend)) {
  stop("trend_fpar_Maize column not found, please check data processing steps.")  # Stop if column is missing
}

# Convert the yeartrend data frame to a terra vector object
yeartrend_vect <- vect(yeartrend, geom = c("x", "y"), crs = "epsg:4326")

# Convert the vector data to raster, adjusting the resolution as needed
yeartrend_rast <- rasterize(yeartrend_vect, rast(yeartrend_vect, res = 0.25), field = "trend_Faod_Maize")

# Ensure city data is in sf format and check for matching projections
if (!inherits(city, "sf")) {
  stop("City object is not in sf format, please check data structure.")  # Stop if city data is not in sf format
}

# Extract the trend (mean of the raster) for the cities
extracted <- exact_extract(yeartrend_rast, city, "mean")

# Check the structure of the extracted data
print("Extracted data structure:")
print(str(extracted))

# Bind city data with extracted trends, rename the column for clarity
extracted <- bind_cols(city, extracted) %>%
  rename(fpar_trend_Maize = `...12`) %>%  # Rename the extracted trend column
  mutate(
    area = units::drop_units(st_area(.)) / 1e6  # Convert area to square kilometers
  )

# Define custom colors for plotting, with both negative and positive colors
custom_colors <- c(
  "#20223e", "#3b1f46", "#7f4b89", "#b46db3", "#e3a5d6", "#f7f7f7", "white", "#f7f7f7", 
  "#fcc893", "#feb424", "#fd8700"
)

# Define the color scale values to map to the custom colors
color_values <- c(-0.02, -0.015, -0.01, -0.005, -0.002, -0.0001, 0, 0.0001, 0.005, 0.01, 0.015)

# Create the plot for Maize FAOD trend
plot <- ggplot(extracted) +
  geom_sf(aes(fill = fpar_trend_Maize), color = NA) +  # Plot trend as a filled map
  geom_sf(data = region_plot, fill = NA, color = "black", size = 1.2) +  # Add region boundaries
  scale_fill_gradientn(
    name = "FAOD Trend (Maize)",       # Legend title
    colours = custom_colors,           # Custom color scale
    values = scales::rescale(color_values, to = c(0, 1)),  # Map the values to the color scale
    limits = c(-0.02, 0.015),          # Set the color scale limits
    oob = scales::squish,              # Squish values outside the limits
    na.value = "grey90",               # Set color for NA values
    guide = guide_colorbar(
      title.position = "top",          # Position of the colorbar title
      barwidth = 10,                   # Width of the colorbar
      label.position = "bottom"        # Position of colorbar labels
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.2, 0.2),     # Position the legend
    legend.direction = "horizontal",   # Set the legend to be horizontal
    axis.text.x = element_blank(),     # Remove X axis labels
    axis.text.y = element_blank(),     # Remove Y axis labels
    axis.title = element_blank(),      # Remove axis titles
    panel.grid = element_blank()       # Remove grid lines
  )

# Display the plot
print(plot)

# Save the plot as a high-resolution TIFF image
ggsave(
  filename = "figures/sfig/fpar_trend_map_Maize.tif",  # File path and name
  plot = plot,                                          # ggplot object
  width = 10, height = 8,                               # Set the image width and height (in inches)
  units = "in",                                         # Units: inches
  dpi = 600,                                            # Set DPI to 600 for high quality
  device = "tiff"                                       # Output format: TIFF
)
