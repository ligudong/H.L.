source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")


# Load statistical data ---------------------------------------------------
Area <- read_xlsx("data/raw/area.xlsx")          # Crop planting area
Production <- read_xlsx("data/raw/production.xlsx")  # Crop production
pop <- read_xlsx("data/raw/pop.xlsx")            # Population

stats <- 
  inner_join(Area, Production) %>%         # Join area and production data
  inner_join(pop) %>%                      # Add population data
  mutate(production = production * 1e4) %>% # Convert production to kg
  drop_na() %>%
  group_by(crop, year) %>%
  summarise(
    yield = sum(production) / sum(area),  # Calculate yield per unit area
    area = sum(area),                     # Total planting area
    population = sum(population),        # Total population
    .groups = "drop"
  )


# Load province shapefile and define regions ------------------------------
province <- st_read("E:/data/inputs/data_archive/shp/sheng/省.shp") %>%
  mutate(
    region = fcase(
      省 %in% c("辽宁省", "吉林省", "黑龙江省"), "Northeast China",
      省 %in% c("上海市", "江苏省", "浙江省", "安徽省", "福建省", "江西省", "山东省", "台湾省"), "East China",
      省 %in% c("北京市", "天津市", "河北省", "山西省", "内蒙古自治区"), "North China",
      省 %in% c("河南省", "湖北省", "湖南省"), "Central China",
      省 %in% c("广东省", "广西壮族自治区", "海南省", "香港特别行政区", "澳门特别行政区"), "South China",
      省 %in% c("四川省", "贵州省", "云南省", "西藏自治区", "重庆市"), "Southwest China",
      省 %in% c("陕西省", "甘肃省", "青海省", "宁夏回族自治区", "新疆维吾尔自治区"), "Northwest China"
    )
  )

region <- bind_rows(
  province %>% group_by(region) %>% summarise(geometry = st_union(geometry)),
  province %>% summarise(geometry = st_union(geometry)) %>% mutate(region = "China")
)


# Process FAOD data -------------------------------------------------------
faod <- read_rds("data/outputs/aerosol/Faod.rds") %>%
  fgroup_by(x, y, year) %>%
  fsummarise(Faod = fmean(Faod)) %>%
  pivot_wider(names_from = year, values_from = Faod) %>%
  rast(type = "xyz", crs = "epsg:4326")

faod <- tibble(
  crop_parent = c("Maize", "Rice", "Wheat"),
  data = map(crop_parent, function(acrop) {
    amask <- rast(str_c("E:/data/outputs/masks/mask_", acrop, ".tif"))
    exact_extract(faod[[1:20]], region, "weighted_mean", stack_apply = TRUE,
                  append_cols = "region", weights = amask, default_weight = 0,
                  progress = FALSE) %>%
      pivot_longer(-region,
                   names_to = c("year", NA),
                   names_prefix = "weighted_mean.",
                   names_sep = "\\.",
                   names_transform = list(year = as.integer),
                   values_to = "Faod")
  })
) %>% unnest()


# Process Ozone data ------------------------------------------------------
ozo <- read_rds("data/outputs/ozone/tidied.rds")

# Remove incomplete grid-year records
rows_to_remove <- ozo %>%
  filter(is.na(O3)) %>%
  select(x, y, year) %>% distinct()

ozo <- ozo %>% anti_join(rows_to_remove, by = c("x", "y", "year"))

ozo <- ozo %>%
  fsubset(month %in% 4:9) %>%
  fgroup_by(x, y, year) %>%
  fsummarise(O3 = fmean(O3)) %>%
  pivot_wider(names_from = year, values_from = O3) %>%
  rast(type = "xyz", crs = "epsg:4326")

ozo <- tibble(
  crop_parent = c("Maize", "Rice", "Wheat"),
  data = map(crop_parent, function(acrop) {
    amask <- rast(str_c("E:/data/outputs/masks/mask_", acrop, ".tif"))
    exact_extract(ozo[[1:15]], region, "weighted_mean", stack_apply = TRUE,
                  append_cols = "region", weights = amask[[6:20]], default_weight = 0,
                  progress = FALSE) %>%
      pivot_longer(-region,
                   names_to = c("year", NA),
                   names_prefix = "weighted_mean.",
                   names_sep = "\\.",
                   names_transform = list(year = as.integer),
                   values_to = "O3")
  })
) %>% unnest()


# Join historical exposure data -------------------------------------------
hist_point <- inner_join(ozo, faod) %>%
  mutate(region = factor(region, order))  # Used to map Figure 4a: exposure level scatter


# Process counterfactual results (Faod) -----------------------------------
Faod <- qread("E:/data/impacts_Faod.qs", nthreads = qn)

Faod <- Faod %>%
  mutate(Faod_results = map(Faod_results, function(adata) {
    adata %>%
      group_by(faod_level) %>%
      summarise(across(contains("%"), ~ weighted.mean(.x, fraction)))
  })) %>%
  unnest() %>%
  group_by(crop_parent, year, faod_level) %>%
  summarise(across(contains("%"), mean), .groups = "drop") %>%
  rename_with(~ str_c("aer", .x), contains("%"))


# Process counterfactual results (Ozone) ----------------------------------
ozone <- qread("data/impacts_ozone_AOT40_5.qs", nthreads = qn)

ozone <- ozone %>%
  mutate(ozone_results = map(ozone_results, function(adata) {
    adata %>%
      group_by(peak_level) %>%
      summarise(across(contains("%"), ~ weighted.mean(.x, fraction)))
  })) %>%
  unnest() %>%
  group_by(crop_parent, year, peak_level) %>%
  summarise(across(contains("%"), mean), .groups = "drop") %>%
  rename_with(~ str_c("ozo", .x), contains("%"))


# Join with crop statistics -----------------------------------------------
joined <- inner_join(stats, Faod, by = c("crop" = "crop_parent", "year")) %>%
  inner_join(ozone, by = c("crop" = "crop_parent", "year"))


# Generate response surface for Figure 4b–4e ------------------------------
p1 <- joined %>%
  group_by(crop, faod_level, peak_level) %>%
  summarise(across(contains("50"), mean), .groups = "drop") %>%
  mutate(value = (`aer50%` + `ozo50%`) * 1e2)

saveRDS(p2,"data/outputs/p1.rds")

# Next steps: use this to plot the 2D yield surface for Maize/Rice/Wheat/All

# Load processed response surface data for Maize
Faod <- read.csv("figures/Faod_Maize_data.csv")
ozone <- read.csv("figures/O3_Maize_data.csv")

# Generate all combinations of Faod and Ozone control levels
combined_data <- expand.grid(Faod_level = Faod$Faod_level, peak_level = ozone$peak_level)

# Merge with fAOD and ozone yield change values
combined_data <- merge(combined_data, Faod, by = "Faod_level", all.x = TRUE)
combined_data <- merge(combined_data, ozone[, c("peak_level", "value")], by = "peak_level", all.x = TRUE)

# Rename and compute combined (additive) impact
combined_data <- combined_data %>%
  rename(value = value.x, value2 = value.y) %>%
  mutate(combined_value = value + value2) %>%
  select(-value, -value2)

# Define custom blue-red diverging color palette (negative to positive change)
colors_blue_to_red <- c(
  "#061178", "#10239E", "#1D39C4", "#4465EB", "#6682F5", "#8BA2FF", "#ADC6FF", "#D6E4FF", "#F0F5FF",
  "#FFE4D6", "#FFD2B9", "#FFB186", "#FF8F50", "#FF762A", "#F55C08", "#D94F03", "#AD2102", "#871400"
)

# Define value scaling: -12 to 12, break at 0
color_positions <- rescale(c(
  seq(-12, 0, length.out = 9),  # blue shades
  seq(1, 12, length.out = 9)   # red shades
), from = c(-12, 12))

# Plot: Maize SIF response to Faod–Ozone combined control
p <- ggplot(combined_data, aes(x = Faod_level, y = peak_level, z = combined_value)) +
  geom_tile(aes(fill = combined_value)) +
  geom_contour(color = "grey70", size = 0.5) +
  geom_contour(breaks = 0, color = "black", size = 1) +  # 0-change line
  scale_fill_gradientn(
    colors = colors_blue_to_red,
    values = color_positions,
    limits = c(-12, 12)
  ) +
  labs(x = "fAOD Level", y = "Ozone Peak Level", fill = "SIF Change (%)") +
  theme_minimal()

# Save Maize heatmap
ggsave("figures/combined_Maize_plot.tif", plot = p, width = 8, height = 6, dpi = 300)

Faod <- read.csv("figures/Faod_Rice_data.csv")
ozone <- read.csv("figures/O3_Rice_data.csv")

combined_data <- expand.grid(Faod_level = Faod$Faod_level, peak_level = ozone$peak_level)
combined_data <- merge(combined_data, Faod, by = "Faod_level", all.x = TRUE)
combined_data <- merge(combined_data, ozone[, c("peak_level", "value")], by = "peak_level", all.x = TRUE)

combined_data <- combined_data %>%
  rename(value = value.x, value2 = value.y) %>%
  mutate(combined_value = value + value2) %>%
  select(-value, -value2)

colors_blue_to_red <- c(
  "#061178", "#10239E", "#1D39C4", "#4465EB", "#6682F5", "#8BA2FF", "#ADC6FF", "#D6E4FF", "#F0F5FF",
  "#FFE4D6", "#FFD2B9", "#FFB186", "#FF8F50", "#FF762A", "#F55C08", "#D94F03", "#AD2102", "#871400"
)

color_positions <- rescale(c(
  seq(-30, 0, length.out = 9),
  seq(1, 15, length.out = 9)
), from = c(-30, 15))

p <- ggplot(combined_data, aes(x = Faod_level, y = peak_level, z = combined_value)) +
  geom_tile(aes(fill = combined_value)) +
  geom_contour(color = "grey70", size = 0.5) +
  geom_contour(breaks = 0, color = "black", size = 1) +
  scale_fill_gradientn(
    colors = colors_blue_to_red,
    values = color_positions,
    limits = c(-30, 15)
  ) +
  labs(x = "fAOD Level", y = "Ozone Peak Level", fill = "SIF Change (%)") +
  theme_minimal()

ggsave("figures/crop/combined_Rice_plot.tif", plot = p, width = 8, height = 6, dpi = 300)



Faod <- read.csv("E:/figures/crop/P6/Faod_Wheat_data.csv")
ozone <- read.csv("E:/figures/crop/P6/O3_Wheat_data.csv")

combined_data <- expand.grid(Faod_level = Faod$Faod_level, peak_level = ozone$peak_level)
combined_data <- merge(combined_data, Faod, by = "Faod_level", all.x = TRUE)
combined_data <- merge(combined_data, ozone[, c("peak_level", "value")], by = "peak_level", all.x = TRUE)

combined_data <- combined_data %>%
  rename(value = value.x, value2 = value.y) %>%
  mutate(combined_value = value + value2) %>%
  select(-value, -value2)

colors_blue_to_red <- c(
  "#061178", "#10239E", "#1D39C4", "#4465EB", "#6682F5", "#8BA2FF", "#ADC6FF", "#D6E4FF", "#F0F5FF",
  "#FFE4D6", "#FFD2B9", "#FFB186", "#FF8F50", "#FF762A", "#F55C08", "#D94F03", "#AD2102", "#871400"
)

color_positions <- rescale(c(
  seq(-50, 0, length.out = 9),
  seq(1, 30, length.out = 9)
), from = c(-50, 30))

p <- ggplot(combined_data, aes(x = Faod_level, y = peak_level, z = combined_value)) +
  geom_tile(aes(fill = combined_value)) +
  geom_contour(color = "grey70", size = 0.5) +
  geom_contour(breaks = 0, color = "black", size = 1) +
  scale_fill_gradientn(
    colors = colors_blue_to_red,
    values = color_positions,
    limits = c(-50, 30)
  ) +
  labs(x = "fAOD Level", y = "Ozone Peak Level", fill = "SIF Change (%)") +
  theme_minimal()

ggsave("figures/combined_Wheat_plot.tif", plot = p, width = 8, height = 6, dpi = 300)













