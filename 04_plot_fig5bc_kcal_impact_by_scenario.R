
source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")

# ---------------------------
# Load statistical data: Area, Production, Population
# ---------------------------
Area <- read_xlsx("data/raw/area.xlsx")
Production <- read_xlsx("data/raw/production.xlsx")
pop <- read_xlsx("data/raw/pop.xlsx")

# Combine area, production, and population data
stats <- inner_join(Area, Production) %>%
  inner_join(pop) %>%
  mutate(production = production * 1e4) %>%  # convert to kg
  drop_na() %>%
  group_by(crop, year) %>%
  summarise(
    yield = sum(production) / sum(area),
    area = sum(area),
    population = sum(population),
    .groups = "drop"
  )

# ---------------------------
# Load provincial shapefile and define regions
# ---------------------------
province <- st_read("E:/data/inputs/data_archive/shp/sheng/省.shp") %>%
  mutate(region = fcase(
    省 %in% c("辽宁省", "吉林省", "黑龙江省"), "Northeast China",
    省 %in% c("上海市", "江苏省", "浙江省", "安徽省", "福建省", "江西省", "山东省", "台湾省"), "East China",
    省 %in% c("北京市", "天津市", "河北省", "山西省", "内蒙古自治区"), "North China",
    省 %in% c("河南省", "湖北省", "湖南省"), "Central China",
    省 %in% c("广东省", "广西壮族自治区", "海南省", "香港特别行政区", "澳门特别行政区"), "South China",
    省 %in% c("四川省", "贵州省", "云南省", "西藏自治区", "重庆市"), "Southwest China",
    省 %in% c("陕西省", "甘肃省", "青海省", "宁夏回族自治区", "新疆维吾尔自治区"), "Northwest China"
  ))

# Aggregate by region
region <- bind_rows(
  province %>% group_by(region) %>% summarise(geometry = st_union(geometry)),
  province %>% summarise(geometry = st_union(geometry)) %>% mutate(region = "China")
)

# ---------------------------
# Load, process, and mask FAOD and O3 data for crop-specific regional exposure
# ---------------------------
# (Omitted for brevity - see original code section above for full preprocessing and masking of `faod` and `ozone` data)

# ---------------------------
# Interpolate missing years using ARIMA for both faod and ozone
# ---------------------------
# (Omitted for brevity - ARIMA-based imputation logic kept as-is)

# ---------------------------
# Join datasets and calculate per-capita calorie intake
# ---------------------------
joined <- inner_join(stats, Faod_full, by = c("crop" = "crop_parent", "year")) %>%
  inner_join(ozone_full, by = c("crop" = "crop_parent", "year")) %>%
  mutate(
    n = fcase(crop == "Wheat", 0.78, crop == "Rice", 1, crop == "Maize", 0.79),
    w = fcase(crop == "Wheat", 0.2, crop == "Rice", 0.1, crop == "Maize", 0.7),
    E = fcase(crop == "Wheat", 3391.67, crop == "Rice", 3882.05, crop == "Maize", 3622.95),
    kcal_percapita_perday = 0.44 * area * yield * 1e3 * n * (1 - w) * E / (population * 1e4) / 365,
    kcal_percapita_perday_our = 0.44 * area * (yield + `aer50%` * yield + `ozo50%` * yield) * 1e3 * n * (1 - w) * E / (population * 1e4) / 365,
    kcal_percapita_perday_our_up = 0.44 * area * (yield + `aer95%` * yield + `ozo95%` * yield) * 1e3 * n * (1 - w) * E / (population * 1e4) / 365,
    kcal_percapita_perday_our_lw = 0.44 * area * (yield + `aer5%` * yield + `ozo5%` * yield) * 1e3 * n * (1 - w) * E / (population * 1e4) / 365
  )

# ---------------------------
# Plot Figure 5c: Caloric impact trend over time
# ---------------------------
p1 <- joined %>%
  filter(faod_level == 0.40, peak_level == 100) %>%
  group_by(year) %>%
  summarise(across(contains("kcal"), sum)) %>%
  ggplot(aes(x = year)) +
  geom_hline(aes(yintercept = kcal_percapita_perday), data = . %>% filter(year == 2019), linetype = "longdash", color = "grey30") +
  geom_text(aes(x = 2006, y = kcal_percapita_perday, label = "Claimed year of self-efficiency"),
            data = . %>% filter(year == 2019), nudge_y = -11, color = "grey30", family = "Roboto Condensed", size = 5) +
  geom_hline(aes(yintercept = kcal_percapita_perday), data = . %>% filter(year == 2015), linetype = "dashed", color = "grey50") +
  geom_text(aes(x = 2006, y = kcal_percapita_perday, label = "Possible year of self-efficiency"),
            data = . %>% filter(year == 2015), nudge_y = 11, color = "grey50", family = "Roboto Condensed", size = 5) +
  geom_col(aes(y = kcal_percapita_perday_our), fill = "#F9A828", width = 0.5, color = "black") +
  geom_errorbar(aes(ymax = kcal_percapita_perday_our_up, ymin = kcal_percapita_perday_our_lw), width = 0.25) +
  geom_col(aes(y = kcal_percapita_perday), fill = "#009d73", width = 0.5, color = "black") +
  scale_y_continuous(name = TeX("calorie intake (kCal capita$^{-1}$ day$^{-1}$)")) +
  scale_x_continuous(name = NULL, breaks = c(2005, 2010, 2015, 2019)) +
  coord_cartesian(ylim = c(850, NA)) +
  theme_half_open(16, font_family = "Roboto Condensed") +
  background_grid(major = "y")

# Save Figure 5c
ggsave("E:/figures/p5/c.tif", plot = p1, width = 8, height = 4, dpi = 300, units = "in", device = "tiff")

# ---------------------------
# Plot Figure 5b: Crop-specific bar plot with O3 level scenarios
# ---------------------------
data <- data.frame(
  Scenario = rep("Rice", times = 3),
  O3_Level = c("O₃ = 60", "O₃ = 80", "O₃ = 100"),
  value = c(16.32, 30.17, 39.04)
)
data$O3_Level <- factor(data$O3_Level, levels = c("O₃ = 100", "O₃ = 80", "O₃ = 60"))

p <- ggplot(data, aes(x = Scenario, y = value)) +
  geom_bar(stat = "identity", fill = "#B7EB8F", width = 0.6, color = "black") +
  theme_minimal(base_size = 14) +
  labs(
    y = "Ozone-induced crop production change (Tg/yr)",
    title = "Ozone Impact on Crop Yield by Scenario"
  ) +
  theme(
    text = element_text(family = "Arial"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 14, face = "bold", color = "black"),
    axis.title.y = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    legend.position = "none"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")

# Save Figure 5b
ggsave(
  filename = "E:/figures/p5/b2.tif",
  plot = p,
  device = "tiff",
  dpi = 300,
  width = 10, height = 6,
  units = "in",
  compression = "lzw"
)
