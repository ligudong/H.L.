
source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")
# 读取地理数据和结果
faod <- read_rds("E:/data/impacts_Faod_summarised.rds")
province_plot <- st_read("https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json")
region_plot <- province_plot %>%
  mutate(region = case_when(
    name %in% c("辽宁省", "吉林省", "黑龙江省") ~ "Northeast China",
    name %in% c("上海市", "江苏省", "浙江省", "安徽省", "福建省", "江西省", "山东省", "台湾省") ~ "East China",
    name %in% c("北京市", "天津市", "河北省", "山西省", "内蒙古自治区") ~ "North China",
    name %in% c("河南省", "湖北省", "湖南省") ~ "Central China",
    name %in% c("广东省", "广西壮族自治区", "海南省", "香港特别行政区", "澳门特别行政区") ~ "South China",
    name %in% c("四川省", "贵州省", "云南省", "西藏自治区", "重庆市") ~ "Southwest China",
    name %in% c("陕西省", "甘肃省", "青海省", "宁夏回族自治区", "新疆维吾尔自治区") ~ "Northwest China",
    TRUE ~ NA_character_
  )) %>%
  group_by(region) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_make_valid()

# 指定作物
acrop <- "Maize"
#acrop <- "Rice"
#acrop <- "Wheat"
# 地图绘图函数
map_plot <- faod %>%
  filter(crop_parent == acrop) %>%
  unnest_wider(Faod_results) %>%
  select(crop, year, crop_parent, province_level) %>%
  unnest() %>%
  group_by(crop_parent, 省, faod_level) %>%
  summarise(across(contains("%"), fmean), .groups = "drop") %>%
  filter(faod_level == 0.4) %>%
  inner_join(province_plot, c("省" = "name")) %>%
  ggplot() +
  geom_sf(aes(fill = `50%` * 1e2, geometry = geometry), size = 0.2, show.legend = TRUE, color = NA) +
  geom_sf(data = region_plot, fill = NA, color = "black", size = 0.6) +
  scale_fill_gradientn(
    name = "Percentage change in SIF",
    limits = c(-10, 10),
    oob = scales::squish,
    colours = c("#5796b6", "#73aeab", "#90cbfb", "#9bd395", "#c6e794",
                "#faf4aa", "yellow", "#fe8e5c", "#e15546", "#86173d"),
    na.value = "grey90"
  ) +
  theme_void() +
  theme(legend.position = "bottom")

# 保存为 .tif
ggsave("figures/hist_impact_faod_map.tif", map_plot, width = 8, height = 9, dpi = 300)




# 加载数据和库

faod <- read_rds("E:/data/impacts_Faod_summarised.rds")
acrop <- "Maize"
#acrop <- "Rice"
#acrop <- "Wheat"
order <- c("Northeast China", "North China", "East China", "Central China", "South China", "Southwest China", "Northwest China")

# 区域响应线图
p2 <- faod %>%
  filter(crop_parent == acrop) %>%
  unnest_wider(Faod_results) %>%
  select(crop, year, crop_parent, region_level) %>%
  unnest() %>%
  drop_na() %>%
  group_by(crop_parent, region, faod_level) %>%
  summarise(across(contains("%"), fmean), .groups = "drop") %>%
  mutate(
    faod_level = as.numeric(faod_level),
    region = factor(region, levels = order),
    across(contains("%"), ~ .x * 1e2)
  ) %>%
  filter(faod_level <= 0.5) %>%
  ggplot(aes(
    x = faod_level,
    y = `50%`,
    color = region
  )) +
  geom_vline(xintercept = 0.2, linetype = "dashed", color = "grey40") +
  geom_ribbon(aes(ymin = `5%`, ymax = `95%`, fill = region), alpha = 0.2, color = NA) +
  geom_line(size = 1) +
  facet_wrap(~ region, scales = "free_y", ncol = 2) +
  scale_color_manual(values = MetBrewer::met.brewer("Juarez", n = 7)) +
  scale_fill_manual(values = MetBrewer::met.brewer("Juarez", n = 7)) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Annual Faod Level",
    y = "Percentage change in SIF",
    color = NULL,
    fill = NULL
  )

# 保存为 .tif
ggsave("figures/hist_impact_faod_line.tif", p2, width = 10, height = 12, dpi = 300)
