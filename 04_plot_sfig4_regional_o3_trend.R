source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")

# 定义 TIFF 文件夹路径和年份范围
tiff_folder <- "data/raw/ozone/O3_tif"
years <- 2001:2023

# 读取中国省级边界数据并划分区域
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

# 合并省份边界生成区域级别的地理数据
region_plot <- province_plot %>%
  st_make_valid() %>%
  group_by(region) %>%
  summarise(geometry = st_union(geometry))

# 初始化空列表存储各区域的数据框
region_data_list <- list()

# 遍历每个年份的 TIFF 文件并计算区域均值
for (year in years) {
  # 构建文件路径并读取 TIFF 文件
  tiff_path <- file.path(tiff_folder, paste0("south", year, ".tif"))
  if (!file.exists(tiff_path)) {
    warning(paste("文件不存在:", tiff_path))
    next
  }
  
  o3_raster <- rast(tiff_path)
  
  # 计算每个区域的均值
  year_mean_values <- exact_extract(o3_raster, region_plot, "mean")
  
  # 创建年份数据框并添加区域和年份信息
  year_mean_df <- data.frame(
    region = region_plot$region,
    year = year,
    mean_O3 = year_mean_values
  )
  
  # 将每年的数据添加到对应区域的列表中
  for (region_name in unique(year_mean_df$region)) {
    region_year_df <- year_mean_df %>% filter(region == region_name)
    
    # 如果该区域在列表中尚未创建，先初始化
    if (!region_name %in% names(region_data_list)) {
      region_data_list[[region_name]] <- data.frame()
    }
    
    # 将当前年份的数据行追加到该区域的数据框中
    region_data_list[[region_name]] <- bind_rows(region_data_list[[region_name]], region_year_df)
  }
}

# 检查示例区域的数据框，例如 North China
print(region_data_list[["North China"]])

# 选择一个区域（例如 "North China"）并获取对应的数据
region_name <- "Northwest China"
region_data <- region_data_list[[region_name]]
# 定义分界点年份
breakpoint_year <- 2014

# 对分界点前后的数据进行线性回归计算
left_model <- lm(mean_O3 ~ year, data = region_data %>% filter(year <= breakpoint_year))
right_model <- lm(mean_O3 ~ year, data = region_data %>% filter(year > breakpoint_year))

# 获取分界点处的O3值，用于连接两条线
left_end_y <- predict(left_model, newdata = data.frame(year = breakpoint_year))
right_start_y <- predict(right_model, newdata = data.frame(year = breakpoint_year))

# 绘制折线图
p <- ggplot(region_data, aes(x = year, y = mean_O3)) +
  geom_line(color = "grey50", size = 0.8) +  # 绘制实际值的折线图
  geom_segment(aes(x = min(region_data$year), y = coef(left_model)[1] + coef(left_model)[2] * min(region_data$year),
                   xend = breakpoint_year, yend = left_end_y), color = "red", size = 1) +  # 左侧回归线
  geom_segment(aes(x = breakpoint_year, y = right_start_y,
                   xend = max(region_data$year), yend = coef(right_model)[1] + coef(right_model)[2] * max(region_data$year)), 
               color = "red", size = 1) +  # 右侧回归线
  geom_vline(xintercept = breakpoint_year, linetype = "dashed", color = "red", size = 0.9) +  # 分界点虚线
  
  # 设置 x 轴和 y 轴刻度线
  scale_x_continuous(breaks = seq(min(region_data$year), max(region_data$year), by = 4),  # 设置 x 轴的刻度间隔
                     labels = seq(min(region_data$year), max(region_data$year), by = 4),
                     expand = expansion(mult = c(0.05, 0.05))) +  # 增加 x 轴两端的间距
  scale_y_continuous(limits = c(60, 110),  # 设置 y 轴范围
                     breaks = scales::pretty_breaks(n = 5),  # 设置 y 轴自动刻度
                     expand = expansion(mult = c(0.05, 0.05))) +  # 增加 y 轴两端的间距
  
  labs(
    title = region_name,  # 设置图表标题
    x = "year",               # 设置 x 轴标签为空
    y = "O3"          # 设置 y 轴标签
  ) +
  
  # 增强主题样式和字体大小
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),  # 增加标题字体大小，设置为加粗并居中
    axis.title.y = element_text(size = 14),  # 增加 y 轴标签字体大小
    axis.text.x = element_text(size = 15, color = "black"),  # 增加 x 轴刻度字体大小，设置颜色为黑色
    axis.text.y = element_text(size = 15, color = "black"),  # 增加 y 轴刻度字体大小，设置颜色为黑色
    axis.ticks = element_line(color = "black"),  # 设置 x 和 y 轴的刻度线颜色为黑色
    panel.grid.major = element_blank(),          # 移除主要网格线
    panel.grid.minor = element_blank(),          # 移除次要网格线
    axis.line = element_line(color = "black")    # 设置 x 和 y 轴线颜色为黑色
  )

# 显示图表
print(p)


#批量导出为tif---------------------
# 创建输出文件夹（如不存在则创建）
output_folder <- "E:/figures/sfig/sfig4_tif"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# 可选：提前清除空区域或无效项（例如 <unknown>）
region_data_list <- region_data_list[!sapply(region_data_list, function(x) is.null(x) || nrow(x) == 0)]

# 定义断点年份
breakpoint_year <- 2014

# 批量遍历区域并生成 .tif 图像
for (region_name in names(region_data_list)) {
  region_data <- region_data_list[[region_name]]
  
  # 防御性判断（尽管上面已过滤）
  if (is.null(region_data) || nrow(region_data) == 0) {
    warning(paste("区域", region_name, "数据为空，已跳过。"))
    next
  }
  
  # 构建左右段回归模型
  left_model <- lm(mean_O3 ~ year, data = region_data %>% filter(year <= breakpoint_year))
  right_model <- lm(mean_O3 ~ year, data = region_data %>% filter(year > breakpoint_year))
  
  # 获取断点值预测
  left_end_y <- predict(left_model, newdata = data.frame(year = breakpoint_year))
  right_start_y <- predict(right_model, newdata = data.frame(year = breakpoint_year))
  
  # 构建图
  p <- ggplot(region_data, aes(x = year, y = mean_O3)) +
    geom_line(color = "grey50", size = 0.8) +
    geom_segment(aes(x = min(region_data$year), y = coef(left_model)[1] + coef(left_model)[2] * min(region_data$year),
                     xend = breakpoint_year, yend = left_end_y), color = "red", size = 1) +
    geom_segment(aes(x = breakpoint_year, y = right_start_y,
                     xend = max(region_data$year), yend = coef(right_model)[1] + coef(right_model)[2] * max(region_data$year)), 
                 color = "red", size = 1) +
    geom_vline(xintercept = breakpoint_year, linetype = "dashed", color = "red", size = 0.9) +
    scale_x_continuous(breaks = seq(min(region_data$year), max(region_data$year), by = 4),
                       labels = seq(min(region_data$year), max(region_data$year), by = 4),
                       expand = expansion(mult = c(0.05, 0.05))) +
    scale_y_continuous(limits = c(60, 110),
                       breaks = scales::pretty_breaks(n = 5),
                       expand = expansion(mult = c(0.05, 0.05))) +
    labs(
      title = region_name,
      x = "year",
      y = "O3"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 15, color = "black"),
      axis.text.y = element_text(size = 15, color = "black"),
      axis.ticks = element_line(color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black")
    )
  
  # 输出为 .tif 文件
  tiff_filename <- file.path(output_folder, paste0(region_name, "_O3_Trend.tif"))
  tiff(tiff_filename, width = 2000, height = 1600, res = 300, compression = "lzw")
  print(p)
  dev.off()
}



