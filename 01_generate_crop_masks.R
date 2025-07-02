source(".../script/loadPackages.R")
source(".../script/loadFunctions.R")

# Read extent of a reference crop raster 
exts <- rast(".../data/raw/China_1km_majorcrop_area/CHN_Maize_2019.tif") %>% ext()

# Build a unified mask at 0.5° resolution based on major crop area --------------------------------
all <- list.files(".../data/inputs/data_archive/China_1km_majorcrop_area", full.names = TRUE) %>%
  map(rast) %>%
  pro_map(~ extend(.x, exts)) %>%   # ensure all rasters share the same extent
  rast() %>%
  tapp(rep(c("Maize", "Rice", "Wheat"), each = 20), "sum", na.rm = TRUE)  # aggregate by crop type

all[all > 0] <- 1
all[all == 0] <- NA
all <- all %>%
  project("epsg:4326", method = "near")
degree <- rast(resolution = 0.5) %>%
  crop(all, snap = "out")
all <- all %>% resample(degree, "sum")
writeRaster(all, ".../data/outputs/masks/mask_extraction.tif", overwrite = TRUE)


#生成单独作物
c("Maize", "Rice", "Wheat") %>%
  walk(function(acrop) {
    # 读取当前作物的数据并处理
    all <-
      list.files("E:/data/inputs/data_archive/China_1km_majorcrop_area", acrop, full.names = T) %>%
      map(rast) %>%
      map(~ extend(.x, exts)) %>%
      rast() %>%
      `names<-`(2000:2019) %>%
      project("epsg:4326", method = "near")
    
    # 裁剪和重采样
    degree <- rast(resolution = 0.05) %>%
      crop(all, snap = "out")
    all <- all %>% resample(degree, "sum")
    
    # 将作物种植区域的像素值设置为1，其它区域设置为NA
    all[all > 0] <- 1
    all[all == 0] <- NA
    
    # 保存每个作物的掩膜
    writeRaster(all, str_c("E:/data/outputs/masks/mask_", acrop, ".tif"), overwrite = T)
  })


# Generate individual crop-specific binary masks (Maize, Rice, Wheat)------------

c("Maize", "Rice", "Wheat") %>%
  walk(function(acrop) {
    
    # Load all 1km rasters corresponding to the current crop across years
    all <- list.files(".../data/raw/data_archive/China_1km_majorcrop_area", 
                      pattern = acrop, full.names = TRUE) %>%
      map(rast) %>%
      map(~ extend(.x, exts)) %>%                   
      rast() %>%
      `names<-`(2000:2019) %>%                      
      project("epsg:4326", method = "near")         
    
    degree <- rast(resolution = 0.5) %>%
      crop(all, snap = "out")
    
    all <- all %>% resample(degree, "sum")          
    
    # Convert values to binary: 1 = crop presence, NA = absence
    all[all > 0] <- 1
    all[all == 0] <- NA
    
    writeRaster(all, str_c(".../data/outputs/masks/mask_", acrop, ".tif"), overwrite = TRUE)
  })


