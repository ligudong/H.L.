source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")

mask <- rast("data/outputs/masks/mask_extraction.tif") %>% sum(na.rm = TRUE)

ozone_data <- list.files("data/raw/ozone/monthly_nc", pattern = "\\.nc$", full.names = TRUE) %>%
  map(function(afile) {
    # Read .nc file and assign time-based layer names
    r <- rast(afile) %>% `names<-`(time(.))
    
    resampled_r <- resample(r, mask, method = "near") %>%
      mask(mask)
    
    # Extract date from filename (format: YYYYMM)
    file_name <- basename(afile)
    year_month <- str_extract(file_name, "\\d{6}")
    year <- substr(year_month, 1, 4)
    month <- substr(year_month, 5, 6)
    
    # Convert raster to dataframe and annotate with metadata
    as.data.frame(resampled_r, xy = TRUE) %>%
      mutate(
        year = year,
        month = month,
        date = make_date(as.numeric(year), as.numeric(month), 1)
      ) %>%
      pivot_longer(
        cols = -c(x, y, year, month, date),
        names_to = "variable",
        values_to = "O3"
      )
  }, .progress = TRUE) %>%
  bind_rows() %>%
  filter(as.numeric(year) %in% 2001:2023) %>%
  trim_xy()

# Optional: spatial consistency check
check_join(ozone_data)

# Load trained GAM models
model <- readRDS("data/outputs/ozone/gam.rds")
model_aot40 <- model[[1]]
model_w126 <- model[[2]]

# Predict AOT40 and W126 using GAM models
ozone_data <- ozone_data %>%
  mutate(
    W126 = exp(predict(model_w126, .)),
    AOT40 = exp(predict(model_aot40, .)),
    year = year(date),
    month = month(date),
    .keep = "unused"
  )

saveRDS(ozone_data, "data/outputs/ozone/tidied.rds")
