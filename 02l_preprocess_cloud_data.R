source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")

mask <- rast("data/outputs/masks/mask_extraction.tif") %>% sum(na.rm = TRUE) %>% wrap()

plan(multisession, workers = 3)

# 1. Load cloud fraction
cloud_fraction <- tibble(
  files_ = list.files("data/inputs/data_archive/cloud_fraction/", pattern = "\\.tif$"),
  files  = list.files("data/inputs/data_archive/cloud_fraction/", full.names = TRUE, pattern = "\\.tif$")
) %>%
  mutate(
    year = str_extract(files_, "\\d{4}"),
    month = str_extract(files_, "(?<=-)\\d{2}"),
    date = ymd(paste0(year, "-", month, "-01"))
  ) %>%
  mutate(year = year(date), month = month(date), .keep = "unused") %>%
  mutate(data = future_map(files, function(afile) {
    rast(afile) %>%
      resample(mask, method = "average") %>%
      mask(mask) %>%
      as.data.frame(xy = TRUE)
  },
  .progress = TRUE,
  .options = furrr_options(packages = c("terra", "tidyverse"))
  ), .keep = "unused")

plan(sequential)

# Flatten and clean
cloud_fraction <- cloud_fraction %>%
  unnest() %>%
  trim_xy()

saveRDS(cloud_fraction, "data/outputs/cloud/cloud_fraction.rds")

# 2. Load cloud_optical_depth
cloud_optical_depth <- tibble(
  files_ = list.files("data/inputs/data_archive/cloud_optical_depth/", pattern = "\\.tif$"),
  files  = list.files("data/inputs/data_archive/cloud_optical_depth/", full.names = TRUE, pattern = "\\.tif$")
) %>%
  mutate(
    year = str_extract(files_, "\\d{4}"),
    month = str_extract(files_, "(?<=-)\\d{2}"),
    date = ymd(paste0(year, "-", month, "-01"))
  ) %>%
  mutate(year = year(date), month = month(date), .keep = "unused") %>%
  mutate(data = future_map(files, function(afile) {
    rast(afile) %>%
      resample(mask, method = "average") %>%
      mask(mask) %>%
      as.data.frame(xy = TRUE)
  },
  .progress = TRUE,
  .options = furrr_options(packages = c("terra", "tidyverse"))
  ), .keep = "unused")

plan(sequential)

# Flatten and clean
cloud_optical_depth <- cloud_optical_depth %>%
  unnest() %>%
  trim_xy()

saveRDS(cloud_optical_depth, "data/outputs/cloud/cloud_optical_depth.rds")


# 3. Load optical depth (already preprocessed elsewhere)

# Merge datasets by (x, y, year, month)
merged_data <- inner_join(
  cloud_fraction,
  cloud_optical_depth,
  by = c("x", "y", "year", "month")
)

# 4. Standardize fraction, then compute cloud index
merged_data <- merged_data %>%
  mutate(cloud_fraction = scale(cloud_fraction)[, 1]) %>%
  mutate(cloud = cloud_fraction * cld_opd_acha)

# 5. Drop unnecessary columns
merged_data <- merged_data %>%
  select(-cloud_fraction, -cld_opd_acha, -files_.x, -files_.y)

# Final object
MODIS <- merged_data

saveRDS(MODIS, "data/outputs/cloud/tidied.rds")
