source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")

mask <- rast("data/outputs/masks/mask_extraction.tif")[[1]] %>% wrap()

plan(multisession, workers = 3)

# Process precipitation rasters
prep <- tibble(
  files_ = list.files("data/raw/precip/", pattern = "\\.tif$"),
  files  = list.files("data/raw/precip/", full.names = TRUE, pattern = "\\.tif$")
) %>%
  mutate(
    year = str_extract(files_, "\\d{4}"),
    month = str_extract(files_, "(?<=-)\\d{2}")
  ) %>%
  mutate(
    data = future_map(files, function(afile) {
      rast(afile) %>%
        resample(mask, method = "average") %>%
        mask(mask) %>%
        as.data.frame(xy = TRUE)
    },
    .progress = TRUE,
    .options = furrr_options(packages = c("terra", "tidyverse"))
    ),
    .keep = "unused"
  )

# Restore to sequential processing
plan(sequential)

prep <- prep %>%
  unnest() %>%
  trim_xy() %>%
  rename(prep = total_precipitation_sum)

saveRDS(prep, "data/outputs/precip/prep.rds")
