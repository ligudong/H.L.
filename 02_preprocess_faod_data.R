source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")

# Load unified crop mask (0.5° resolution)
mask <- rast("data/outputs/masks/mask_extraction.tif") %>% wrap()

# Enable parallel processing
plan(multisession, workers = 3)

# Process monthly fAOD rasters
faod <- tibble(
  files_ = list.files("data/raw/fAOD", pattern = "\\.tif$"),
  files  = list.files("data/raw/fAOD", full.names = TRUE, pattern = "\\.tif$")
) %>%
  mutate(
    year = substr(files_, 1, 4),
    month = substr(files_, 5, 6),
    date = ymd(paste0(year, "-", month, "-01"))
  ) %>%
  mutate(
    year = year(date),
    month = month(date),
    .keep = "unused"
  ) %>%
  mutate(
    data = future_map(files, function(afile) {
      # Load raster
      r <- rast(afile)
      
      # Resample and apply crop mask
      resample(r, rast(mask), method = "average") %>%
        mask(rast(mask)) %>%
        as.data.frame(xy = TRUE)
    },
    .progress = TRUE,
    .options = furrr_options(packages = c("terra", "tidyverse"))
    ),
    .keep = "unused"
  )
plan(sequential)

faod <- faod %>%
  unnest() %>%
  trim_xy()
check_join(faod)

saveRDS(faod, "data/outputs/aerosol/fAOD.rds")
