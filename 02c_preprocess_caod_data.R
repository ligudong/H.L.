source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")

# Load unified crop mask
mask <- rast("data/outputs/masks/mask_extraction.tif") %>% wrap()

# Enable parallel processing
plan(multisession, workers = 3)

# Process cAOD rasters
caod <- tibble(
  files_ = list.files("data/raw/cAOD", pattern = "\\.tif$"),
  files  = list.files("data/raw/cAOD", full.names = TRUE, pattern = "\\.tif$")
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
      r <- rast(afile)
      
      # Resample and apply mask
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

caod <- caod %>%
  unnest() %>%
  trim_xy()

saveRDS(caod, "data/outputs/aerosol/cAOD.rds")
