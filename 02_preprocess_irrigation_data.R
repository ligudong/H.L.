source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")

mask <- rast("data/outputs/masks/mask_extraction.tif")

# 1. Load yearly irrigation rasters and resample to mask resolution
irg_stack <- list.files("data/inputs/irrigation/", full.names = TRUE) %>%
  pro_map(~ rast(.x) %>% resample(mask, method = "sum")) %>%
  rast()

baseline <- rast("data/inputs/irrigation/2000.tif")
baseline[is.na(baseline)] <- 1
baseline <- resample(baseline, mask, method = "sum")

# 3. Compute irrigation fraction (relative to 2000 baseline)
irg_fraction <- irg_stack / baseline
irg_fraction[is.na(irg_fraction)] <- 0

# 4. Convert to long-format dataframe
irg_fraction_df <- irg_fraction %>%
  as.data.frame(xy = TRUE) %>%
  pivot_longer(
    cols = -c(x, y),
    names_to = "year",
    names_transform = list(year = as.integer),
    values_to = "irg_fraction"
  ) %>%
  trim_xy()

check_join(irg_fraction_df)

saveRDS(irg_fraction_df, "data/outputs/crop/maize/irrigation/tidied.rds")
