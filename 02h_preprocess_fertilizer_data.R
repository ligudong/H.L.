source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")

mask <- rast("data/outputs/masks/mask_extraction.tif") %>% wrap()

plan(multisession, workers = 3)

# Process fertilizer rasters
fertilizer_data <- tibble(
  files_ = list.files("data/raw/fertilizer", pattern = "\\.tif$"),
  files  = list.files("data/raw/fertilizer", full.names = TRUE, pattern = "\\.tif$")
) %>%
  mutate(
    type     = str_extract(files_, "^[A-Za-z]+"),               # e.g., AA, AN, etc.
    position = str_extract(files_, "(deep|surface)"),           # deep or surface
    year     = str_extract(files_, "\\d{4}") %>% as.integer()
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

# Restore sequential mode
plan(sequential)

# Flatten and trim
fertilizer_data <- fertilizer_data %>%
  unnest() %>%
  trim_xy()

# Save split datasets by position
fertilizer_data %>%
  filter(position == "deep") %>%
  saveRDS("data/outputs/fertilizer/fertilizer_deep.rds")

fertilizer_data %>%
  filter(position == "surface") %>%
  saveRDS("data/outputs/fertilizer/fertilizer_surface.rds")
