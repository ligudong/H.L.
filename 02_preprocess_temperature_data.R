source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")

mask <- rast("data/outputs/masks/mask_extraction.tif") %>% wrap()

plan(multisession, workers = 3)

# Load and process temperature rasters
temp <- tibble(
  files_ = list.files("data/raw/temperature/monthly/", pattern = "\\.tif$"),
  files  = list.files("data/raw/temperature/monthly/", full.names = TRUE, pattern = "\\.tif$")
) %>%
  mutate(
    date = str_extract(files_, "\\d{4}-\\d{2}-\\d{2}") %>% ymd(),
    year = year(date),
    month = month(date),
    .keep = "unused"
  ) %>%
  mutate(data = future_map(files, function(afile) {
    rast(afile) %>%
      resample(mask, method = "average") %>%
      mask(mask) %>%
      as.data.frame(xy = TRUE)
  },
  .progress = TRUE,
  .options = furrr_options(packages = c("terra", "tidyverse"))
  ),
  .keep = "unused"
  ) %>%
  unnest() %>%
  trim_xy() %>%
  rename(temp = last_col())  # Ensure last column is temperature value

saveRDS(temp, "data/outputs/temperature/temp.rds")

# Convert to Celsius
temp <- temp %>%
  mutate(temp_celsius = temp - 273.15)

# Define temperature bins
bins <- c(-Inf, seq(0, 40, by = 1), Inf)
bin_labels <- c("<0", as.character(0:39), ">40")

# Count frequencies by temperature bin
temperature_summary <- temp %>%
  mutate(
    bin = cut(temp_celsius, breaks = bins, labels = bin_labels, include.lowest = TRUE, right = FALSE)
  ) %>%
  group_by(year, month, x, y, bin) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = bin,
    values_from = freq,
    values_fill = 0
  )

# Rename bin columns
bin_names <- c("<0", as.character(0:39), ">40")
new_names <- paste0("temp_", seq_along(bin_names))
names(temperature_summary)[-(1:4)] <- new_names

# Save binned summary (compressed binary)
qsave(temperature_summary, "data/outputs/temperature/temp_bins.qs", nthreads = 5)

plan(sequential)
