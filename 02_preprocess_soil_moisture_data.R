source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")

mask <- rast("data/outputs/masks/mask_extraction.tif") %>% wrap()

plan(multisession, workers = 3)

# -------------------------------
# 1. Surface Soil Moisture
# -------------------------------
surface_data <- tibble(
  files_ = list.files("data/raw/soil_moisture/surface/", pattern = "\\.tif$"),
  files  = list.files("data/raw/soil_moisture/surface/", full.names = TRUE, pattern = "\\.tif$")
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
  ), .keep = "unused") %>%
  unnest() %>%
  trim_xy() %>%
  rename(surface = last_col())

# Save raw surface data
saveRDS(surface_data, "data/outputs/soil_moisture/surface_data.rds")

# Bin surface values and count frequencies
surface_summary <- surface_data %>%
  mutate(bin = cut(surface,
                   breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 1000),
                   labels = c("0–5", "5–10", "10–15", "15–20", "20–25", "25–30", "30–35", "35–40", "40–1000"),
                   include.lowest = TRUE)) %>%
  group_by(year, month, x, y, bin) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = bin,
    values_from = freq,
    values_fill = 0
  ) %>%
  rename_with(~ paste0("surface_", seq_along(.x)), -c(x, y, year, month))

saveRDS(surface_summary, "data/outputs/soil_moisture/surface.rds")


# -------------------------------
# 2. Root-zone Soil Moisture
# -------------------------------
root_data <- tibble(
  files_ = list.files("data/raw/soil_moisture/root/", pattern = "\\.tif$"),
  files  = list.files("data/raw/soil_moisture/root/", full.names = TRUE, pattern = "\\.tif$")
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
  ), .keep = "unused") %>%
  unnest() %>%
  trim_xy() %>%
  rename(root = last_col())

saveRDS(root_data, "data/outputs/soil_moisture/root_data.rds")

# Bin root-zone values and count frequencies
root_summary <- root_data %>%
  mutate(bin = cut(root,
                   breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 10000),
                   labels = c("0–100", "100–200", "200–300", "300–400", "400–500",
                              "500–600", "600–700", "700–800", "800–10000"),
                   include.lowest = TRUE)) %>%
  group_by(year, month, x, y, bin) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = bin,
    values_from = freq,
    values_fill = 0
  ) %>%
  rename_with(~ paste0("root_", seq_along(.x)), -c(x, y, year, month))

saveRDS(root_summary, "data/outputs/soil_moisture/root.rds")

plan(sequential)
