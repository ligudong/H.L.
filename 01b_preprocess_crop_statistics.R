source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")

# --- Load province-level area data ---
Area <- tibble(
  files = list.files("data/inputs/province_data/area/", full.names = TRUE),
  data = map(files, ~ fread(.x, skip = 3, nrows = 31)),
  crop = map_chr(files, ~ fread(.x, skip = 1, nrows = 1) %>% pull(V1))
) %>%
  select(-files) %>%
  mutate(crop = str_remove_all(crop, "指标：|播种面积\\(千公顷\\)")) %>%
  unnest() %>%
  rename(year = 时间) %>%
  pivot_longer(-c(crop, year), names_to = "province", values_to = "area") %>%
  drop_na() %>%
  mutate(year = parse_number(year))

# --- Load production data ---
Production <- tibble(
  files = list.files("data/inputs/province_data/production/", full.names = TRUE),
  data = map(files, ~ fread(.x, skip = 3, nrows = 31)),
  crop = map_chr(files, ~ fread(.x, skip = 1, nrows = 1) %>% pull(V1))
) %>%
  select(-files) %>%
  mutate(crop = str_remove_all(crop, "指标：|产量\\(万吨\\)")) %>%
  unnest() %>%
  rename(year = 时间) %>%
  pivot_longer(-c(crop, year), names_to = "province", values_to = "production") %>%
  drop_na() %>%
  mutate(year = parse_number(year))

# --- Load population data ---
pop <- read_xlsx("data/inputs/province_data/population.xlsx", range = "A4:AF26") %>%
  rename(year = 时间) %>%
  pivot_longer(-year, names_to = "province", values_to = "population") %>%
  mutate(year = parse_number(year))

# --- Merge all three datasets and compute yield ---
stats <- inner_join(Area, Production, by = c("crop", "province", "year")) %>%
  inner_join(pop, by = c("province", "year")) %>%
  mutate(
    production = production * 1e4,  # convert from 10,000 tons to kg
    crop = fcase(
      crop == "小麦", "Wheat",
      crop == "玉米", "Maize",
      crop == "稻谷", "Rice"
    )
  ) %>%
  drop_na() %>%
  group_by(crop, year) %>%
  summarise(
    yield = sum(production) / sum(area),
    area = sum(area),
    population = sum(population),
    .groups = "drop"
  )

saveRDS(stats, "data/outputs/crop_statistics/summary_by_crop_year.rds")
