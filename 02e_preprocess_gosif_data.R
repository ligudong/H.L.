source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")

mask <- rast("data/outputs/masks/mask_extraction.tif") %>% wrap()


# 1. GOSIF (monthly, 5km) -------------------------------------------
gosif <- tibble(
  files_ = list.files("data/raw/sif/GOSIF_monthly", pattern = "\\.tif$"),
  files  = list.files("data/raw/sif/GOSIF_monthly", full.names = TRUE, pattern = "\\.tif$")
) %>%
  mutate(
    year = str_extract(files_, "(?<=GOSIF_)\\d{4}"),
    month = str_extract(files_, "(?<=\\.M)\\d{2}"),
    date = ymd(paste0(year, "-", month, "-01"))
  ) %>%
  mutate(
    year = year(date),
    month = month(date),
    .keep = "unused"
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

plan(sequential)

gosif <- gosif %>%
  unnest() %>%
  trim_xy()

saveRDS(gosif, "data/outputs/sif/GOSIF.rds")

# 2. RTSIF (8-day, aggregated monthly) ------------------
rtsif <- tibble(
  files_ = list.files("data/raw/sif/RTSIF_8day", pattern = "\\.tif$"),
  files  = list.files("data/raw/sif/RTSIF_8day", full.names = TRUE, pattern = "\\.tif$")
) %>%
  separate(files_, into = c(NA, "date", NA), sep = "_|\\.") %>%
  mutate(
    date = ymd(date),
    ym = str_c(year(date), "_", month(date)),
    data = map(files, rast)
  )

rtsif_stack <- rast(rtsif$data)
rtsif_monthly <- tapp(rtsif_stack, rtsif$ym, mean) %>%
  resample(mask, method = "near") %>%
  mask(mask)

rtsif <- rtsif_monthly %>%
  as.data.frame(xy = TRUE) %>%
  pivot_longer(-c(x, y),
               names_prefix = "X",
               names_to = c("year", "month"),
               names_sep = "_",
               names_transform = list(year = as.integer, month = as.integer),
               values_to = "RTSIF"
  ) %>%
  trim_xy()

saveRDS(rtsif, "data/outputs/sif/RTSIF.rds")

# 3. CSIF (monthly, netCDF format) ---------------------------------
csif <- tibble(
  files_ = list.files("data/raw/sif/CSIF_monthly", recursive = TRUE, pattern = "\\.nc$"),
  files  = list.files("data/raw/sif/CSIF_monthly", recursive = TRUE, full.names = TRUE, pattern = "\\.nc$")
) %>%
  separate(files_, into = c(NA, NA, NA, NA, "date", NA, NA), sep = "\\.") %>%
  mutate(
    date = parse_date_time(date, "Yj"),
    year = year(date),
    month = month(date)
  ) %>%
  filter(date >= ymd("2000-03-01")) %>%
  mutate(
    data = pro_map(files, function(afile) {
      temp <- raster(afile, varname = "clear_daily_SIF")[]
      rast(resolution = c(0.05, 0.05)) %>%
        terra::`values<-`(temp) %>%
        resample(mask, method = "near") %>%
        mask(mask)
    }),
    .keep = "unused"
  )

csif_data <- tapp(rast(csif$data), str_c(csif$year, "_", csif$month), mean) %>%
  as.data.frame(xy = TRUE) %>%
  pivot_longer(-c(x, y),
               names_prefix = "X",
               names_to = c("year", "month"),
               names_sep = "_",
               names_transform = list(year = as.integer, month = as.integer),
               values_to = "CSIF"
  ) %>%
  trim_xy()

saveRDS(csif_data, "data/outputs/sif/CSIF.rds")
