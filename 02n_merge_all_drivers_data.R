library(collapse, exclude = "F")
library(tidyverse)
library(terra)
library(Matrix)
library(dtplyr)
library(sf)
sf_use_s2(FALSE)
library(qs)

# Utility function to round x/y coordinates
trim_xy <- function(adata_xy) {
  adata_xy %>% mutate(across(c(x, y), ~ round(.x, 4)))
}

# -------------------------------------------
# Step 1: Load and filter planting calendar
# -------------------------------------------
cldr <- read_rds("data/outputs/calendar/tidied.rds") %>%
  filter((MA - `GR&EM`) >= 2) %>%
  trim_xy()

# -------------------------------------------
# Step 2: Load crop fraction mask data
# -------------------------------------------
fraction <- tibble(
  crop = c("Maize", "Rice(LR)", "Rice(SR&ER)", "Wheat"),
  data = map(c("Maize", "Rice", "Rice", "Wheat"), function(acrop) {
    afile <- str_c("data/outputs/masks/mask_", acrop, ".tif")
    rast(afile) %>%
      as.data.frame(xy = TRUE, na.rm = FALSE) %>%
      pivot_longer(-c(x, y), names_to = "year", names_transform = list(year = as.integer),
                   values_drop_na = TRUE, values_to = "fraction")
  })
) %>% unnest() %>% trim_xy()

# -------------------------------------------
# Step 3: Binning temperature data
# -------------------------------------------
temp_bins <- qread("data/outputs/temperature/temp_bins.qs") %>%
  rename_with(~ str_replace(.x, "^temp_", "bins"), starts_with("temp"))

df <- 4
bins <- seq(1, 42, by = 1)
steps <- seq(min(bins), max(bins), df)
idx <- rep(1:length(steps), each = df)[1:length(bins)]

B <- sparseMatrix(i = 1:length(bins), j = idx, x = 1)
bindata <- temp_bins[, grepl("bin", names(temp_bins))]
names(bindata) <- paste0("bin", bins)
bdata <- as.matrix(bindata) %*% B %>% as.data.frame()
colnames(bdata) <- paste0("step", unique(idx))
temp_bins <- bind_cols(temp_bins, bdata)

# -------------------------------------------
# Step 4: Load additional environmental and crop variables
# -------------------------------------------
GOSIF <- read_rds("data/outputs/sif/GOSIF.rds") %>% mutate(GOSIF = GOSIF * 0.0001)
tmax <- read_rds("data/outputs/temperature/tmax.rds") %>%
  mutate(maxtmp = temp - 273.15) %>% select(-files_,-temp) %>%
  group_by(x, y, year, month) %>% summarise(maxtmp = mean(maxtmp, na.rm = TRUE))
prep <- read_rds("data/outputs/precip/prep.rds")
surface <- read_rds("data/outputs/soil_moisture/surface.rds")
root <- read_rds("data/outputs/soil_moisture/root.rds")
sm_root <- read_rds("data/outputs/soil_moisture/root_data.rds")
sm_surface <- read_rds("data/outputs/soil_moisture/surface_data.rds")
cloud <- read_rds("data/outputs/cloud/tidied.rds")
Caod <- read_rds("data/outputs/aerosol/cAOD.rds")
Faod <- read_rds("data/outputs/aerosol/fAOD.rds")
ozone <- read_rds("data/outputs/ozone/tidied.rds")

irrigation <- read_rds("data/outputs/irrigation/tidied.rds") %>%
  fgroup_by(x, y) %>% fsummarise(irg_fraction = fmean(irg_fraction)) %>% fungroup()

fertilizer_deep <- read_rds("data/outputs/fertilizer/fertilizer_deep.rds") %>%
  select(-position) %>%
  rename_with(~ paste0("D", seq_along(.)), everything())
fertilizer_surface <- read_rds("data/outputs/fertilizer/fertilizer_surface.rds") %>%
  select(-position) %>%
  rename_with(~ paste0("d", seq_along(.)), everything())

# Ensure all year/month columns are integers
vars_to_convert <- list(cldr, fraction, GOSIF, temp_bins, tmax, prep, surface, root,
                        cloud, Faod, Caod, sm_root, sm_surface)
vars_to_convert <- lapply(vars_to_convert, function(df) df %>% mutate(across(c(year, month), as.integer)))
list2env(setNames(vars_to_convert, c("cldr","fraction","GOSIF","temp_bins","tmax","prep",
                                     "surface","root","cloud","Faod","Caod","sm_root","sm_surface")), .GlobalEnv)

# -------------------------------------------
# Step 5: Merge all variables
# -------------------------------------------
data <- cldr %>%
  inner_join(fraction) %>%
  left_join(GOSIF) %>%
  left_join(temp_bins) %>%
  left_join(tmax) %>%
  left_join(prep) %>%
  left_join(surface) %>%
  left_join(root) %>%
  left_join(sm_root) %>%
  left_join(sm_surface) %>%
  left_join(cloud) %>%
  left_join(fertilizer_deep) %>%
  left_join(fertilizer_surface) %>%
  left_join(Faod) %>%
  left_join(Caod)

# -------------------------------------------
# Step 6: Spatial join to county/city/province codes
# -------------------------------------------
shp_file <- st_read("E:/data/inputs/data_archive/shp/县.shp") %>%
  transmute(county = NAME, county_code = PAC,
            city = 市, city_code = 市代码,
            province = 省, province_code = 省代码)

xy_info <- data %>%
  distinct(x, y) %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(shp_file), remove = FALSE) %>%
  st_intersection(shp_file) %>%
  st_drop_geometry()

# -------------------------------------------
# Step 7: Aggregate ozone over ±n months
# -------------------------------------------
ozone_s <- map(1:7, function(anum) {
  cldr %>% filter((MA - month) < anum) %>%
    inner_join(fraction) %>%
    left_join(ozone) %>%
    lazy_dt() %>%
    group_by(x, y, year, crop) %>%
    summarise(O3 = mean(O3), across(c(W126, AOT40), sum)) %>%
    as_tibble() %>%
    rename_with(~ str_c(.x, "_", anum), c("O3", "W126", "AOT40"))
}) %>% reduce(full_join)

# -------------------------------------------
# Step 8: Final aggregation
# -------------------------------------------
fnl_data <- data %>%
  lazy_dt() %>%
  group_by(x, y, year, crop) %>%
  summarise(
    across(c(GOSIF), list(peak = max, sum = sum)),
    across(c(cloud, Faod, Caod, HE, MA, `GR&EM`, maxtmp, mean_root,
             mean_surface, GOSIF, fraction), mean),
    across(c(prep, starts_with("bin"), starts_with("step"), starts_with("D"),
             starts_with("d"), starts_with("surface_"), starts_with("root_")), sum)
  ) %>%
  mutate(x_y = str_c(x, "_", y)) %>%
  inner_join(xy_info) %>%
  inner_join(irrigation) %>%
  left_join(ozone_s) %>%
  as_tibble() %>%
  filter(!is.na(GOSIF))

fnl_data[] <- lapply(fnl_data, function(x) ifelse(is.na(x), 0, x))
fnl_data <- fnl_data %>% rename_with(~ str_replace_all(., "bins", "bin"))

# -------------------------------------------
# Step 9: Save final dataset
# -------------------------------------------
qsave(fnl_data, "data/outputs/tidied.qs", nthreads = 10)
