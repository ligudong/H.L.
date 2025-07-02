#-------------- Get Faod Pollution Scenarios -----------------------

source("scripts/00_load_packages.R")
source("scrip/loadFormulas_Maize.R")

# Read fine-mode aerosol optical depth (fAOD) dataset
faod <- read_rds("data/outputs/aerosol/fAOD.rds")
summary(faod)

# Construct monthly climatology: normalize each month's Faod relative to its spatial mean
# This normalization (mean = 1) enables scalable simulation of seasonal pollution variations
month_climatology <- faod %>%
  fgroup_by(x, y, month) %>%                             # Group by spatial grid and month
  fsummarise(Faod_month = fmean(Faod)) %>%               # Compute monthly mean Faod
  fgroup_by(x, y) %>%                                    # Regroup by spatial grid
  fmutate(Faod_month_percent = Faod_month / fmean(Faod_month), 
          .keep = "unused") %>%                          # Normalize each month to get percentage
  fungroup()

# Construct Faod control scenarios at different pollution levels (0 to 1, step = 0.1)
faod_cft_all <- 
  tibble(
    faod_level = seq(0, 1, by = 0.1),  # Simulated pollution levels (from 0 to 1)
    faod_data = map(faod_level, function(anum) {
      month_climatology %>%
        distinct(x, y) %>%                            # Keep unique spatial points
        mutate(Faod = anum) %>%                       # Assign uniform pollution level
        mutate(Faod = fifelse(Faod < 0, 0, Faod)) %>% # Ensure Faod is non-negative
        inner_join(month_climatology) %>%            # Join with monthly climatology
        mutate(Faod_cft = Faod_month_percent * Faod, .keep = "unused") # Apply seasonal scaling
    })
  )

# Save the full set of control scenario data for downstream impact modeling
saveRDS(faod_cft_all, "data/faod_cft_scenario_0.1.rds")



#------------------ Calculate Relative Impacts under fAOD Control Scenarios ------------------

# Load required packages and model formulas
source("scripts/00_load_packages.R")
source("scrip/loadFormulas.R")

#--- Step 1: Filter crop calendar to ensure valid growing periods ---
# Keep only records where maturity (MA) occurs at least 2 units after germination/emergence (GR&EM)
# This ensures a biologically meaningful crop cycle and removes potential data errors
cldr <- read_rds("E:/data/outputs/calendar/tidied.rds") %>%
  filter((MA - `GR&EM`) >= 2) %>%
  trim_xy()

#--- Step 2: Load crop area fraction raster data for each crop and year ---
fraction <- tibble(
  crop = c("Maize", "Rice(LR)", "Rice(SR&ER)", "Wheat"),
  data = map(c("Maize", "Rice", "Rice", "Wheat"), function(acrop) {
    afile <- str_c("E:/data/outputs/masks/mask_", acrop, ".tif")
    rast(afile) %>%
      as.data.frame(xy = TRUE, na.rm = FALSE) %>%
      pivot_longer(
        -c(x, y),
        names_to = "year",
        names_transform = list(year = as.integer),
        values_drop_na = TRUE,
        values_to = "fraction"
      )
  })
) %>%
  unnest() %>%
  trim_xy()

#--- Step 3: Load model coefficients and harmonize crop labels ---
f1 <- read_rds("E:/data/boots_f1.rds") %>%
  mutate(crop = crop_parent) %>%
  bind_rows((.) %>% filter(crop == "Rice") %>% mutate(crop = "Rice(LR)")) %>%
  mutate(crop = fifelse(crop == "Rice", "Rice(SR&ER)", crop))

#--- Step 4: Load pollution datasets (fAOD & ozone) and merge ---
Faod <- read_rds("data/outputs/aerosol/fAOD.rds")
ozone <- read_rds("data/outputs/ozone/tidied.rds")  # unit: µg/m³

# Remove spatial-year combinations with missing ozone values to ensure alignment
rows_to_remove <- ozone %>%
  filter(is.na(O3)) %>%
  select(x, y, year) %>%
  distinct()

ozone <- ozone %>% anti_join(rows_to_remove, by = c("x", "y", "year"))

# Merge ozone and Faod by spatial and temporal coordinates
Faod <- merge(
  ozone[, c("x", "y", "year", "month", "AOT40")],
  Faod[, c("x", "y", "year", "month", "Faod")],
  by = c("x", "y", "year", "month"),
  all = TRUE
) %>% drop_na()

# Aggregate to crop-year level and join crop calendar and fraction data
Faod <- cldr %>%
  lazy_dt() %>%
  inner_join(Faod) %>%
  group_by(crop, x, y, year) %>%
  summarise(
    Faod = mean(Faod),
    AOT40 = mean(AOT40),
    .groups = "drop"
  ) %>%
  inner_join(fraction) %>%
  as_tibble() %>%
  nest(fdata = -c(crop, year)) %>%
  arrange(crop, year)

#--- Step 5: Load fAOD control scenarios (monthly normalized) ---
faod_ctr <- read_rds("data/faod_cft_scenario_0.1.rds")

# Apply seasonal scaling and aggregate to crop-year level for each scenario
faod_ctr <- faod_ctr %>%
  mutate(faod_data = map(faod_data, function(adata) {
    cldr %>%
      lazy_dt() %>%
      inner_join(adata, by = c("x", "y", "month")) %>%
      group_by(crop, year, x, y) %>%
      summarise(Faod_cft = mean(Faod_cft), .groups = "drop") %>%
      as_tibble()
  }, .progress = TRUE)) %>%
  unnest() %>%
  nest(faod_data = -c(crop, year))

#--- Step 6: Merge actual fAOD, model coefficients, and control scenarios ---
impacts <- reduce(list(Faod, f1, faod_ctr), inner_join)

#--- Step 7: Calculate relative impacts under each fAOD scenario ---------------------
plan(multisession, workers = 6)

impacts <- impacts %>%
  mutate(Faod_results = future_pmap(list(fdata, coefs, faod_data), function(adata, coefs, faod_data) {
    # Reshape coefficient table and retain only Faod-related terms
    coefs <- coefs %>%
      select(id, term, estimate) %>%
      pivot_wider(names_from = term, values_from = estimate) %>%
      select(contains("Faod"))
    
    # Merge actual and scenario data, and constrain Faod_cft ≤ Faod
    cal_data <- adata %>%
      inner_join(faod_data, by = c("x", "y")) %>%
      mutate(Faod_cft = fifelse(Faod_cft > Faod, Faod, Faod_cft))
    
    # Calculate difference terms and interaction terms with ozone
    rel_X <- cal_data %>%
      mutate(
        Faod1 = Faod_cft - Faod,
        Faod2 = Faod_cft^2 - Faod^2,
        OFaod1 = Faod1 * AOT40,
        OFaod2 = Faod2 * AOT40
      ) %>%
      select(Faod1, Faod2, OFaod1, OFaod2)
    
    # Compute impact estimates using matrix multiplication
    rel_results <- tcrossprod(as.matrix(rel_X), as.matrix(coefs)) %>%
      expm1() %>%
      rowQuantiles(probs = c(0.05, 0.5, 0.95)) %>%
      as_tibble() %>%
      bind_cols(cal_data %>% select(x, y, fraction, faod_level))
    
    return(rel_results)
  }, .progress = TRUE)) %>%
  select(-c(fdata, coefs, faod_data))

plan(sequential)

#--- Step 8: Save the results for visualization or further analysis ---
qsave(impacts, "data/impacts_Faod_0.1.qs", nthread = qn)

