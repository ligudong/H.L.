source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")

data2 <-readRDS("E:/data/outputs/p1.rds")

# --- Part 1: Filter for Selected Scenario Values ---
Faod <- Faod %>%
  filter(faod_level == 0.40)

# ozone <- ozone %>% filter(peak_level == 100)
ozone <- ozone %>%
  filter(peak_level == 80)  # Filter ozone peak level to 80 ppb

# --- Part 2: Fill Missing Years for fAOD Using ARIMA ---
library(data.table)
library(forecast)

Faod <- as.data.table(Faod)
all_years <- 2005:2022

all_data <- CJ(
  crop_parent = unique(Faod$crop_parent),
  year = all_years,
  faod_level = 0.4
)

Faod_full <- merge(all_data, Faod, by = c("crop_parent", "year", "faod_level"), all.x = TRUE)

for (crop in unique(Faod_full$crop_parent)) {
  crop_data <- Faod_full[crop_parent == crop]
  for (var in c("aer5%", "aer50%", "aer95%")) {
    ts_data <- ts(crop_data[[var]], start = 2005, frequency = 1)
    if (any(is.na(ts_data))) {
      fit <- auto.arima(ts_data, stepwise = FALSE, approximation = FALSE)
      predicted <- forecast(fit, h = sum(is.na(ts_data)))$mean
      crop_data[is.na(get(var)), (var) := predicted]
    }
  }
  Faod_full[crop_parent == crop] <- crop_data
}

# --- Part 3: Fill Missing Years for Ozone Data Using ARIMA ---
ozone <- as.data.table(ozone)
ozone <- ozone[peak_level == 80]

all_data <- CJ(
  crop_parent = unique(ozone$crop_parent),
  year = all_years,
  peak_level = 80
)

ozone_full <- merge(all_data, ozone, by = c("crop_parent", "year", "peak_level"), all.x = TRUE)

for (crop in unique(ozone_full$crop_parent)) {
  crop_data <- ozone_full[crop_parent == crop]
  for (var in c("ozo5%", "ozo50%", "ozo95%")) {
    ts_data <- ts(crop_data[[var]], start = 2005, frequency = 1)
    if (any(is.na(ts_data))) {
      fit <- auto.arima(ts_data, stepwise = FALSE, approximation = FALSE)
      predicted <- forecast(fit, h = sum(is.na(ts_data)))$mean
      crop_data[is.na(get(var)), (var) := predicted]
    }
  }
  ozone_full[crop_parent == crop] <- crop_data
}

# --- Part 4: Merge fAOD and Ozone Data with Crop Statistics ---
joined <- inner_join(stats, Faod_full, by = c("crop" = "crop_parent", "year")) %>%
  inner_join(ozone_full, by = c("crop" = "crop_parent", "year"))

# --- Part 5: Calculate Calorie Intake Metrics ---
joined <- joined %>%
  mutate(
    n = fcase(
      crop == "Wheat", 0.78,
      crop == "Rice", 1,
      crop == "Maize", 0.79
    ),
    w = fcase(
      crop == "Wheat", 0.2,
      crop == "Rice", 0.1,
      crop == "Maize", 0.7
    ),
    E = fcase(
      crop == "Wheat", 3391.67,
      crop == "Rice", 3882.05,
      crop == "Maize", 3622.95
    ),
    kcal_percapita_perday = 0.44 * area * yield * 1e3 * n * (1 - w) * E / (population * 1e4) / 365,
    kcal_percapita_perday_our = 0.44 * area * (yield + `aer50%` * yield + `ozo50%` * yield ) * 1e3 * n * (1 - w) * E / (population * 1e4) / 365,
    kcal_percapita_perday_our_up = 0.44 * area * (yield + `aer95%` * yield + `ozo95%` * yield) * 1e3 * n * (1 - w) * E / (population * 1e4) / 365,
    kcal_percapita_perday_our_lw = 0.44 * area * (yield + `aer5%` * yield + `ozo5%` * yield) * 1e3 * n * (1 - w) * E / (population * 1e4) / 365,
    kcal_percapita_perday_our2 = 0.44 * area * (yield + `aer50%` * yield + `ozo50%` * yield) * 1e3 * n * (1 - w) * E / (population * 1e4) / 365,
    kcal_percapita_perday_our_up2 = 0.44 * area * (yield + `aer95%` * yield + `ozo95%` * yield ) * 1e3 * n * (1 - w) * E / (population * 1e4) / 365,
    kcal_percapita_perday_our_lw2 = 0.44 * area * (yield + `aer5%` * yield + `ozo5%` * yield) * 1e3 * n * (1 - w) * E / (population * 1e4) / 365,
    kcal_percapita_perday_delete = kcal_percapita_perday_our - kcal_percapita_perday,
    kcal_percapita_perday_delete_up = kcal_percapita_perday_our_up - kcal_percapita_perday,
    kcal_percapita_perday_delete_lw = kcal_percapita_perday_our_lw - kcal_percapita_perday
  )

# --- Part 6: Export to Excel for Reference ---
library(writexl)
write_xlsx(joined, path = "E:/data/outputs/joined_export.xlsx")

# --- Part 7: Calculate Crop-Averaged Calorie Intake ---
joined_yearly_avg <- joined %>%
  group_by(crop) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")


# Merge statistical crop data with Faod and Ozone impact estimates
joined <- stats %>%
  inner_join(Faod_full, by = c("crop" = "crop_parent", "year")) %>%
  inner_join(ozone_full, by = c("crop" = "crop_parent", "year"))

# Add crop-specific nutritional parameters:
# n: conversion ratio (harvested-to-edible)
# w: moisture content
# E: energy content (kCal/kg)
joined <- joined %>%
  mutate(
    n = fcase(crop == "Wheat", 0.78, crop == "Rice", 1, crop == "Maize", 0.79),
    w = fcase(crop == "Wheat", 0.2,  crop == "Rice", 0.1, crop == "Maize", 0.7),
    E = fcase(crop == "Wheat", 3391.67, crop == "Rice", 3882.05, crop == "Maize", 3622.95)
  )

# Calculate national average per capita calorie supply under observed and counterfactual pollution scenarios
joined <- joined %>%
  mutate(
    kcal_percapita_perday = 0.44 * area * yield * 1e3 * n * (1 - w) * E / (population * 1e4) / 365,
    kcal_percapita_perday_our = 0.44 * area * (yield + `aer50%` * yield + `ozo50%` * yield) * 1e3 * n * (1 - w) * E / (population * 1e4) / 365,
    kcal_percapita_perday_our_up = 0.44 * area * (yield + `aer95%` * yield + `ozo95%` * yield) * 1e3 * n * (1 - w) * E / (population * 1e4) / 365,
    kcal_percapita_perday_our_lw = 0.44 * area * (yield + `aer5%` * yield + `ozo5%` * yield) * 1e3 * n * (1 - w) * E / (population * 1e4) / 365,
    
    # Alternate version (duplicated for robustness)
    kcal_percapita_perday_our2 = 0.44 * area * (yield + `aer50%` * yield + `ozo50%` * yield) * 1e3 * n * (1 - w) * E / (population * 1e4) / 365,
    kcal_percapita_perday_our_up2 = 0.44 * area * (yield + `aer95%` * yield + `ozo95%` * yield) * 1e3 * n * (1 - w) * E / (population * 1e4) / 365,
    kcal_percapita_perday_our_lw2 = 0.44 * area * (yield + `aer5%` * yield + `ozo5%` * yield) * 1e3 * n * (1 - w) * E / (population * 1e4) / 365,
    
    # Absolute losses due to pollution (difference between baseline and counterfactual)
    kcal_percapita_perday_delete = kcal_percapita_perday_our - kcal_percapita_perday,
    kcal_percapita_perday_delete_up = kcal_percapita_perday_our_up - kcal_percapita_perday,
    kcal_percapita_perday_delete_lw = kcal_percapita_perday_our_lw - kcal_percapita_perday
  )


