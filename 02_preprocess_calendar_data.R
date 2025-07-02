source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")

exts <- rast("data/raw/calendar/CHN_Maize_HE_2019.tif") %>% ext()

# Define crop growth stages and matching patterns
datap <- tibble(
  period_ = c(
    "Maize_HE", "Maize_MA", "Maize_V3",
    "Rice\\(LR\\)_HE", "Rice\\(LR\\)_MA", "Rice\\(LR\\)_TR",
    "Rice\\(SR&ER\\)_HE", "Rice\\(SR&ER\\)_MA", "Rice\\(SR&ER\\)_TR",
    "Wheat_GR&EM", "Wheat_HE", "Wheat_MA"
  ),
  period = c(
    "Maize_HE", "Maize_MA", "Maize_V3",
    "Rice(LR)_HE", "Rice(LR)_MA", "Rice(LR)_TR",
    "Rice(SR&ER)_HE", "Rice(SR&ER)_MA", "Rice(SR&ER)_TR",
    "Wheat_GR&EM", "Wheat_HE", "Wheat_MA"
  )
) %>%
  # Standardize phenological stage names
  mutate(
    period = str_replace_all(period, "V3|TR", "GR&EM"),
    
    # Load, project, resample, and extract monthly data for each period
    data = map2(period_, period, function(aperiod, ap) {
      print(aperiod)
      
      all <- list.files("data/raw/calendar/", aperiod, full.names = TRUE) %>%
        map(rast) %>%
        map(~ extend(.x, exts)) %>%
        rast() %>%
        `names<-`(2000:2019) %>%
        project("epsg:4326", method = "near")
      
      # Resample to 0.05° resolution
      degree <- rast(resolution = 0.05) %>%
        crop(all, snap = "out")
      all <- all %>% resample(degree, "average")
      
      # Convert raster stack to tidy dataframe
      all <- all %>%
        as.data.frame(xy = TRUE, na.rm = FALSE) %>%
        pivot_longer(-c(x, y), names_to = "year", names_transform = list(year = as.integer), values_drop_na = TRUE) %>%
        mutate(
          value = round(value) %>%
            as.character() %>%
            str_c(year, "-", .) %>%
            parse_date_time("Y-j"),
          month = month(value),
          day = day(value)
        )
      
      # Determine final stage month based on crop stage
      if (str_detect(ap, "GR&EM")) {
        all <- all %>% mutate(month_fnl = fifelse(day >= 15, month + 1, month))
      } else if (str_detect(ap, "MA")) {
        all <- all %>% mutate(month_fnl = fifelse(day >= 15, month, month - 1))
      } else if (str_detect(ap, "HE")) {
        all <- all %>% mutate(month_fnl = month)
      }
      
      all %>% select(x, y, year, month_fnl)
    })
  ) %>%
  select(-period_)

# Combine all crop-period data
datap <- datap %>%
  separate(period, c("crop", "stage"), "_") %>%
  unnest(cols = c(data)) %>%
  pivot_wider(names_from = "stage", values_from = month_fnl) %>%
  mutate(
    # Generate full monthly sequence between emergence and maturity
    month = map2(`GR&EM`, MA, ~ if (!is.na(.x) && !is.na(.y)) seq(.x, .y) else NA)
  ) %>%
  unnest(cols = month) %>%
  filter(MA >= `GR&EM`)

saveRDS(datap, "data/outputs/calendar/tidied.rds")
