# Load required packages and custom functions
source("script/00_loadPackages.R")
source("script/loadFunctions.R")

# Load spatial mask raster and wrap it to ensure correct processing
mask <- rast("data/outputs/masks/mask_extraction.tif") %>% wrap()

# Set parallel processing plan with 3 workers
plan(multisession, workers = 3)

# Load and preprocess fPAR raster files
fpar <- tibble(
  files_ = list.files("data/raw/vpr/fpar4g", pattern = "\\.tif$"),             # List file names (short)
  files  = list.files("data/raw/vpr/fpar4g", full.names = TRUE, pattern = "\\.tif$") # List full file paths
) %>%
  mutate(
    year  = substr(files_, 7, 10),     # Extract year from file name
    month = substr(files_, 11, 12)     # Extract month from file name
  ) %>%
  mutate(date = paste0(year, "-", month, "-01") %>% ymd()) %>%  # Generate date string
  mutate(
    year  = year(date),  # Ensure numeric year
    month = month(date), # Ensure numeric month
    .keep = "unused"
  ) %>%
  mutate(
    data = future_map(files, function(afile) {
      # Read each .tif file as raster
      r <- rast(afile)
      
      # Resample to match mask resolution, apply spatial mask, and convert to dataframe
      resample(r, rast(mask), method = "average") %>%
        mask(rast(mask)) %>%
        as.data.frame(xy = TRUE)
    },
    .progress = TRUE,
    .options = furrr_options(packages = c("terra", "tidyverse"))  # Load necessary packages inside future workers
    ),
    .keep = "unused"
  )

# Switch back to sequential mode (important after parallel tasks are done)
plan(sequential)

# Flatten the nested data and trim invalid coordinates (e.g., out-of-bound pixels)
fpar <- fpar %>%
  unnest() %>%
  trim_xy()

# Perform coordinate validation check (custom function, checks missing or invalid entries)
check_join(fpar)

# Create a single fPAR column by summing across all bands (e.g., different layers)
fpar <- fpar %>%
  mutate(
    fpar = rowSums(select(., -(1:5)), na.rm = TRUE)  # Sum all columns after column 5
  ) %>%
  select(1:5, fpar)  # Retain coordinate and date info + final fpar

# Drop temporary column if present
fpar <- fpar %>%
  select(-files_)

# Save fpar data as RDS (optionally save for each crop)
# saveRDS(fpar, "data/outputs/fpar_maize.rds")
# saveRDS(fpar, "data/outputs/fpar_Rice.rds")
# saveRDS(fpar, "data/outputs/fpar_Wheat.rds")
saveRDS(fpar, "data/outputs/fpar.rds")  # Save general fpar output
