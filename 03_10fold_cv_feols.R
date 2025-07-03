source("scripts/00_loadpackages.R")
source("scripts/loadFunctions.R")
source("scripts/loadFormulas.R")

# Load preprocessed panel data
data <- get_data()

# Number of folds for k-fold cross-validation
n_folds <- 10

# Nest the data by crop type for later processing
data <- data %>% nest(fdata = -crop_parent)

# Set a global random seed for reproducibility
set.seed(2)

# Perform k-fold cross-validation for each crop
data <- data %>%
  mutate(coefs = map(fdata, function(adata) {
    
    # Nest the data by 'city' for spatial-level demeaning
    adata <- adata %>%
      nest(.by = city, .key = "ffdata")
    
    # Loop over each fold and collect model results
    results <- map_dfr(1:n_folds, function(fold) {
      
      # Set fold-specific seed to ensure reproducibility
      set.seed(fold)
      
      # Unnest city-level data and demean variables by city and year
      aadata <- adata %>%
        unnest(ffdata) %>%
        group_by(city, year) %>%
        mutate(across(
          c(Caod, Faod, cloud, AOT40, bin1:bin42, root_1:root_9, d1:d13),
          ~ . - mean(.), .names = "demeaned_{.col}"
        )) %>%
        ungroup()
      
      # Estimate the model using fixed-effects regression (feols)
      # Cluster standard errors by spatial-temporal unit (x_y)
      model <- feols(
        fml_no_jiaohu, aadata,
        weights = ~fraction,
        nthreads = 0, notes = FALSE, lean = TRUE,
        cluster = ~x_y
      )
      
      # Extract tidy regression results
      tidy_res <- tidy(model)
      conf_int <- confint(model)
      tidy_res <- cbind(tidy_res, conf_int)
      
      # Extract model goodness-of-fit statistics
      glance_res <- glance(model)
      
      # Combine all results into a single data frame
      tidy_res %>%
        mutate(
          r_squared = glance_res$r.squared,
          adj_r_squared = glance_res$adj.r.squared,
          within_r_squared = glance_res$within.r.squared,
          fold_id = fold
        )
      
    }, .id = "id", .progress = TRUE)
    
    return(results)
  }))

# Summarize cross-validation results by averaging across folds
results_summary <- data %>%
  mutate(
    coefs_summary = map(coefs, function(coef_data) {
      coef_data %>%
        group_by(term) %>%
        summarise(
          mean_estimate = mean(estimate, na.rm = TRUE),
          mean_p_value = mean(p.value, na.rm = TRUE),
          mean_r_squared = mean(r_squared, na.rm = TRUE),
          mean_adj_r_squared = mean(adj_r_squared, na.rm = TRUE),
          mean_within_r_squared = mean(within_r_squared, na.rm = TRUE)
        )
    })
  ) %>%
  select(crop_parent, coefs_summary)

# Unnest the summary results for final presentation
final_results <- results_summary %>%
  unnest(coefs_summary)

# Print the final output (or save to file)
print(final_results)

# Optionally save results to file
# saveRDS(final_results, "data/summary_results.rds")
