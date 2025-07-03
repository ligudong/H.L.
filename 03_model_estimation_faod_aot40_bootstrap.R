# Load required packages and user-defined functions
source("scripts/00_load_packages.R")
source("scripts/loadFunctions.R")
source("scrip/loadFormulas.R")

# ------------------------------------------------------------------------------
# Bootstrap estimation of model coefficients (500 resamples) -------------------
# ------------------------------------------------------------------------------

# Load cleaned dataset
data <- qread("E:/data/tidied.qs")

# Apply additional formatting or filtering if required
data <- get_data()

# Set bootstrap sample size
n <- 500

# Nest data by crop type (e.g., Maize, Rice, Wheat)
data <- data %>% nest(fdata = -crop_parent)

# Perform bootstrap regression for each crop
data <- data %>%
  mutate(coefs = map(fdata, function(adata) {
    
    # Nest data by county (or administrative unit)
    adata <- adata %>% nest(.by = county, .key = "ffdata")
    
    # Bootstrap loop over n iterations
    map_dfr(1:n, function(anum) {
      set.seed(anum)  # ensure reproducibility
      
      # Resample counties with replacement and unnest to flat dataframe
      aadata <- adata %>%
        slice_sample(prop = 1, replace = TRUE) %>%
        unnest(ffdata)
      
      # Run fixed-effects regression
      feols(fml_base, aadata,
            weights = ~fraction,
            nthreads = 0, notes = FALSE, lean = TRUE
      ) %>%
        tidy()  # Extract coefficient results
    }, .id = "id", .progress = TRUE)
  }))

# Save results (excluding the large nested data)
saveRDS(data %>% select(-fdata), "E:/data/boots_f1.rds")

# ------------------------------------------------------------------------------
# Visualize coefficient distribution (boxplot for Faod and AOT40) --------------
# ------------------------------------------------------------------------------

# Extract bootstrap coefficients for specific variables
coef_data <- data %>%
  unnest(coefs) %>%
  filter(term %in% c("Faod", "AOT40")) %>%
  select(crop_parent, term, estimate, id)

# Plot boxplot to compare coefficient distributions across crops
boxplot <- coef_data %>%
  ggplot(aes(x = term, y = estimate, fill = term)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  facet_wrap(~crop_parent, scales = "free") +
  scale_fill_manual(values = c("Faod" = "#1f77b4", "AOT40" = "#ff7f0e")) +
  labs(
    title = "Bootstrap Coefficient Distributions for Faod and AOT40",
    x = "Variable",
    y = "Coefficient Estimate",
    fill = "Variable"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

print(boxplot)

# ------------------------------------------------------------------------------
# Compute summary statistics from bootstrap distribution ------------------------
# ------------------------------------------------------------------------------

data <- get_data()  # Re-load or re-prepare data
n <- 500
data <- data %>% nest(fdata = -crop_parent)

# Compute coefficient mean, p-value, and R² per crop and term
data <- data %>%
  mutate(coefs = map(fdata, function(adata) {
    adata <- adata %>% nest(.by = city, .key = "ffdata")
    
    map_dfr(1:n, function(anum) {
      set.seed(3)
      aadata <- adata %>%
        slice_sample(prop = 1, replace = TRUE) %>%
        unnest(ffdata)
      
      model <- feols(fml_base, aadata,
                     weights = ~fraction,
                     nthreads = 0, notes = FALSE, lean = TRUE,
                     cluster = ~x_y)
      
      tidy_res <- tidy(model)
      conf_int <- confint(model)
      r_squared <- glance(model)$r.squared
      tidy_res <- cbind(tidy_res, conf_int)
      tidy_res$r_squared <- r_squared
      
      tidy_res
    }, .id = "id", .progress = TRUE)
  }))

# Compute aggregated statistics
results_summary <- data %>%
  mutate(
    coefs_summary = map(coefs, function(coef_data) {
      coef_data %>%
        group_by(term) %>%
        summarise(
          mean_estimate = mean(estimate, na.rm = TRUE),
          mean_p_value = mean(p.value, na.rm = TRUE),
          mean_r_squared = mean(r_squared, na.rm = TRUE)
        )
    })
  ) %>%
  select(crop_parent, coefs_summary)

final_results <- results_summary %>% unnest(coefs_summary)
final_results

# ------------------------------------------------------------------------------
# Estimate fixed-effects model without bootstrapping ---------------------------
# ------------------------------------------------------------------------------

data <- get_data()
data <- data %>% nest(fdata = -crop_parent)

# Direct regression for each crop without resampling
data <- data %>%
  mutate(coefs = map(fdata, function(adata) {
    adata <- adata %>% nest(.by = county, .key = "ffdata")
    aadata <- adata %>% unnest(ffdata)
    
    model <- feols(fml_base, aadata,
                   weights = ~fraction,
                   nthreads = 0, notes = FALSE, lean = TRUE)
    
    tidy_res <- tidy(model)
    conf_int <- confint(model)
    tidy_res <- cbind(tidy_res, conf_int)
    glance_res <- glance(model)
    
    list(tidy_res = tidy_res, glance_res = glance_res)
  }))

# Extract coefficient table and model fit stats
coef_results <- data %>%
  mutate(tidy_res = map(coefs, "tidy_res")) %>%
  select(crop_parent, tidy_res) %>%
  unnest(tidy_res)

goodness_of_fit <- data %>%
  mutate(glance_res = map(coefs, "glance_res")) %>%
  select(crop_parent, glance_res) %>%
  unnest(glance_res)
