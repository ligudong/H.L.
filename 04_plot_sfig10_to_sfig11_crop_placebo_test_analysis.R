# Load necessary custom scripts
source("scripts/00_loadPackages.R")  
source("scrip/loadFunctio.R")        
source("scrip/loadFormulas.R")      

# Load data using the custom get_data function
data <- get_data()

# Set the number of bootstrap samples
n <- 500

# Nest the data by crop type (for example, maize, rice, wheat)
data <- data %>% nest(fdata = -crop_parent)

# Perform bootstrap regression analysis for each crop
data <- data %>%
  mutate(coefs = map(fdata, function(adata) {
    # Nest data by county
    adata <- adata %>%
      nest(.by = county, .key = "ffdata")
    
    # Run regression for each bootstrap sample
    map_dfr(1:n, function(anum) {
      set.seed(3)  # Set seed for reproducibility
      
      # Sample data with replacement for bootstrap
      aadata <- adata %>%
        slice_sample(prop = 1, replace = TRUE) %>%
        unnest(ffdata)
      
      # Perform fixed-effects regression
      model <- feols(fml_fix1, aadata,  
                     weights = ~fraction, 
                     nthreads = 0, 
                     notes = FALSE, 
                     lean = TRUE, 
                     cluster = ~x_y)
      
      # Extract regression results
      tidy_res <- tidy(model)
      conf_int <- confint(model)
      r_squared <- glance(model)$r.squared
      
      # Combine regression results with confidence intervals
      tidy_res <- cbind(tidy_res, conf_int)
      tidy_res$r_squared <- r_squared  # Add R-squared to results
      
      tidy_res  # Return results for the current bootstrap sample
    }, .id = "id", .progress = TRUE)  # Track progress
  }))

# Calculate summary statistics (mean) for each coefficient
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

# Unnest results for easy access and viewing
final_results <- results_summary %>%
  unnest(coefs_summary)

# Print the results
print(final_results)


# ----------------------------
# Add placebo test (shuffle time - individual fixed effects)
# ----------------------------

# Load AOT40 data for placebo test
ozone_data <- qread("data/impacts_ozone_AOT40.qs", nthreads = qn)

# Create placebo test data (AOT40 shuffled only)
# Define the crop for placebo (e.g., "Maize", "Rice", or "Wheat")
maize_data <- get_data() %>% filter(crop_parent == "Wheat")

# Set parameters for placebo test
set.seed(123)
n_simulations <- 500
placebo_results <- data.frame(aot40_beta = numeric(n_simulations))

# Perform placebo test for AOT40
for (i in 1:n_simulations) {
  
  # Shuffle AOT40 data, keep other variables intact
  maize_data_placebo <- maize_data %>%
    mutate(AOT40_placebo = sample(AOT40))   # Shuffle only AOT40
  
  # Construct the model formula for placebo test
  fml_placebo <- str_c("GOSIF_sum ~
            Caod + Caod^2 +
            Faod + Faod^2 +
            cloud + cloud^2 + AOT40_placebo + Faod*AOT40_placebo + (Faod^2)*AOT40_placebo +
            ", add_terms(c(
              str_c("bin", 1:42),
              str_c("root_", 1:9),
              str_c("d", 1:13)
            )), "|
            x_y[year]") %>%    # Fixed effects based on x_y[year]
    as.formula()
  
  # Run the regression for placebo test
  model_placebo <- feols(fml_placebo, data = maize_data_placebo, weights = ~fraction, notes = FALSE, lean = TRUE)
  
  # Record the coefficient for AOT40_placebo from the model
  placebo_results$aot40_beta[i] <- coef(model_placebo)["AOT40_placebo"]
}

# Create placebo histogram
p_placebo <- ggplot(placebo_results, aes(x = aot40_beta)) +
  geom_histogram(
    binwidth = 0.000002, 
    boundary = 0,         
    fill = "lightblue", 
    color = "darkblue", 
    alpha = 0.5
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  labs(x = "Placebo AOT40 Coefficient", y = "Count") +
  theme_minimal() +
  ggtitle("Placebo Test: Shuffling AOT40 Only") +
  theme(
    plot.title = element_text(face = "plain"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(breaks = scales::breaks_width(0.01))

print(p_placebo)

# Save the placebo histogram as a high-resolution TIF file
ggsave(
  filename = "figures/sfig/placebo_AOT40_Wheat.tif",   # Save path and filename
  plot = p_placebo,
  width = 6,          # Width in inches
  height = 4.5,       # Height in inches
  dpi = 300,          # Resolution (300 DPI for publication quality)
  device = "tiff"     # Output format (TIF)
)

# Perform placebo test for FAOD and AOT40 interaction
# Set up for placebo test for FAOD and AOT40 interaction (similar to previous)
Wheat_data <- get_data() %>% filter(crop_parent == "Wheat")

# Set parameters for simulation
set.seed(123)
n_simulations <- 500
placebo_results <- data.frame(faod_aot40_beta = numeric(0))  # Initialize empty result storage

# Count number of valid simulations
count_valid_models <- 0  # Counter for valid simulations

while (count_valid_models < n_simulations) {
  
  # Shuffle FAOD and AOT40 and create interaction term
  maize_data_placebo <- Wheat_data %>%
    mutate(
      Faod_placebo = sample(Faod),
      AOT40_placebo = sample(AOT40),
      Faod_AOT40_placebo = Faod_placebo * AOT40_placebo
    ) %>%
    drop_na(GOSIF_sum)  # Ensure dependent variable (GOSIF_sum) has no NA values
  
  # Skip if too few valid data points
  if (nrow(maize_data_placebo) < 10) next
  
  # Create formula for regression
  fml_placebo <- str_c("GOSIF_sum ~
            Caod + Caod^2 +
            Faod_placebo + Faod_placebo^2 +
            cloud + cloud^2 + AOT40_placebo + Faod_AOT40_placebo + (Faod_placebo^2)*AOT40_placebo +
            ", add_terms(c(
              str_c("bin", 1:42),
              str_c("root_", 1:9),
              str_c("d", 1:13)
            )), "|
            x_y[year]") %>%
    as.formula()
  
  # Perform regression and store results
  model_placebo <- tryCatch(
    feols(fml_placebo, data = maize_data_placebo, weights = ~fraction, notes = FALSE, lean = TRUE),
    error = function(e) NULL  # Handle errors in regression
  )
  
  # If model is valid, store the coefficient
  if (!is.null(model_placebo)) {
    coef_val <- coef(model_placebo)["Faod_AOT40_placebo"]
    if (!is.na(coef_val)) {
      placebo_results <- rbind(placebo_results, data.frame(faod_aot40_beta = coef_val))
      count_valid_models <- count_valid_models + 1  # Increment valid simulation count
    }
  }
}

# Confirm the number of valid simulations
cat("Completed valid simulations:", count_valid_models, "\n")

# Create placebo histogram for Faod and AOT40 interaction
p_placebo <- ggplot(placebo_results, aes(x = faod_aot40_beta)) +
  geom_histogram(
    binwidth = 0.000005,
    boundary = 0,         
    fill = "lightblue", 
    color = "darkblue", 
    alpha = 0.5
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  labs(x = "Placebo FaOD*AOT40 Coefficient", y = "Count") +
  theme_minimal() +
  ggtitle("Placebo Test: Shuffling Both FaOD and AOT40") +
  theme(
    plot.title = element_text(face = "plain"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(
    breaks = seq(-0.0001, 0.0001, by = 0.00005),
    limits = c(-0.0001, 0.0001),
    expand = c(0, 0)
  )

print(p_placebo)

# Save the placebo histogram as a high-resolution TIF file for Faod and AOT40 interaction
ggsave(
  filename = "figures/sfig/placebo_FaodAOT40_bothshuffled_Rice.tif",
  plot = p_placebo,
  width = 6,
  height = 4.5,
  dpi = 300,
  device = "tiff"
)
ggsave(
  filename = "figures/sfig/placebo_FaodAOT40_bothshuffled_Maize.tif",
  plot = p_placebo,
  width = 6,
  height = 4.5,
  dpi = 300,
  device = "tiff"
)
ggsave(
  filename = "figures/sfig/placebo_FaodAOT40_bothshuffled_Wheat.tif",
  plot = p_placebo,
  width = 6,
  height = 4.5,
  dpi = 300,
  device = "tiff"
)
