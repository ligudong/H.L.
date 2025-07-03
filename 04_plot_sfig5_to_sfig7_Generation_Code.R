# Load necessary packages
source("script/00_loadPackages.R")
source("script/loadFunctions.R")
source("scrip/00_ablation_formulas.R")


# Load the data
data <- qread("data/tidied.qs")

data <- get_data()  # Function to retrieve the data

# Define the crops to be analyzed
crops <- c("Maize", "Rice", "Wheat")

# Initialize an empty list to store results for each crop
all_results <- list()

# Loop through each crop and run the regression analysis
for (crop in crops) {
  
  # Filter the data for the current crop
  crop_data <- data %>% filter(crop_parent == crop)
  
  # Nest the data by crop
  crop_data <- crop_data %>% nest(fdata = -crop_parent)
  
  # Create an empty data frame to store the results for this crop
  crop_results <- data.frame(Formula = character(), R2 = numeric(), Within_R2 = numeric(), stringsAsFactors = FALSE)
  
  # Run the regression experiments for each formula and store the results
  for (i in 1:229) {
    fml <- get(paste0("fml_", i))  # Retrieve the formula fml_1 to fml_229
    
    # Perform regression analysis for each formula (without sampling)
    crop_data <- crop_data %>%
      mutate(coefs = map(fdata, function(adata) {
        adata <- adata %>%
          nest(.by = county, .key = "ffdata")  # Nest data by county
        
        # Unnest the data and run the regression directly
        aadata <- adata %>%
          unnest(ffdata)  # Unnest the data without sampling
        
        # Run regression analysis and calculate coefficients and confidence intervals
        model <- feols(fml, aadata,
                       weights = ~fraction,
                       nthreads = 0, notes = FALSE, lean = TRUE)
        
        # Extract goodness-of-fit (R² and within R²) from the model
        glance_res <- glance(model)
        within_r2 <- glance_res$`within R2`
        r2 <- glance_res$r.squared
        
        # Add the results to the crop_results data frame
        crop_results <<- rbind(crop_results, data.frame(Formula = paste0("fml_", i), R2 = r2, Within_R2 = within_r2))
      }))
  }
  
  # Save the results for the current crop to the list
  all_results[[crop]] <- crop_results
  
  # Optionally, export the crop-specific results to separate Excel files
  write_xlsx(crop_results, path = paste0("E:/figures/ablation_results_", crop, ".xlsx"))
}

# Optionally, combine all crop results into one final Excel file
all_results_combined <- bind_rows(
  lapply(crops, function(crop) {
    all_results[[crop]] %>%
      mutate(Crop = crop)
  })
)

# Export the combined results for all crops
write_xlsx(all_results_combined, path = "figures/ablation_results_all_crops.xlsx")

# Load necessary packages for plotting
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(readxl)

# Read the Excel file for Wheat results (can be changed to Maize or Rice)
df_ablation <- read_excel("figures/ablation_results_all_crops/ablation_results_Wheat.xlsx")
# df_ablation <- read_excel("figures/ablation_results_all_crops/ablation_results_Maize.xlsx")
# df_ablation <- read_excel("figures/ablation_results_all_crops/ablation_results_Rice.xlsx")

# Factor variable list for the upset plot
factor_vars <- c("Caod", "Faod", "AOT40", "cloud", "faodxAOT40", "bin", "root", "d")

# Set the formula order
df_ablation <- df_ablation %>%
  mutate(fml_id = factor(fml_id, levels = paste0("fml_", 1:240)))

# Define the plotting function for regression results
plot_crop_r2_ablation <- function(df, crop_name) {
  crop_r2 <- df %>% select(fml_id, all_of(crop_name))
  
  # Upper part: Bar plot of R² (keep grid lines)
  p1 <- ggplot(crop_r2, aes(x = fml_id, y = .data[[crop_name]], fill = .data[[crop_name]])) +
    geom_col(width = 0.8, color = "black") +
    scale_fill_gradientn(colors = c("#599CB4")) +
    labs(y = paste0(crop_name, " within R²"), x = NULL) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "right",
      plot.margin = margin(5, 5, 0, 5)
    )
  
  # Lower part: Upset plot with included and excluded factors
  df_long <- df %>%
    select(fml_id, all_of(factor_vars)) %>%
    pivot_longer(cols = all_of(factor_vars), names_to = "Factor", values_to = "Included") %>%
    mutate(color_group = ifelse(Included == 1, "included", "excluded"))
  
  p2 <- ggplot(df_long, aes(x = fml_id, y = Factor)) +
    geom_point(aes(color = color_group), size = 2) +
    scale_color_manual(values = c("included" = "black", "excluded" = "#D3D3D3")) +
    scale_y_discrete(limits = rev(factor_vars)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5),
      plot.margin = margin(0, 5, 5, 5),
      legend.position = "none"  # Hide legend
    ) +
    labs(x = "Model (fml_1 ~ fml_240)", y = NULL)
  
  # Combine the plots (p1 and p2) into a single layout
  p <- p1 / p2 + plot_layout(heights = c(3, 1))
  return(p)
}

# Plot results for Wheat
p_wheat <- plot_crop_r2_ablation(df_ablation, "Wheat")
print(p_wheat)

# Save as high-resolution TIFF
ggsave("figures/sfig/sfigablation_Wheat_clean.tif", 
       plot = p_wheat, 
       width = 28, height = 14, 
       dpi = 600, 
       compression = "lzw")
