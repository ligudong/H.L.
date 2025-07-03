# Check if the object is a raster object (supports both raster and terra formats)
is.Raster <- function(x) {
  return((
    class(x)[1] == "RasterLayer" ||
      class(x)[1] == "RasterBrick" ||
      class(x)[1] == "RasterStack" || class(x)[1] == "SpatRaster"
  ))
}

# Compute weighted R-squared between observed and predicted values
get_wr2_ss <- function(y, y_pred, w) {
  ss_residual <- sum(w * (y - y_pred)^2)  # weighted residual sum of squares
  ss_total <- sum(w * (y - weighted.mean(y, w))^2)  # weighted total sum of squares
  return(1 - ss_residual / ss_total)
}

# Combine a vector of variable names into a formula string like "x1 + x2 + x3"
add_terms <- function(av) {
  reduce(av, function(x, y) {
    str_c(x, " + ", y)
  })
}

# Print the memory size of an object with automatic unit conversion
object_size <- function(aobject) {
  aobject %>%
    object.size() %>%
    print(unit = "auto")
}

# Set number of threads for parallel operations based on OS
if (Sys.info()[1] == "Windows") {
  qn <- 5
} else {
  qn <- 3
}

# Round x and y coordinates to 4 decimal places to ensure consistent merging
trim_xy <- function(adata_xy) {
  adata_xy %>%
    mutate(across(c(x, y), ~ round(.x, 4)))
}

# Check if input data fully covers the spatial extent of the mask
check_join <- function(adata) {
  targeted <- rast("data/outputs/masks/mask_extraction.tif") %>%
    sum(na.rm = T) %>%
    as.data.frame(xy = T) %>%
    distinct(x, y) %>%
    trim_xy()
  
  de <- adata %>%
    distinct(x, y)
  
  joined <- inner_join(targeted, de)
  antied <- anti_join(targeted, de)
  
  tibble(
    mask_grid = nrow(targeted),   # total pixels in mask
    test_grid = nrow(de),         # pixels in test dataset
    inner_joined = nrow(joined),  # overlapping pixels
    anti_joined = nrow(antied)    # mask pixels not covered by test data
  )
}

# Define a standard arrow style for ggplot2 (e.g. for annotation or vector direction)
arrow <- arrow(
  length = unit(0.015, "npc"),
  ends = "last",
  type = "open"
)

# Load and preprocess main dataset with filtering, transformations, and joins
get_data <- function(varss =
                       c(
                         "GOSIF_sum",
                         "cloud",
                         "Faod",
                         "Caod",
                         "W126_1",
                         "mean_surface",
                         "mean_root",
                         "maxtmp",
                         str_c("bin", 1:42),
                         str_c("step", 1:10),
                         str_c("root_", 1:9),
                         str_c("D", 1:13),
                         str_c("d", 1:13)
                       )) {
  qread("data/outputs/tidied.qs", nthreads = qn) %>%
    lazy_dt() %>%
    mutate(
      crop_parent = fifelse(str_detect(crop, "Rice"), "Rice", crop),
      x_y = str_c(crop, x_y),
      across(
        c(
          starts_with("GOSIF"),
        ),
        log
      ),
      # Crop-specific ozone metrics
      AOT40 = fcase(
        crop_parent == "Maize", AOT40_6,
        crop_parent == "Rice", AOT40_7,
        crop_parent == "Wheat", AOT40_7 - AOT40_1
      ),
      W126 = fcase(
        crop_parent == "Maize", W126_6,
        crop_parent == "Rice", W126_7,
        crop_parent == "Wheat", W126_7 - W126_1
      ),
      O3 = fcase(
        crop_parent == "Maize", O3_6,
        crop_parent == "Rice", O3_7,
        crop_parent == "Wheat", (O3_7 * (MA - `GR&EM` + 1) - O3_1) / (MA - `GR&EM` + 0)
      )
    ) %>%
    drop_na(all_of(varss)) %>%
    group_by(crop, x, y) %>%
    filter(n() >= 10) %>%
    ungroup() %>%
    as_tibble() %>%
    group_by(crop) %>%
    ungroup() %>%
    arrange(crop) %>%
    inner_join(
      read_xlsx("data/regions.xlsx") %>% mutate(region = str_c("R", region)),
      by = join_by(province, crop_parent)
    )
}

# Predefined regional order for consistent plotting
order <- c(
  "North China",
  "Northeast China",
  "East China",
  "China",
  "South China",
  "Central China",
  "Southwest China",
  "Northwest China"
)

# Generate relative predictions from a regression model given new data and baseline
relpred <- function(object, newdata, baseline = NULL, level = 0.90) {
  # Check that newdata only includes numeric columns
  if (any(sapply(newdata, function(x) { !is.numeric(x) }))) {
    stop("For now, only numeric variables are supported in newdata.")
  }
  
  # Ensure baseline length matches columns
  if (!is.null(baseline) & ncol(newdata) != length(baseline)) {
    stop("baseline vector length must be equal to number of columns in newdata.")
  }
  
  # Fill in missing variables in newdata based on model
  newdata_filled <- fill_missing_vars(object, newdata)
  
  # Create baseline dataframe (all zeros if NULL)
  baseline_df <- if (is.null(baseline)) {
    newdata
  } else {
    as.data.frame(baseline)
  }
  baseline_df[] <- 0
  baseline_filled <- fill_missing_vars(object, baseline_df)
  
  # Create design matrix (differences from baseline)
  X <- as.matrix(newdata_filled - baseline_filled)
  
  # Get coefficient vector
  B <- as.numeric(coef(object))
  
  # Determine degrees of freedom
  df <- if (inherits(object, "fixest")) {
    attributes(vcov(object, attr = T))$G
  } else {
    object$df.residual
  }
  
  # Subset design matrix to model terms only
  X <- X[, names(coef(object))]
  
  # Compute fit and confidence interval
  fit <- data.frame(fit = X %*% B)
  sig <- vcov(object)
  se <- apply(X, MARGIN = 1, FUN = get_se, sig = sig)
  t_val <- qt((1 - level) / 2 + level, df = df)
  
  fit$lwr <- fit$fit - t_val * se
  fit$upr <- fit$fit + t_val * se
  
  fit %>% as_tibble()
}

# Fill in missing variables or factor levels for prediction
fill_missing_vars <- function(object, X) {
  orig_vars <- all.vars(formula(object))[-1]
  fact_vars <- names(object$xlevels)
  
  for (f in fact_vars) {
    if (!(f %in% names(X))) {
      X[[f]] <- factor(object$xlevels[[f]][1], levels = object$xlevels[[f]])
    } else {
      X[[f]] <- factor(X[[f]], levels = object$xlevels[[f]])
    }
  }
  
  for (v in orig_vars) {
    if (!(v %in% names(X))) {
      X[[v]] <- 0
    }
  }
  
  tt <- terms(object)
  Terms <- delete.response(tt)
  as.data.frame(model.matrix(Terms, data = X))
}

# Compute standard error of a single prediction row
get_se <- function(r, sig) {
  r <- matrix(r, nrow = 1)
  sqrt(r %*% sig %*% t(r))
}
