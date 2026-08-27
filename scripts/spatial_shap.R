# scripts/run_spatial_shap.R
# XGBoost multi-class classification and SHAP attribution for spatial sensitivity results.
# Predicts optimal technology per location from spatial features and maps dominant SHAP drivers.

library(data.table)
library(dplyr)
library(xgboost)
library(shapviz)
library(ggplot2)
library(patchwork)
library(terra)
library(sf)

source("scripts/manuscript_figures.R")

INPUT_FILE <- "results/spatial_sensitivity_results.csv"
OUTPUT_CSV <- "results/spatial_shap_values_by_location.csv"

message("Loading spatial sensitivity results from: ", INPUT_FILE)
df <- fread(INPUT_FILE, data.table = FALSE)

# Filter to active viable cells where best_technology is defined
df_valid <- df %>% filter(!is.na(best_technology))

if (nrow(df_valid) == 0) {
  stop("No valid cells found in spatial sensitivity results.")
}

message("Total viable cells across all regions: ", nrow(df_valid))

# Define spatial features to use as predictors
features <- c(
  "biomass_density", "soil_temp", "elec_price", "dist_sink_km",
  "dist_sink_saline_km", "sink_is_offshore", "soil_ph", "soil_cec", "ff_c_intensity"
)

features <- intersect(features, names(df_valid))

# Prepare feature matrix X and target y
X_df <- df_valid[, features, drop = FALSE]

# Drop constant columns if any
variances <- sapply(X_df, var, na.rm = TRUE)
constant_cols <- names(variances)[is.na(variances) | variances <= 1e-8]
if (length(constant_cols) > 0) {
  message("Dropping constant predictors: ", paste(constant_cols, collapse = ", "))
  features <- setdiff(features, constant_cols)
  X_df <- X_df[, features, drop = FALSE]
}

X_mat <- as.matrix(X_df)

class_levels <- sort(unique(df_valid$best_technology))
y <- as.numeric(factor(df_valid$best_technology, levels = class_levels)) - 1

message("Training multi-class XGBoost classification tree...")
message("Target classes: ", paste(class_levels, collapse = ", "))

dtrain <- xgb.DMatrix(data = X_mat, label = y)

params <- list(
  objective = "multi:softprob",
  num_class = length(class_levels),
  max_depth = 6,
  eta = 0.05,
  nthread = 1
)

set.seed(42)
model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 120,
  verbose = 0
)

# Evaluate training accuracy
preds_prob <- predict(model, X_mat, reshape = TRUE)
preds_class <- class_levels[max.col(preds_prob, ties.method = "first")]
acc <- mean(preds_class == df_valid$best_technology)
message(sprintf("XGBoost Training Accuracy: %.2f%%", acc * 100))

message("Calculating SHAP values using shapviz...")
shp <- shapviz(model, X_pred = X_mat)
names(shp) <- class_levels

dir.create("results", showWarnings = FALSE)

# 1. Generate per-class SHAP Beeswarm Plots
message("Saving SHAP Beeswarm Plots...")
for (cls in class_levels) {
  p_bee <- sv_importance(shp[[cls]], kind = "beeswarm") +
    theme_bw(base_size = 12) +
    labs(
      title = paste0("SHAP Feature Importance for Predicting ", cls),
      x = "SHAP Value (Impact on Log-Odds of Class Prediction)"
    ) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))

  out_png <- sprintf("results/spatial_shap_beeswarm_%s.png", cls)
  ggsave(out_png, plot = p_bee, width = 8, height = 6, dpi = 300)
}

# 2. Extract cell-by-cell SHAP metrics for each location
message("Extracting location-level dominant SHAP features and maximum SHAP values...")
n_cells <- nrow(df_valid)
dominant_feat_winning <- character(n_cells)
max_shap_winning <- numeric(n_cells)
dominant_feat_overall <- character(n_cells)
max_shap_overall <- numeric(n_cells)

# Pre-extract SHAP matrices for speed
shap_matrices <- lapply(class_levels, function(cls) shp[[cls]]$S)
names(shap_matrices) <- class_levels

for (i in seq_len(n_cells)) {
  winning_cls <- df_valid$best_technology[i]
  S_win <- shap_matrices[[winning_cls]][i, ]

  idx_win <- which.max(abs(S_win))
  dominant_feat_winning[i] <- features[idx_win]
  max_shap_winning[i] <- S_win[idx_win]

  # Across all classes
  abs_all <- sapply(shap_matrices, function(M) abs(M[i, ]))
  # Find row (feature) with overall maximum absolute SHAP
  max_per_feat <- apply(abs_all, 1, max)
  idx_overall <- which.max(max_per_feat)
  dominant_feat_overall[i] <- features[idx_overall]
  max_shap_overall[i] <- max_per_feat[idx_overall]
}

df_shap_loc <- df_valid %>%
  select(x, y, region, best_technology) %>%
  mutate(
    dominant_feature_winning_class = dominant_feat_winning,
    max_shap_value_winning_class = max_shap_winning,
    dominant_feature_overall = dominant_feat_overall,
    max_shap_magnitude_overall = max_shap_overall
  )

write.csv(df_shap_loc, OUTPUT_CSV, row.names = FALSE)
message("Saved location-level SHAP table to: ", OUTPUT_CSV)

# 3. Plot Multi-Region Spatial Maps of Largest SHAP Value by Location
message("Generating multi-region spatial maps...")

regions <- c("US", "Europe", "China", "India")

# Color palette for features
feature_palette <- c(
  "biomass_density" = "#2ca02c",
  "soil_temp" = "#d62728",
  "elec_price" = "#1f77b4",
  "dist_sink_km" = "#9467bd",
  "dist_sink_saline_km" = "#8c564b",
  "sink_is_offshore" = "#e377c2",
  "soil_ph" = "#7f7f7f",
  "soil_cec" = "#bcbd22",
  "ff_c_intensity" = "#ff7f0e"
)

# Function to plot a 4-panel stitched map for categorical dominant feature
plot_dominant_feature_map <- function(data_loc, var_name, title_text, out_path) {
  plots <- list()
  for (r in regions) {
    reg_df <- data_loc %>% filter(region == r)
    dat <- load_region_data(r)

    p <- ggplot() +
      geom_tile(data = reg_df, aes(x = x, y = y, fill = .data[[var_name]]))

    if (!is.null(dat$admin0)) {
      p <- p + geom_sf(data = dat$admin0, fill = NA, color = "black", linewidth = 0.4)
    }

    p <- p +
      coord_sf(crs = 4326) +
      scale_fill_manual(
        name = "Dominant SHAP Predictor",
        values = feature_palette,
        na.value = "grey80"
      ) +
      theme_void(base_size = 11) +
      labs(subtitle = r) +
      theme(
        plot.subtitle = element_text(face = "bold", hjust = 0.5, margin = margin(b = 4))
      )

    plots[[r]] <- p
  }

  combined <- patchwork::wrap_plots(plots, ncol = 2) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = title_text,
      theme = theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.position = "bottom"
      )
    )

  ggsave(out_path, plot = combined, width = 12, height = 9, dpi = 300)
  message("Saved categorical dominant SHAP map to: ", out_path)
}

# Function to plot a 4-panel stitched map for numeric SHAP magnitude
plot_shap_magnitude_map <- function(data_loc, var_name, title_text, out_path) {
  plots <- list()
  for (r in regions) {
    reg_df <- data_loc %>% filter(region == r)
    dat <- load_region_data(r)

    p <- ggplot() +
      geom_tile(data = reg_df, aes(x = x, y = y, fill = abs(.data[[var_name]])))

    if (!is.null(dat$admin0)) {
      p <- p + geom_sf(data = dat$admin0, fill = NA, color = "black", linewidth = 0.4)
    }

    p <- p +
      coord_sf(crs = 4326) +
      scale_fill_viridis_c(
        name = "|SHAP Value|",
        option = "inferno",
        direction = 1
      ) +
      theme_void(base_size = 11) +
      labs(subtitle = r) +
      theme(
        plot.subtitle = element_text(face = "bold", hjust = 0.5, margin = margin(b = 4))
      )

    plots[[r]] <- p
  }

  combined <- patchwork::wrap_plots(plots, ncol = 2) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = title_text,
      theme = theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.position = "bottom"
      )
    )

  ggsave(out_path, plot = combined, width = 12, height = 9, dpi = 300)
  message("Saved numeric SHAP magnitude map to: ", out_path)
}

# Generate Maps
plot_dominant_feature_map(
  df_shap_loc,
  "dominant_feature_winning_class",
  "Dominant Spatial Driver of Optimal Technology (Largest SHAP Feature by Location)",
  "results/map_dominant_shap_feature.png"
)

plot_shap_magnitude_map(
  df_shap_loc,
  "max_shap_value_winning_class",
  "Magnitude of Strongest Spatial Driver (|SHAP Value| of Dominant Feature by Location)",
  "results/map_max_shap_magnitude.png"
)

message("XGBoost + SHAP spatial analysis and mapping complete!")
