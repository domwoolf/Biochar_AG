# scripts/MC_analysis.R
# R port of the Monte Carlo sensitivity analysis using xgboost and shapviz.
# Generates sensitivity evolution plot, beeswarm plot, and partial dependence plots.

library(data.table)
library(dplyr)
library(xgboost)
library(shapviz)
library(ggplot2)

plot_sensitivity_evolution <- function(
  data_path = "results/mc_analysis_results.csv",
  technology_name = "BECCS",
  region_name = "Europe",
  discount_rate = 0.08,
  target_c_price = 100
) {
  # 1. Load the Monte Carlo results
  message("Loading data from ", data_path, "...")
  df <- fread(data_path, data.table = FALSE)

  # Define the discrete scenario steps to track (carbon prices)
  c_prices <- sort(unique(df$c_price))
  message("Detected carbon prices: ", paste(c_prices, collapse = ", "))

  # Data structure to store the SHAP importance
  importance_list <- list()

  # Define columns to drop to isolate the purely uncertain input parameters and toggles
  cols_to_drop <- c(
    "scenario_id", "mc_run_id", "region", "technology", "c_price", "discount_rate",
    "area_best_km2", "area_viable_km2", "biomass_processed_yr_mg",
    "npv_min", "npv_max", "npv_mean"
  )

  # Also drop columns starting with mean_ or total_
  all_names <- names(df)
  cols_to_drop <- c(
    cols_to_drop,
    all_names[startsWith(all_names, "mean_") | startsWith(all_names, "total_")]
  )

  # Store the shapviz object for the target carbon price to plot beeswarm/dependence later
  target_shp <- NULL

  # 2. Iterate through Carbon Prices and fit SHAP model
  for (cp in c_prices) {
    # Filter conditionally to the specific scenario
    sub_df <- df %>%
      filter(
        technology == technology_name,
        region == region_name,
        discount_rate == !!discount_rate,
        c_price == !!cp,
        !is.na(npv_mean)
      )

    if (nrow(sub_df) < 30) {
      message(sprintf("Skipping c_price=%d: only %d runs available (locked out or inactive)", cp, nrow(sub_df)))
      next
    }

    message(sprintf("Processing c_price=%d (%d runs)...", cp, nrow(sub_df)))

    y <- sub_df$npv_mean
    X <- sub_df[, !(names(sub_df) %in% cols_to_drop), drop = FALSE]

    # Convert logical columns to numeric (0/1) for xgboost compatibility
    for (col in names(X)) {
      if (is.logical(X[[col]])) {
        X[[col]] <- as.numeric(X[[col]])
      } else if (is.character(X[[col]])) {
        # Try to map strings like TRUE/FALSE or convert to factor/numeric
        X[[col]] <- ifelse(X[[col]] %in% c("TRUE", "True", "T"), 1,
          ifelse(X[[col]] %in% c("FALSE", "False", "F"), 0,
            as.numeric(as.factor(X[[col]]))
          )
        )
      }
    }

    # Drop constant columns (variance <= 1e-8)
    variances <- sapply(X, var, na.rm = TRUE)
    constant_cols <- names(variances)[is.na(variances) | variances <= 1e-8]
    X <- X[, !(names(X) %in% constant_cols), drop = FALSE]

    # Train XGBoost model
    X_mat <- as.matrix(X)
    dtrain <- xgb.DMatrix(data = X_mat, label = y)

    params <- list(
      max_depth = 5,
      eta = 0.05,
      objective = "reg:squarederror",
      nthread = 1
    )

    model <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = 100,
      verbose = 0
    )

    # Calculate SHAP values
    shp <- shapviz(model, X_pred = X_mat)

    # Save target shapviz object if it matches the target carbon price
    if (cp == target_c_price) {
      target_shp <- shp
    }

    # Calculate Mean Absolute SHAP (Feature Importance)
    mean_abs_shap <- colMeans(abs(shp$S))

    # Save to list
    importance_df <- data.frame(
      c_price = cp,
      feature = names(mean_abs_shap),
      mean_abs_shap = as.numeric(mean_abs_shap),
      stringsAsFactors = FALSE
    )
    importance_list[[as.character(cp)]] <- importance_df
  }

  if (length(importance_list) == 0) {
    stop("Error: No data populated in sensitivity tracker. Verify scenario filters.")
  }

  # Combine importance data
  importance_all <- do.call(rbind, importance_list)

  # 3. Plot the Evolution
  # Filter to top 6 most important features overall to avoid chart clutter
  overall_importance <- importance_all %>%
    group_by(feature) %>%
    summarise(avg_importance = mean(mean_abs_shap), .groups = "drop") %>%
    arrange(desc(avg_importance)) %>%
    slice_head(n = 6)

  top_features <- overall_importance$feature
  message("Top 6 key features overall: ", paste(top_features, collapse = ", "))

  importance_top <- importance_all %>%
    filter(feature %in% top_features)

  # Evolution Plot
  p_ev <- ggplot(importance_top, aes(x = c_price, y = mean_abs_shap, color = feature, group = feature)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    theme_bw(base_size = 12) +
    labs(
      title = paste0(
        "Evolution of Parameter Sensitivity vs. Carbon Price (R Port)\n",
        technology_name, " in ", region_name, " (DR=", discount_rate * 100, "%)"
      ),
      x = "Carbon Price ($/tCO2e)",
      y = "Mean Absolute SHAP Value (Impact on NPV)",
      color = "Key Parameters / Toggles"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      legend.position = "right",
      panel.grid.minor = element_blank()
    )

  dir.create("results", showWarnings = FALSE)

  ev_png <- sprintf("results/sensitivity_evolution_R_%s_%s_toggles.png", technology_name, region_name)
  ggsave(ev_png, plot = p_ev, width = 10, height = 6, dpi = 300)
  message("Saved evolution plot to ", ev_png)

  # 4. Generate Beeswarm and Dependence Plots if target SHAP is available
  if (!is.null(target_shp)) {
    message("Generating SHAP diagnostic plots for target carbon price $", target_c_price, "...")

    # Beeswarm Plot
    p_bee <- sv_importance(target_shp, kind = "beeswarm") +
      theme_bw(base_size = 12) +
      labs(
        title = paste0(
          "SHAP Beeswarm Plot (Carbon Price = $", target_c_price, ")\n",
          technology_name, " in ", region_name
        )
      ) +
      theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5))

    bee_png <- sprintf("results/beeswarm_R_%s_%s_c_%d.png", technology_name, region_name, target_c_price)
    ggsave(bee_png, plot = p_bee, width = 9, height = 6, dpi = 300)
    message("Saved beeswarm plot to ", bee_png)

    # Partial Dependence / SHAP Dependence Plot for the top features
    # Find the top 2 features at this specific carbon price
    top_features_at_c <- importance_all %>%
      filter(c_price == target_c_price) %>%
      arrange(desc(mean_abs_shap)) %>%
      slice_head(n = 2) %>%
      pull(feature)

    for (feat in top_features_at_c) {
      if (feat %in% colnames(target_shp$X)) {
        p_dep <- sv_dependence(target_shp, v = feat) +
          theme_bw(base_size = 12) +
          labs(
            title = paste0(
              "SHAP Dependence Plot for ", feat, " (Carbon Price = $", target_c_price, ")\n",
              technology_name, " in ", region_name
            )
          ) +
          theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5))

        dep_png <- sprintf("results/dependence_R_%s_%s_%s_c_%d.png", technology_name, region_name, feat, target_c_price)
        ggsave(dep_png, plot = p_dep, width = 8, height = 5, dpi = 300)
        message("Saved dependence plot for ", feat, " to ", dep_png)
      }
    }
  } else {
    warning("Target carbon price $", target_c_price, " was not processed; skipping beeswarm and dependence plots.")
  }
}

plot_global_beeswarm <- function(
  data_path = "results/mc_analysis_results.csv",
  technology_name = "BECCS",
  discount_rate = 0.08,
  target_c_price = 100
) {
  # 1. Load the Monte Carlo results
  message("Loading data for global beeswarm from ", data_path, "...")
  df <- fread(data_path, data.table = FALSE)

  # Filter conditionally to the specific technology and carbon price across all regions
  sub_df <- df %>%
    filter(
      technology == technology_name,
      discount_rate == !!discount_rate,
      c_price == !!target_c_price,
      !is.na(npv_mean)
    )

  if (nrow(sub_df) < 30) {
    message(sprintf("Skipping global beeswarm for %s: only %d runs available", technology_name, nrow(sub_df)))
    return(NULL)
  }

  message(sprintf("Processing global beeswarm for %s (%d runs)...", technology_name, nrow(sub_df)))

  # Define columns to drop (note: region is NOT dropped!)
  cols_to_drop <- c(
    "scenario_id", "mc_run_id", "technology", "c_price", "discount_rate",
    "area_best_km2", "area_viable_km2", "biomass_processed_yr_mg",
    "npv_min", "npv_max", "npv_mean"
  )

  # Also drop columns starting with mean_ or total_
  all_names <- names(df)
  cols_to_drop <- c(
    cols_to_drop,
    all_names[startsWith(all_names, "mean_") | startsWith(all_names, "total_")]
  )

  y <- sub_df$npv_mean
  X <- sub_df[, !(names(sub_df) %in% cols_to_drop), drop = FALSE]

  # Convert logical and character columns to numeric for xgboost compatibility
  for (col in names(X)) {
    if (is.logical(X[[col]])) {
      X[[col]] <- as.numeric(X[[col]])
    } else if (is.character(X[[col]]) || is.factor(X[[col]])) {
      X[[col]] <- as.numeric(as.factor(X[[col]]))
    }
  }

  # Drop constant columns (variance <= 1e-8)
  variances <- sapply(X, var, na.rm = TRUE)
  constant_cols <- names(variances)[is.na(variances) | variances <= 1e-8]
  X <- X[, !(names(X) %in% constant_cols), drop = FALSE]

  # Train XGBoost model
  X_mat <- as.matrix(X)
  dtrain <- xgb.DMatrix(data = X_mat, label = y)

  params <- list(
    max_depth = 5,
    eta = 0.05,
    objective = "reg:squarederror",
    nthread = 1
  )

  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    verbose = 0
  )

  # Calculate SHAP values
  shp <- shapviz(model, X_pred = X_mat)

  # Beeswarm Plot
  p_bee <- sv_importance(shp, kind = "beeswarm") +
    theme_bw(base_size = 12) +
    labs(
      title = paste0(
        "Global SHAP Beeswarm Plot (Carbon Price = $", target_c_price, ")\n",
        technology_name, " - All Regions Aggregated (DR=", discount_rate * 100, "%)"
      )
    ) +
    theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5))

  # Add second color legend for region mapping
  opt <- getOption("shapviz.viridis_args", list(begin = 0.25, end = 0.85, option = "inferno"))
  cols <- viridisLite::viridis(100, begin = opt$begin, end = opt$end, option = opt$option)
  unique_regions <- sort(unique(na.omit(sub_df$region)))
  n_regions <- length(unique_regions)
  if (n_regions > 0) {
    region_colors <- cols[round(1 + seq(0, 1, length.out = n_regions) * 99)]
    names(region_colors) <- unique_regions

    legend_data <- data.frame(
      x = NA_real_,
      y = p_bee$data$feature[1],
      Region = factor(unique_regions, levels = unique_regions)
    )

    p_bee <- p_bee +
      geom_point(data = legend_data, aes(x = x, y = y, fill = Region), shape = 21, size = 3, stroke = 0) +
      scale_fill_manual(
        name = "Region",
        values = region_colors
      ) +
      guides(
        fill = guide_legend(override.aes = list(shape = 21, size = 4, stroke = 0))
      )
  }

  dir.create("results", showWarnings = FALSE)
  bee_png <- sprintf("results/beeswarm_global_%s_c_%d.png", technology_name, target_c_price)
  ggsave(bee_png, plot = p_bee, width = 10, height = 7, dpi = 300)
  message("Saved global beeswarm plot to ", bee_png)

  return(p_bee)
}

generate_evolution_plots <- function() {
  plot_sensitivity_evolution(
    data_path = "results/mc_analysis_results.csv",
    technology_name = "BECCS",
    region_name = "Europe",
    discount_rate = 0.08,
    target_c_price = 100
  )

  plot_sensitivity_evolution(
    data_path = "results/mc_analysis_results.csv",
    technology_name = "BECCS",
    region_name = "US",
    discount_rate = 0.08,
    target_c_price = 100
  )

  plot_sensitivity_evolution(
    data_path = "results/mc_analysis_results.csv",
    technology_name = "BECCS",
    region_name = "China",
    discount_rate = 0.08,
    target_c_price = 100
  )

  plot_sensitivity_evolution(
    data_path = "results/mc_analysis_results.csv",
    technology_name = "BECCS",
    region_name = "India",
    discount_rate = 0.08,
    target_c_price = 100
  )
}

generate_global_beeswarm_plots <- function() {
  plot_global_beeswarm(
    data_path = "results/mc_analysis_results.csv",
    technology_name = "BECCS",
    discount_rate = 0.08,
    target_c_price = 100
  )

  plot_global_beeswarm(
    data_path = "results/mc_analysis_results.csv",
    technology_name = "BES",
    discount_rate = 0.08,
    target_c_price = 100
  )

  plot_global_beeswarm(
    data_path = "results/mc_analysis_results.csv",
    technology_name = "BEBCS",
    discount_rate = 0.08,
    target_c_price = 100
  )
}

# Run the analysis
if (sys.nframe() == 0L) {
  generate_evolution_plots()
  generate_global_beeswarm_plots()
}
