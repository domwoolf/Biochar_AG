# scripts/run_mc_analysis.R
# Executes Monte Carlo sensitivity and uncertainty analysis across scenario parameter combinations.
# Evaluates technologies competitively across spatial layers using randomized parameter draws.
# Highly optimized for performance using vectorization and multi-core parallelization.

library(terra)
library(parallel)
library(dplyr)
library(tidyr)

# Sourcing script for helper load function and devtools packages loading
source("scripts/generate_manuscript_figures.R")

# Configuration
n_runs <- 500 # Number of MC iterations per scenario combination (default 20 for testing)
test_mode <- FALSE # Set to FALSE for full production run across all 720 scenario combinations
n_cores <- 12 # Set to integer to override default cores detection (detectCores() - 1)
append <- TRUE # Set to TRUE to append to existing results file

factorial_grid <- expand.grid(
  region = c("US", "China", "Europe", "India"),
  c_price = c(0, 50, 100, 150, 200),
  discount_rate = c(0.02, 0.08, 0.15),
  allow_eor = c(TRUE, FALSE),
  early_adoption = c(TRUE, FALSE),
  plant_mw_th = c(50, 100, 150, 250)
)

if (test_mode) {
  message("Running in TEST MODE: truncating factorial grid to 2 scenarios for speed.")
  factorial_grid <- head(factorial_grid, 2)
}

# Determine number of cores to use
if (is.null(n_cores)) {
  n_cores <- parallel::detectCores() - 1
}
if (is.na(n_cores) || n_cores < 1) {
  n_cores <- 1
}
message("Using ", n_cores, " core(s) for parallel processing.")

# 1. Load Parameter Definitions & Set Up Classifications
params_df <- read.csv("parameters_mc_ready.csv", stringsAsFactors = FALSE)

scenario_params <- c(
  "region", "c_price", "discount_rate", "allow_eor", "early_adoption", "plant_mw_th",
  "plant_sizes_mw_th", "optimize_scale", "use_flat_ci", "flat_ci_tCO2_GJ",
  "beccs_available", "bc_valuation_method", "time_frame", "n_app_rate", "rebound",
  "py_temp", "bm_feed_rate"
)

excluded_params <- c("bc_price", "bc_ag_value", "ccs_distance", "bc_stab_factor")
spatial_scalar_params <- c("ff_c_intensity")

# Filter uncertain extrinsic parameters to sample continuously
uncertain_params <- params_df %>%
  dplyr::filter(
    tolower(distribution) != "none",
    !is.na(distribution),
    distribution != "",
    !name %in% scenario_params,
    !name %in% excluded_params,
    !name %in% spatial_scalar_params
  )

# 2. Generate Randomized MC Parameter Tables per Region (Common Random Numbers across scenarios)
generate_param_draws <- function(row, n, local_mean = NULL) {
  dist <- tolower(gsub("[- ]", "", row$distribution))
  def_val <- suppressWarnings(as.numeric(row$default_value))
  target_val <- if (!is.null(local_mean) && !is.na(local_mean)) local_mean else def_val
  scale_ratio <- if (def_val != 0) target_val / def_val else 1.0

  disp <- as.numeric(row$dispersion) * scale_ratio
  min_val <- as.numeric(row$minimum) * scale_ratio
  max_val <- as.numeric(row$maximum) * scale_ratio

  if (dist == "normal") {
    draws <- rnorm(n, mean = target_val, sd = disp)
  } else if (dist == "lognormal") {
    meanlog <- log(target_val) - (disp^2) / 2
    draws <- rlnorm(n, meanlog = meanlog, sdlog = disp)
  } else if (dist == "uniform") {
    draws <- runif(n, min = min_val, max = max_val)
  } else {
    draws <- rep(target_val, n)
  }

  # Clamp to physical/mathematical bounds
  if (!is.na(min_val)) draws <- pmax(draws, min_val)
  if (!is.na(max_val)) draws <- pmin(draws, max_val)

  return(draws)
}

set.seed(42) # For reproducible random draws
mc_tables_by_region <- list()

for (r in unique(factorial_grid$region)) {
  p_local <- BiocharAG::set_scenario(region = r)
  mc_draws_list <- list()

  for (i in seq_len(nrow(uncertain_params))) {
    p_name <- uncertain_params$name[i]
    local_val <- if (!is.null(p_local[[p_name]])) p_local[[p_name]] else NULL
    mc_draws_list[[p_name]] <- generate_param_draws(uncertain_params[i, ], n_runs, local_mean = local_val)
  }

  # Generate scalar multiplier for ff_c_intensity (spatial raster parameter)
  ff_row <- params_df[params_df$name == "ff_c_intensity", ]
  if (nrow(ff_row) > 0 && !is.na(ff_row$minimum) && !is.na(ff_row$maximum)) {
    ff_def <- as.numeric(ff_row$default_value)
    ff_min <- as.numeric(ff_row$minimum) / ff_def
    ff_max <- as.numeric(ff_row$maximum) / ff_def
    mc_draws_list[["ff_ci_multiplier"]] <- runif(n_runs, min = ff_min, max = ff_max)
  } else {
    mc_draws_list[["ff_ci_multiplier"]] <- rep(1.0, n_runs)
  }

  mc_table_r <- as.data.frame(mc_draws_list, stringsAsFactors = FALSE)
  mc_table_r$mc_run_id <- seq_len(n_runs)
  mc_tables_by_region[[r]] <- mc_table_r
}

# Helper functions for spatial metrics extraction on vectors
extract_masked_vector_mean <- function(vec, is_best) {
  if (is.null(vec) || length(vec) == 0) {
    return(NA)
  }
  if (length(vec) == 1) {
    return(vec)
  }
  if (identical(is_best, FALSE)) is_best <- !is_best # Return all values if is_best is FALSE
  vals <- vec[is_best]
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) {
    return(NA)
  }
  return(mean(vals))
}

extract_masked_vector_min <- function(vec, is_best) {
  if (is.null(vec) || length(vec) == 0) {
    return(NA)
  }
  if (length(vec) == 1) {
    return(vec)
  }
  if (identical(is_best, FALSE)) is_best <- !is_best # Return all values if is_best is FALSE
  vals <- vec[is_best]
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) {
    return(NA)
  }
  return(min(vals))
}

extract_masked_vector_max <- function(vec, is_best) {
  if (is.null(vec) || length(vec) == 0) {
    return(NA)
  }
  if (length(vec) == 1) {
    return(vec)
  }
  if (identical(is_best, FALSE)) is_best <- !is_best # Return all values if is_best is FALSE
  vals <- vec[is_best]
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) {
    return(NA)
  }
  return(max(vals))
}

# 3. Pre-load and vectorize region spatial data
message("Pre-loading and vectorizing spatial data for all regions...")
region_names <- unique(factorial_grid$region)
vectorized_regions <- list()

for (r in region_names) {
  message("  Vectorizing data for: ", r)
  dat <- load_region_data(r)
  layers <- dat$layers

  # Find indices where biomass_density > 0 and is not NA
  bm_vals <- terra::values(layers$biomass_density, mat = FALSE)
  active_indices <- which(!is.na(bm_vals) & bm_vals > 0)

  # Cell area in km2
  cell_area_raster <- terra::cellSize(layers$biomass_density, unit = "km")
  cell_area_vals <- terra::values(cell_area_raster, mat = FALSE)[active_indices]

  # Extract values of all layers for active indices
  vectorized_layers <- list()
  for (layer_name in names(layers)) {
    vals <- terra::values(layers[[layer_name]], mat = FALSE)
    if (is.matrix(vals)) {
      vectorized_layers[[layer_name]] <- vals[active_indices, 1]
    } else {
      vectorized_layers[[layer_name]] <- vals[active_indices]
    }
  }

  vectorized_regions[[r]] <- list(
    active_indices = active_indices,
    cell_area = cell_area_vals,
    layers = vectorized_layers
  )
}

message("Starting parallel Monte Carlo Analysis: ", nrow(factorial_grid), " scenario combinations x ", n_runs, " MC runs each.")

# Run scenario combinations in parallel
results_list <- parallel::mclapply(seq_len(nrow(factorial_grid)), function(s) {
  s_row <- factorial_grid[s, ]
  r_data <- vectorized_regions[[s_row$region]]
  spatial_layers <- r_data$layers
  cell_area <- r_data$cell_area

  # Create a scenario-specific results accumulator
  scenario_results <- data.frame()

  for (m in seq_len(n_runs)) {
    mc_row <- mc_tables_by_region[[s_row$region]][m, ]

    # Base Setup from Scenario
    p <- BiocharAG::set_scenario(region = s_row$region)
    p$c_price <- s_row$c_price
    p$discount_rate <- s_row$discount_rate
    p$allow_eor <- s_row$allow_eor
    p$early_adoption <- s_row$early_adoption
    p$plant_mw_th <- s_row$plant_mw_th

    # Inject all uncertain extrinsic scalar parameters from mc_row into p
    for (p_name in names(mc_row)) {
      if (p_name != "mc_run_id" && p_name != "ff_ci_multiplier") {
        p[[p_name]] <- mc_row[[p_name]]
      }
    }

    # Inject spatial layers (overriding scalar defaults if layer exists)
    if ("soil_temp" %in% names(spatial_layers)) p$soil_temp <- spatial_layers$soil_temp
    if ("elec_price" %in% names(spatial_layers)) {
      factor <- if (!is.null(p$wholesale_discount_factor)) p$wholesale_discount_factor else 0.4
      p$elec_price <- spatial_layers$elec_price * factor
    }
    if ("soil_ph" %in% names(spatial_layers)) p$soil_ph <- spatial_layers$soil_ph
    if ("soil_cec" %in% names(spatial_layers)) p$soil_cec <- spatial_layers$soil_cec
    if ("dist_sink_km" %in% names(spatial_layers)) p$dist_sink_km <- spatial_layers$dist_sink_km
    if ("dist_sink_saline_km" %in% names(spatial_layers)) p$dist_sink_saline_km <- spatial_layers$dist_sink_saline_km
    if ("sink_is_offshore" %in% names(spatial_layers)) p$sink_is_offshore <- spatial_layers$sink_is_offshore

    # Apply ff_ci_multiplier to ff_c_intensity (whether raster or scalar)
    if ("ff_c_intensity" %in% names(spatial_layers)) {
      p$ff_c_intensity <- spatial_layers$ff_c_intensity * mc_row$ff_ci_multiplier
    } else if (!is.null(p$ff_c_intensity)) {
      p$ff_c_intensity <- p$ff_c_intensity * mc_row$ff_ci_multiplier
    }

    for (layer_name in c("cn_weather_risk", "cn_expansion_risk", "eu_base_eur", "us_base_cost")) {
      if (layer_name %in% names(spatial_layers)) p[[layer_name]] <- spatial_layers[[layer_name]]
    }

    dist_layer_name <- paste0("dist_", s_row$plant_mw_th, "MWth")
    if (dist_layer_name %in% names(spatial_layers)) {
      p$avg_dist <- spatial_layers[[dist_layer_name]]
    }

    feedstock_region <- if (s_row$region == "Europe") "EU" else s_row$region
    p$feedstock_cost <- BiocharAG::calculate_regional_feedstock_cost(feedstock_region, p)

    # Execute All 3 Technologies Competitively
    res_bes <- BiocharAG::calculate_bes(p)
    res_beccs <- BiocharAG::calculate_beccs(p)
    res_bebcs <- BiocharAG::calculate_bebcs(p)

    # Stack NPVs and find winner (in-memory matrix math)
    npv_matrix <- cbind(res_bes$net_value, res_beccs$net_value, res_bebcs$net_value)
    opt_idx <- max.col(npv_matrix, ties.method = "first")

    # If all NPVs are NA, opt_idx is NA
    opt_idx[rowSums(is.na(npv_matrix)) == 3] <- NA

    biomass_amount <- spatial_layers$biomass_density * cell_area

    techs <- c("BES", "BECCS", "BEBCS")
    res_list <- list(res_bes, res_beccs, res_bebcs)

    for (t_idx in 1:3) {
      t_name <- techs[t_idx]
      tech_res <- res_list[[t_idx]]

      # Area and Biomass Calculations
      is_best <- !is.na(opt_idx) & opt_idx == t_idx
      is_viable <- is_best & !is.na(tech_res$net_value) & (tech_res$net_value > 0)

      area_best_km2 <- sum(cell_area[is_best], na.rm = TRUE)
      area_viable_km2 <- sum(cell_area[is_viable], na.rm = TRUE)
      biomass_processed_yr <- sum(biomass_amount[is_viable], na.rm = TRUE)

      # Create result row combining scenario columns, MC parameter draws, and TEA results
      new_row <- data.frame(
        scenario_id = s,
        mc_run_id = m,
        region = s_row$region,
        c_price = s_row$c_price,
        discount_rate = s_row$discount_rate,
        allow_eor = s_row$allow_eor,
        early_adoption = s_row$early_adoption,
        plant_mw_th = s_row$plant_mw_th,
        technology = t_name,
        stringsAsFactors = FALSE
      )

      # Append MC parameter columns
      param_cols <- mc_row[, names(mc_row) != "mc_run_id", drop = FALSE]
      new_row <- cbind(new_row, param_cols)

      # Append TEA result columns
      tea_cols <- data.frame(
        area_best_km2 = if (area_best_km2 == 0) NA else area_best_km2,
        area_viable_km2 = if (area_viable_km2 == 0) NA else area_viable_km2,
        biomass_processed_yr_mg = if (area_viable_km2 == 0) NA else biomass_processed_yr,
        npv_min = extract_masked_vector_min(tech_res$net_value, is_best),
        npv_max = extract_masked_vector_max(tech_res$net_value, is_best),
        npv_mean = extract_masked_vector_mean(tech_res$net_value, is_best),
        mean_co2_transport_distance_km = extract_masked_vector_mean(tech_res$co2_transport_distance_km, is_best),
        mean_biomass_transport_distance_km = extract_masked_vector_mean(tech_res$biomass_transport_distance_km, is_best),
        mean_capital_cost_mg = extract_masked_vector_mean(tech_res$capital_cost_mg, is_best),
        mean_om_cost_mg = extract_masked_vector_mean(tech_res$om_cost_mg, is_best),
        mean_biomass_cost_mg = extract_masked_vector_mean(tech_res$biomass_cost_mg, is_best),
        mean_co2_transport_cost_mg = extract_masked_vector_mean(tech_res$co2_transport_cost_mg, is_best),
        mean_net_cdr = extract_masked_vector_mean(tech_res$tot_c_abatement, is_best),
        mean_carbon_removal_revenue_mg = extract_masked_vector_mean(tech_res$abatement_revenue_mg, is_best),
        mean_electricity_production_mwh = extract_masked_vector_mean(tech_res$elec_prod, is_best),
        mean_electricity_revenue_mg = extract_masked_vector_mean(tech_res$elec_revenue_mg, is_best),
        mean_agronomic_revenue_mg = extract_masked_vector_mean(tech_res$agronomic_revenue_mg, is_best),
        mean_lcoe_usd_mwh = extract_masked_vector_mean(tech_res$lcoe, is_best),
        mean_cost_of_co2_avoided = extract_masked_vector_mean(tech_res$cost_of_co2_avoided, is_best),
        mean_abatement_efficiency = extract_masked_vector_mean(tech_res$abatement_efficiency, is_best),
        mean_total_capex_m = extract_masked_vector_mean(tech_res$total_capex_m, is_best)
      )

      new_row <- cbind(new_row, tea_cols)
      scenario_results <- rbind(scenario_results, new_row)
    }
  }

  message(sprintf("Finished scenario %d of %d (Region: %s, C price: %d)", s, nrow(factorial_grid), s_row$region, s_row$c_price))
  return(scenario_results)
}, mc.cores = n_cores)

# Check for errors in parallel workers
errors <- sapply(results_list, inherits, "try-error")
if (any(errors)) {
  stop("One or more parallel workers failed. First error:\n", results_list[[which(errors)[1]]])
}

# Combine all parallel result chunks
results_df <- do.call(rbind, results_list)

dir.create("results", showWarnings = FALSE)
file_path <- "results/mc_analysis_results.csv"
file_exists <- file.exists(file_path)
write.table(
  results_df,
  file = file_path,
  row.names = FALSE,
  col.names = !file_exists || !append,
  sep = ",",
  dec = ".",
  qmethod = "double",
  append = append && file_exists
)
message("Monte Carlo Analysis Complete. Results saved to results/mc_analysis_results.csv")
