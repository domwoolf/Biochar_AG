# scripts/run_mc_analysis.R
# Executes Monte Carlo sensitivity and uncertainty analysis across scenario parameter combinations.
# Evaluates technologies competitively across spatial layers using randomized parameter draws.
# Highly optimized for performance using vectorization and multi-core parallelization.

library(terra)
library(parallel)
library(dplyr)
library(tidyr)

# Sourcing script for helper load function and devtools packages loading
source("scripts/manuscript_figures.R")

# Configuration
n_runs <- 5000 # Number of MC iterations per region
test_mode <- FALSE # Set to FALSE for full production run
n_cores <- 12 # Set to integer to override default cores detection (detectCores() - 1)
append <- FALSE # Set to TRUE to append to existing results file

regions <- c("US", "China", "Europe", "India")

if (test_mode) {
  message("Running in TEST MODE: truncating runs to 50 for speed.")
  n_runs <- 50
  regions <- head(regions, 2)
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
params_df <- read.csv("BiocharAG/inst/extdata/parameters.csv", stringsAsFactors = FALSE)
correlations_df <- read.csv("BiocharAG/inst/extdata/parameter_correlations.csv", stringsAsFactors = FALSE)

# Scenario dimensions are sampled discretely below; control flags (allow_eor, early_adoption, ...)
# stay at their scenario values and are never sampled.
scenario_params <- c("c_price", "discount_rate", "plant_mw_th")

# Spatial layers perturbed by a scalar multiplier (their bounds must be relative)
spatial_multiplier_params <- c(elec_price = "elec_price_multiplier", ff_c_intensity = "ff_ci_multiplier")

# 2. Pre-load and vectorize region spatial data
message("Pre-loading and vectorizing spatial data for all regions...")
region_names <- regions
vectorized_regions <- list()

for (r in region_names) {
  message("  Loading vectorized data for: ", r)
  dat <- load_region_data(r)
  vectorized_regions[[r]] <- dat[["vec", exact = TRUE]]
}

# 3. Generate Randomized MC Parameter Tables per Region
# Marginals are bounded PERT/uniform distributions centred on each region's values (see
# dist_min/dist_max/dist_bounds in parameters.csv); correlated parameters are drawn jointly
# via a Gaussian copula (parameter_correlations.csv).
set.seed(42) # For reproducible random draws
mc_tables_by_region <- list()

for (r in regions) {
  p_local <- BiocharAG::set_scenario(region = r)
  for (sp in names(spatial_multiplier_params)) {
    if (tolower(params_df$dist_bounds[params_df$name == sp]) != "relative") stop(sp, " must use relative bounds.")
    p_local[[sp]] <- 1 # Sampled as a multiplier on the spatial layer
  }

  dist_table <- BiocharAG::mc_distribution_table(params_df, central = p_local)
  dist_table <- dist_table[!dist_table$name %in% scenario_params, ]
  mc_table_r <- BiocharAG::sample_mc_parameters(dist_table, n_runs, correlations = correlations_df)
  for (sp in names(spatial_multiplier_params)) {
    names(mc_table_r)[names(mc_table_r) == sp] <- spatial_multiplier_params[[sp]]
  }

  # Discrete scenario sampling
  mc_table_r$c_price <- sample(c(0, 50, 100, 150, 200), n_runs, replace = TRUE)
  regional_dr <- if (!is.null(p_local$discount_rate)) p_local$discount_rate else 0.08
  mc_table_r$discount_rate <- sample(c(0.02, regional_dr), n_runs, replace = TRUE)
  mc_table_r$plant_mw_th <- sample(c(50, 150, 250), n_runs, replace = TRUE)

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
# (Moved to before generate_param_draws to allow spatial means for MC bounds)

message("Starting parallel Monte Carlo Analysis: ", length(regions), " regions x ", n_runs, " MC runs each.")

# Run scenario combinations in parallel
results_list <- parallel::mclapply(seq_along(regions), function(s) {
  r_name <- regions[s]
  r_data <- vectorized_regions[[r_name]]
  spatial_layers <- r_data$layers
  cell_area <- r_data$cell_area

  # Create a scenario-specific results accumulator
  scenario_results <- data.frame()

  for (m in seq_len(n_runs)) {
    mc_row <- mc_tables_by_region[[r_name]][m, ]

    # Base Setup from Scenario
    p <- BiocharAG::set_scenario(region = r_name)

    # Inject all uncertain extrinsic scalar parameters from mc_row into p
    for (p_name in names(mc_row)) {
      if (!(p_name %in% c("mc_run_id", "ff_ci_multiplier", "elec_price_multiplier"))) {
        p[[p_name]] <- mc_row[[p_name]]
      }
    }

    # Inject spatial layers (overriding scalar defaults if layer exists)
    # TODO (Future): If spatially explicit parameters with strict physical boundaries
    # (e.g. fractions strictly <= 1.0) are added and subjected to uncertainty multipliers,
    # explicit terra::clamp() logic must be added below to prevent the multiplier from
    # pushing pixel values out of bounds. Current spatial parameters (elec_price, ff_c_intensity)
    # are unbounded upper-limit quantities, so proportional scaling is safe.

    if ("soil_temp" %in% names(spatial_layers)) p$soil_temp <- spatial_layers$soil_temp

    if ("elec_price" %in% names(spatial_layers)) {
      factor <- if (!is.null(p$wholesale_discount_factor)) p$wholesale_discount_factor else 0.4
      ep_mult <- if (!is.null(mc_row$elec_price_multiplier)) mc_row$elec_price_multiplier else 1.0
      p$elec_price <- spatial_layers$elec_price * ep_mult * factor
    } else if (!is.null(p$elec_price)) {
      ep_mult <- if (!is.null(mc_row$elec_price_multiplier)) mc_row$elec_price_multiplier else 1.0
      p$elec_price <- p$elec_price * ep_mult
    }

    if ("soil_ph" %in% names(spatial_layers)) p$soil_ph <- spatial_layers$soil_ph
    if ("soil_cec" %in% names(spatial_layers)) p$soil_cec <- spatial_layers$soil_cec
    if ("dist_sink_km" %in% names(spatial_layers)) p$dist_sink_km <- spatial_layers$dist_sink_km
    if ("dist_sink_saline_km" %in% names(spatial_layers)) p$dist_sink_saline_km <- spatial_layers$dist_sink_saline_km
    if ("sink_is_offshore" %in% names(spatial_layers)) p$sink_is_offshore <- spatial_layers$sink_is_offshore
    if ("sink_is_offshore_saline" %in% names(spatial_layers)) p$sink_is_offshore_saline <- spatial_layers$sink_is_offshore_saline

    # Apply ff_ci_multiplier to ff_c_intensity (whether raster or scalar)
    ff_mult <- if (!is.null(mc_row$ff_ci_multiplier)) mc_row$ff_ci_multiplier else 1.0
    if ("ff_c_intensity" %in% names(spatial_layers)) {
      p$ff_c_intensity <- spatial_layers$ff_c_intensity * ff_mult
    } else if (!is.null(p$ff_c_intensity)) {
      p$ff_c_intensity <- p$ff_c_intensity * ff_mult
    }

    for (layer_name in c("cn_weather_risk", "cn_expansion_risk", "eu_base_eur", "us_base_cost")) {
      if (layer_name %in% names(spatial_layers)) p[[layer_name]] <- spatial_layers[[layer_name]]
    }

    if ("biomass_density" %in% names(spatial_layers)) {
      p$biomass_density <- spatial_layers$biomass_density
    }

    dist_layer_name <- paste0("dist_", mc_row$plant_mw_th, "MWth")
    if (!dist_layer_name %in% names(spatial_layers)) stop("Missing spatial distance layer: ", dist_layer_name)
    p$avg_dist <- spatial_layers[[dist_layer_name]]

    p$feedstock_cost <- BiocharAG::calculate_regional_feedstock_cost(r_name, p)

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
        mc_run_id = m,
        region = r_name,
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
        mean_energy_production_mwh = extract_masked_vector_mean(tech_res$energy_prod, is_best),
        mean_energy_revenue_mg = extract_masked_vector_mean(tech_res$energy_revenue_mg, is_best),
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

  message(sprintf("Finished Region: %s (%d runs)", r_name, n_runs))
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

# --- AI Summary Export ---
ai_dir <- "figures/ai_summaries/"
dir.create(ai_dir, showWarnings = FALSE, recursive = TRUE)

if (nrow(results_df) > 0) {
  ai_summary <- results_df %>%
    dplyr::group_by(region, technology, c_price) %>%
    dplyr::summarize(
      net_value_p05 = quantile(npv_mean, 0.05, na.rm = TRUE),
      net_value_p50 = median(npv_mean, na.rm = TRUE),
      net_value_p95 = quantile(npv_mean, 0.95, na.rm = TRUE),
      lcoe_p05 = quantile(mean_lcoe_usd_mwh, 0.05, na.rm = TRUE),
      lcoe_p50 = median(mean_lcoe_usd_mwh, na.rm = TRUE),
      lcoe_p95 = quantile(mean_lcoe_usd_mwh, 0.95, na.rm = TRUE),
      .groups = "drop"
    )
  ai_csv <- paste0(ai_dir, "mc_quantiles_summary.csv")
  write.table(ai_summary, file = ai_csv, row.names = FALSE, sep = ",", append = append && file_exists, col.names = !file_exists || !append)
  message("Saved AI summary quantiles to: ", ai_csv)
}
