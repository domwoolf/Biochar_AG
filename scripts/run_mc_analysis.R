# scripts/run_mc_analysis.R
# Executes Monte Carlo sensitivity and uncertainty analysis across scenario parameter combinations.
# Evaluates technologies competitively across spatial layers using randomized parameter draws.

source("scripts/generate_manuscript_figures.R")

# Configuration
n_runs <- 20       # Number of MC iterations per scenario combination (default 20 for testing)
test_mode <- TRUE  # Set to FALSE for full production run across all 720 scenario combinations

factorial_grid <- expand.grid(
  region = c("US", "China", "Europe", "India"),
  c_price = c(0, 50, 100, 150, 200),
  discount_rate = c(0.02, 0.08, 0.15),
  allow_eor = c(TRUE, FALSE),
  early_adoption = c(TRUE, FALSE),
  plant_mw_th = c(50, 150, 250),
  stringsAsFactors = FALSE
)

if (test_mode) {
  message("Running in TEST MODE: truncating factorial grid to 2 scenarios for speed.")
  factorial_grid <- head(factorial_grid, 2)
}

# 1. Load Parameter Definitions & Set Up Classifications
params_df <- read.csv("parameters_mc_ready.csv", stringsAsFactors = FALSE)

scenario_params <- c("region", "c_price", "discount_rate", "allow_eor", "early_adoption", "plant_mw_th",
                     "plant_sizes_mw_th", "optimize_scale", "use_flat_ci", "flat_ci_tCO2_GJ",
                     "beccs_available", "bc_valuation_method", "time_frame", "n_app_rate", "rebound",
                     "py_temp", "bm_feed_rate")

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

# 2. Generate Randomized MC Parameter Table (Common Random Numbers across scenarios)
generate_param_draws <- function(row, n) {
  dist <- tolower(gsub("[- ]", "", row$distribution))
  def_val <- suppressWarnings(as.numeric(row$default_value))
  disp <- as.numeric(row$dispersion)
  min_val <- as.numeric(row$minimum)
  max_val <- as.numeric(row$maximum)
  
  if (dist == "normal") {
    draws <- rnorm(n, mean = def_val, sd = disp)
  } else if (dist == "lognormal") {
    meanlog <- log(def_val) - (disp^2) / 2
    draws <- rlnorm(n, meanlog = meanlog, sdlog = disp)
  } else if (dist == "uniform") {
    draws <- runif(n, min = min_val, max = max_val)
  } else {
    draws <- rep(def_val, n)
  }
  
  # Clamp to physical/mathematical bounds
  if (!is.na(min_val)) draws <- pmax(draws, min_val)
  if (!is.na(max_val)) draws <- pmin(draws, max_val)
  
  return(draws)
}

set.seed(42) # For reproducible random draws
mc_draws_list <- list()

for (i in seq_len(nrow(uncertain_params))) {
  p_name <- uncertain_params$name[i]
  mc_draws_list[[p_name]] <- generate_param_draws(uncertain_params[i, ], n_runs)
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

mc_table <- as.data.frame(mc_draws_list, stringsAsFactors = FALSE)
mc_table$mc_run_id <- seq_len(n_runs)

# Helper functions for spatial metrics extraction
extract_masked_mean <- function(layer, opt_idx, target_i) {
  if (is.null(layer)) return(NA)
  if (!inherits(layer, "SpatRaster")) return(layer)
  masked <- terra::ifel(opt_idx == target_i, layer, NA)
  return(terra::global(masked, fun = "mean", na.rm = TRUE)[[1]])
}

extract_masked_min <- function(layer, opt_idx, target_i) {
  if (is.null(layer)) return(NA)
  if (!inherits(layer, "SpatRaster")) return(layer)
  masked <- terra::ifel(opt_idx == target_i, layer, NA)
  return(terra::global(masked, fun = "min", na.rm = TRUE)[[1]])
}

extract_masked_max <- function(layer, opt_idx, target_i) {
  if (is.null(layer)) return(NA)
  if (!inherits(layer, "SpatRaster")) return(layer)
  masked <- terra::ifel(opt_idx == target_i, layer, NA)
  return(terra::global(masked, fun = "max", na.rm = TRUE)[[1]])
}

# 3. Execution Loop: Scenario Combinations x MC Iterations
results_df <- data.frame()
region_cache <- list()

message("Starting Monte Carlo Analysis: ", nrow(factorial_grid), " scenario combinations x ", n_runs, " MC runs each.")
total_evals <- nrow(factorial_grid) * n_runs
eval_count <- 0

for (s in seq_len(nrow(factorial_grid))) {
  s_row <- factorial_grid[s, ]
  
  if (is.null(region_cache[[s_row$region]])) {
    region_cache[[s_row$region]] <- load_region_data(s_row$region)
  }
  dat <- region_cache[[s_row$region]]
  spatial_layers <- dat$layers
  
  for (m in seq_len(n_runs)) {
    eval_count <- eval_count + 1
    if (eval_count %% 10 == 0 || eval_count == 1) {
      message(sprintf("Processing MC evaluation %d of %d (Scenario %d/%d, MC Run %d/%d)...", eval_count, total_evals, s, nrow(factorial_grid), m, n_runs))
    }
    mc_row <- mc_table[m, ]
    
    # Base Setup from Scenario
    p <- BiocharAG::set_scenario()
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
    
    # Stack NPVs and find winner
    npv_stack <- c(res_bes$net_value, res_beccs$net_value, res_bebcs$net_value)
    
    # Ensure we only evaluate areas with biomass
    bm_mask <- spatial_layers$biomass_density > 0
    opt_idx_raw <- terra::which.max(npv_stack)
    opt_idx <- terra::ifel(bm_mask, opt_idx_raw, NA)
    
    cell_area <- terra::cellSize(spatial_layers$biomass_density, unit="km")
    biomass_amount <- spatial_layers$biomass_density * cell_area
    
    techs <- c("BES", "BECCS", "BEBCS")
    res_list <- list(res_bes, res_beccs, res_bebcs)
    
    for (t_idx in 1:3) {
      t_name <- techs[t_idx]
      tech_res <- res_list[[t_idx]]
      
      # Area and Biomass Calculations
      is_best <- opt_idx == t_idx
      is_viable <- is_best & (tech_res$net_value > 0)
      
      area_best_km2 <- terra::global(terra::ifel(is_best, cell_area, NA), fun="sum", na.rm=TRUE)[[1]]
      area_viable_km2 <- terra::global(terra::ifel(is_viable, cell_area, NA), fun="sum", na.rm=TRUE)[[1]]
      biomass_processed_yr <- terra::global(terra::ifel(is_viable, biomass_amount, NA), fun="sum", na.rm=TRUE)[[1]]
      
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
        area_best_km2 = area_best_km2,
        area_viable_km2 = area_viable_km2,
        biomass_processed_yr_mg = biomass_processed_yr,
        
        npv_min = extract_masked_min(tech_res$net_value, opt_idx, t_idx),
        npv_max = extract_masked_max(tech_res$net_value, opt_idx, t_idx),
        npv_mean = extract_masked_mean(tech_res$net_value, opt_idx, t_idx),
        
        mean_co2_transport_distance_km = extract_masked_mean(tech_res$co2_transport_distance_km, opt_idx, t_idx),
        mean_biomass_transport_distance_km = extract_masked_mean(tech_res$biomass_transport_distance_km, opt_idx, t_idx),
        mean_capital_cost_mg = extract_masked_mean(tech_res$capital_cost_mg, opt_idx, t_idx),
        mean_om_cost_mg = extract_masked_mean(tech_res$om_cost_mg, opt_idx, t_idx),
        mean_biomass_cost_mg = extract_masked_mean(tech_res$biomass_cost_mg, opt_idx, t_idx),
        mean_co2_transport_cost_mg = extract_masked_mean(tech_res$co2_transport_cost_mg, opt_idx, t_idx),
        
        mean_net_cdr = extract_masked_mean(tech_res$tot_c_abatement, opt_idx, t_idx),
        mean_carbon_removal_revenue_mg = extract_masked_mean(tech_res$abatement_revenue_mg, opt_idx, t_idx),
        mean_electricity_production_mwh = extract_masked_mean(tech_res$elec_prod, opt_idx, t_idx),
        mean_electricity_revenue_mg = extract_masked_mean(tech_res$elec_revenue_mg, opt_idx, t_idx),
        mean_agronomic_revenue_mg = extract_masked_mean(tech_res$agronomic_revenue_mg, opt_idx, t_idx),
        
        mean_lcoe_usd_mwh = extract_masked_mean(tech_res$lcoe, opt_idx, t_idx),
        mean_cost_of_co2_avoided = extract_masked_mean(tech_res$cost_of_co2_avoided, opt_idx, t_idx),
        mean_abatement_efficiency = extract_masked_mean(tech_res$abatement_efficiency, opt_idx, t_idx),
        mean_total_capex_m = extract_masked_mean(tech_res$total_capex_m, opt_idx, t_idx),
        stringsAsFactors = FALSE
      )
      
      new_row <- cbind(new_row, tea_cols)
      results_df <- rbind(results_df, new_row)
    }
  }
}

dir.create("results", showWarnings = FALSE)
write.csv(results_df, "results/mc_analysis_results.csv", row.names = FALSE)
message("Monte Carlo Analysis Complete. Results saved to results/mc_analysis_results.csv")
