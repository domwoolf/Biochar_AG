# scripts/run_factorial_analysis.R
# Executes a full factorial spatial TEA across 720 scenarios, evaluating technologies competitively.

source("scripts/generate_manuscript_figures.R")

factorial_grid <- expand.grid(
  region = c("US", "China", "Europe", "India"),
  c_price = c(0, 50, 100, 150, 200),
  discount_rate = c(0.02, 0.08, 0.15),
  allow_eor = c(TRUE, FALSE),
  early_adoption = c(TRUE, FALSE),
  plant_mw_th = c(50, 150, 250),
  stringsAsFactors = FALSE
)

# For testing, truncate this to head(factorial_grid, 2)
# factorial_grid <- head(factorial_grid, 2)

results_df <- data.frame()
region_cache <- list()

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

message("Starting Factorial Analysis: ", nrow(factorial_grid), " total runs (each evaluating 3 technologies competitively).")

for (i in 1:nrow(factorial_grid)) {
  if (i %% 50 == 0) message(sprintf("Processing run %d of %d...", i, nrow(factorial_grid)))
  row <- factorial_grid[i, ]
  
  if (is.null(region_cache[[row$region]])) {
    region_cache[[row$region]] <- load_region_data(row$region)
  }
  dat <- region_cache[[row$region]]
  spatial_layers <- dat$layers
  
  # Base Setup
  p <- BiocharAG::set_scenario()
  p$c_price <- row$c_price
  p$discount_rate <- row$discount_rate
  p$allow_eor <- row$allow_eor
  p$early_adoption <- row$early_adoption
  p$plant_mw_th <- row$plant_mw_th
  
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
  if ("ff_c_intensity" %in% names(spatial_layers)) p$ff_c_intensity <- spatial_layers$ff_c_intensity
  
  for (layer_name in c("cn_weather_risk", "cn_expansion_risk", "eu_base_eur", "us_base_cost")) {
      if (layer_name %in% names(spatial_layers)) p[[layer_name]] <- spatial_layers[[layer_name]]
  }
  
  dist_layer_name <- paste0("dist_", row$plant_mw_th, "MWth")
  if (dist_layer_name %in% names(spatial_layers)) {
      p$avg_dist <- spatial_layers[[dist_layer_name]]
  }
  
  feedstock_region <- if (row$region == "Europe") "EU" else row$region
  p$feedstock_cost <- BiocharAG::calculate_regional_feedstock_cost(feedstock_region, p)
  
  # Execute All 3 Technologies
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
    
    new_row <- data.frame(
      region = row$region,
      c_price = row$c_price,
      discount_rate = row$discount_rate,
      allow_eor = row$allow_eor,
      early_adoption = row$early_adoption,
      plant_mw_th = row$plant_mw_th,
      technology = t_name,
      
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
    
    results_df <- rbind(results_df, new_row)
  }
}

dir.create("results", showWarnings = FALSE)
write.csv(results_df, "results/factorial_analysis_results.csv", row.names = FALSE)
message("Factorial Analysis Complete. Results saved to results/factorial_analysis_results.csv")
