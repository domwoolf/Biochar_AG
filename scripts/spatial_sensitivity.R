# scripts/run_spatial_sensitivity.R
# Runs spatial Techno-Economic Assessment (TEA) on a selected scenario.
# Extracts spatial input layers and detailed NPV breakdowns for all active cells.

library(terra)
library(dplyr)
library(tidyr)

# Sourcing script for helper load function and package loading
source("scripts/manuscript_figures.R")

# --- Configuration ---
SCENARIO_NAME <- "CP100_MW250" # Predefined scenario name (e.g. "default", "CP100_MW250", "EA", "EA_CP100_MW250", "EA_CP100_MW250_EOR")
OUTPUT_FILE <- "results/spatial_sensitivity_results.csv"

message("Starting Spatial Sensitivity Analysis...")
message("Selected Scenario: ", SCENARIO_NAME)

# 1. Load Parameter Definitions & Scenario
params <- load_parameters("parameters.csv")
if (SCENARIO_NAME %in% names(BiocharAG::scenarios)) {
  overrides <- BiocharAG::scenarios[[SCENARIO_NAME]]
  params[names(overrides)] <- overrides
  message("Successfully loaded scenario overrides.")
} else {
  stop("Scenario '", SCENARIO_NAME, "' not found in BiocharAG::scenarios.")
}

# Resolve general scenario parameters
c_price <- if (!is.null(params$c_price)) params$c_price else 150
tort <- if (!is.null(params$tortuosity)) params$tortuosity else 1.3
tf <- if (!is.null(params$bm_transport_fixed)) params$bm_transport_fixed else 5.0
tv <- if (!is.null(params$bm_transport_var)) params$bm_transport_var else 0.15
trans_em_factor <- if (!is.null(params$transport_emissions_factor)) params$transport_emissions_factor else 0.0001

# Function to run technology evaluation cell-by-cell using vectorization
evaluate_tech_vectorized <- function(tech_fun, tech_name, base_params, spatial_layers, cell_area, region_name) {
  optimize_scale <- if (!is.null(base_params$optimize_scale)) base_params$optimize_scale else FALSE
  plant_sizes_mw_th <- if (!is.null(base_params$plant_sizes_mw_th)) base_params$plant_sizes_mw_th else c(5, 25, 50, 100, 250, 500)

  p <- base_params
  p$region <- region_name

  # Inject spatial layers (as vectors)
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

  if (optimize_scale) {
    results_by_size <- list()
    npv_matrix <- matrix(NA, nrow = length(spatial_layers$biomass_density), ncol = length(plant_sizes_mw_th))

    for (i in seq_along(plant_sizes_mw_th)) {
      sz <- plant_sizes_mw_th[i]
      p_sz <- p
      p_sz$plant_mw_th <- sz

      dist_layer_name <- paste0("dist_", sz, "MWth")
      if (dist_layer_name %in% names(spatial_layers)) {
        p_sz$avg_dist <- spatial_layers[[dist_layer_name]]
      } else {
        stop("Missing spatial distance layer: ", dist_layer_name)
      }

      p_sz$feedstock_cost <- BiocharAG::calculate_regional_feedstock_cost(
        if (region_name == "Europe") "EU" else region_name,
        p_sz
      )

      res <- tech_fun(p_sz)
      results_by_size[[i]] <- res
      npv_matrix[, i] <- res$net_value
    }

    opt_size_idx <- max.col(npv_matrix, ties.method = "first")
    opt_size_idx[rowSums(is.na(npv_matrix)) == ncol(npv_matrix)] <- NA

    n_cells <- length(spatial_layers$biomass_density)
    out_res <- list()
    res_names <- names(results_by_size[[1]])

    for (name in res_names) {
      val1 <- results_by_size[[1]][[name]]
      if (length(val1) == 1 && is.na(val1)) {
        out_res[[name]] <- rep(NA, n_cells)
      } else if (length(val1) == 1) {
        out_res[[name]] <- rep(val1, n_cells)
      } else {
        out_res[[name]] <- rep(NA, n_cells)
      }
    }
    out_res[["plant_mw_th_chosen"]] <- rep(NA, n_cells)
    out_res[["avg_dist_chosen"]] <- rep(NA, n_cells)

    for (i in seq_along(plant_sizes_mw_th)) {
      cells_mask <- !is.na(opt_size_idx) & (opt_size_idx == i)
      if (any(cells_mask)) {
        for (name in res_names) {
          val <- results_by_size[[i]][[name]]
          if (length(val) > 1) {
            out_res[[name]][cells_mask] <- val[cells_mask]
          } else if (!is.na(val)) {
            out_res[[name]][cells_mask] <- val
          }
        }
        dist_layer_name <- paste0("dist_", plant_sizes_mw_th[i], "MWth")
        out_res[["plant_mw_th_chosen"]][cells_mask] <- plant_sizes_mw_th[i]
        out_res[["avg_dist_chosen"]][cells_mask] <- spatial_layers[[dist_layer_name]][cells_mask]
      }
    }
    return(out_res)
  } else {
    sz <- max(5, round(BiocharAG:::resolve_plant_mw_th(p$plant_mw_th, tech_name) / 5) * 5)
    p$plant_mw_th <- sz

    dist_layer_name <- paste0("dist_", sz, "MWth")
    if (dist_layer_name %in% names(spatial_layers)) {
      p$avg_dist <- spatial_layers[[dist_layer_name]]
    } else {
      stop("Missing spatial distance layer: ", dist_layer_name)
    }

    p$feedstock_cost <- BiocharAG::calculate_regional_feedstock_cost(
      if (region_name == "Europe") "EU" else region_name,
      p
    )

    res <- tech_fun(p)
    res[["plant_mw_th_chosen"]] <- rep(sz, length(spatial_layers$biomass_density))
    res[["avg_dist_chosen"]] <- p$avg_dist
    return(res)
  }
}

regions <- c("US", "China", "Europe", "India")
all_regions_results <- list()

for (r in regions) {
  message("Processing region: ", r)
  dat <- load_region_data(r)
  layers <- dat$layers

  # Identify active cells
  bm_vals <- terra::values(layers$biomass_density, mat = FALSE)
  active_indices <- which(!is.na(bm_vals) & bm_vals > 0)

  # Coordinates of active cells
  coords <- terra::xyFromCell(layers$biomass_density, active_indices)

  # Cell area in km2
  cell_area_raster <- terra::cellSize(layers$biomass_density, unit = "km")
  cell_area_vals <- terra::values(cell_area_raster, mat = FALSE)[active_indices]

  # Extract values of all layers for active indices
  spatial_layers <- list()
  for (layer_name in names(layers)) {
    vals <- terra::values(layers[[layer_name]], mat = FALSE)
    if (is.matrix(vals)) {
      spatial_layers[[layer_name]] <- vals[active_indices, 1]
    } else {
      spatial_layers[[layer_name]] <- vals[active_indices]
    }
  }

  message("  Running competitive vectorized spatial TEA...")
  res_bes <- evaluate_tech_vectorized(BiocharAG::calculate_bes, "BES", params, spatial_layers, cell_area_vals, r)
  res_beccs <- evaluate_tech_vectorized(BiocharAG::calculate_beccs, "BECCS", params, spatial_layers, cell_area_vals, r)
  res_bebcs <- evaluate_tech_vectorized(BiocharAG::calculate_bebcs, "BEBCS", params, spatial_layers, cell_area_vals, r)

  message("  Calculating economic metrics and breakdowns...")
  # Logistics transport and feedstock calculations
  effective_dist_bes <- res_bes$avg_dist_chosen * tort
  logistics_cost_bes <- tf + (tv * effective_dist_bes)
  feedstock_cost_bes <- res_bes$biomass_cost_mg - logistics_cost_bes

  effective_dist_beccs <- res_beccs$avg_dist_chosen * tort
  logistics_cost_beccs <- tf + (tv * effective_dist_beccs)
  feedstock_cost_beccs <- res_beccs$biomass_cost_mg - logistics_cost_beccs

  effective_dist_bebcs <- res_bebcs$avg_dist_chosen * tort
  logistics_cost_bebcs <- tf + (tv * effective_dist_bebcs)
  feedstock_cost_bebcs <- res_bebcs$biomass_cost_mg - logistics_cost_bebcs

  # Carbon abatement components
  ff_ci_vals <- if ("ff_c_intensity" %in% names(spatial_layers)) spatial_layers$ff_c_intensity else params$ff_c_intensity

  c_displaced_bes <- res_bes$energy_output * ff_ci_vals
  c_displaced_beccs <- res_beccs$energy_output * ff_ci_vals
  c_displaced_bebcs <- res_bebcs$energy_output * ff_ci_vals

  carbon_transport_emissions_cost_bes <- effective_dist_bes * trans_em_factor * c_price
  carbon_transport_emissions_cost_beccs <- effective_dist_beccs * trans_em_factor * c_price
  carbon_transport_emissions_cost_bebcs <- effective_dist_bebcs * trans_em_factor * c_price

  # 1. NPV under scenario carbon price
  npv_bes <- res_bes$net_value
  npv_beccs <- res_beccs$net_value
  npv_bebcs <- res_bebcs$net_value

  # 2. NPV at C=0
  npv0_bes <- npv_bes - c_price * res_bes$tot_c_abatement
  npv0_beccs <- npv_beccs - c_price * res_beccs$tot_c_abatement
  npv0_bebcs <- npv_bebcs - c_price * res_bebcs$tot_c_abatement

  # 3. Break-even carbon price per technology
  breakeven_cprice_bes <- ifelse(res_bes$tot_c_abatement <= 0, NA, -npv0_bes / res_bes$tot_c_abatement)
  breakeven_cprice_beccs <- ifelse(res_beccs$tot_c_abatement <= 0, NA, -npv0_beccs / res_beccs$tot_c_abatement)
  breakeven_cprice_bebcs <- ifelse(res_bebcs$tot_c_abatement <= 0, NA, -npv0_bebcs / res_bebcs$tot_c_abatement)

  # 4. Minimum break-even carbon price across the technologies
  c_matrix <- cbind(breakeven_cprice_bes, breakeven_cprice_beccs, breakeven_cprice_bebcs)
  min_breakeven_cprice <- apply(c_matrix, 1, function(row) {
    if (all(is.na(row))) {
      return(NA)
    }
    min(row, na.rm = TRUE)
  })

  # 5. Best Technology
  npv_matrix <- cbind(npv_bes, npv_beccs, npv_bebcs)
  best_idx <- max.col(npv_matrix, ties.method = "first")
  best_idx[rowSums(is.na(npv_matrix)) == 3] <- NA
  best_technology <- c("BES", "BECCS", "BEBCS")[best_idx]

  # Build region dataframe
  region_df <- data.frame(
    x = coords[, 1],
    y = coords[, 2],
    region = r,
    cell_area_km2 = cell_area_vals,

    # Spatial Inputs
    biomass_density = spatial_layers$biomass_density,
    soil_temp = if ("soil_temp" %in% names(spatial_layers)) spatial_layers$soil_temp else NA,
    elec_price = if ("elec_price" %in% names(spatial_layers)) spatial_layers$elec_price else NA,
    dist_sink_km = if ("dist_sink_km" %in% names(spatial_layers)) spatial_layers$dist_sink_km else NA,
    dist_sink_saline_km = if ("dist_sink_saline_km" %in% names(spatial_layers)) spatial_layers$dist_sink_saline_km else NA,
    sink_is_offshore = if ("sink_is_offshore" %in% names(spatial_layers)) spatial_layers$sink_is_offshore else NA,
    soil_ph = if ("soil_ph" %in% names(spatial_layers)) spatial_layers$soil_ph else NA,
    soil_cec = if ("soil_cec" %in% names(spatial_layers)) spatial_layers$soil_cec else NA,
    ff_c_intensity = ff_ci_vals,

    # Results
    min_breakeven_cprice = min_breakeven_cprice,
    best_technology = best_technology,

    # BES Metrics
    npv_BES = npv_bes,
    breakeven_cprice_BES = breakeven_cprice_bes,
    biomass_transport_cost_BES = logistics_cost_bes,
    feedstock_cost_BES = feedstock_cost_bes,
    capex_BES = res_bes$capital_cost_mg,
    opex_BES = res_bes$om_cost_mg,
    energy_revenue_BES = res_bes$elec_revenue_mg,
    fossil_fuel_offset_revenue_BES = c_displaced_bes * c_price,
    cdr_revenue_BES = 0,
    co2_transport_storage_cost_BES = 0,
    carbon_transport_emissions_cost_BES = carbon_transport_emissions_cost_bes,

    # BECCS Metrics
    npv_BECCS = npv_beccs,
    breakeven_cprice_BECCS = breakeven_cprice_beccs,
    biomass_transport_cost_BECCS = logistics_cost_beccs,
    feedstock_cost_BECCS = feedstock_cost_beccs,
    capex_BECCS = res_beccs$capital_cost_mg,
    opex_BECCS = res_beccs$om_cost_mg,
    energy_revenue_BECCS = res_beccs$elec_revenue_mg,
    fossil_fuel_offset_revenue_BECCS = c_displaced_beccs * c_price,
    cdr_revenue_BECCS = res_beccs$c_sequestered * c_price,
    co2_transport_storage_cost_BECCS = res_beccs$ts_cost,
    carbon_transport_emissions_cost_BECCS = carbon_transport_emissions_cost_beccs,

    # BEBCS Metrics
    npv_BEBCS = npv_bebcs,
    breakeven_cprice_BEBCS = breakeven_cprice_bebcs,
    biomass_transport_cost_BEBCS = logistics_cost_bebcs,
    feedstock_cost_BEBCS = feedstock_cost_bebcs,
    capex_BEBCS = res_bebcs$capital_cost_mg,
    opex_BEBCS = res_bebcs$om_cost_mg,
    energy_revenue_BEBCS = res_bebcs$elec_revenue_mg,
    fossil_fuel_offset_revenue_BEBCS = c_displaced_bebcs * c_price,
    cdr_revenue_BEBCS = res_bebcs$c_sequestered * c_price,
    agronomic_revenue_BEBCS = res_bebcs$agronomic_revenue_mg,
    soil_ghg_offset_revenue_BEBCS = 0.1 * c_price,
    co2_transport_storage_cost_BEBCS = 0,
    carbon_transport_emissions_cost_BEBCS = carbon_transport_emissions_cost_bebcs,
    stringsAsFactors = FALSE
  )

  all_regions_results[[r]] <- region_df
}

# Combine and save results
results_df <- do.call(rbind, all_regions_results)
dir.create("results", showWarnings = FALSE)
write.csv(results_df, OUTPUT_FILE, row.names = FALSE)
message("Spatial Sensitivity Analysis Complete. Results saved to: ", OUTPUT_FILE)
