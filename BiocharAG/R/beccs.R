#' Calculate Bioenergy Carbon Capture and Storage (BECCS) Metrics
#'
#' Modernized logic (2024 Basis):
#' - Explicitly tracks Scope 3 transport emissions and road tortuosity.
#' - Converts sequestered carbon pools to CO2e ratios accurately (44/12).
#'
#' @param params A list of parameters.
#' @return A list of calculated metrics for BECCS.
#' @export
calculate_beccs <- function(params) {
  # BECCS efficiency = BES baseline minus the parasitic capture/compression penalty
  # (derived before fuel adjustment so the ash multiplier scales it like BES).
  if (is.null(params$bes_energy_efficiency)) params$bes_energy_efficiency <- 0.30
  if (is.null(params$beccs_eff_penalty)) params$beccs_eff_penalty <- 0.08
  if (is.null(params$beccs_efficiency)) {
    params$beccs_efficiency <- params$bes_energy_efficiency - params$beccs_eff_penalty
  }
  if (is.null(params$capture_rate)) params$capture_rate <- 0.90
  if (is.null(params$early_adoption)) params$early_adoption <- FALSE
  if (is.null(params$beccs_om_factor)) params$beccs_om_factor <- 0.05
  allow_eor <- if (!is.null(params$allow_eor)) as.logical(params$allow_eor) else TRUE
  dist_spatial <- NULL
  if (allow_eor) {
    if (!is.null(params$dist_sink_km)) dist_spatial <- params$dist_sink_km
  } else {
    if (!is.null(params$dist_sink_saline_km)) dist_spatial <- params$dist_sink_saline_km
  }

  # Without EOR the sink is the nearest saline store, so use that sink's onshore/offshore flag
  if (!allow_eor && !is.null(params$sink_is_offshore_saline)) {
    params$sink_is_offshore <- params$sink_is_offshore_saline
  }

  if (!is.null(dist_spatial)) {
    params$ccs_distance <- dist_spatial
  } else if (is.null(params$ccs_distance)) {
    if (!is.null(params$lat) && !is.null(params$lon)) {
      geo <- find_nearest_sink(params$lat, params$lon)
      params$ccs_distance <- geo$distance_km
    } else {
      params$ccs_distance <- 100
    }
  }

  if (is.null(params$bes_capital_cost)) params$bes_capital_cost <- 3000
  if (is.null(params$bes_capex_ref_eff)) params$bes_capex_ref_eff <- 0.30
  if (is.null(params$beccs_capex_premium)) params$beccs_capex_premium <- 0.40
  params <- adjust_costs_for_fuel(params)

  with(params, {
    # 1. Energy Output
    energy_output <- bm_lhv * beccs_efficiency
    gj_to_mwh_conv <- if (!is.null(params$gj_to_mwh)) gj_to_mwh else 0.277778
    energy_prod <- energy_output * gj_to_mwh_conv

    # 2. Carbon Capture
    molar_ratio_c <- if (!is.null(params$molar_ratio_co2_c)) molar_ratio_co2_c else (44/12)
    co2_produced <- bm_c * molar_ratio_c
    co2_captured <- co2_produced * capture_rate # Mg CO2e / Mg Biomass

    # 3. Scale & Total Mass Flow
    if (!is.null(params$plant_mw_th)) {
      plant_mw_th <- resolve_plant_mw_th(params$plant_mw_th, "BECCS")
      plant_mw <- plant_mw_th * beccs_efficiency
    } else {
      plant_mw <- if (!is.null(params$plant_mw)) params$plant_mw else 50
      plant_mw_th <- plant_mw / beccs_efficiency
    }

    capacity_factor_val <- if (!is.null(params$capacity_factor)) capacity_factor else 0.85
    annual_biomass <- (plant_mw_th * 8760 * capacity_factor_val) / (bm_lhv * gj_to_mwh_conv)
    annual_co2_total <- annual_biomass * co2_captured

    # --- CCS Transport & Storage Component ---
    dist_onshore <- if (!is.null(params$dist_onshore)) params$dist_onshore else Inf
    dist_offshore <- if (!is.null(params$dist_offshore)) params$dist_offshore else Inf

    is_inf_onshore <- !inherits(dist_onshore, "SpatRaster") && is.infinite(dist_onshore)
    is_inf_offshore <- !inherits(dist_offshore, "SpatRaster") && is.infinite(dist_offshore)
    if (is_inf_onshore && is_inf_offshore && !is.null(params$ccs_distance)) {
      if (!is.null(params$sink_is_offshore)) {
        if (inherits(params$sink_is_offshore, "SpatRaster")) {
          dist_offshore <- terra::ifel(params$sink_is_offshore == 1, params$ccs_distance, Inf)
          dist_onshore <- terra::ifel(params$sink_is_offshore == 0, params$ccs_distance, Inf)
        } else {
          dist_offshore <- ifelse_raster(params$sink_is_offshore == 1, params$ccs_distance, Inf)
          dist_onshore <- ifelse_raster(params$sink_is_offshore == 0, params$ccs_distance, Inf)
        }
      } else {
        dist_onshore <- params$ccs_distance
      }
    }

    # Ship route legs for offshore sinks: pipeline to the coast, then sea voyage to the assigned sink.
    # TODO (see Article/TODO.md): these layers are not generated yet; when absent the ship cost falls back to
    # a voyage over the full sink distance with no inland pipeline leg.
    dist_coast <- params$dist_coast_km
    dist_sea <- if (!allow_eor && !is.null(params$dist_sea_saline_km)) params$dist_sea_saline_km else params$dist_sea_km
    capex_loc <- location_factor(params, "capex")
    om_loc <- location_factor(params, "om")

    base_cost_onshore_storage <- if (!is.null(params$ccs_storage_cost)) params$ccs_storage_cost else 12.0
    base_cost_offshore_storage <- if (!is.null(params$cost_offshore_storage)) cost_offshore_storage else 40.0

    cost_onshore_trans <- calculate_ccs_transport(
      co2_mass = annual_co2_total,
      distance = dist_onshore,
      is_offshore = FALSE,
      discount_rate = discount_rate,
      lifetime = bes_life,
      early_adoption = early_adoption,
      capex_factor = capex_loc,
      om_factor = om_loc
    )
    ts_cost_onshore_calc <- (cost_onshore_trans + base_cost_onshore_storage) * co2_captured
    ts_cost_onshore <- ifelse_raster(is.infinite(dist_onshore), Inf, ts_cost_onshore_calc)

    cost_offshore_trans <- calculate_ccs_transport(
      co2_mass = annual_co2_total,
      distance = dist_offshore,
      is_offshore = TRUE,
      discount_rate = discount_rate,
      lifetime = bes_life,
      early_adoption = early_adoption,
      dist_coast = dist_coast,
      dist_sea = dist_sea,
      capex_factor = capex_loc,
      om_factor = om_loc
    )
    ts_cost_offshore_calc <- (cost_offshore_trans + base_cost_offshore_storage) * co2_captured
    ts_cost_offshore <- ifelse_raster(is.infinite(dist_offshore), Inf, ts_cost_offshore_calc)

    ts_cost <- pmin_raster(ts_cost_onshore, ts_cost_offshore)

    # 4. Plant Costs (CAPEX/OPEX)
    scaling_factor_val <- if (!is.null(params$scaling_factor)) scaling_factor else 0.7
    # Equivalent BES plant for the same thermal input, plus the capture/compression premium
    total_capex <- combustion_plant_capex(bes_capital_cost, plant_mw_th, bes_capex_ref_eff, scaling_factor_val) *
      (1 + beccs_capex_premium) * location_factor(params, "capex")
    annuity_fac <- calculate_annuity_factor(discount_rate, bes_life)
    annual_capex_payment <- total_capex / annuity_fac

    capex_per_mg <- annual_capex_payment / annual_biomass
    # Annual O&M is a fraction of total CAPEX. Costs are levelised per year: discounting this constant
    # annual cost over the plant life and re-annualising at the same rate returns the annual value.
    opex_per_mg <- (total_capex * beccs_om_factor * location_factor(params, "om")) / annual_biomass

    # --- 5. Logistics Cost & Transport Emissions ---
    logistics <- biomass_logistics(params)
    effective_dist <- logistics$effective_dist
    logistics_cost <- logistics$cost
    transport_emissions_co2e <- logistics$emissions

    feedstock_cost <- if (!is.null(params$feedstock_cost)) params$feedstock_cost else 0
    total_cost <- capex_per_mg + opex_per_mg + ts_cost + logistics_cost + feedstock_cost

    # 6. Revenue & Value
    energy_revenue <- energy_prod * elec_price

    # Carbon Abatement (CO2e conversion & transport penalty applied)
    co2e_sequestered <- bm_c * capture_rate * molar_ratio_c
    c_displaced <- energy_output * ff_c_intensity
    tot_c_abatement <- co2e_sequestered + c_displaced - transport_emissions_co2e + residue_counterfactual_ghg(params)
    abatement_value <- tot_c_abatement * c_price

    ash_value <- calculate_ash_value(params) # Recycled combustion ash (lime + P)
    total_revenue <- energy_revenue + ash_value + abatement_value
    net_value <- total_revenue - total_cost

    # Added diagnostics for factorial
    biomass_cost <- feedstock_cost + logistics_cost
    lcoe <- (capex_per_mg + opex_per_mg + ts_cost + biomass_cost - ash_value) / energy_prod
    cost_of_co2_avoided <- ifelse_raster(tot_c_abatement > 0, total_cost / tot_c_abatement, Inf)
    abatement_efficiency <- ifelse_raster(co2e_sequestered > 0, tot_c_abatement / co2e_sequestered, 0)
    total_capex_m <- total_capex / 1e6 # Convert to millions
    co2_dist_chosen <- ifelse_raster(ts_cost_onshore < ts_cost_offshore, dist_onshore, dist_offshore)

    list(
      technology = "BECCS",
      energy_output = energy_output,
      energy_prod = energy_prod,
      c_sequestered = co2e_sequestered, # Now safely in CO2e
      tot_c_abatement = tot_c_abatement,
      total_cost = total_cost,
      ts_cost = ts_cost,
      total_revenue = total_revenue,
      net_value = net_value,
      # Granular outputs
      capital_cost_mg = capex_per_mg,
      om_cost_mg = opex_per_mg,
      biomass_cost_mg = biomass_cost,
      co2_transport_cost_mg = ts_cost,
      co2_transport_distance_km = co2_dist_chosen,
      biomass_transport_distance_km = effective_dist,
      energy_revenue_mg = energy_revenue,
      abatement_revenue_mg = abatement_value,
      agronomic_revenue_mg = ash_value,
      lcoe = lcoe,
      cost_of_co2_avoided = cost_of_co2_avoided,
      abatement_efficiency = abatement_efficiency,
      total_capex_m = total_capex_m
    )
  })
}
