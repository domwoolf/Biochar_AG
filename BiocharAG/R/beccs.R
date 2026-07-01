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
  if (is.null(params$beccs_efficiency)) params$beccs_efficiency <- 0.28
  if (is.null(params$capture_rate)) params$capture_rate <- 0.90
  if (is.null(params$early_adoption)) params$early_adoption <- FALSE

  allow_eor <- if (!is.null(params$allow_eor)) as.logical(params$allow_eor) else TRUE
  dist_spatial <- NULL
  if (allow_eor) {
    if (!is.null(params$dist_sink_km)) dist_spatial <- params$dist_sink_km
  } else {
    if (!is.null(params$dist_sink_saline_km)) dist_spatial <- params$dist_sink_saline_km
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

  if (is.null(params$beccs_capital_cost)) params$beccs_capital_cost <- 4000
  params <- adjust_costs_for_fuel(params)

  with(params, {
    # 1. Energy Output
    energy_output <- bm_lhv * beccs_efficiency
    elec_prod <- energy_output * 0.277778

    # 2. Carbon Capture
    co2_produced <- bm_c * (44 / 12)
    co2_captured <- co2_produced * capture_rate # Mg CO2e / Mg Biomass

    # 3. Scale & Total Mass Flow
    if (!is.null(params$plant_mw_th)) {
      plant_mw_th <- resolve_plant_mw_th(params$plant_mw_th, "BECCS")
      plant_mw <- plant_mw_th * beccs_efficiency
    } else {
      plant_mw <- if (!is.null(params$plant_mw)) params$plant_mw else 50
      plant_mw_th <- plant_mw / beccs_efficiency
    }

    capacity_factor <- 0.85
    annual_biomass <- (plant_mw_th * 8760 * capacity_factor) / (bm_lhv * 0.277778)
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
          if (params$sink_is_offshore) {
            dist_offshore <- params$ccs_distance
          } else {
            dist_onshore <- params$ccs_distance
          }
        }
      } else {
        dist_onshore <- params$ccs_distance
      }
    }

    base_cost_onshore_storage <- if (!is.null(params$ccs_storage_cost)) params$ccs_storage_cost else 12.0
    base_cost_offshore_storage <- 40.0

    cost_onshore_trans <- calculate_ccs_transport(
      co2_mass = annual_co2_total,
      distance = dist_onshore,
      is_offshore = FALSE,
      discount_rate = discount_rate,
      lifetime = bes_life,
      early_adoption = early_adoption
    )
    ts_cost_onshore_calc <- (cost_onshore_trans + base_cost_onshore_storage) * co2_captured
    ts_cost_onshore <- ifelse_raster(is.infinite(dist_onshore), Inf, ts_cost_onshore_calc)

    cost_offshore_trans <- calculate_ccs_transport(
      co2_mass = annual_co2_total,
      distance = dist_offshore,
      is_offshore = TRUE,
      discount_rate = discount_rate,
      lifetime = bes_life,
      early_adoption = early_adoption
    )
    ts_cost_offshore_calc <- (cost_offshore_trans + base_cost_offshore_storage) * co2_captured
    ts_cost_offshore <- ifelse_raster(is.infinite(dist_offshore), Inf, ts_cost_offshore_calc)

    ts_cost <- pmin_raster(ts_cost_onshore, ts_cost_offshore)

    # 4. Plant Costs (CAPEX/OPEX)
    scaling_factor <- 0.7
    base_cost_beccs <- beccs_capital_cost * 50 * 1000

    total_capex <- base_cost_beccs * ((plant_mw / 50)^scaling_factor)
    annuity_fac <- calculate_annuity_factor(discount_rate, bes_life)
    annual_capex_payment <- total_capex / annuity_fac

    capex_per_mg <- annual_capex_payment / annual_biomass
    opex_per_mg <- capex_per_mg * 0.05

    # --- 5. Logistics Cost & Transport Emissions ---
    if (!is.null(params$avg_dist)) {
      avg_dist <- params$avg_dist
    } else {
      radius <- if (!is.null(params$collection_radius)) params$collection_radius else 50
      avg_dist <- (2 / 3) * radius
    }

    tort <- if (!is.null(params$tortuosity)) params$tortuosity else 1.3
    effective_dist <- avg_dist * tort

    tf <- if (!is.null(params$bm_transport_fixed)) params$bm_transport_fixed else 5.0
    tv <- if (!is.null(params$bm_transport_var)) params$bm_transport_var else 0.15
    logistics_cost <- tf + (tv * effective_dist)

    trans_em_factor <- if (!is.null(params$transport_emissions_factor)) params$transport_emissions_factor else 0.0001
    transport_emissions_co2e <- effective_dist * trans_em_factor

    feedstock_cost <- if (!is.null(params$feedstock_cost)) params$feedstock_cost else 0
    total_cost <- capex_per_mg + opex_per_mg + ts_cost + logistics_cost + feedstock_cost

    # 6. Revenue & Value
    elec_revenue <- elec_prod * elec_price

    # Carbon Abatement (CO2e conversion & transport penalty applied)
    co2e_sequestered <- bm_c * capture_rate * (44 / 12)
    c_displaced <- energy_output * ff_c_intensity
    tot_c_abatement <- co2e_sequestered + c_displaced - transport_emissions_co2e
    abatement_value <- tot_c_abatement * c_price

    total_revenue <- elec_revenue + abatement_value
    net_value <- total_revenue - total_cost

    # Added diagnostics for factorial
    biomass_cost <- feedstock_cost + logistics_cost
    lcoe <- (capex_per_mg + opex_per_mg + ts_cost + biomass_cost) / elec_prod
    cost_of_co2_avoided <- ifelse_raster(tot_c_abatement > 0, total_cost / tot_c_abatement, Inf)
    abatement_efficiency <- ifelse_raster(co2e_sequestered > 0, tot_c_abatement / co2e_sequestered, 0)
    total_capex_m <- total_capex / 1e6
    co2_dist_chosen <- ifelse_raster(ts_cost_onshore < ts_cost_offshore, dist_onshore, dist_offshore)

    list(
      technology = "BECCS",
      energy_output = energy_output,
      elec_prod = elec_prod,
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
      elec_revenue_mg = elec_revenue,
      abatement_revenue_mg = abatement_value,
      agronomic_revenue_mg = NA,
      lcoe = lcoe,
      cost_of_co2_avoided = cost_of_co2_avoided,
      abatement_efficiency = abatement_efficiency,
      total_capex_m = total_capex_m
    )
  })
}
