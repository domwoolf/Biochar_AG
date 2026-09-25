#' Calculate Bioenergy System (BES) Metrics
#'
#' Modernized logic (2024 Basis):
#' - Uses modernized Capital Cost ($3,000/kW) and Efficiency (30%) defaults.
#' - Calculates Levelized Cost of Electricity components (CAPEX/OPEX).
#' - Explicitly tracks Scope 3 transport emissions and road tortuosity.
#'
#' @param params A list of parameters.
#' @return A list of calculated metrics for BES.
#' @export
calculate_bes <- function(params) {
  # Default to modern params if not present
  if (is.null(params$bes_capital_cost)) params$bes_capital_cost <- 3000
  if (is.null(params$bes_energy_efficiency)) params$bes_energy_efficiency <- 0.30
  if (is.null(params$bes_om_factor)) params$bes_om_factor <- 0.04
  if (is.null(params$bes_life)) params$bes_life <- 30
  if (is.null(params$bes_capex_ref_eff)) params$bes_capex_ref_eff <- 0.30

  # Apply Fuel Quality Penalties (High Ash -> Higher Cost)
  params <- adjust_costs_for_fuel(params)

  with(params, {
    # 1. Energy Output
    energy_output <- bm_lhv * bes_energy_efficiency
    gj_to_mwh_conv <- if (!is.null(params$gj_to_mwh)) gj_to_mwh else 0.277778
    energy_prod <- energy_output * gj_to_mwh_conv # MWh / Mg biomass

    # 2. Plant Costs (CAPEX/OPEX)
    if (!is.null(params$plant_mw_th)) {
      plant_mw_th <- resolve_plant_mw_th(params$plant_mw_th, "BES")
      plant_mw <- plant_mw_th * bes_energy_efficiency
    } else {
      plant_mw <- if (!is.null(params$plant_mw)) params$plant_mw else 50
      plant_mw_th <- plant_mw / bes_energy_efficiency
    }

    capacity_factor_val <- if (!is.null(params$capacity_factor)) capacity_factor else 0.85
    annual_biomass <- (plant_mw_th * 8760 * capacity_factor_val) / (bm_lhv * gj_to_mwh_conv)

    # Total Capex ($), sized on thermal input at the reference efficiency
    scaling_factor_val <- if (!is.null(params$scaling_factor)) scaling_factor else 0.7
    total_capex <- combustion_plant_capex(bes_capital_cost, plant_mw_th, bes_capex_ref_eff, scaling_factor_val)

    # Annual Capex ($/yr)
    annuity_fac <- calculate_annuity_factor(discount_rate, bes_life)
    annual_capex_payment <- total_capex / annuity_fac

    # Capex and OPEX per Mg Biomass
    capex_per_mg <- annual_capex_payment / annual_biomass
    # Annual O&M is a fraction of total CAPEX. Costs are levelised per year: discounting this constant
    # annual cost over the plant life and re-annualising at the same rate returns the annual value.
    opex_per_mg <- (total_capex * bes_om_factor) / annual_biomass

    # --- 3. Logistics Cost & Transport Emissions ---
    if (!is.null(params$avg_dist)) {
      avg_dist <- params$avg_dist
    } else {
      radius <- if (!is.null(params$collection_radius)) params$collection_radius else 50
      avg_dist <- (2 / 3) * radius
    }

    # Apply tortuosity to get actual road distance
    tort <- if (!is.null(params$tortuosity)) params$tortuosity else 1.3
    effective_dist <- avg_dist * tort

    tf <- if (!is.null(params$bm_transport_fixed)) params$bm_transport_fixed else 5.0
    tv <- if (!is.null(params$bm_transport_var)) params$bm_transport_var else 0.15
    logistics_cost <- tf + (tv * effective_dist)

    # Calculate Scope 3 Transport Emissions (Default: 0.0001 Mg CO2e / Mg-km for heavy diesel truck)
    trans_em_factor <- if (!is.null(params$transport_emissions_factor)) params$transport_emissions_factor else 0.0001
    transport_emissions_co2e <- effective_dist * trans_em_factor

    feedstock_cost <- if (!is.null(params$feedstock_cost)) params$feedstock_cost else 0
    total_cost <- capex_per_mg + opex_per_mg + logistics_cost + feedstock_cost

    # 4. Revenue & Value
    energy_revenue <- energy_prod * elec_price

    # Carbon Abatement (No Sequestration, only displacement minus transport penalty)
    c_displaced <- energy_output * ff_c_intensity
    tot_c_abatement <- c_displaced - transport_emissions_co2e
    abatement_value <- tot_c_abatement * c_price

    total_revenue <- energy_revenue + abatement_value
    net_value <- total_revenue - total_cost

    # Added diagnostics for factorial
    biomass_cost <- feedstock_cost + logistics_cost
    lcoe <- (capex_per_mg + opex_per_mg + biomass_cost) / energy_prod
    cost_of_co2_avoided <- ifelse_raster(tot_c_abatement > 0, total_cost / tot_c_abatement, Inf)
    abatement_efficiency <- 0 # No gross sequestration for BES
    total_capex_m <- total_capex / 1e6

    list(
      technology = "BES",
      energy_output = energy_output,
      energy_prod = energy_prod,
      c_sequestered = 0,
      tot_c_abatement = tot_c_abatement,
      total_cost = total_cost,
      total_revenue = total_revenue,
      net_value = net_value,
      # Granular outputs
      capital_cost_mg = capex_per_mg,
      om_cost_mg = opex_per_mg,
      biomass_cost_mg = biomass_cost,
      co2_transport_cost_mg = 0,
      co2_transport_distance_km = NA,
      biomass_transport_distance_km = effective_dist,
      energy_revenue_mg = energy_revenue,
      abatement_revenue_mg = abatement_value,
      agronomic_revenue_mg = NA,
      lcoe = lcoe,
      cost_of_co2_avoided = cost_of_co2_avoided,
      abatement_efficiency = abatement_efficiency,
      total_capex_m = total_capex_m
    )
  })
}

#' Total CAPEX of a Combustion Power Plant
#'
#' Sizes the plant for costing on its thermal input at a fixed reference net efficiency, so that
#' efficiency changes (sampled efficiency, ash penalty, CCS energy penalty) alter output but not
#' equipment cost. Costs scale from a 50 MWe reference plant.
#'
#' @param capital_cost Specific CAPEX ($/kWe net) quoted at `ref_eff`.
#' @param plant_mw_th Thermal input capacity (MWth).
#' @param ref_eff Net electrical efficiency at which `capital_cost` is quoted.
#' @param scaling_factor Capital cost scale exponent.
#' @return Total CAPEX ($).
#' @keywords internal
combustion_plant_capex <- function(capital_cost, plant_mw_th, ref_eff, scaling_factor = 0.7) {
  ref_mw <- plant_mw_th * ref_eff
  capital_cost * 50 * 1000 * (ref_mw / 50)^scaling_factor # 1000 converts MW to kW
}
