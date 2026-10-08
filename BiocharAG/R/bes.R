#' Calculate Bioenergy System (BES) Metrics
#'
#' Modernized logic (2024 Basis):
#' - Uses modernized Capital Cost ($3,000/kW) and Efficiency (30%) defaults.
#' - Calculates Levelized Cost of Electricity components (CAPEX/OPEX).
#' - Explicitly tracks Scope 3 transport emissions and road tortuosity.
#'
#' BES is evaluated in two operating modes within the same run, and each cell takes the better one:
#' base load (`capacity_factor_bes_base`, price `elec_price * bes_base_price_capture`) and flexible,
#' load-following operation (`capacity_factor_bes_flex`, price `elec_price * bes_flex_price_capture`).
#' The capture ratios (realised / time-averaged price) come from hourly day-ahead prices
#' (data-raw/elec_prices/price_capture.R). Abatement per Mg is the same
#' in both modes apart from small differences in haulage emissions, so the mode is chosen on net value
#' excluding carbon revenue; the choice is then independent of the carbon price, which keeps
#' `run_price_sweep()` exact. The allocation of biomass between modes also depends on the fleet mix and
#' demand dynamics of each grid, which are not modelled.
#'
#' @param params A list of parameters.
#' @return A list of calculated metrics for BES, with `bes_mode` (1 = base load, 2 = flexible).
#' @export
calculate_bes <- function(params) {
  run_mode <- function(tech, price_mult) {
    p <- params
    p$capacity_factor_bes <- tech_capacity_factor(params, tech)
    d <- params[[paste0("avg_dist_", tech), exact = TRUE]]
    if (!is.null(d)) p$avg_dist_BES <- d
    if (!is.null(p$elec_price)) p$elec_price <- p$elec_price * price_mult
    calculate_bes_mode(p)
  }
  base <- run_mode("BES_BASE", if (!is.null(params$bes_base_price_capture)) params$bes_base_price_capture else 1.05)
  flex <- run_mode("BES_FLEX", if (!is.null(params$bes_flex_price_capture)) params$bes_flex_price_capture else 1.30)
  n0 <- function(r) r$net_value - r$abatement_revenue_mg
  use_flex <- n0(flex) > n0(base)
  out <- base
  for (nm in names(base)) {
    b <- base[[nm]]
    f <- flex[[nm]]
    if (is.character(b) || (length(b) == 1 && !inherits(b, "SpatRaster") && identical(b, f))) next
    out[[nm]] <- ifelse_raster(use_flex, f, b)
  }
  out$bes_mode <- ifelse_raster(use_flex, 2, 1)
  out
}

#' BES Metrics for One Operating Mode
#'
#' @param params A list of parameters (`capacity_factor_bes`, `avg_dist_BES` and `elec_price` set for
#'   the mode by `calculate_bes()`).
#' @return A list of calculated metrics for BES.
#' @keywords internal
calculate_bes_mode <- function(params) {
  # Default to modern params if not present
  if (is.null(params$bes_capital_cost)) params$bes_capital_cost <- 4700
  if (is.null(params$bes_energy_efficiency)) params$bes_energy_efficiency <- 0.30
  if (is.null(params$bes_life)) params$bes_life <- 25
  if (is.null(params$bes_capex_ref_eff)) params$bes_capex_ref_eff <- 0.30

  # Apply Fuel Quality Penalties (High Ash -> Higher Cost)
  params <- adjust_costs_for_fuel(params)
  params$ff_c_intensity <- displaced_grid_ci(params) # MEF(P): displaced grid intensity at this carbon price

  # Collection distance at this technology's capacity factor (attach_size_layers)
  if (!is.null(params[["avg_dist_BES", exact = TRUE]])) params$avg_dist <- params[["avg_dist_BES", exact = TRUE]]

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

    capacity_factor_val <- tech_capacity_factor(params, "BES")
    annual_biomass <- (plant_mw_th * 8760 * capacity_factor_val) / (bm_lhv * gj_to_mwh_conv)

    # Total Capex ($), sized on thermal input at the reference efficiency
    scaling_factor_val <- if (!is.null(params$scaling_factor)) scaling_factor else 0.7
    # bes_capital_cost is a local (regional) value, so the CAPEX location factor is not applied
    total_capex <- combustion_plant_capex(bes_capital_cost, plant_mw_th, bes_capex_ref_eff, scaling_factor_val)

    # Annual Capex ($/yr)
    annuity_fac <- calculate_annuity_factor(discount_rate, bes_life)
    annual_capex_payment <- total_capex / annuity_fac

    # Capex and OPEX per Mg Biomass
    capex_per_mg <- annual_capex_payment / annual_biomass
    # Annual O&M is a fraction of total CAPEX. Costs are levelised per year: discounting this constant
    # annual cost over the plant life and re-annualising at the same rate returns the annual value.
    opex_per_mg <- (total_capex * plant_om_fraction(params, "bes_om_factor") * location_factor(params, "om")) / annual_biomass

    # --- 3. Logistics Cost & Transport Emissions ---
    logistics <- biomass_logistics(params)
    effective_dist <- logistics$effective_dist
    # Bottom ash returns to the fields as a backhaul, handled and spread like biochar (ash_return())
    ash_ret <- ash_return(params)
    logistics_cost <- logistics$cost + ash_ret$cost
    transport_emissions_co2e <- logistics$emissions + ash_ret$emissions

    feedstock_cost <- if (!is.null(params$feedstock_cost)) params$feedstock_cost else 0
    removal_charge <- residue_removal_charge(params) # nutrients and alkalinity removed with the residue
    total_cost <- capex_per_mg + opex_per_mg + logistics_cost + feedstock_cost + removal_charge

    # 4. Revenue & Value
    energy_revenue <- energy_prod * elec_price

    # Carbon Abatement (No Sequestration, only displacement minus transport penalty)
    c_displaced <- energy_output * ff_c_intensity
    tot_c_abatement <- c_displaced - transport_emissions_co2e + residue_counterfactual_ghg(params)
    abatement_value <- tot_c_abatement * c_price

    ash_value <- calculate_ash_value(params) # Recycled bottom ash (lime, P and K)
    total_revenue <- energy_revenue + ash_value + abatement_value
    net_value <- total_revenue - total_cost

    # Added diagnostics for factorial
    biomass_cost <- feedstock_cost + logistics_cost
    lcoe <- (capex_per_mg + opex_per_mg + biomass_cost - ash_value) / energy_prod
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
      ash_return_cost_mg = ash_ret$cost, # included in biomass_cost_mg
      removal_charge_mg = removal_charge, # included in total_cost
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
