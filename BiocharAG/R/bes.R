#' Calculate Bioenergy System (BES) Metrics
#'
#' Modernizated logic (2024 Basis):
#' - Uses modernized Capital Cost ($3,000/kW) and Efficiency (30%) defaults.
#' - Calculates Levelized Cost of Electricity components (CAPEX/OPEX).
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

  # Apply Fuel Quality Penalties (High Ash -> Higher Cost)
  params <- adjust_costs_for_fuel(params)

  with(params, {
    # 1. Energy Output
    energy_output <- bm_lhv * bes_energy_efficiency
    elec_prod <- energy_output * 0.277778 # MWh / Mg biomass

    # 2. Plant Costs (CAPEX/OPEX)
    # Scale calculation methodology identical to BECCS for consistency
    plant_mw <- if (!is.null(params$plant_mw)) params$plant_mw else 50
    capacity_factor <- 0.85
    annual_biomass <- (plant_mw * 8760 * capacity_factor) / elec_prod

    # Total Capex ($)
    # Apply Scaling Factor (Economies of Scale)
    # Reference: 50 MW
    # Scaling Factor: 0.7 (typical for thermal plants)
    scaling_factor <- 0.7
    base_cost <- bes_capital_cost * 50 * 1000 # Cost of 50 MW plant

    total_capex <- base_cost * ((plant_mw / 50)^scaling_factor)

    # Annual Capex ($/yr)
    annuity_fac <- calculate_annuity_factor(discount_rate, bes_life)
    annual_capex_payment <- total_capex / annuity_fac

    # Capex per Mg Biomass
    capex_per_mg <- annual_capex_payment / annual_biomass

    # OPEX ($/Mg)
    opex_per_mg <- capex_per_mg * bes_om_factor

    # Logistics Cost (Biomass Transport)
    # Default radius 50km if not provided
    radius <- if (!is.null(params$collection_radius)) params$collection_radius else 50
    avg_dist <- (2 / 3) * radius

    # Defaults in case params missing
    tf <- if (!is.null(params$bm_transport_fixed)) params$bm_transport_fixed else 5.0
    tv <- if (!is.null(params$bm_transport_var)) params$bm_transport_var else 0.15

    logistics_cost <- tf + (tv * avg_dist)

    total_cost <- capex_per_mg + opex_per_mg + logistics_cost

    # 3. Revenue & Value
    elec_revenue <- elec_prod * elec_price

    # Carbon Abatement (No Sequestration, only displacement)
    c_sequestered <- 0
    c_displaced <- energy_output * ff_c_intensity
    tot_c_abatement <- c_sequestered + c_displaced
    abatement_value <- tot_c_abatement * c_price

    total_revenue <- elec_revenue + abatement_value
    net_value <- total_revenue - total_cost

    list(
      technology = "BES",
      energy_output = energy_output,
      elec_prod = elec_prod,
      c_sequestered = c_sequestered,
      tot_c_abatement = tot_c_abatement,
      total_cost = total_cost,
      total_revenue = total_revenue,
      net_value = net_value
    )
  })
}
