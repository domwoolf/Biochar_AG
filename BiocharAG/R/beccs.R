#' Calculate Bioenergy Carbon Capture and Storage (BECCS) Metrics
#'
#' Modernizated logic (2024 Basis):
#' - Uses distinct BECCS efficiency (accounting for capture penalty).
#' - Explicitly calculates Transport & Storage costs based on CO2 mass and distance.
#' - Updated Capital Costs estimates.
#'
#' @param params A list of parameters.
#' @return A list of calculated metrics for BECCS.
#' @export
calculate_beccs <- function(params) {
  # Default to modern params if not present
  if (is.null(params$beccs_efficiency)) params$beccs_efficiency <- 0.28
  if (is.null(params$capture_rate)) params$capture_rate <- 0.90
  if (is.null(params$ccs_distance)) {
    if (!is.null(params$lat) && !is.null(params$lon)) {
      geo <- find_nearest_sink(params$lat, params$lon)
      params$ccs_distance <- geo$distance_km
      # Optional: Store sink name in params/results if useful
    } else {
      params$ccs_distance <- 100
    }
  }
  if (is.null(params$ccs_storage_cost)) params$ccs_storage_cost <- 15
  if (is.null(params$beccs_capital_cost)) params$beccs_capital_cost <- 4000

  with(params, {
    # 1. Energy Output
    # BECCS efficiency (e.g., 28%)
    energy_output <- bm_lhv * beccs_efficiency
    elec_prod <- energy_output * 0.277778 # MWh / Mg biomass

    # 2. Carbon Capture
    # CO2 Produced = C_content * (44/12)
    # CO2 Captured = Produced * Capture Rate
    co2_produced <- bm_c * (44 / 12)
    co2_captured <- co2_produced * capture_rate # Mg CO2 / Mg Biomass

    # 3. Transport & Storage Costs
    # Annual CO2 Flow: depend on plant capacity.
    # We operate on "Per Mg Biomass" basis, but Transport Cost scales with Total Mass.
    # Assess Total Annual Biomass Throughput to find Scale.
    # Assume: Plant Capacity (MW_e) -> Biomass Input
    # This is circular if we don't define Plant Size.
    # Let's assume a reference commercial scale plant input for scale calculation?
    # Or add 'plant_capacity_mw' to parameters?
    # Defaulting to a standard 50 MW plant scale for cost estimation if not provided.

    plant_mw <- 50
    # Annual Biomass = (MW * 8760 * CapacityFactor) / (MWh/Mg)
    # Elec_Prod (MWh/Mg)
    capacity_factor <- 0.85
    annual_biomass <- (plant_mw * 8760 * capacity_factor) / elec_prod

    annual_co2_total <- annual_biomass * co2_captured

    # Explicit Transport Cost ($/Mg CO2)
    transport_cost_per_ton <- calculate_ccs_transport(
      co2_mass = annual_co2_total,
      distance = ccs_distance,
      discount_rate = discount_rate,
      lifetime = bes_life
    )

    # Total T&S Cost ($/Mg Biomass)
    # (Transport + Storage) * CO2_Captured
    ts_cost <- (transport_cost_per_ton + ccs_storage_cost) * co2_captured

    # 4. Plant Costs (CAPEX/OPEX)
    # CAPEX ($/Mg Biomass)
    # Capital Cost parameter is usually $/kW (installed capacity).
    # Convert $/kW to $/Mg Biomass/yr ??
    # Or calculate Annual Capex Payment per Mg Biomass processed.

    # Total Capex ($) = Cost/kW * MW * 1000
    total_capex <- beccs_capital_cost * plant_mw * 1000

    # Annual Capex ($/yr)
    annuity_fac <- calculate_annuity_factor(discount_rate, bes_life)
    annual_capex_payment <- total_capex / annuity_fac

    # Capex per Mg Biomass
    capex_per_mg <- annual_capex_payment / annual_biomass

    # OPEX ($/Mg)
    # Fixed % of Capex (converted to per Mg) + Variable?
    # Simplified: % of Capex per Mg
    opex_per_mg <- capex_per_mg * 0.05 # 5% O&M

    total_cost <- capex_per_mg + opex_per_mg + ts_cost

    # 5. Revenue & Value
    elec_revenue <- elec_prod * elec_price

    # Carbon Abatement
    c_sequestered <- bm_c * capture_rate # C equivalent
    c_displaced <- energy_output * ff_c_intensity
    tot_c_abatement <- c_sequestered + c_displaced
    abatement_value <- tot_c_abatement * c_price

    total_revenue <- elec_revenue + abatement_value
    net_value <- total_revenue - total_cost

    list(
      technology = "BECCS",
      energy_output = energy_output,
      elec_prod = elec_prod,
      c_sequestered = c_sequestered,
      tot_c_abatement = tot_c_abatement,
      total_cost = total_cost,
      ts_cost = ts_cost, # Breakout for analysis
      total_revenue = total_revenue,
      net_value = net_value
    )
  })
}
