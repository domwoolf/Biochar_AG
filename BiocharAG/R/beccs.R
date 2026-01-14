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
  # Determine CCS Distance
  # Priority: 1. Spatial Raster (dist_sink_km) if passed
  #           2. Explicit params$ccs_distance (if not the default placeholder or if meant to override)
  #           3. Nearest Sink Calculation (if Lat/Lon provided)
  #           4. Default (100 km)

  # Check for EOR Toggle
  allow_eor <- if (!is.null(params$allow_eor)) as.logical(params$allow_eor) else TRUE

  # Check for Spatial Inputs
  dist_spatial <- NULL
  if (allow_eor) {
    if (!is.null(params$dist_sink_km)) dist_spatial <- params$dist_sink_km
  } else {
    if (!is.null(params$dist_sink_saline_km)) dist_spatial <- params$dist_sink_saline_km
  }

  if (!is.null(dist_spatial)) {
    # Spatial Input Wins
    params$ccs_distance <- dist_spatial
  } else if (is.null(params$ccs_distance)) {
    # No spatial input AND no manual distance -> Calc from Lat/Lon or Default
    if (!is.null(params$lat) && !is.null(params$lon)) {
      geo <- find_nearest_sink(params$lat, params$lon)
      params$ccs_distance <- geo$distance_km
    } else {
      params$ccs_distance <- 100
    }
  }
  # Else: params$ccs_distance exists and no spatial input -> Use it (Default behavior is 100)
  # WARNING: default_parameters() sets ccs_distance = 100.
  # If spatial inputs ARE provided (dist_spatial != NULL), we overwrite it above.
  # This works.
  if (is.null(params$ccs_storage_cost)) params$ccs_storage_cost <- 15
  if (is.null(params$beccs_capital_cost)) params$beccs_capital_cost <- 4000

  # Apply Fuel Quality Penalties (High Ash -> Higher Cost)
  params <- adjust_costs_for_fuel(params)

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

    # Scale calculation
    # Allow user to specify plant_mw (e.g. from spatial analysis of feedstock availability)
    plant_mw <- if (!is.null(params$plant_mw)) params$plant_mw else 50

    # Annual Biomass = (MW * 8760 * CapacityFactor) / (MWh/Mg)
    # Elec_Prod (MWh/Mg)
    capacity_factor <- 0.85
    annual_biomass <- (plant_mw * 8760 * capacity_factor) / elec_prod

    annual_co2_total <- annual_biomass * co2_captured

    # Explicit Transport Cost ($/Mg CO2)
    # Extract is_offshore from params (populated by run_spatial_tea)
    is_offshore <- if (!is.null(params$sink_is_offshore)) params$sink_is_offshore else FALSE

    transport_cost_per_ton <- calculate_ccs_transport(
      co2_mass = annual_co2_total,
      distance = ccs_distance,
      is_offshore = is_offshore,
      discount_rate = discount_rate,
      lifetime = bes_life
    )

    # Total T&S Cost ($/Mg Biomass)
    # (Transport + Storage) * CO2_Captured
    ts_cost <- (transport_cost_per_ton + ccs_storage_cost) * co2_captured

    # 4. Plant Costs (CAPEX/OPEX)
    # Scale calculation methodology identical to BECCS for consistency
    # Capital Cost parameter is usually $/kW (installed capacity).
    # Convert $/kW to $/Mg Biomass/yr ??
    # Or calculate Annual Capex Payment per Mg Biomass processed.

    # Total Capex ($)
    # Apply Scaling Factor (Economies of Scale)
    scaling_factor <- 0.7
    base_cost_beccs <- beccs_capital_cost * 50 * 1000

    total_capex <- base_cost_beccs * ((plant_mw / 50)^scaling_factor)

    # Annual Capex ($/yr)
    annuity_fac <- calculate_annuity_factor(discount_rate, bes_life)
    annual_capex_payment <- total_capex / annuity_fac

    # Capex per Mg Biomass
    capex_per_mg <- annual_capex_payment / annual_biomass

    # OPEX ($/Mg)
    # Fixed % of Capex (converted to per Mg) + Variable?
    # Simplified: % of Capex per Mg
    opex_per_mg <- capex_per_mg * 0.05 # 5% O&M

    # Logistics Cost (Biomass Transport)
    radius <- if (!is.null(params$collection_radius)) params$collection_radius else 50
    avg_dist <- (2 / 3) * radius
    tf <- if (!is.null(params$bm_transport_fixed)) params$bm_transport_fixed else 5.0
    tv <- if (!is.null(params$bm_transport_var)) params$bm_transport_var else 0.15
    logistics_cost <- tf + (tv * avg_dist)

    total_cost <- capex_per_mg + opex_per_mg + ts_cost + logistics_cost

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
