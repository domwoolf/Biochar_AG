#' Calculate CO2 Transport Costs (Pipeline vs. Shipping)
#'
#' Implements the technoeconomic cost functions from the "Global Geologic Carbon Storage Assessment".
#' Uses a hybrid routing algorithm: Pipelines for short distance (<1000km), Shipping for long distance.
#'
#' @param mass_flow_mtpa Numeric. Annual CO2 mass flow in Million Tonnes Per Annum (Mtpa).
#' @param distance_km Numeric. Transport distance in kilometers.
#' @param region Character. One of "US", "Europe", "China", "India".
#' @param is_offshore Logical. If TRUE, applies offshore multipliers to pipeline costs.
#' @param force_mode Character (optional). "pipeline" or "shipping" to override the optimization logic.
#'
#' @return A list containing Total Cost ($/tonne), CAPEX, OPEX, and selected Mode.
#' @export
calc_transport_cost <- function(mass_flow_mtpa, distance_km, region, is_offshore = FALSE, force_mode = NULL) {
  # --- 1. Constants & Physics [cite: 223, 224, 225] ---
  CO2_DENSITY_KG_M3 <- 800 # Dense phase density
  VELOCITY_M_S <- 2.0 # Economic velocity (1.5 - 3.0 m/s)
  PIPELINE_AVAIL <- 0.95 # Availability factor

  # Convert Mass Flow to kg/s
  # 1 Mtpa = 1e9 kg / (365 * 24 * 3600) seconds
  mass_flow_kgs <- (mass_flow_mtpa * 1e9) / (365 * 24 * 3600)

  # --- 2. Regional Factors [cite: 281] ---
  # US = 1.0 (Base), EU = 1.2, China/India = 0.7
  reg_factor <- dplyr::case_when(
    region == "US" ~ 1.0,
    region == "Europe" ~ 1.2,
    region %in% c("China", "India") ~ 0.7,
    TRUE ~ 1.0
  )

  # --- 3. Mode Selection  ---
  # Recommendation: Pipeline if < 1000km, Shipping if > 1000km (or if offshore distance is vast)
  if (!is.null(force_mode)) {
    mode <- force_mode
  } else {
    if (distance_km > 1000) {
      mode <- "shipping"
    } else {
      mode <- "pipeline"
    }
  }

  # --- 4. Pipeline Cost Model [cite: 222, 228, 230] ---
  if (mode == "pipeline") {
    # A. Hydraulic Design: Calculate Internal Diameter (meters)
    # Area = Flow / (Density * Velocity) -> D = sqrt(4*Area/pi)
    area_m2 <- mass_flow_kgs / (CO2_DENSITY_KG_M3 * VELOCITY_M_S)
    diameter_m <- sqrt((4 * area_m2) / pi)

    # B. CAPEX Calculation (Euro base converted to USD approx 1.1x)
    # Formula: I_pipe (EUR) = (2157 * D_m + 18) * Length_m
    # We convert L to meters
    length_m <- distance_km * 1000

    base_capex_usd <- (2157 * diameter_m + 18) * length_m * 1.1

    # Apply Terrain/Offshore Multipliers
    # Offshore multiplier 1.4 - 1.7
    loc_factor <- if (is_offshore) 1.5 else 1.0

    total_capex <- base_capex_usd * reg_factor * loc_factor

    # C. OPEX Calculation
    # Fixed OPEX: 2.5% of CAPEX [cite: 239]
    opex_fixed <- 0.025 * total_capex

    # Variable OPEX (Compression): ~90 kWh/t for initial, ~7.5 kWh/t/100km for booster [cite: 241, 242]
    # Assuming electricity cost $0.06/kWh (US) to $0.15/kWh (EU). Simplified to $0.10 avg
    elec_price <- 0.10
    energy_per_tonne <- 90 + (7.5 * (distance_km / 100))
    opex_variable_annual <- energy_per_tonne * elec_price * (mass_flow_mtpa * 1e6)

    total_annual_cost <- (total_capex / 20) + opex_fixed + opex_variable_annual # 20yr depreciation
    unit_cost <- total_annual_cost / (mass_flow_mtpa * 1e6)
  } else {
    # --- 5. Shipping Cost Model [cite: 245, 246] ---
    if (mode == "shipping") {
      # A. Liquefaction Cost ($15-$25/t) [cite: 249]
      liq_cost <- 20.0

      # B. Terminal Handling ($10-$20/t) [cite: 252]
      term_cost <- 15.0

      # C. Voyage Cost ($0.02 - $0.05 / t / km) [cite: 255]
      voyage_rate <- 0.035
      voyage_cost <- voyage_rate * distance_km

      # Total Unit Cost
      # Note: Shipping has high OPEX/Variables, lower infrastructure CAPEX scaling
      unit_cost <- liq_cost + term_cost + voyage_cost

      # Apply regional labor discounts to Terminal/Liquefaction operations
      unit_cost <- unit_cost * reg_factor
    }
  }

  return(list( # lintr:ok
    mode = mode,
    unit_cost_usd_per_tonne = round(unit_cost, 2),
    details = paste0("Region: ", region, " | Dist: ", distance_km, "km")
  ))
}


#' Calculate CCS Transport Cost
#'
#' Onshore sinks are reached by pipeline. Offshore sinks are reached by ship: a pipeline leg from
#' the source to the coast, liquefaction and port terminal, and a sea voyage to the sink.
#' Pipelines use a hub-and-spoke power-law cost model (ZEP-style): CAPEX scales with distance and
#' with capacity^0.6. Distances are terrain-routed least-cost distances, so no further tortuosity
#' factor is applied.
#'
#' @param co2_mass Annual CO2 mass to transport (Mg/year).
#' @param distance Distance to the sink (km); used for onshore sinks, and as the voyage distance for
#'   offshore sinks when `dist_sea` is not supplied.
#' @param is_offshore Logical (scalar, vector or raster); TRUE for offshore sinks.
#' @param discount_rate Discount rate (decimal). Default 0.10.
#' @param lifetime Project lifetime (years). Default 20.
#' @param early_adoption Logical. If TRUE, pipelines are sized to the single facility over the entire
#'   distance (no shared trunkline). Default FALSE.
#' @param dist_coast Pipeline distance from the source to the coast (km) for ship transport. Default 0.
#' @param dist_sea Sea voyage distance from the port to the offshore sink (km). Defaults to `distance`.
#' @param capex_factor Regional CAPEX location factor (pipelines, liquefaction and terminals).
#' @param om_factor Regional O&M location factor (pipeline O&M fraction).
#' @return Transport cost ($/Mg CO2).
#' @export
calculate_ccs_transport <- function(co2_mass, distance, is_offshore = FALSE, discount_rate = 0.10, lifetime = 20,
                                    early_adoption = FALSE, dist_coast = NULL, dist_sea = NULL,
                                    capex_factor = 1, om_factor = 1) {
  safe_co2_mass <- pmax(co2_mass, 1e-6)
  annuity_fac <- (1 - (1 + discount_rate)^(-lifetime)) / discount_rate

  pipeline_cost <- function(dist) {
    ref_mass <- 1000000
    ref_dist <- 100
    base_capex_ref <- 50000000 * capex_factor
    scale_factor <- 0.6
    opex_factor <- 0.04 * om_factor
    feeder_threshold_km <- 50
    booster_threshold_km <- 700
    booster_penalty <- 2.0
    trunk_mass_flow <- pmax(safe_co2_mass, 3000000)

    # Path A: hub-and-spoke (dedicated feeder, then a share of a regional trunkline)
    capex_f <- base_capex_ref * (feeder_threshold_km / ref_dist) * (safe_co2_mass / ref_mass)^scale_factor
    scaler_t <- (trunk_mass_flow / ref_mass)^scale_factor
    capex_t_std <- base_capex_ref * ((dist - feeder_threshold_km) / ref_dist) * scaler_t
    capex_t_base <- base_capex_ref * ((booster_threshold_km - feeder_threshold_km) / ref_dist) * scaler_t
    capex_t_booster <- (base_capex_ref * booster_penalty) * ((dist - booster_threshold_km) / ref_dist) * scaler_t
    capex_t_total <- ifelse_raster(dist <= booster_threshold_km, capex_t_std, capex_t_base + capex_t_booster)
    total_capex_share_far <- capex_f + capex_t_total * (safe_co2_mass / trunk_mass_flow)

    # Path B: dedicated direct pipeline
    total_capex_share_close <- base_capex_ref * (dist / ref_dist) * (safe_co2_mass / ref_mass)^scale_factor

    total_capex_share <- ifelse_raster(
      early_adoption,
      total_capex_share_close,
      ifelse_raster(dist > feeder_threshold_km, total_capex_share_far, total_capex_share_close)
    )
    (total_capex_share / annuity_fac + total_capex_share * opex_factor) / safe_co2_mass
  }

  # Ship transport: pipeline to the coast, liquefaction and terminal, then voyage.
  # TODO (see Article/TODO.md): no dist_coast/dist_sea layers exist yet, so the inland pipeline leg is
  # zero and the voyage is priced over the full (friction-weighted) distance to the sink. Port choice
  # should minimise total pipeline + ship cost rather than use the nearest coast.
  ship_cost <- function() {
    coast <- if (is.null(dist_coast)) 0 else dist_coast
    sea <- if (is.null(dist_sea)) distance else dist_sea
    cost_liq_term <- (20.0 + 15.0) * capex_factor
    pipeline_cost(coast) + cost_liq_term + 0.035 * sea
  }

  if (isTRUE(all(is_offshore))) {
    final_cost <- ship_cost()
  } else if (isTRUE(any(is_offshore))) {
    final_cost <- ifelse_raster(is_offshore, ship_cost(), pipeline_cost(distance))
  } else {
    final_cost <- pipeline_cost(distance)
  }

  return(ifelse_raster(co2_mass <= 0, 0, final_cost))
}
