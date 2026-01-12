#' Calculate CO2 Transport Costs (Pipeline vs. Shipping)
#'
#' Implements the technoeconomic cost functions from the "Global Geologic Carbon Storage Assessment".
#' Uses a hybrid routing algorithm: Pipelines for short distance (<1000km), Shipping for long distance.
#'
#' @param mass_flow_mtpa Numeric. Annual CO2 mass flow in Million Tonnes Per Annum (Mtpa).
#' @param distance_km Numeric. Transport distance in kilometers.
#' @param region Character. One of "North America", "Europe", "China", "India".
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
        region == "North America" ~ 1.0,
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


#' Calculate CCS Pipeline Transport Cost
#'
#' Estimates the cost of transporting CO2 via pipeline based on mass flow and distance.
#' Uses a power-law scaling model for economies of scale.
#'
#' Reference: Generally derived from Zero Emissions Platform (ZEP) and similar engineering cost models.
#' CAPEX propto Distance * Capacity^0.5 (Diameter scaling).
#' But typically Cost/t propto Capacity^-0.4 to -0.6.
#'
#' @param co2_mass Annual CO2 mass to transport (Mg/year).
#' @param distance Transport distance (km).
#' @param discount_rate Discount rate (decimal). Default 0.10.
#' @param lifetime Project lifetime (years). Default 20.
#' @return Transport cost ($/Mg CO2).
#' @export
calculate_ccs_transport <- function(co2_mass, distance, discount_rate = 0.10, lifetime = 20) {
    if (co2_mass <= 0) {
        return(0)
    }

    # --- Cost Model Assumptions (2024 USD) ---

    # Reference Project (Medium/Large Scale)
    # Ref: 1 Mt/y (1,000,000 Mg/y), 100 km
    ref_mass <- 1000000
    ref_dist <- 100

    # Reference LCOT (Levelized Cost of Transport) for 1Mt, 100km
    # Literature: ~10-15 $/t for this scale/distance.
    # Pipelines are expensive at small scale.
    # Let's set a base cost parameter derived from ZEP:
    # Base CAPEX for 100km, 1Mt/y ~ $50M - $80M -> Annual $8M -> $8/t
    # + OPEX. Total ~ $10-12/t.
    ref_cost_per_ton_km <- 0.12 # $0.12 / t / km at reference scale? No, transport is cheaper per km at long dist.

    # Construction Cost Formula:
    # Investment = A * Distance * (Capacity)^b
    # A: Base cost coefficient
    # b: Scale factor (typically 0.5 - 0.6 for diameter/capacity)

    # Let's use a calibrated formula:
    # CAPEX ($) = Base_Capex_Per_Km * Distance * (Capacity / Ref_Capacity)^0.6 / Ref_Capacity ??
    # No, Capacity^0.6 is total cost.

    # ZEP/NETL simple approximation:
    # CAPEX_ref (100km, 1Mt) = $50,000,000
    base_capex_ref <- 50000000
    scale_factor <- 0.6 # Economies of scale exponent

    # Calculate CAPEX for specific project
    scaler <- (co2_mass / ref_mass)^scale_factor
    capex <- base_capex_ref * (distance / ref_dist) * scaler

    # Annualize CAPEX
    annuity_fac <- (1 - (1 + discount_rate)^(-lifetime)) / discount_rate
    annual_capex <- capex / annuity_fac

    # OPEX (Maintenance, Monitoring, Booster Energy)
    # Typically 3-5% of CAPEX + Energy
    # Energy: ~10 kWh/t/100km? (Liquid pumping is low energy)
    # Let's assume flat rate % of CAPEX for O&M + Energy
    opex_factor <- 0.05
    annual_opex <- capex * opex_factor

    # Total Annual Cost
    total_annual_cost <- annual_capex + annual_opex

    # Cost per Tonne
    cost_per_ton <- total_annual_cost / co2_mass

    return(cost_per_ton)
}
