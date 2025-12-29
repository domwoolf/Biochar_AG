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
