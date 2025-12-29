#' Calculate Pyrolysis Yields and Energy Balance (Woolf et al. 2016)
#'
#' Implements the sophisticated mass and energy balance from op_space_2.41.xlsm.
#' Mass balance is constrained by elemental conservation (C, H, O).
#' Bio-oil, CO2, and H2O yields are solved to close the balance.
#'
#' @param py_temp Pyrolysis temperature (Celsius).
#' @param lignin Lignin fraction of biomass.
#' @param bm_lhv Biomass LHV (GJ/Mg).
#' @param moisture Biomass moisture fraction.
#' @param ash Biomass ash fraction.
#' @param feed_c Feedstock Carbon fraction (DAF). Default 0.50.
#' @param feed_h Feedstock Hydrogen fraction (DAF). Default 0.06.
#' @param feed_o Feedstock Oxygen fraction (DAF). Default 0.44.
#' @return A list containing yields, HHVs, and net energy flux.
#' @export
calculate_pyrolysis_physics <- function(py_temp, lignin, bm_lhv, moisture = 0.1, ash = 0.05,
                                        feed_c = 0.50, feed_h = 0.06, feed_o = 0.44) {
    T_k <- py_temp + 273.15

    # --- 1. Biochar Yield & Composition ---
    # Yield (DAF basis) - Row 4
    yield_bc <- 0.1260917 + 0.27332 * lignin + 0.5391409 * exp(-0.004 * py_temp)

    # Composition - Rows 5, 6, 7
    bc_c <- 0.99 - 0.78 * exp(-0.0042 * py_temp)
    bc_h <- -0.0041 + 0.1 * exp(-0.0024 * py_temp)
    bc_o <- 1 - bc_c - bc_h

    # --- 2. Gas Yields (Explicit) ---
    # Rows 19, 20, 22, 23
    y_h2 <- 0.029528 * (1 - exp(-0.003496 * T_k))^62.980403
    y_co <- (0.043) / (1 + exp(-0.03 * T_k + 17.2)) + (0.36 - 0.043) / (1 + exp(-0.01 * T_k + 10.9))
    y_ch4 <- 0.07818 * (1 - exp(-0.0033788 * T_k))^30.14865
    y_c2h4 <- 0.035637 * (1 - exp(-0.005221 * T_k))^154.974

    # --- 3. Unaccounted Element Balance (Uc, Uh, Uo) ---
    # "Available" for Oil, CO2, H2O

    # Molar masses
    mm_c <- 12
    mm_h <- 1
    mm_o <- 16
    mm_co <- 28
    mm_ch4 <- 16
    mm_c2h4 <- 28
    mm_co2 <- 44
    mm_h2o <- 18

    # Uc (Row 26) = FeedC - CharC - CO_C - CH4_C - C2H4_C
    uc <- feed_c - (yield_bc * bc_c) -
        (y_co * 12 / 28) - (y_ch4 * 12 / 16) - (y_c2h4 * 24 / 28)

    # Uh (Row 27) = FeedH - CharH - H2 - CH4_H - C2H4_H
    uh <- feed_h - (yield_bc * bc_h) -
        y_h2 - (y_ch4 * 4 / 16) - (y_c2h4 * 4 / 28)

    # Uo (Row 28) = FeedO - CharO - CO_O
    uo <- feed_o - (yield_bc * bc_o) - (y_co * 16 / 28)

    # --- 4. Solve for Bio-oil, CO2, H2O ---
    # Bio-oil Composition (Fixed - Rows 11, 12, 13)
    bo_c <- 0.625
    bo_h <- 0.0756
    bo_o <- 0.2994

    # Bio-oil Yield (Row 10)
    # Y_oil = (Uo - Uc*32/12 - Uh*16/2) / (Oil_O - Oil_C*32/12 - Oil_H*16/2)
    num <- uo - uc * (32 / 12) - uh * (16 / 2)
    den <- bo_o - bo_c * (32 / 12) - bo_h * (16 / 2)
    yield_bo <- num / den

    # CO2 Yield (Row 21)
    # CO2_Y = (Uc - Oil_C * Y_oil) / (12/44)
    yield_co2 <- (uc - bo_c * yield_bo) / (12 / 44)

    # H2O Yield (Row 15 - Reaction Water)
    # H2O_Y = (Uh - Oil_H * Y_oil) / (2/18)
    yield_h2o_rxn <- (uh - bo_h * yield_bo) / (2 / 18)

    # Verify Balance
    # Check if yields are positive. If physics breaks (extrapolation), clamp?
    yield_bc <- pmax(0, yield_bc)
    yield_bo <- pmax(0, yield_bo)
    yield_co2 <- pmax(0, yield_co2)
    # yield_h2o_rxn can be close to 0

    yield_gas_total <- y_h2 + y_co + y_ch4 + y_c2h4 + yield_co2

    # --- 5. Ash & Moisture Adjustment ---
    feed_daf <- 1 - moisture - ash
    mass_bc <- yield_bc * feed_daf + ash # Ash reports to char
    mass_bo <- yield_bo * feed_daf
    mass_gas <- yield_gas_total * feed_daf
    mass_h2o <- (yield_h2o_rxn * feed_daf) + moisture

    # --- 6. Energy Balance ---

    # HHVs
    # Biochar HHV (Dulong) - Row 279
    bc_hhv <- (0.3491 * bc_c + 1.1783 * bc_h - 0.1034 * bc_o) * 100 # MJ/kg = GJ/Mg

    # Bio-oil HHV (Constant) - Row 271 ~ 27.6
    bo_hhv <- 27.63

    # Gas HHV (Weighted Sum) - Row 265
    # Heat of Comb (MJ/kg): H2=141.8, CH4=55.5, CO=10.1, C2H4=50.33
    energy_gas_components <- (y_h2 * 141.8 + y_ch4 * 55.5 + y_co * 10.1 + y_c2h4 * 50.33) # MJ/kg_feedDAF
    # Note: CO2 contributes 0 energy.

    # Total Product Energy (Enthalpy Out per kg Feed DAF)
    # We work in GJ/Mg (same as MJ/kg)

    # Enthalpy of Products (DAF basis * DAF mass)
    e_bc <- yield_bc * bc_hhv * feed_daf
    e_bo <- yield_bo * bo_hhv * feed_daf
    e_gas <- energy_gas_components * feed_daf

    # Heat Losses (Row 31)
    heat_loss <- 2.35 # GJ/Mg feed

    # Process Heat Required
    # Heat_In = H_products + Losses - H_feed
    # H_feed ~ LHV_biomass
    e_products_total <- e_bc + e_bo + e_gas
    heat_required <- e_products_total + heat_loss - bm_lhv

    # Net Energy
    # Try to meet load with Gas + Oil
    e_avail_fuel <- e_gas + e_bo

    if (e_avail_fuel >= heat_required) {
        e_net_fuel <- e_avail_fuel - heat_required
        e_net_bc <- e_bc # All char conserved
    } else {
        e_net_fuel <- 0
        deficit <- heat_required - e_avail_fuel
        # Burn char to meet deficit?
        # Usually avoided, but consistent physics requires checks.
        # Assuming deficit met by char or external.
        e_net_bc <- max(0, e_bc - deficit)
    }

    list(
        yield_bc = mass_bc, # As received mass fraction
        bc_c_content = bc_c, # C fraction of the organic part (simplified)
        # Note: bc_c_content usually refers to C fraction of the final char (including ash).
        # Recalculate C content of final char:
        # Mass C = yield_bc * bc_c * feed_daf
        # Mass Char = mass_bc
        # Final C% = (yield_bc * bc_c * feed_daf) / mass_bc
        bc_c_content_final = (yield_bc * bc_c * feed_daf) / mass_bc,
        energy_net = e_net_fuel,
        energy_char = e_net_bc
    )
}
