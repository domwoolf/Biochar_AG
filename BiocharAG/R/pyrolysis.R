#' Calculate Pyrolysis Yields and Energy Balance (Woolf et al. 2016)
#'
#' Mass and energy balance following the BEBCS sheet of nets1.xlsm. Char and gas yields and
#' char composition are empirical functions of temperature; bio-oil, CO2 and H2O yields close
#' the elemental (C, H, O) balance.
#'
#' All quantities are per Mg of dry, ash-free (daf) feed, the same basis on which `bm_lhv` is
#' defined and on which BES/BECCS convert feed to energy. Moisture and ash accompany each Mg daf
#' in proportion to `moisture` (wet basis) and `ash` (dry basis).
#'
#' The energy balance is on a consistent HHV basis: process heat demand = explicit heat losses
#' + HHV of products - HHV of feed. The loss term for water vapour includes the latent heat of
#' both feed moisture and reaction water, which is what the LHV/HHV difference would otherwise
#' represent. Process heat (at `heater_eff`) and parasitic electricity are supplied from the
#' pyrolysis fuel, extra biomass, or biochar according to `e_source`. The net fuel available
#' for energy export is returned on an LHV basis for use with LHV conversion efficiencies.
#'
#' @param py_temp Pyrolysis temperature (Celsius).
#' @param lignin Lignin fraction of biomass.
#' @param bm_lhv Feed LHV (GJ/Mg daf).
#' @param moisture Feed moisture content (wet basis fraction).
#' @param ash Feed ash content (dry basis fraction).
#' @param feed_c,feed_h Feed C and H mass fractions (daf).
#' @param feed_o Feed O mass fraction (daf). Defaults to the balance `1 - feed_c - feed_h`.
#' @param feed_rate_kg_hr Plant feed rate (kg daf / hr); sizes the reactor for wall heat loss.
#' @param heater_eff Efficiency of the process heater (HHV basis).
#' @param parasitic_power Parasitic electricity demand (GJe / Mg daf feed).
#' @param power_eff Electrical efficiency (LHV basis) used to supply parasitic power from fuel.
#' @param exhaust_temp Temperature at which vapours and gases leave heat recovery (Celsius).
#' @param e_source Process energy source: "fuel" (pyrolysis gas and vapours; any shortfall is met
#'   by burning biochar), "biomass" (additional feed burned; results are per Mg of total feed) or
#'   "biochar" (biochar burned; all fuel exported).
#' @return A list with yields, char properties, heat losses and net energy flows (GJ / Mg daf feed).
#' @export
calculate_pyrolysis_physics <- function(py_temp, lignin, bm_lhv, moisture = 0.1, ash = 0.05,
                                        feed_c = 0.50, feed_h = 0.06, feed_o = 1 - feed_c - feed_h,
                                        feed_rate_kg_hr = 250, heater_eff = 0.8, parasitic_power = 0.07,
                                        power_eff = 0.35, exhaust_temp = 170, e_source = "fuel") {
    T_k <- py_temp + 273

    # --- 1. Biochar yield & composition (daf) ---
    yield_bc <- 0.1260917 + 0.27332 * lignin + 0.5391409 * exp(-0.004 * py_temp)
    bc_c <- 0.99 - 0.78 * exp(-0.0042 * py_temp)
    bc_h <- -0.0041 + 0.1 * exp(-0.0024 * py_temp)
    bc_o <- 1 - bc_c - bc_h

    # --- 2. Permanent gas yields ---
    y_h2 <- 0.029528 * (1 - exp(-0.003496 * T_k))^62.980403
    y_co <- 0.043 / (1 + exp(-0.03 * T_k + 17.2)) + (0.36 - 0.043) / (1 + exp(-0.01 * T_k + 10.9))
    y_ch4 <- 0.07818 * (1 - exp(-0.0033788 * T_k))^30.14865
    y_c2h4 <- 0.035637 * (1 - exp(-0.005221 * T_k))^154.974

    # --- 3. Elements unaccounted for by char and permanent gases ---
    uc <- feed_c - yield_bc * bc_c - y_co * 12 / 28 - y_ch4 * 12 / 16 - y_c2h4 * 24 / 28
    uh <- feed_h - yield_bc * bc_h - y_h2 - y_ch4 * 4 / 16 - y_c2h4 * 4 / 28
    uo <- feed_o - yield_bc * bc_o - y_co * 16 / 28

    # --- 4. Close the balance with bio-oil, CO2 and H2O ---
    # Bio-oil composition scales with the feed
    bo_c <- 1.25 * feed_c
    bo_h <- 1.26 * feed_h
    bo_o <- 1 - bo_c - bo_h
    yield_bo <- (uo - uc * 32 / 12 - uh * 16 / 2) / (bo_o - bo_c * 32 / 12 - bo_h * 16 / 2)
    yield_co2 <- (uc - bo_c * yield_bo) * 44 / 12
    yield_h2o_rxn <- (uh - bo_h * yield_bo) * 18 / 2

    yield_bc <- pmax(0, yield_bc)
    yield_bo <- pmax(0, yield_bo)
    yield_co2 <- pmax(0, yield_co2)
    yield_gas <- y_h2 + y_co + yield_co2 + y_ch4 + y_c2h4

    # Water leaving as vapour: feed moisture plus reaction water
    moisture_daf <- moisture / ((1 - moisture) * (1 - ash))
    water_total <- moisture_daf + yield_h2o_rxn

    # --- 5. Heating values (HHV, GJ/Mg) ---
    h2o_latent <- 2.442 # GJ/Mg water at 25 C
    h2o_per_h <- 18.015 / 2.016 # Mg water formed per Mg H combusted
    feed_hhv <- bm_lhv + h2o_latent * h2o_per_h * feed_h
    bc_hhv <- 100 * (0.3491 * bc_c + 1.1783 * bc_h - 0.1034 * bc_o)
    bo_hhv <- 100 * (0.3491 * bo_c + 1.1783 * bo_h - 0.1034 * bo_o)
    e_bc <- bc_hhv * yield_bc
    e_bo <- bo_hhv * yield_bo
    e_gas <- y_h2 * 141.8 + y_ch4 * 55.5 + y_co * 10.1 + y_c2h4 * 50.33
    e_fuel <- e_gas + e_bo

    # LHV of the fuel stream (for power generation at an LHV efficiency)
    h_fuel <- y_h2 + y_ch4 * 4 / 16 + y_c2h4 * 4 / 28 + yield_bo * bo_h
    fuel_lhv_ratio <- (e_fuel - h2o_latent * h2o_per_h * h_fuel) / e_fuel

    # --- 6. Heat losses (GJ/Mg daf feed) ---
    # Reactor wall: insulated cylinder sized from feed rate and residence time
    feed_rate_wet <- feed_rate_kg_hr / ((1 - ash) * (1 - moisture))
    residence_hr <- ifelse(py_temp > 340, 654 * (py_temp - 332)^-0.56 / 60, 12)
    vessel_vol <- feed_rate_wet * residence_hr / 250 # feed bulk density 250 kg/m3
    aspect <- 20
    radius <- (vessel_vol / (pi * aspect))^(1 / 3)
    wall_thk <- 0.025
    insul_thk <- 0.05
    area <- 2 * pi * radius * (aspect * radius) + 2 * pi * (radius + insul_thk + wall_thk)^2
    wall_kw <- 0.123 * (py_temp - 40) * area / (insul_thk * 1000) # CaSiO insulation, 40 C outer surface
    loss_wall <- wall_kw * 3.6 / feed_rate_kg_hr

    # Sensible heat of char discharged at reactor temperature (cp 8.5 J/mol K)
    loss_bc <- (8.5 / 12) * (py_temp - 20) * yield_bc / 1000
    # Bio-oil vapour leaving heat recovery (latent + sensible)
    loss_bo <- yield_bo * (1.22 + (exhaust_temp - 20) * 0.002)
    # Permanent gases (cp 30 J/mol K at the mixture's molar mass)
    gas_molar_mass <- yield_gas / (y_h2 / 2.016 + y_co / 28.01 + yield_co2 / 44.01 + y_ch4 / 16.04 + y_c2h4 / 28.05)
    loss_gas <- (30 / gas_molar_mass) * (exhaust_temp - 20) * yield_gas / 1000
    # Water vapour (enthalpy of steam relative to liquid water)
    loss_h2o <- (2.676 + 0.0021 * (exhaust_temp - 100)) * water_total
    heat_losses <- loss_wall + loss_bc + loss_bo + loss_gas + loss_h2o

    # --- 7. Process energy supply ---
    heat_supply <- heat_losses + e_bc + e_fuel - feed_hhv
    fuel_required <- pmax(0, heat_supply) / heater_eff + parasitic_power / (power_eff * fuel_lhv_ratio)

    if (e_source == "fuel") {
        deficit <- pmax(0, fuel_required - e_fuel)
        net_fuel <- pmax(0, e_fuel - fuel_required)
        yield_bc_net <- yield_bc * pmax(0, 1 - deficit / e_bc)
    } else if (e_source == "biomass") {
        scale <- 1 / (1 + fuel_required / feed_hhv)
        net_fuel <- e_fuel * scale
        yield_bc_net <- yield_bc * scale
    } else if (e_source == "biochar") {
        net_fuel <- e_fuel
        yield_bc_net <- yield_bc * pmax(0, 1 - fuel_required / e_bc)
    } else {
        stop("e_source must be 'fuel', 'biomass' or 'biochar'.")
    }

    ash_daf <- ash / (1 - ash)
    mass_bc <- yield_bc_net + ash_daf # Ash reports to char

    list(
        yield_bc = mass_bc, # Mg char (incl. ash) / Mg daf feed
        yield_bc_daf = yield_bc_net,
        bc_c_content = bc_c, # C fraction of the organic char
        bc_c_content_final = yield_bc_net * bc_c / mass_bc, # C fraction of char incl. ash
        bc_c_yield = yield_bc_net * bc_c, # Mg biochar C / Mg daf feed
        bc_h_c_molar = (bc_h / 1.008) / (bc_c / 12.011), # Organic H:C molar ratio (permanence proxy)
        yield_bo = yield_bo,
        yield_gas = yield_gas,
        yield_h2o = water_total,
        heat_losses = c(wall = loss_wall, biochar = loss_bc, biooil = loss_bo, gas = loss_gas, h2o = loss_h2o),
        heat_supply = heat_supply,
        fuel_required = fuel_required,
        energy_net = net_fuel * fuel_lhv_ratio, # Net fuel for export, LHV basis
        energy_char = yield_bc_net * bc_hhv
    )
}
