#' Calculate Biochar-Energy (BEBCS) Metrics
#'
#' @param params A list of parameters.
#' @return A list of calculated metrics for BEBCS.
#' @export
calculate_bebcs <- function(params) {
  # Ensure optional parameters exist
  h_c_org <- if (!is.null(params$h_c_org)) params$h_c_org else 0.35
  soil_temp <- if (!is.null(params$soil_temp)) params$soil_temp else 14.9

  with(params, {
    # Pyrolysis Yields (from Excel B4, B5)
    # Yield = 0.126... + ...
    # Simplified for now or implement exact if essential.
    # Excel B4: 0.1260917 + 0.27332*lignin + 0.5391409*EXP(-0.004*py_temp)
    bc_yield <- 0.1260917 + 0.27332 * lignin + 0.5391409 * exp(-0.004 * py_temp)

    # Carbon in Char (Excel B5)
    # 0.99 - 0.78 * EXP(-0.0042 * py_temp)
    bc_c_content <- 0.99 - 0.78 * exp(-0.0042 * py_temp)

    # Stable Fraction using Fperm (Woolf et al. 2021)
    bc_stability <- calculate_fperm(h_c_org, method = "HC", soil_temp = soil_temp)
    # Carbon Sequestration
    # C sequestered = bc_yield * bc_c_content * bc_stability
    c_sequestered <- bc_yield * bc_c_content * bc_stability

    # Energy Output (Syngas/Oil)
    # Assume remaining mass is volatile and converted to energy?
    # Excel F7: feed_rate logic...
    # Let's approximate Energy = (1 - bc_yield) * bm_lhv * efficiency ?
    # Excel calculates Net Energy differently.

    # Energy Output (Syngas/Oil)
    # Assume remaining mass (volatiles) is converted to energy.
    # Volatile fraction = 1 - bc_yield.
    # Energy content of volatiles approx same as biomass per kg? (Simplified).
    # Power generation efficiency assumed similar to BES (or slightly lower?).
    # We apply bes_energy_efficiency to the volatile energy fraction.

    energy_output <- bm_lhv * (1 - bc_yield) * bes_energy_efficiency

    elec_prod <- energy_output * 0.277778
    elec_revenue <- elec_prod * elec_price

    # Biochar Revenue
    bc_revenue <- bc_yield * bc_price

    # Costs
    # Pyrolysis Plant Cost (py_cc) + Power Plant Cost.
    # Power Plant Cost is scaled by capacity/throughput of volatiles?
    # bes_cc is $/Mg Biomass capacity for a dedicated plant.
    # BEBCS power unit handles (1 - bc_yield) mass.
    # Let's approximate Power Capex = bes_cc * (1 - bc_yield).
    # Plus Pyrolysis Capex.

    annuity_fac_py <- calculate_annuity_factor(discount_rate, py_life)
    annuity_fac_bes <- calculate_annuity_factor(discount_rate, bes_life)

    annual_capex_py <- py_cc / annuity_fac_py
    annual_capex_power <- (bes_cc * (1 - bc_yield)) / annuity_fac_bes

    annual_om <- (py_cc + bes_cc * (1 - bc_yield)) * O_M_factor

    total_cost <- annual_capex_py + annual_capex_power + annual_om

    # Abatement
    # C sequestered + C displaced by energy
    c_displaced <- energy_output * ff_c_intensity

    # Soil GHG benefits (N2O suppression)
    # Excel J23: Annual N2O benefit
    # Paper uses GWP_N2O (approx 298 or similar from AR5)
    # n_app_rate (kg N/ha), n2o_factor (% emitted)
    # BEBCS reduces emissions by avoiding fertilizer or reducing N2O rate?
    # Excel calculates N2O benefit based on n_app_rate * n2o_factor.
    # Simplified 'soil_ghg_abatement' parameter for C eq per Mg Biochar.
    soil_ghg_abatement <- 0.1 # Placeholder Mg CO2e/Mg BC

    tot_c_abatement <- c_sequestered + c_displaced + soil_ghg_abatement
    abatement_value <- tot_c_abatement * c_price

    # Crop Yield Benefits (Nbcf)
    # Equation 13: value / (discount + decay_rate)
    # value = crop_price * yield_increase
    # We add this as 'ag_value' separate from carbon market.

    # Calculate decay rate from biochar half-life or stability
    # Paper: T1/2. Excel: J8 = 10^(bc_stab_factor * ...)
    # Assume mean residence time (MRT) = 1/decay_rate.
    # Excel J12: EXP(-time * (LN(2)/J8)). So J8 is Half Life?
    # Yes, Half Life T1/2.
    bc_half_life <- 10^(bc_stab_factor * (1 - 0.1)) # approximated from Excel J8 logic
    decay_rate <- log(2) / bc_half_life

    # Soil Fertility Value (Annual)
    # This is R_f,bc in Equation 13.
    # We need a parameter for this.
    annual_ag_benefit <- bc_ag_value # $/Mg BC/year (Parameter)

    # NPV of Soil Fertility (Nbcf)
    # Equation 13: R / (i + ln(2)/T1/2)
    # i.e. R / (discount + decay)
    nbcf <- annual_ag_benefit / (discount_rate + decay_rate)

    total_revenue <- elec_revenue + bc_revenue + abatement_value + nbcf
    net_value <- total_revenue - total_cost

    list(
      technology = "BEBCS",
      bc_yield = bc_yield,
      bc_c_content = bc_c_content,
      energy_output = energy_output,
      elec_prod = elec_prod,
      c_sequestered = c_sequestered,
      tot_c_abatement = tot_c_abatement,
      total_cost = total_cost,
      total_revenue = total_revenue,
      nbcf = nbcf,
      net_value = net_value
    )
  })
}
