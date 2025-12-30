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
    # Pyrolysis Physics (Mass & Energy Balance)
    # Uses sophisticated Woolf et al. (2016) / Excel logic
    phys <- calculate_pyrolysis_physics(
      py_temp = py_temp,
      lignin = lignin,
      bm_lhv = bm_lhv,
      moisture = if (exists("bm_h2o")) bm_h2o else 0.1,
      ash = if (exists("bm_ash")) bm_ash else 0.05
    )

    bc_yield <- phys$yield_bc
    bc_c_content <- phys$bc_c_content_final

    # Stable Fraction using Fperm (Woolf et al. 2021)
    bc_stability <- calculate_fperm_approx(h_c_org, method = "HC", soil_temp = soil_temp)
    # Carbon Sequestration
    # C sequestered = bc_yield * bc_c_content * bc_stability
    c_sequestered <- bc_yield * bc_c_content * bc_stability

    # Energy Output
    # phys$energy_net is the Net Fuel Energy available (GJ/Mg feedstock)
    # after satisfying process heat requirements.
    # Convert to Electricity using BES efficiency.
    energy_output <- phys$energy_net * bes_energy_efficiency

    elec_prod <- energy_output * 0.277778
    elec_revenue <- elec_prod * elec_price

    # Biochar Revenue
    bc_revenue <- bc_yield * bc_price

    # Costs
    # Pyrolysis Plant Cost (py_cc) + Power Plant Cost.
    # Assume 50 MW Scale reference for unit cost derivation
    # (Consistent with BES/BECCS modern logic)

    # 1. Pyrolysis Unit Cost
    # Assume py_cc is capital cost per Mg capacity? Or $/kW equiv?
    # Original logic used py_cc directly. Let's keep py_cc as $/Mg annual capacity?
    # Or modernize: Pyrolysis Plant Cost ~ $500-800 / annual ton?
    # For now, let's treat py_cc as $/Mg Annual Capacity constant.

    annuity_fac_py <- calculate_annuity_factor(discount_rate, py_life)
    annual_capex_py <- py_cc / annuity_fac_py

    # 2. Power Generation Unit Cost (for Volatiles)
    # Volatiles Mass = (1 - bc_yield) * Feed
    # We use bes_capital_cost ($/kW) to find cost per Mg.

    # Calculate Power Capex per Mg Biomass (same logic as BES)
    # Calculate Power Capex per Mg Biomass (same logic as BES)
    plant_mw <- if (!is.null(params$plant_mw)) params$plant_mw else 50
    capacity_factor <- 0.85
    # Elec Prod for pure BES:
    bes_elec_prod_ref <- bm_lhv * bes_energy_efficiency * 0.277778
    ref_annual_biomass <- (plant_mw * 8760 * capacity_factor) / bes_elec_prod_ref

    total_bes_capex <- bes_capital_cost * plant_mw * 1000
    annuity_fac_bes <- calculate_annuity_factor(discount_rate, bes_life)
    annual_bes_payment <- total_bes_capex / annuity_fac_bes

    # Base Power Capex per Mg Input
    base_power_capex_per_mg <- annual_bes_payment / ref_annual_biomass

    # BEBCS Power Unit handles only (1 - bc_yield) fraction?
    # Or is it sized for the volatiles energy?
    # Volatiles Energy approx proportional to mass? (Simplification)
    # Let's scale by mass fraction of volatiles.
    annual_capex_power <- base_power_capex_per_mg * (1 - bc_yield)

    # O&M
    # Pyrolysis O&M + Power O&M
    annual_om <- (py_cc * O_M_factor) + (base_power_capex_per_mg * (1 - bc_yield) * bes_om_factor)

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
    # Biochar Valuation
    # Refactored to separate function (handling Market vs Ag Value switch)
    bc_val_res <- calculate_biochar_value(params, bc_yield)

    # This value is already in $/Mg Feedstock
    biochar_economic_value <- bc_val_res$value_usd_per_mg_feedstock

    # Note: For strict financial accounting, 'nbcf' (Ag Value) might be considered separate from 'sales revenue'
    # but for TEA Net Value comparison, we sum them based on the selected mode.

    total_revenue <- elec_revenue + biochar_economic_value + abatement_value
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
      biochar_value = biochar_economic_value,
      val_method = bc_val_res$method_used,
      net_value = net_value
    )
  })
}
