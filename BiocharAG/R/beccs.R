#' Calculate Bioenergy Carbon Capture and Storage (BECCS) Metrics
#'
#' @param params A list of parameters.
#' @return A list of calculated metrics for BECCS.
#' @export
calculate_beccs <- function(params) {
  with(params, {
    # 1. Annuity Factor (uses bes_life usually or specific beccs life?)
    # Excel F6 in BECCS seems to ref bes_annual_cc logic or beccs specific?
    # Excel F6: bes_annual_cc.
    # Excel F7: ccs_cc * bm_c ? No. "ccs_cc*bm_c" seems odd if ccs_cc is Cost.
    # Maybe ccs_cc is $/tC?

    # Let's follow BES pattern but modify for CCS

    # Efficiency penalty
    eff <- bes_energy_efficiency - beccs_eff_penalty
    energy_output <- bm_lhv * eff
    beccs_elec_prod <- energy_output * 0.277778

    # Costs
    # Base plant cost (BES) + CCS Cost
    # Excel F10: SUM(F6:F9).
    # F6: bes_annual_cc (from BES)
    # F7: ccs_cc * bm_c ? (This implies ccs_cc is cost per unit carbon?)
    # F8: bes_cc * O_M_factor
    # F9: ...

    # Let's approximate:
    base_annual_capex <- bes_cc / calculate_annuity_factor(discount_rate, bes_life)

    # CCS Cost:
    # If parameters$ccs_cc is $/Mg Biomass capacity or $/tCO2?
    # "ccs cost 244 $/Mg ???" in label
    # Let's assume ccs_cc is an additional capital cost per Mg biomass.
    ccs_annual_capex <- ccs_cc / calculate_annuity_factor(discount_rate, bes_life)

    annual_om <- bes_cc * O_M_factor # Base O&M
    # Maybe add CCS O&M?

    total_cost <- base_annual_capex + ccs_annual_capex + annual_om

    # Revenue
    elec_revenue <- beccs_elec_prod * elec_price

    # Carbon
    # Sequestration = bm_c * beccs_seq_fraction
    c_sequestered <- bm_c * beccs_seq_fraction
    # Displaced Fossil C = energy_output * ff_c_intensity
    c_displaced <- energy_output * ff_c_intensity

    tot_c_abatement <- c_sequestered + c_displaced
    abatement_value <- tot_c_abatement * c_price

    total_revenue <- elec_revenue + abatement_value
    net_value <- total_revenue - total_cost

    list(
      technology = "BECCS",
      energy_output = energy_output,
      elec_prod = beccs_elec_prod,
      c_sequestered = c_sequestered,
      tot_c_abatement = tot_c_abatement,
      total_cost = total_cost,
      total_revenue = total_revenue,
      net_value = net_value
    )
  })
}
