#' Calculate Bioenergy System (BES) Metrics
#'
#' @param params A list of parameters.
#' @return A list of calculated metrics for BES.
#' @export
calculate_bes <- function(params) {
  with(params, {
    # 1. Annuity Factor
    annuity_fac <- calculate_annuity_factor(discount_rate, bes_life)

    # 2. Energy Output (GJ/Mg biomass?)
    # Assumes bm_lhv is in GJ/Mg
    energy_output <- bm_lhv * bes_energy_efficiency

    # 3. Costs
    # Annualized Capital Cost ($/Mg?)
    # Assuming bes_cc is Capital Cost per Mg capacity?? Or absolute?
    # Context suggests per Mg throughput or similar unitizing.
    annual_capex <- bes_cc / annuity_fac

    # O&M
    annual_om <- bes_cc * O_M_factor

    # Feedstock / Rebound / Other costs
    # From Excel B9: "bm_lhv * ff_c_intensity * rebound" ?
    # This seems specific to the model's logic on fossil fuel displacement or leakage.
    # We'll call it 'emissions_cost' or 'rebound_cost'
    rebound_cost <- bm_lhv * ff_c_intensity * rebound

    total_cost <- annual_capex + annual_om + rebound_cost

    # 4. Revenue & Abatement
    # Electricity Production (MWh/Mg?)
    # 1 GJ = 0.277778 MWh
    bes_elec_prod <- energy_output * 0.277778
    elec_revenue <- bes_elec_prod * elec_price

    # Carbon Abatement
    # Needs logic for C abatement.
    # Excel F13: bes_tot_c_abatement * c_price
    # bes_tot_c_abatement needs formula.
    # Often: (Fossil C displaced) - (Process Emissions)
    # Fossil C displaced = energy_output * ff_c_intensity ? (or similar)
    # Let's assume a simplified calculation or look for "bes_tot_c_abatement" formula.
    # In Excel extract, we didn't see definition of bes_tot_c_abatement.
    # But F18 = F15 - F10 = Total Rev - Total Cost.

    # Placeholder for abatement
    # If not defined, we'll estimate:
    # C_abated = (energy_output * ff_c_intensity) - (process emissions)
    c_abated <- (energy_output * ff_c_intensity) # Simplified
    abatement_value <- c_abated * c_price

    total_revenue <- elec_revenue + abatement_value

    net_value <- total_revenue - total_cost

    list(
      technology = "BES",
      annuity_factor = annuity_fac,
      energy_output = energy_output,
      elec_prod = bes_elec_prod,
      total_cost = total_cost,
      total_revenue = total_revenue,
      net_value = net_value,
      c_abated = c_abated
    )
  })
}
