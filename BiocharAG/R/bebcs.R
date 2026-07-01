#' Calculate Biochar-Energy (BEBCS) Metrics
#'
#' @param params A list of parameters.
#' @return A list of calculated metrics for BEBCS.
#' @export
calculate_bebcs <- function(params) {
  h_c_org <- if (!is.null(params$h_c_org)) params$h_c_org else 0.35
  soil_temp <- if (!is.null(params$soil_temp)) params$soil_temp else 14.9

  with(params, {
    phys <- calculate_pyrolysis_physics(
      py_temp = py_temp,
      lignin = lignin,
      bm_lhv = bm_lhv,
      moisture = if (exists("bm_h2o")) bm_h2o else 0.1,
      ash = if (exists("bm_ash")) bm_ash else 0.05
    )

    bc_yield <- phys$yield_bc
    bc_c_content <- phys$bc_c_content_final

    bc_stability <- calculate_fperm_approx(h_c_org, method = "HC", soil_temp = soil_temp)

    # 1. Energy Output
    energy_output <- phys$energy_net * bes_energy_efficiency
    elec_prod <- energy_output * 0.277778
    elec_revenue <- elec_prod * elec_price

    # 2. Costs (Scale & CAPEX)
    if (!is.null(params$plant_mw_th)) {
      plant_mw_th <- resolve_plant_mw_th(params$plant_mw_th, "BEBCS")
      plant_mw <- plant_mw_th * bes_energy_efficiency
    } else {
      plant_mw <- if (!is.null(params$plant_mw)) params$plant_mw else 50
      plant_mw_th <- plant_mw / bes_energy_efficiency
    }

    capacity_factor <- 0.85
    scaling_factor <- 0.7

    bes_elec_prod_ref <- bm_lhv * bes_energy_efficiency * 0.277778
    ref_50mw_biomass <- (50 * 8760 * capacity_factor) / bes_elec_prod_ref
    actual_annual_biomass <- (plant_mw_th * 8760 * capacity_factor) / (bm_lhv * 0.277778)

    base_py_capex <- py_cc * ref_50mw_biomass
    total_py_capex <- base_py_capex * ((plant_mw / 50)^scaling_factor)
    annuity_fac_py <- calculate_annuity_factor(discount_rate, py_life)
    annual_py_payment <- total_py_capex / annuity_fac_py
    annual_capex_py <- annual_py_payment / actual_annual_biomass

    base_cost_ref <- bes_capital_cost * 50 * 1000
    total_bes_capex <- base_cost_ref * ((plant_mw / 50)^scaling_factor)
    annuity_fac_bes <- calculate_annuity_factor(discount_rate, bes_life)
    annual_bes_payment <- total_bes_capex / annuity_fac_bes
    base_power_capex_per_mg <- annual_bes_payment / actual_annual_biomass
    annual_capex_power <- base_power_capex_per_mg * (1 - bc_yield)

    annual_om <- ((total_py_capex / actual_annual_biomass) * O_M_factor) + (base_power_capex_per_mg * (1 - bc_yield) * bes_om_factor)

    # --- 3. Logistics Cost & Transport Emissions ---
    if (!is.null(params$avg_dist)) {
      avg_dist <- params$avg_dist
    } else {
      radius <- if (!is.null(params$collection_radius)) params$collection_radius else 50
      avg_dist <- (2 / 3) * radius
    }

    tort <- if (!is.null(params$tortuosity)) params$tortuosity else 1.3
    effective_dist <- avg_dist * tort

    tf <- if (!is.null(params$bm_transport_fixed)) params$bm_transport_fixed else 5.0
    tv <- if (!is.null(params$bm_transport_var)) params$bm_transport_var else 0.15
    logistics_cost <- tf + (tv * effective_dist)

    trans_em_factor <- if (!is.null(params$transport_emissions_factor)) params$transport_emissions_factor else 0.0001
    transport_emissions_co2e <- effective_dist * trans_em_factor

    feedstock_cost <- if (!is.null(params$feedstock_cost)) params$feedstock_cost else 0
    total_cost <- annual_capex_py + annual_capex_power + annual_om + logistics_cost + feedstock_cost

    # 4. Abatement & Value
    # Explicit conversion to CO2e
    co2e_sequestered <- bc_yield * bc_c_content * bc_stability * (44 / 12)
    c_displaced <- energy_output * ff_c_intensity
    soil_ghg_abatement <- 0.1

    tot_c_abatement <- co2e_sequestered + c_displaced + soil_ghg_abatement - transport_emissions_co2e
    abatement_value <- tot_c_abatement * c_price

    bc_val_res <- calculate_biochar_value(params, bc_yield)
    biochar_economic_value <- bc_val_res$value_usd_per_mg_feedstock

    total_revenue <- elec_revenue + biochar_economic_value + abatement_value
    net_value <- total_revenue - total_cost

    # Added diagnostics for factorial
    biomass_cost <- feedstock_cost + logistics_cost
    total_capex_per_mg <- annual_capex_py + annual_capex_power
    lcoe <- (total_capex_per_mg + annual_om + biomass_cost - biochar_economic_value) / elec_prod
    cost_of_co2_avoided <- ifelse_raster(tot_c_abatement > 0, total_cost / tot_c_abatement, Inf)
    abatement_efficiency <- ifelse_raster(co2e_sequestered > 0, tot_c_abatement / co2e_sequestered, 0)
    total_capex_m <- (total_py_capex + total_bes_capex) / 1e6

    list(
      technology = "BEBCS",
      bc_yield = bc_yield,
      bc_c_content = bc_c_content,
      energy_output = energy_output,
      elec_prod = elec_prod,
      c_sequestered = co2e_sequestered, # Now safely in CO2e
      tot_c_abatement = tot_c_abatement,
      total_cost = total_cost,
      total_revenue = total_revenue,
      biochar_value = biochar_economic_value,
      val_method = bc_val_res$method_used,
      net_value = net_value,
      # Granular outputs
      capital_cost_mg = total_capex_per_mg,
      om_cost_mg = annual_om,
      biomass_cost_mg = biomass_cost,
      co2_transport_cost_mg = 0,
      co2_transport_distance_km = NA,
      biomass_transport_distance_km = effective_dist,
      elec_revenue_mg = elec_revenue,
      abatement_revenue_mg = abatement_value,
      agronomic_revenue_mg = biochar_economic_value,
      lcoe = lcoe,
      cost_of_co2_avoided = cost_of_co2_avoided,
      abatement_efficiency = abatement_efficiency,
      total_capex_m = total_capex_m
    )
  })
}
