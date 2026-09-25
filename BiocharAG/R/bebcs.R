#' Calculate Biochar-Energy (BEBCS) Metrics
#'
#' @param params A list of parameters.
#' @return A list of calculated metrics for BEBCS.
#' @export
calculate_bebcs <- function(params) {
  soil_temp <- if (!is.null(params$soil_temp)) params$soil_temp else 14.9

  with(params, {
    phys <- calculate_pyrolysis_physics(
      py_temp = py_temp,
      lignin = lignin,
      bm_lhv = bm_lhv,
      moisture = if (!is.null(params$bm_h2o)) params$bm_h2o else 0.1,
      ash = if (!is.null(params$bm_ash)) params$bm_ash else 0.05,
      feed_c = if (!is.null(params$bm_c)) params$bm_c else 0.50,
      feed_h = if (!is.null(params$bm_h)) params$bm_h else 0.06,
      feed_o = if (!is.null(params$bm_o)) params$bm_o else 0.44
    )

    bc_yield <- phys$yield_bc
    bc_c_content <- phys$bc_c_content_final

    # H:C follows from pyrolysis temperature unless supplied explicitly
    h_c_org <- if (!is.null(params$h_c_org)) params$h_c_org else phys$bc_h_c_molar
    bc_stability <- calculate_fperm_approx(h_c_org, method = "HC", soil_temp = soil_temp)

    # 1. Energy Mode & Output
    bebcs_energy_mode <- if (!is.null(params$bebcs_energy_mode)) params$bebcs_energy_mode else "power"
    gj_to_mwh_conv <- if (!is.null(params$gj_to_mwh)) gj_to_mwh else 0.277778

    if (bebcs_energy_mode == "power") {
      eff <- if (!is.null(params$bebcs_power_efficiency)) params$bebcs_power_efficiency else 0.35
      energy_output <- phys$energy_net * eff
      energy_prod <- energy_output * gj_to_mwh_conv
      energy_revenue <- energy_prod * (if (!is.null(params$elec_price)) params$elec_price else 100)
      c_intensity <- if (!is.null(params$ff_c_intensity)) params$ff_c_intensity else (12 / 3600)

      base_energy_capex <- if (!is.null(params$bebcs_power_capital_cost)) params$bebcs_power_capital_cost else 1500
      life <- if (!is.null(params$bebcs_power_life)) params$bebcs_power_life else 25
      om_fac <- if (!is.null(params$bebcs_power_om_factor)) params$bebcs_power_om_factor else 0.05
    } else {
      # Heat mode
      eff <- if (!is.null(params$bebcs_heat_efficiency)) params$bebcs_heat_efficiency else 0.80
      energy_output <- phys$energy_net * eff
      energy_prod <- energy_output * gj_to_mwh_conv
      energy_revenue <- energy_prod * (if (!is.null(params$heat_price)) params$heat_price else 30)
      c_intensity <- if (!is.null(params$heat_c_intensity)) params$heat_c_intensity else 0.08

      base_energy_capex <- if (!is.null(params$bebcs_heat_capital_cost)) params$bebcs_heat_capital_cost else 400
      life <- if (!is.null(params$bebcs_heat_life)) params$bebcs_heat_life else 25
      om_fac <- if (!is.null(params$bebcs_heat_om_factor)) params$bebcs_heat_om_factor else 0.03
    }

    # 2. Costs (Scale & CAPEX)
    if (!is.null(params$plant_mw_th)) {
      plant_mw_th <- resolve_plant_mw_th(params$plant_mw_th, "BEBCS")
      plant_mw <- plant_mw_th * eff
    } else {
      plant_mw <- if (!is.null(params$plant_mw)) params$plant_mw else 50
      plant_mw_th <- plant_mw / eff
    }

    capacity_factor_val <- if (!is.null(params$capacity_factor)) capacity_factor else 0.85
    scaling_factor_val <- if (!is.null(params$scaling_factor)) scaling_factor else 0.7

    bes_elec_prod_ref <- bm_lhv * eff * gj_to_mwh_conv
    ref_50mw_biomass <- (50 * 8760 * capacity_factor_val) / bes_elec_prod_ref # biomass (Mg/year) to run a 50 MW reference plant. 8760 is hours per year.
    actual_annual_biomass <- (plant_mw_th * 8760 * capacity_factor_val) / (bm_lhv * gj_to_mwh_conv)

    base_py_capex <- py_cc * ref_50mw_biomass
    total_py_capex <- base_py_capex * ((plant_mw / 50)^scaling_factor_val)
    annuity_fac_py <- calculate_annuity_factor(discount_rate, py_life)
    annual_py_payment <- total_py_capex / annuity_fac_py
    annual_capex_py <- annual_py_payment / actual_annual_biomass

    base_cost_ref <- base_energy_capex * 50 * 1000 # 1000 converts MW to kW
    total_energy_capex <- base_cost_ref * ((plant_mw / 50)^scaling_factor_val)
    annuity_fac_energy <- calculate_annuity_factor(discount_rate, life)
    annual_energy_payment <- total_energy_capex / annuity_fac_energy
    base_power_capex_per_mg <- annual_energy_payment / actual_annual_biomass
    annual_capex_power <- base_power_capex_per_mg * (1 - bc_yield)

    annual_om <- ((total_py_capex / actual_annual_biomass) * O_M_factor) + (base_power_capex_per_mg * (1 - bc_yield) * om_fac)

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
    molar_ratio_c <- if (!is.null(params$molar_ratio_co2_c)) molar_ratio_co2_c else (44 / 12)
    co2e_sequestered <- bc_yield * bc_c_content * bc_stability * molar_ratio_c
    c_displaced <- energy_output * c_intensity
    soil_ghg_abatement <- 0.1

    tot_c_abatement <- co2e_sequestered + c_displaced + soil_ghg_abatement - transport_emissions_co2e
    abatement_value <- tot_c_abatement * c_price

    bc_val_res <- calculate_biochar_value(params, bc_yield)
    biochar_economic_value <- bc_val_res$value_usd_per_mg_feedstock

    total_revenue <- energy_revenue + biochar_economic_value + abatement_value
    net_value <- total_revenue - total_cost

    # Added diagnostics for factorial
    biomass_cost <- feedstock_cost + logistics_cost
    total_capex_per_mg <- annual_capex_py + annual_capex_power
    lcoe <- (total_capex_per_mg + annual_om + biomass_cost - biochar_economic_value) / energy_prod
    cost_of_co2_avoided <- ifelse_raster(tot_c_abatement > 0, total_cost / tot_c_abatement, Inf)
    abatement_efficiency <- ifelse_raster(co2e_sequestered > 0, tot_c_abatement / co2e_sequestered, 0)
    total_capex_m <- (total_py_capex + total_energy_capex) / 1e6

    list(
      technology = "BEBCS",
      bc_yield = bc_yield,
      bc_c_content = bc_c_content,
      energy_output = energy_output,
      energy_prod = energy_prod,
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
      energy_revenue_mg = energy_revenue,
      abatement_revenue_mg = abatement_value,
      agronomic_revenue_mg = biochar_economic_value,
      lcoe = lcoe,
      cost_of_co2_avoided = cost_of_co2_avoided,
      abatement_efficiency = abatement_efficiency,
      total_capex_m = total_capex_m
    )
  })
}
