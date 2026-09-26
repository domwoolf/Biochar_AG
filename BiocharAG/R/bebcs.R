#' Calculate Biochar-Energy (BEBCS) Metrics
#'
#' @param params A list of parameters.
#' @return A list of calculated metrics for BEBCS.
#' @export
calculate_bebcs <- function(params) {
  soil_temp <- if (!is.null(params$soil_temp)) params$soil_temp else 14.9

  with(params, {
    bebcs_energy_mode <- if (!is.null(params$bebcs_energy_mode)) params$bebcs_energy_mode else "power"
    gj_to_mwh_conv <- if (!is.null(params$gj_to_mwh)) gj_to_mwh else 0.277778
    power_eff <- if (!is.null(params$bebcs_power_efficiency)) params$bebcs_power_efficiency else 0.35

    if (bebcs_energy_mode == "power") {
      eff <- power_eff
      price <- if (!is.null(params$elec_price)) params$elec_price else 100
      c_intensity <- if (!is.null(params$ff_c_intensity)) params$ff_c_intensity else (12 / 3600)
      base_energy_capex <- if (!is.null(params$bebcs_power_capital_cost)) params$bebcs_power_capital_cost else 1500
      life <- if (!is.null(params$bebcs_power_life)) params$bebcs_power_life else 25
      om_fac <- if (!is.null(params$bebcs_power_om_factor)) params$bebcs_power_om_factor else 0.05
    } else {
      # Heat mode
      eff <- if (!is.null(params$bebcs_heat_efficiency)) params$bebcs_heat_efficiency else 0.80
      price <- if (!is.null(params$heat_price)) params$heat_price else 30
      c_intensity <- if (!is.null(params$heat_c_intensity)) params$heat_c_intensity else 0.08
      base_energy_capex <- if (!is.null(params$bebcs_heat_capital_cost)) params$bebcs_heat_capital_cost else 400
      life <- if (!is.null(params$bebcs_heat_life)) params$bebcs_heat_life else 25
      om_fac <- if (!is.null(params$bebcs_heat_om_factor)) params$bebcs_heat_om_factor else 0.03
    }

    # 1. Plant scale (thermal input of the biomass feed)
    if (!is.null(params$plant_mw_th)) {
      plant_mw_th <- resolve_plant_mw_th(params$plant_mw_th, "BEBCS")
    } else {
      plant_mw_th <- (if (!is.null(params$plant_mw)) params$plant_mw else 50) / eff
    }
    capacity_factor_val <- if (!is.null(params$capacity_factor)) capacity_factor else 0.85
    scaling_factor_val <- if (!is.null(params$scaling_factor)) scaling_factor else 0.7
    feed_mg_hr <- plant_mw_th * 3.6 / bm_lhv # Mg daf feed / hr
    actual_annual_biomass <- feed_mg_hr * 8760 * capacity_factor_val

    # 2. Pyrolysis mass & energy balance
    feed_c <- if (!is.null(params$bm_c)) params$bm_c else 0.50
    feed_h <- if (!is.null(params$bm_h)) params$bm_h else 0.06
    phys <- calculate_pyrolysis_physics(
      py_temp = py_temp,
      lignin = lignin,
      bm_lhv = bm_lhv,
      moisture = if (!is.null(params$bm_h2o)) params$bm_h2o else 0.1,
      ash = if (!is.null(params$bm_ash)) params$bm_ash else 0.05,
      feed_c = feed_c,
      feed_h = feed_h,
      feed_o = if (!is.null(params$bm_o)) params$bm_o else 1 - feed_c - feed_h,
      feed_rate_kg_hr = feed_mg_hr * 1000,
      heater_eff = if (!is.null(params$py_heater_eff)) params$py_heater_eff else 0.8,
      parasitic_power = if (!is.null(params$py_parasitic_power)) params$py_parasitic_power else 0.07,
      power_eff = power_eff,
      exhaust_temp = if (!is.null(params$py_exhaust_temp)) params$py_exhaust_temp else 170,
      e_source = if (!is.null(params$py_e_source)) params$py_e_source else "fuel"
    )

    bc_yield <- phys$yield_bc
    bc_c_content <- phys$bc_c_content_final

    # H:C follows from pyrolysis temperature unless supplied explicitly
    h_c_org <- if (!is.null(params$h_c_org)) params$h_c_org else phys$bc_h_c_molar
    bc_stability <- calculate_fperm_approx(h_c_org, method = "HC", soil_temp = soil_temp)

    # 3. Energy output (net pyrolysis fuel, LHV basis)
    energy_output <- phys$energy_net * eff
    energy_prod <- energy_output * gj_to_mwh_conv
    energy_revenue <- energy_prod * price

    # 4. Costs (CAPEX/OPEX)
    # Pyrolysis unit: py_cc per Mg/yr of feed, referenced to the feed of a 50 MWe plant at bebcs_power_efficiency
    ref_50mw_biomass <- (50 / power_eff) * 3.6 / bm_lhv * 8760 * capacity_factor_val
    capex_loc <- location_factor(params, "capex")
    total_py_capex <- py_cc * ref_50mw_biomass * (actual_annual_biomass / ref_50mw_biomass)^scaling_factor_val * capex_loc
    annuity_fac_py <- calculate_annuity_factor(discount_rate, py_life)
    annual_capex_py <- (total_py_capex / annuity_fac_py) / actual_annual_biomass

    # Energy block sized on the net fuel it actually receives (MWth of fuel x efficiency)
    fuel_mw_th <- feed_mg_hr * phys$energy_net / 3.6
    total_energy_capex <- combustion_plant_capex(base_energy_capex, fuel_mw_th, eff, scaling_factor_val) * capex_loc
    annuity_fac_energy <- calculate_annuity_factor(discount_rate, life)
    annual_capex_power <- (total_energy_capex / annuity_fac_energy) / actual_annual_biomass

    # Annual O&M is a fraction of total CAPEX. Costs are levelised per year: discounting this constant
    # annual cost over the plant life and re-annualising at the same rate returns the annual value.
    annual_om <- (total_py_capex * py_om_factor + total_energy_capex * om_fac) * location_factor(params, "om") / actual_annual_biomass

    # --- 3. Logistics Cost & Transport Emissions ---
    logistics <- biomass_logistics(params)
    effective_dist <- logistics$effective_dist
    logistics_cost <- logistics$cost
    transport_emissions_co2e <- logistics$emissions

    feedstock_cost <- if (!is.null(params$feedstock_cost)) params$feedstock_cost else 0
    total_cost <- annual_capex_py + annual_capex_power + annual_om + logistics_cost + feedstock_cost

    # 4. Abatement & Value
    # Explicit conversion to CO2e
    molar_ratio_c <- if (!is.null(params$molar_ratio_co2_c)) molar_ratio_co2_c else (44 / 12)
    co2e_sequestered <- bc_yield * bc_c_content * bc_stability * molar_ratio_c
    c_displaced <- energy_output * c_intensity
    # Soil N2O reduction (Woolf et al. 2021): fractional reduction in fertiliser-induced N2O on fields
    # receiving bc_app_rate_c Mg biochar C/ha, lasting n2o_years; credited per Mg biochar C applied
    n2o_ef <- if (!is.null(params$n2o_ef)) params$n2o_ef else 0.01
    n2o_reduction <- if (!is.null(params$n2o_reduction)) params$n2o_reduction else 0.23
    n2o_years <- if (!is.null(params$n2o_years)) params$n2o_years else 2
    n_app <- if (!is.null(params$n_app_rate)) params$n_app_rate else 68
    bc_app_rate_c <- if (!is.null(params$bc_app_rate_c)) params$bc_app_rate_c else 10
    gwp_n2o <- if (!is.null(params$gwp_n2o)) params$gwp_n2o else 273
    n2o_n_avoided_kg <- n_app * n2o_ef * n2o_reduction * n2o_years * phys$bc_c_yield / bc_app_rate_c
    soil_ghg_abatement <- n2o_n_avoided_kg * (44 / 28) * gwp_n2o / 1000 # Mg CO2e / Mg feed

    tot_c_abatement <- co2e_sequestered + c_displaced + soil_ghg_abatement - transport_emissions_co2e +
      residue_counterfactual_ghg(params)
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
