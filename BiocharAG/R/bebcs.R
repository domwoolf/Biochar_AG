#' Calculate Biochar-Energy (BEBCS) Metrics
#'
#' `bebcs_energy_mode` selects how the surplus pyrolysis vapours and gases are used: "power" (power block),
#' "none" (no energy co-product: the surplus is flared, with no energy block, revenue or grid credit),
#' "heat" (heat-only block; sensitivity scenario, issue #106) or "flex" (default): "power" and "none"
#' are both evaluated and each cell takes the one with the higher net value at the current carbon price.
#' Heat is not part of "flex", because year-round heat demand is niche and opportunistic.
#'
#' @param params A list of parameters.
#' @return A list of calculated metrics for BEBCS; under "flex", `bebcs_mode` (1 = power, 2 = none).
#' @export
calculate_bebcs <- function(params) {
  mode <- if (!is.null(params$bebcs_energy_mode)) params$bebcs_energy_mode else "flex"
  if (mode != "flex") return(calculate_bebcs_mode(params))
  run_mode <- function(m) {
    p <- params
    p$bebcs_energy_mode <- m
    calculate_bebcs_mode(p)
  }
  pw <- run_mode("power")
  no <- run_mode("none")
  use_none <- no$net_value > pw$net_value
  out <- pw
  for (nm in names(pw)) {
    a <- pw[[nm]]
    b <- no[[nm]]
    if (is.character(a) || (length(a) == 1 && !inherits(a, "SpatRaster") && identical(a, b))) next
    out[[nm]] <- ifelse_raster(use_none, b, a)
  }
  out$bebcs_mode <- ifelse_raster(use_none, 2, 1)
  out
}

#' BEBCS Metrics for One Energy Mode
#'
#' @param params A list of parameters with `bebcs_energy_mode` "power", "none" or "heat".
#' @return A list of calculated metrics for BEBCS.
#' @keywords internal
calculate_bebcs_mode <- function(params) {
  soil_temp <- if (!is.null(params$soil_temp)) params$soil_temp else 14.9
  params$ff_c_intensity <- displaced_grid_ci(params) # MEF(P): displaced grid intensity at this carbon price

  # Collection distance at this technology's capacity factor (attach_size_layers)
  if (!is.null(params[["avg_dist_BEBCS", exact = TRUE]])) params$avg_dist <- params[["avg_dist_BEBCS", exact = TRUE]]

  with(params, {
    bebcs_energy_mode <- if (!is.null(params$bebcs_energy_mode)) params$bebcs_energy_mode else "power"
    if (!bebcs_energy_mode %in% c("power", "none", "heat")) stop("Unknown bebcs_energy_mode: ", bebcs_energy_mode)
    gj_to_mwh_conv <- if (!is.null(params$gj_to_mwh)) gj_to_mwh else 0.277778
    power_eff <- if (!is.null(params$bebcs_power_efficiency)) params$bebcs_power_efficiency else 0.35

    if (bebcs_energy_mode == "none") {
      # No energy co-product: the surplus vapours and gases are flared (no energy block, revenue or credit).
      # Process heat and the parasitic load are still met from the pyrolysis fuel.
      eff <- 0
      price <- 0
      c_intensity <- 0
      base_energy_capex <- 0
      life <- 25
      om_fac <- 0
    } else if (bebcs_energy_mode == "power") {
      eff <- power_eff
      price <- if (!is.null(params$elec_price)) params$elec_price else 100
      c_intensity <- if (!is.null(params$ff_c_intensity)) params$ff_c_intensity else (12 / 3600)
      base_energy_capex <- if (!is.null(params$bebcs_power_capital_cost)) params$bebcs_power_capital_cost else 2600
      life <- if (!is.null(params$bebcs_power_life)) params$bebcs_power_life else 25
      om_fac <- plant_om_fraction(params, "bebcs_power_om_factor")
    } else {
      # Heat mode
      eff <- if (!is.null(params$bebcs_heat_efficiency)) params$bebcs_heat_efficiency else 0.80
      price <- if (!is.null(params$heat_price)) params$heat_price else 30
      # Heat displaces the same price-dependent intensity as electricity: over time, fossil heating is
      # replaced by decarbonised electric heating (tCO2/GJ heat)
      c_intensity <- if (!is.null(params$ff_c_intensity)) params$ff_c_intensity else (12 / 3600)
      # Exploratory (issue #106): if heat_boiler_ci (t CO2/MWh heat) is given, heat displaces the cleaner
      # of a fossil boiler and a grid heat pump (MEF(P) / COP); heat_offtake is the share of heat sold.
      if (!is.null(params$heat_boiler_ci)) {
        cop <- if (!is.null(params$heat_pump_cop)) params$heat_pump_cop else 3
        c_intensity <- pmin_raster(params$heat_boiler_ci / 3.6, c_intensity / cop)
      }
      offtake <- if (!is.null(params$heat_offtake)) params$heat_offtake else 1
      c_intensity <- c_intensity * offtake
      price <- price * offtake
      base_energy_capex <- if (!is.null(params$bebcs_heat_capital_cost)) params$bebcs_heat_capital_cost else 400
      life <- if (!is.null(params$bebcs_heat_life)) params$bebcs_heat_life else 25
      om_fac <- plant_om_fraction(params, "bebcs_heat_om_factor")
    }

    # 1. Plant scale (thermal input of the biomass feed)
    if (!is.null(params$plant_mw_th)) {
      plant_mw_th <- resolve_plant_mw_th(params$plant_mw_th, "BEBCS")
    } else {
      plant_mw_th <- (if (!is.null(params$plant_mw)) params$plant_mw else 50) / (if (eff > 0) eff else power_eff)
    }
    capacity_factor_val <- tech_capacity_factor(params, "BEBCS")
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
    annual_om <- (total_py_capex * plant_om_fraction(params, "py_om_factor") + total_energy_capex * om_fac) * location_factor(params, "om") / actual_annual_biomass

    # --- 3. Logistics Cost & Transport Emissions ---
    logistics <- biomass_logistics(params)
    effective_dist <- logistics$effective_dist
    # Biochar returns to the fields as a backhaul in the feedstock trucks, which run year-round from
    # field-edge storage and whose empty return leg is already in the per-km cost: only loading and
    # handling are charged.
    bc_haul_cost <- if (isFALSE(as.logical(params$bc_return_haul))) 0 else
      bc_yield * (if (!is.null(params$bm_transport_fixed)) params$bm_transport_fixed else 6.27) * location_factor(params, "haulage")
    # Field application on the cell's own cropland: dose strategy with the highest net value at this carbon
    # price (yield response, spreading passes, soil N2O; biochar_field_table())
    field_tab <- biochar_field_table(params, bc_yield, bc_stability)
    field <- choose_bc_dose(field_tab, c_price)
    bc_field_cost <- field$v_spread
    bc_field_emissions <- field$e_diesel
    logistics_cost <- logistics$cost + bc_haul_cost + bc_field_cost
    transport_emissions_co2e <- logistics$emissions + bc_field_emissions
    removal_charge <- residue_removal_charge(params) # nutrients and alkalinity removed with the residue

    feedstock_cost <- if (!is.null(params$feedstock_cost)) params$feedstock_cost else 0
    total_cost <- annual_capex_py + annual_capex_power + annual_om + logistics_cost + feedstock_cost + removal_charge

    # 4. Abatement & Value
    # Explicit conversion to CO2e
    molar_ratio_c <- if (!is.null(params$molar_ratio_co2_c)) molar_ratio_co2_c else (44 / 12)
    co2e_sequestered <- bc_yield * bc_c_content * bc_stability * molar_ratio_c
    c_displaced <- energy_output * c_intensity
    soil_ghg_abatement <- field$a_n2o # avoided soil N2O, Mg CO2e / Mg feed

    tot_c_abatement <- co2e_sequestered + c_displaced + soil_ghg_abatement - transport_emissions_co2e +
      residue_counterfactual_ghg(params)
    abatement_value <- tot_c_abatement * c_price

    bc_val_res <- calculate_biochar_value(params, bc_yield) # liming and P, K returned
    biochar_economic_value <- bc_val_res$value_usd_per_mg_feedstock + field$v_yield

    total_revenue <- energy_revenue + biochar_economic_value + abatement_value
    net_value <- total_revenue - total_cost

    # Added diagnostics for factorial
    biomass_cost <- feedstock_cost + logistics_cost
    total_capex_per_mg <- annual_capex_py + annual_capex_power
    lcoe <- if (eff > 0) (total_capex_per_mg + annual_om + biomass_cost - biochar_economic_value) / energy_prod else NA
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
      biochar_haul_cost_mg = bc_haul_cost, # included in biomass_cost_mg
      biochar_field_cost_mg = bc_field_cost, # included in biomass_cost_mg
      removal_charge_mg = removal_charge, # included in total_cost
      bc_yield_value_mg = field$v_yield, # included in agronomic_revenue_mg
      bc_mineral_value_mg = bc_val_res$value_usd_per_mg_feedstock, # liming, P and K; in agronomic_revenue_mg
      soil_n2o_abatement = soil_ghg_abatement, # included in tot_c_abatement
      bc_dose = field$dose, # dose option (Mg biochar/ha; 0 = annual application)
      bc_dose_eff = field$d_eff, # biochar per application (Mg/ha)
      bc_cohorts = field$cohorts,
      bc_supply_ha = field_tab$b, # biochar per hectare of cropland per year (Mg)
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
