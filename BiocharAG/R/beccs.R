#' Calculate Bioenergy Carbon Capture and Storage (BECCS) Metrics
#'
#' Modernized logic (2024 Basis):
#' - Explicitly tracks Scope 3 transport emissions and road tortuosity.
#' - Converts sequestered carbon pools to CO2e ratios accurately (44/12).
#'
#' @param params A list of parameters.
#' @return A list of calculated metrics for BECCS.
#' @export
calculate_beccs <- function(params) {
  # BECCS efficiency = BES baseline minus the parasitic capture/compression penalty
  # (derived before fuel adjustment so the ash multiplier scales it like BES).
  if (is.null(params$bes_energy_efficiency)) params$bes_energy_efficiency <- 0.30
  if (is.null(params$beccs_eff_penalty)) params$beccs_eff_penalty <- 0.08
  if (is.null(params$beccs_efficiency)) {
    params$beccs_efficiency <- params$bes_energy_efficiency - params$beccs_eff_penalty
  }
  if (is.null(params$capture_rate)) params$capture_rate <- 0.90
  if (is.null(params$early_adoption)) params$early_adoption <- FALSE
  allow_eor <- if (!is.null(params$allow_eor)) as.logical(params$allow_eor) else TRUE
  dist_spatial <- NULL
  if (allow_eor) {
    if (!is.null(params$dist_sink_km)) dist_spatial <- params$dist_sink_km
  } else {
    if (!is.null(params$dist_sink_saline_km)) dist_spatial <- params$dist_sink_saline_km
  }

  # Without EOR the sink is the nearest saline store, so use that sink's onshore/offshore flag
  if (!allow_eor && !is.null(params$sink_is_offshore_saline)) {
    params$sink_is_offshore <- params$sink_is_offshore_saline
  }

  if (!is.null(dist_spatial)) {
    params$ccs_distance <- dist_spatial
  } else if (is.null(params$ccs_distance)) {
    if (!is.null(params$lat) && !is.null(params$lon)) {
      geo <- find_nearest_sink(params$lat, params$lon)
      params$ccs_distance <- geo$distance_km
    } else {
      params$ccs_distance <- 100
    }
  }

  if (is.null(params$bes_capital_cost)) params$bes_capital_cost <- 4700
  if (is.null(params$bes_capex_ref_eff)) params$bes_capex_ref_eff <- 0.30
  if (is.null(params$beccs_capex_premium)) params$beccs_capex_premium <- 0.40
  params <- adjust_costs_for_fuel(params)
  params$ff_c_intensity <- displaced_grid_ci(params) # MEF(P): displaced grid intensity at this carbon price

  # Collection distance at this technology's capacity factor (attach_size_layers)
  if (!is.null(params[["avg_dist_BECCS", exact = TRUE]])) params$avg_dist <- params[["avg_dist_BECCS", exact = TRUE]]

  with(params, {
    # 1. Energy Output
    energy_output <- bm_lhv * beccs_efficiency
    gj_to_mwh_conv <- if (!is.null(params$gj_to_mwh)) gj_to_mwh else 0.277778
    energy_prod <- energy_output * gj_to_mwh_conv

    # 2. Carbon Capture
    molar_ratio_c <- if (!is.null(params$molar_ratio_co2_c)) molar_ratio_co2_c else (44/12)
    co2_produced <- bm_c * molar_ratio_c
    co2_captured <- co2_produced * capture_rate # Mg CO2e / Mg Biomass

    # 3. Scale & Total Mass Flow
    if (!is.null(params$plant_mw_th)) {
      plant_mw_th <- resolve_plant_mw_th(params$plant_mw_th, "BECCS")
      plant_mw <- plant_mw_th * beccs_efficiency
    } else {
      plant_mw <- if (!is.null(params$plant_mw)) params$plant_mw else 50
      plant_mw_th <- plant_mw / beccs_efficiency
    }

    capacity_factor_val <- tech_capacity_factor(params, "BECCS")
    annual_biomass <- (plant_mw_th * 8760 * capacity_factor_val) / (bm_lhv * gj_to_mwh_conv)
    annual_co2_total <- annual_biomass * co2_captured

    # --- CCS Transport & Storage Component ---
    capex_loc <- location_factor(params, "capex")
    om_loc <- location_factor(params, "om")
    base_cost_onshore_storage <- if (!is.null(params$ccs_storage_cost)) params$ccs_storage_cost else 10.0
    base_cost_offshore_storage <- if (!is.null(params$cost_offshore_storage)) cost_offshore_storage else 20.0
    ship_fixed <- if (!is.null(params$co2_ship_emis_fixed)) params$co2_ship_emis_fixed else 0.022
    ship_km <- if (!is.null(params$co2_ship_emis_per_km)) params$co2_ship_emis_per_km else 1.3e-5
    pv_num <- function(n, d) if (!is.null(params[[n, exact = TRUE]])) params[[n, exact = TRUE]] else d
    ship_costs <- c(liquefaction = pv_num("co2_liquefaction_cost", 24.5), terminal = pv_num("co2_ship_terminal_cost", 15),
                    voyage = pv_num("co2_ship_voyage_cost", 0.010))

    if (!is.null(params$onsal_len_km)) {
      # v2 route layers: physical route length, route-average terrain cost multiplier and lift for each
      # sink class. The sink is chosen here, on transport + storage cost for this plant's CO2 flow.
      trans_args <- list(
        co2_mass = annual_co2_total, discount_rate = discount_rate, lifetime = bes_life,
        early_adoption = early_adoption, capex_factor = capex_loc, om_factor = om_loc,
        terrain_share = if (!is.null(params$co2_pipeline_terrain_share)) params$co2_pipeline_terrain_share else 1,
        elec_price = if (!is.null(params$elec_price)) params$elec_price else 0,
        ship_costs = ship_costs
      )
      zero_na <- function(x) ifelse_raster(is.na(x), 0, x)
      # Storage cost of the sink each route reaches (issue #103), scaled by storage_cost_factor for
      # Monte Carlo sampling; the regional parameter applies where the sink is unclassified
      storage_factor <- if (!is.null(params$storage_cost_factor)) params$storage_cost_factor else 1
      sink_storage <- function(layer, base) {
        if (is.null(layer)) return(base)
        ifelse_raster(is.na(layer), base, layer * storage_factor)
      }
      pipe_class <- function(cls) {
        len <- params[[paste0(cls, "_len_km"), exact = TRUE]]
        if (is.null(len)) return(list(cost = Inf, len = NA_real_))
        tc <- do.call(calculate_ccs_transport, c(trans_args, list(
          distance = zero_na(len),
          terrain_mult = params[[paste0(cls, "_terrain_mult"), exact = TRUE]],
          hrel_max_m = params[[paste0(cls, "_hrel_max_m"), exact = TRUE]]
        )))
        stor <- sink_storage(params[[paste0(cls, "_storage_cost"), exact = TRUE]], base_cost_onshore_storage)
        list(cost = ifelse_raster(is.na(len), Inf, tc + stor), len = len)
      }
      r_onsal <- pipe_class("onsal")
      r_oneor <- if (allow_eor) pipe_class("oneor") else list(cost = Inf, len = NA_real_)

      # Offshore sinks: by ship (pipeline to a port, liquefaction and terminal, voyage) or by pipeline
      # (onshore to a landfall, then subsea at co2_subsea_capex_factor x onshore CAPEX per km). Port and
      # landfall were chosen in the GIS step on land + sea cost. With EOR allowed, use the routes to any
      # offshore sink where they exist.
      off_layer <- function(cls, var) {
        if (allow_eor && !is.null(params[[paste0(cls, "_any_len_km"), exact = TRUE]])) cls <- paste0(cls, "_any")
        params[[paste0(cls, "_", var), exact = TRUE]]
      }
      no_route <- list(cost = Inf, len = NA_real_)
      r_ship <- no_route
      if (!is.null(off_layer("offship", "len_km"))) {
        land <- off_layer("offship", "len_km")
        sea <- off_layer("offship", "sea_km")
        tc <- do.call(calculate_ccs_transport, c(trans_args, list(
          distance = 0, is_offshore = TRUE, dist_coast = zero_na(land), dist_sea = zero_na(sea),
          terrain_mult = off_layer("offship", "terrain_mult"), hrel_max_m = off_layer("offship", "hrel_max_m")
        )))
        stor <- sink_storage(off_layer("offship", "storage_cost"), base_cost_offshore_storage)
        r_ship <- list(cost = ifelse_raster(is.na(land) | is.na(sea), Inf, tc + stor), len = land + sea)
      }
      r_pipe <- no_route
      if (!is.null(off_layer("offpipe", "len_km"))) {
        land <- zero_na(off_layer("offpipe", "len_km"))
        sea <- zero_na(off_layer("offpipe", "sea_km"))
        subsea_fac <- if (!is.null(params$co2_subsea_capex_factor)) params$co2_subsea_capex_factor else 1.5
        tm_land <- off_layer("offpipe", "terrain_mult")
        tm_land <- 1 + trans_args$terrain_share * (pmax_raster(ifelse_raster(is.na(tm_land), 1, tm_land), 1) - 1)
        # One pipeline over land + subsea length; subsea km carry the offshore CAPEX factor
        tm_all <- (land * tm_land + sea * subsea_fac) / pmax_raster(land + sea, 1e-9)
        tc <- do.call(calculate_ccs_transport, c(utils::modifyList(trans_args, list(terrain_share = 1)), list(
          distance = land + sea, terrain_mult = tm_all, hrel_max_m = off_layer("offpipe", "hrel_max_m")
        )))
        missing <- is.na(off_layer("offpipe", "len_km")) | is.na(off_layer("offpipe", "sea_km"))
        stor <- sink_storage(off_layer("offpipe", "storage_cost"), base_cost_offshore_storage)
        r_pipe <- list(cost = ifelse_raster(missing, Inf, tc + stor), len = land + sea)
      }

      ts_per_t <- pmin_raster(pmin_raster(r_onsal$cost, r_oneor$cost), pmin_raster(r_ship$cost, r_pipe$cost))
      co2_sink_class <- ifelse_raster(ts_per_t == r_onsal$cost, 1, ifelse_raster(ts_per_t == r_oneor$cost, 2,
        ifelse_raster(ts_per_t == r_ship$cost, 3, 4)))
      co2_sink_class <- ifelse_raster(is.infinite(ts_per_t), NA, co2_sink_class)
      co2_dist_chosen <- ifelse_raster(co2_sink_class == 1, r_onsal$len, ifelse_raster(co2_sink_class == 2, r_oneor$len,
        ifelse_raster(co2_sink_class == 3, r_ship$len, r_pipe$len)))
      ts_cost <- ts_per_t * co2_captured

      # CO2 transport emissions (t CO2 per t CO2 captured) of the chosen route: lift pumping electricity
      # at the displaced grid intensity for pipelines; liquefaction, loading and voyage for ships
      grid_t_mwh <- ff_c_intensity * 3.6
      lift_emis <- function(hrel) if (is.null(hrel)) 0 else zero_na(co2_lift_elec_mwh_per_t(hrel)) * grid_t_mwh
      e_onsal <- lift_emis(params$onsal_hrel_max_m)
      e_oneor <- lift_emis(params$oneor_hrel_max_m)
      e_ship <- ship_fixed + ship_km * zero_na(if (is.null(off_layer("offship", "sea_km"))) 0 else off_layer("offship", "sea_km")) +
        lift_emis(off_layer("offship", "hrel_max_m"))
      e_pipe <- lift_emis(off_layer("offpipe", "hrel_max_m"))
      co2_ts_emis_rate <- ifelse_raster(co2_sink_class == 1, e_onsal, ifelse_raster(co2_sink_class == 2, e_oneor,
        ifelse_raster(co2_sink_class == 3, e_ship, e_pipe)))
      co2_ts_emis_rate <- ifelse_raster(is.na(co2_sink_class), 0, co2_ts_emis_rate)
    } else {
      # v1 layers: least-cost distance to the nearest (saline) sink, with its onshore/offshore flag
      dist_onshore <- if (!is.null(params$dist_onshore)) params$dist_onshore else Inf
      dist_offshore <- if (!is.null(params$dist_offshore)) params$dist_offshore else Inf

      is_inf_onshore <- !inherits(dist_onshore, "SpatRaster") && is.infinite(dist_onshore)
      is_inf_offshore <- !inherits(dist_offshore, "SpatRaster") && is.infinite(dist_offshore)
      if (is_inf_onshore && is_inf_offshore && !is.null(params$ccs_distance)) {
        if (!is.null(params$sink_is_offshore)) {
          if (inherits(params$sink_is_offshore, "SpatRaster")) {
            dist_offshore <- terra::ifel(params$sink_is_offshore == 1, params$ccs_distance, Inf)
            dist_onshore <- terra::ifel(params$sink_is_offshore == 0, params$ccs_distance, Inf)
          } else {
            dist_offshore <- ifelse_raster(params$sink_is_offshore == 1, params$ccs_distance, Inf)
            dist_onshore <- ifelse_raster(params$sink_is_offshore == 0, params$ccs_distance, Inf)
          }
        } else {
          dist_onshore <- params$ccs_distance
        }
      }

      # Ship route legs for offshore sinks: pipeline to the coast, then sea voyage to the assigned sink.
      # When absent the ship cost falls back to a voyage over the full sink distance with no inland leg.
      dist_coast <- params$dist_coast_km
      dist_sea <- if (!allow_eor && !is.null(params$dist_sea_saline_km)) params$dist_sea_saline_km else params$dist_sea_km

      cost_onshore_trans <- calculate_ccs_transport(
        co2_mass = annual_co2_total,
        distance = dist_onshore,
        is_offshore = FALSE,
        discount_rate = discount_rate,
        lifetime = bes_life,
        early_adoption = early_adoption,
        capex_factor = capex_loc,
        om_factor = om_loc
      )
      ts_cost_onshore_calc <- (cost_onshore_trans + base_cost_onshore_storage) * co2_captured
      ts_cost_onshore <- ifelse_raster(is.infinite(dist_onshore), Inf, ts_cost_onshore_calc)

      cost_offshore_trans <- calculate_ccs_transport(
        co2_mass = annual_co2_total,
        distance = dist_offshore,
        is_offshore = TRUE,
        discount_rate = discount_rate,
        lifetime = bes_life,
        early_adoption = early_adoption,
        dist_coast = dist_coast,
        dist_sea = dist_sea,
        capex_factor = capex_loc,
        om_factor = om_loc,
        ship_costs = ship_costs
      )
      ts_cost_offshore_calc <- (cost_offshore_trans + base_cost_offshore_storage) * co2_captured
      ts_cost_offshore <- ifelse_raster(is.infinite(dist_offshore), Inf, ts_cost_offshore_calc)

      ts_cost <- pmin_raster(ts_cost_onshore, ts_cost_offshore)
      co2_sink_class <- ifelse_raster(ts_cost_onshore < ts_cost_offshore, 1, 3)
      co2_dist_chosen <- ifelse_raster(ts_cost_onshore < ts_cost_offshore, dist_onshore, dist_offshore)
      voyage_km <- if (is.null(dist_sea)) dist_offshore else dist_sea
      co2_ts_emis_rate <- ifelse_raster(co2_sink_class == 3, ship_fixed + ship_km * ifelse_raster(is.finite(voyage_km), voyage_km, 0), 0)
    }
    co2_transport_emissions <- co2_ts_emis_rate * co2_captured # Mg CO2 / Mg feed

    # 4. Plant Costs (CAPEX/OPEX)
    scaling_factor_val <- if (!is.null(params$scaling_factor)) scaling_factor else 0.7
    # Equivalent BES plant for the same thermal input, plus the capture/compression premium. bes_capital_cost
    # is a local (regional) value, so the CAPEX location factor is not applied (it still scales CO2 transport).
    total_capex <- combustion_plant_capex(bes_capital_cost, plant_mw_th, bes_capex_ref_eff, scaling_factor_val) *
      (1 + beccs_capex_premium)
    annuity_fac <- calculate_annuity_factor(discount_rate, bes_life)
    annual_capex_payment <- total_capex / annuity_fac

    capex_per_mg <- annual_capex_payment / annual_biomass
    # Annual O&M is a fraction of total CAPEX. Costs are levelised per year: discounting this constant
    # annual cost over the plant life and re-annualising at the same rate returns the annual value.
    opex_per_mg <- (total_capex * plant_om_fraction(params, "beccs_om_factor") * location_factor(params, "om")) / annual_biomass

    # --- 5. Logistics Cost & Transport Emissions ---
    logistics <- biomass_logistics(params)
    effective_dist <- logistics$effective_dist
    # Bottom ash returns to the fields as a backhaul, handled and spread like biochar (ash_return())
    ash_ret <- ash_return(params)
    logistics_cost <- logistics$cost + ash_ret$cost
    transport_emissions_co2e <- logistics$emissions + ash_ret$emissions

    feedstock_cost <- if (!is.null(params$feedstock_cost)) params$feedstock_cost else 0
    total_cost <- capex_per_mg + opex_per_mg + ts_cost + logistics_cost + feedstock_cost

    # 6. Revenue & Value
    energy_revenue <- energy_prod * elec_price

    # Carbon Abatement (CO2e conversion & transport penalty applied)
    co2e_sequestered <- bm_c * capture_rate * molar_ratio_c
    c_displaced <- energy_output * ff_c_intensity
    tot_c_abatement <- co2e_sequestered + c_displaced - transport_emissions_co2e - co2_transport_emissions +
      residue_counterfactual_ghg(params)
    abatement_value <- tot_c_abatement * c_price

    ash_value <- calculate_ash_value(params) # Recycled bottom ash (lime, P and K)
    total_revenue <- energy_revenue + ash_value + abatement_value
    net_value <- total_revenue - total_cost

    # Added diagnostics for factorial
    biomass_cost <- feedstock_cost + logistics_cost
    lcoe <- (capex_per_mg + opex_per_mg + ts_cost + biomass_cost - ash_value) / energy_prod
    cost_of_co2_avoided <- ifelse_raster(tot_c_abatement > 0, total_cost / tot_c_abatement, Inf)
    abatement_efficiency <- ifelse_raster(co2e_sequestered > 0, tot_c_abatement / co2e_sequestered, 0)
    total_capex_m <- total_capex / 1e6 # Convert to millions

    list(
      technology = "BECCS",
      energy_output = energy_output,
      energy_prod = energy_prod,
      c_sequestered = co2e_sequestered, # Now safely in CO2e
      tot_c_abatement = tot_c_abatement,
      total_cost = total_cost,
      ts_cost = ts_cost,
      total_revenue = total_revenue,
      net_value = net_value,
      # Granular outputs
      capital_cost_mg = capex_per_mg,
      om_cost_mg = opex_per_mg,
      biomass_cost_mg = biomass_cost,
      co2_transport_cost_mg = ts_cost,
      co2_transport_distance_km = co2_dist_chosen,
      co2_sink_class = co2_sink_class, # 1 onshore saline, 2 onshore EOR, 3 offshore by ship, 4 offshore by pipeline
      co2_transport_emissions = co2_transport_emissions, # Mg CO2 / Mg feed
      biomass_transport_distance_km = effective_dist,
      ash_return_cost_mg = ash_ret$cost, # included in biomass_cost_mg
      energy_revenue_mg = energy_revenue,
      abatement_revenue_mg = abatement_value,
      agronomic_revenue_mg = ash_value,
      lcoe = lcoe,
      cost_of_co2_avoided = cost_of_co2_avoided,
      abatement_efficiency = abatement_efficiency,
      total_capex_m = total_capex_m
    )
  })
}
