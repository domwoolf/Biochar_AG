#' Calculate CCS Transport Cost
#'
#' Onshore sinks are reached by pipeline. Offshore sinks are reached by ship: a pipeline leg from
#' the source to the port, liquefaction and port terminal, and a sea voyage to the sink.
#' Pipelines use a hub-and-spoke power-law cost model: CAPEX scales with distance and with capacity to
#' the power `pipe_costs[["scale"]]` (see [pipeline_cost_params()]).
#'
#' With route layers from `data-raw/process_transport_layers.R` (v2), `distance` and `dist_coast` are
#' physical route lengths, terrain enters as a CAPEX multiplier (`terrain_mult`, the construction-cost
#' factor averaged along the chosen route), and elevation enters through the booster pumping needed to
#' lift dense-phase CO2 over the highest point of the route (`hrel_max_m`). With the older least-cost
#' distance layers (v1), leave `terrain_mult = 1` and `hrel_max_m = NULL`.
#'
#' @param co2_mass Annual CO2 mass to transport (Mg/year).
#' @param distance Pipeline length to the sink (km); used for onshore sinks, and as the voyage distance
#'   for offshore sinks when `dist_sea` is not supplied.
#' @param is_offshore Logical (scalar, vector or raster); TRUE for offshore sinks.
#' @param discount_rate Discount rate (decimal). Default 0.10.
#' @param lifetime Project lifetime (years). Default 20.
#' @param early_adoption Logical. If TRUE, pipelines are sized to the single facility over the entire
#'   distance (no shared trunkline). Default FALSE.
#' @param dist_coast Pipeline length from the source to the export port (km) for ship transport. Default 0.
#' @param dist_sea Sea voyage distance from the port to the offshore sink (km). Defaults to `distance`.
#' @param capex_factor Regional CAPEX location factor (pipelines, pumps, liquefaction and terminals).
#' @param om_factor Regional O&M location factor (pipeline and pump O&M fraction).
#' @param terrain_mult Route-average pipeline construction-cost multiplier (>= 1; NA treated as 1).
#' @param terrain_share Fraction of pipeline CAPEX that scales with terrain. Default 1.
#' @param hrel_max_m Highest point of the pipeline route above the source (m). NULL = no lift cost.
#' @param elec_price Electricity price for booster pumping ($/MWh). Default 0.
#' @param ship_costs Named ship transport costs (2024 USD): `liquefaction` and `terminal` ($/t, scaled by
#'   `capex_factor`) and `voyage` ($/t/km). Defaults from parameters.csv (`co2_liquefaction_cost`,
#'   `co2_ship_terminal_cost`, `co2_ship_voyage_cost`).
#' @param pipe_costs Named pipeline cost parameters from [pipeline_cost_params()].
#' @return Transport cost ($/Mg CO2).
#' @export
calculate_ccs_transport <- function(co2_mass, distance, is_offshore = FALSE, discount_rate = 0.10, lifetime = 20,
                                    early_adoption = FALSE, dist_coast = NULL, dist_sea = NULL,
                                    capex_factor = 1, om_factor = 1, terrain_mult = 1, terrain_share = 1,
                                    hrel_max_m = NULL, elec_price = 0,
                                    ship_costs = c(liquefaction = 24.5, terminal = 15, voyage = 0.010),
                                    pipe_costs = pipeline_cost_params(list())) {
  safe_co2_mass <- pmax(co2_mass, 1e-6)
  annuity_fac <- (1 - (1 + discount_rate)^(-lifetime)) / discount_rate
  opex_factor <- pipe_costs[["om"]] * om_factor
  tm <- ifelse_raster(is.na(terrain_mult), 1, terrain_mult)
  tm <- 1 + terrain_share * (pmax_raster(tm, 1) - 1)

  pipeline_cost <- function(dist) {
    ref_mass <- 1000000
    ref_dist <- 100
    base_capex_ref <- pipe_costs[["capex_ref"]] * ref_dist * capex_factor # US$ per ref_dist km at 1 Mt/yr
    scale_factor <- pipe_costs[["scale"]]
    feeder_threshold_km <- pipe_costs[["feeder_km"]]
    booster_threshold_km <- pipe_costs[["booster_km"]]
    booster_penalty <- pipe_costs[["booster_mult"]]
    trunk_mass_flow <- pmax(safe_co2_mass, pipe_costs[["trunk_flow"]])

    # Path A: hub-and-spoke (dedicated feeder, then a share of a regional trunkline)
    capex_f <- base_capex_ref * (feeder_threshold_km / ref_dist) * (safe_co2_mass / ref_mass)^scale_factor
    scaler_t <- (trunk_mass_flow / ref_mass)^scale_factor
    capex_t_std <- base_capex_ref * ((dist - feeder_threshold_km) / ref_dist) * scaler_t
    capex_t_base <- base_capex_ref * ((booster_threshold_km - feeder_threshold_km) / ref_dist) * scaler_t
    capex_t_booster <- (base_capex_ref * booster_penalty) * ((dist - booster_threshold_km) / ref_dist) * scaler_t
    capex_t_total <- ifelse_raster(dist <= booster_threshold_km, capex_t_std, capex_t_base + capex_t_booster)
    total_capex_share_far <- capex_f + capex_t_total * (safe_co2_mass / trunk_mass_flow)

    # Path B: dedicated direct pipeline
    total_capex_share_close <- base_capex_ref * (dist / ref_dist) * (safe_co2_mass / ref_mass)^scale_factor

    total_capex_share <- ifelse_raster(
      early_adoption,
      total_capex_share_close,
      ifelse_raster(dist > feeder_threshold_km, total_capex_share_far, total_capex_share_close)
    ) * tm
    lift <- if (is.null(hrel_max_m)) 0 else co2_lift_cost(safe_co2_mass, hrel_max_m, annuity_fac, opex_factor, capex_factor, elec_price)
    (total_capex_share / annuity_fac + total_capex_share * opex_factor) / safe_co2_mass + lift
  }

  # Ship transport: pipeline to the port, liquefaction and terminal, then voyage.
  # With v2 layers the port and land route are chosen jointly on land + sea cost (#17). Without
  # dist_coast/dist_sea (v1 layers) the inland leg is zero and the voyage is priced over the full
  # (friction-weighted) distance to the sink.
  ship_cost <- function() {
    coast <- if (is.null(dist_coast)) 0 else dist_coast
    sea <- if (is.null(dist_sea)) distance else dist_sea
    cost_liq_term <- (ship_costs[["liquefaction"]] + ship_costs[["terminal"]]) * capex_factor
    pipeline_cost(coast) + cost_liq_term + ship_costs[["voyage"]] * sea
  }

  if (isTRUE(all(is_offshore))) {
    final_cost <- ship_cost()
  } else if (isTRUE(any(is_offshore))) {
    final_cost <- ifelse_raster(is_offshore, ship_cost(), pipeline_cost(distance))
  } else {
    final_cost <- pipeline_cost(distance)
  }

  return(ifelse_raster(co2_mass <= 0, 0, final_cost))
}

#' Booster Pumping Electricity for Lifting CO2
#'
#' Electricity to restore the pressure lost lifting dense-phase CO2 over the highest point of a
#' pipeline route, beyond the design inlet margin (see `co2_lift_cost()`).
#'
#' @inheritParams co2_lift_cost
#' @return Electricity use (MWh per Mg CO2).
#' @keywords internal
co2_lift_elec_mwh_per_t <- function(hrel_max_m, rho = 900, dp_margin_mpa = 1, pump_eff = 0.75) {
  h <- ifelse_raster(is.na(hrel_max_m), 0, pmax_raster(hrel_max_m, 0))
  dp_pa <- pmax_raster(rho * 9.81 * h - dp_margin_mpa * 1e6, 0)
  1000 * dp_pa / (rho * pump_eff) / 3.6e9
}

#' Booster Pumping Cost of Lifting CO2 Over a Pipeline Route
#'
#' Dense-phase CO2 loses about 0.9 MPa per 100 m of lift, so a pipeline must arrive at the highest
#' point of its route above the minimum operating pressure. The pressure needed beyond the design
#' inlet margin is restored by booster pump stations (fractional count, for smooth cost surfaces).
#' Pump CAPEX follows McCollum & Ogden (2006): $1.11M per MW of pump power plus $0.07M per station
#' (2005 USD, escalated to 2024 USD by `cpi_2005`).
#'
#' @param co2_mass Annual CO2 mass (Mg/year).
#' @param hrel_max_m Highest route elevation above the source (m); negative or NA = no lift.
#' @param annuity_fac Annuity factor for CAPEX.
#' @param opex_factor Annual O&M as a fraction of CAPEX.
#' @param capex_factor Regional CAPEX location factor.
#' @param elec_price Electricity price ($/MWh).
#' @param rho Dense-phase CO2 density (kg/m3).
#' @param dp_margin_mpa Head covered by the design inlet-pressure margin (MPa).
#' @param dp_station_mpa Pressure restored by one booster station (MPa).
#' @param pump_eff Pump efficiency.
#' @param cpi_2005 Escalation from 2005 to 2024 USD (US CPI-U).
#' @return Lift cost ($/Mg CO2).
#' @keywords internal
co2_lift_cost <- function(co2_mass, hrel_max_m, annuity_fac, opex_factor, capex_factor = 1, elec_price = 0,
                          rho = 900, dp_margin_mpa = 1, dp_station_mpa = 5, pump_eff = 0.75, cpi_2005 = 1.6) {
  h <- ifelse_raster(is.na(hrel_max_m), 0, pmax_raster(hrel_max_m, 0))
  dp_pa <- pmax_raster(rho * 9.81 * h - dp_margin_mpa * 1e6, 0)
  n_stations <- dp_pa / (dp_station_mpa * 1e6)
  mass_kg_s <- co2_mass * 1000 / (8760 * 3600)
  pump_mw <- mass_kg_s * dp_pa / (rho * pump_eff) / 1e6
  capex <- (1.11e6 * pump_mw + 0.07e6 * n_stations) * cpi_2005 * capex_factor
  elec_mwh_per_t <- 1000 * dp_pa / (rho * pump_eff) / 3.6e9
  (capex / annuity_fac + capex * opex_factor) / co2_mass + elec_mwh_per_t * elec_price
}

#' Pipeline Cost Parameters
#'
#' Capital cost per km of a pipeline carrying 1 Mt CO2/yr (`co2_pipe_capex_km`, US$/km, scaled by the
#' regional CAPEX location factor), the flow-scaling exponent (`co2_pipe_scale_exp`), the annual O&M
#' fraction of CAPEX (`co2_pipe_om_frac`), the route length beyond which booster pumping raises the
#' marginal CAPEX (`co2_pipe_booster_km`) and its multiplier (`co2_pipe_booster_mult`), the design flow of
#' shared trunklines (`co2_trunk_flow`, Mg/yr) and the length of the dedicated feeder (`co2_feeder_km`).
#' Sources in parameters.csv (issue #64).
#'
#' @param params Parameter list.
#' @return Named numeric vector.
#' @export
pipeline_cost_params <- function(params) {
  pv <- function(n, d) if (!is.null(params[[n, exact = TRUE]])) params[[n, exact = TRUE]] else d
  c(capex_ref = pv("co2_pipe_capex_km", 5e5), scale = pv("co2_pipe_scale_exp", 0.5), om = pv("co2_pipe_om_frac", 0.04),
    booster_km = pv("co2_pipe_booster_km", 700), booster_mult = pv("co2_pipe_booster_mult", 2),
    trunk_flow = pv("co2_trunk_flow", 3e6), feeder_km = pv("co2_feeder_km", 50))
}
