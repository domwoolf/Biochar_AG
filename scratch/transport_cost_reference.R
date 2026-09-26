# nolint start: line_length_linter, object_usage_linter
# ==============================================================================
# transport_cost_reference.R
# ------------------------------------------------------------------------------
# Reference cost functions for the layers written by process_transport_layers.R
# (<prefix>_transport_layers.tif). Intended as a template for updating
# calculate_ccs_transport(); keep your own unit costs and parameter table.
#
# Principles
#   * Every length that enters a cost function is a PHYSICAL route length
#     (<class>_len_km). The hub-and-spoke breakpoints (50 km, 700 km) are
#     physical, so they stay physical.
#   * Terrain enters as a CAPEX multiplier: terrain_mult = costlen_km / len_km,
#     the route-average construction-cost factor measured along the chosen route.
#   * Elevation enters through hydraulics (hrel_max_m = highest point on the
#     route above the source), not through length.
#   * Sink choice (onshore saline / EOR / offshore via port) is made HERE, on
#     total transport + injection cost, not in the GIS step.
#
# All money in one consistent currency-year. All functions are vectorised.
# NOTE: written without access to an R session; check with the toy example at
# the bottom before wiring into the model.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Pipeline CAPEX: hub-and-spoke on physical length, terrain as a multiplier
# ------------------------------------------------------------------------------
#' @param len_km        physical route length (km)
#' @param terrain_mult  route-average construction-cost multiplier (>= 1)
#' @param feeder_per_km  $/km of a dedicated feeder sized for this source
#'                       (your capacity-scaled CAPEX; scalar or vector)
#' @param trunk_share_per_km $/km share of an efficient-scale trunk line
#'                       attributable to this source (C_base scaled to mass share)
#' @param terrain_share  fraction of per-km CAPEX that scales with terrain.
#'                       1 = multiplier applies to all of it (consistent with how
#'                       the IEA-type terrain factors are usually quoted);
#'                       < 1 if your unit costs already include some allowance.
#' @return list(feeder, trunk, total) CAPEX in $
pipeline_capex <- function(len_km, terrain_mult, feeder_per_km, trunk_share_per_km,
                           d_feeder_km = 50, d_booster_km = 700, P_booster = 2,
                           terrain_share = 1) {
    L <- pmax(0, len_km)
    tm <- 1 + terrain_share * (pmax(1, terrain_mult) - 1)
    L_feed <- pmin(L, d_feeder_km)
    L_base <- pmax(0, pmin(L, d_booster_km) - d_feeder_km)
    L_boost <- pmax(0, L - d_booster_km)
    feeder <- feeder_per_km * L_feed * tm
    trunk <- trunk_share_per_km * (L_base + P_booster * L_boost) * tm
    list(feeder = feeder, trunk = trunk, total = feeder + trunk)
}

# ------------------------------------------------------------------------------
# 2. Elevation hydraulics: extra pressure to lift dense-phase CO2 over the route
# ------------------------------------------------------------------------------
#' Dense-phase CO2 (rho ~ 800-950 kg/m3) needs ~0.8-0.9 MPa per 100 m of lift.
#' The pipeline must arrive at its highest point above the minimum operating
#' pressure, so the requirement depends on the highest point above the source
#' (hrel_max_m), not on the net source-sink elevation difference.
#'
#' @param hrel_max_m  highest route elevation above the source (m); <0 -> 0
#' @param mass_tpy    CO2 mass through the source's pipe (t/yr)
#' @param station_capex_fun function(n_stations, mass_tpy) -> $ ; REQUIRED,
#'        e.g. from the NETL CO2 Transport Cost Model booster-pump costing
#' @param dP_station_MPa pressure one booster station restores (~ operating window)
#' @param dP_margin_MPa  head already covered by the design inlet-pressure margin
#' @param fractional  TRUE returns a continuous station count (smooth NPV maps);
#'                    FALSE rounds up to whole stations
#' @param elec_price  $/MWh
#' @return list(n_stations, capex, elec_MWh_per_t, opex_energy_per_yr)
elevation_booster <- function(hrel_max_m, mass_tpy, station_capex_fun,
                              rho = 900, dP_station_MPa = 5, dP_margin_MPa = 0,
                              pump_eff = 0.75, elec_price = 0, fractional = TRUE) {
    H <- pmax(0, hrel_max_m)
    H[!is.finite(H)] <- 0
    dP_MPa <- rho * 9.81 * H / 1e6
    n <- pmax(0, dP_MPa - dP_margin_MPa) / dP_station_MPa
    if (!fractional) n <- ceiling(n - 1e-9)
    capex <- ifelse(n > 0, station_capex_fun(n, mass_tpy), 0)
    # Lift energy per tonne: m g H / eta, J -> MWh (1 MWh = 3.6e9 J)
    elec_MWh_per_t <- 1000 * 9.81 * H / pump_eff / 3.6e9
    list(n_stations = n, capex = capex, elec_MWh_per_t = elec_MWh_per_t,
         opex_energy_per_yr = elec_MWh_per_t * mass_tpy * elec_price)
}

# ------------------------------------------------------------------------------
# 3. Levelised $/t (for sink selection and diagnostics only; the NPV model
#    should use the CAPEX and annual OPEX components directly)
# ------------------------------------------------------------------------------
crf <- function(rate, years) rate * (1 + rate)^years / ((1 + rate)^years - 1)

levelised_per_t <- function(capex, opex_per_yr, mass_tpy, crf_value, om_frac = 0.025) {
    (capex * (crf_value + om_frac) + opex_per_yr) / pmax(mass_tpy, 1e-9)
}

# ------------------------------------------------------------------------------
# 4. Sink selection on total cost
# ------------------------------------------------------------------------------
#' @param df data.frame of layer values (e.g. terra::values(stack, dataframe = TRUE))
#'        plus a mass_tpy column (captured CO2, t/yr) for each cell
#' @param p  list of cost parameters, see example below
#' @return data.frame: class, cost_per_t, physical length, sink flags, components
select_sink_by_cost <- function(df, p) {
    m <- df$mass_tpy
    k <- crf(p$discount_rate, p$lifetime_yr)

    pipe_leg <- function(prefix) {
        L <- df[[paste0(prefix, "_len_km")]]
        tm <- df[[paste0(prefix, "_terrain_mult")]]
        H <- df[[paste0(prefix, "_hrel_max_m")]]
        if (is.null(L)) return(list(per_t = rep(Inf, nrow(df)), L = rep(NA_real_, nrow(df))))
        tm[!is.finite(tm)] <- 1
        pc <- pipeline_capex(L, tm, p$feeder_per_km(m), p$trunk_share_per_km(m),
                             p$d_feeder_km, p$d_booster_km, p$P_booster, p$terrain_share)
        eb <- elevation_booster(H, m, p$station_capex_fun, p$rho, p$dP_station_MPa,
                                p$dP_margin_MPa, p$pump_eff, p$elec_price, p$fractional_stations)
        per_t <- levelised_per_t(pc$total + eb$capex, eb$opex_energy_per_yr, m, k, p$om_frac)
        per_t[!is.finite(L)] <- Inf
        list(per_t = per_t, L = L, capex = pc$total + eb$capex, n_boost = eb$n_stations)
    }

    on_sal <- pipe_leg("onsal")
    c_onsal <- on_sal$per_t + p$ccs_storage_cost

    c_oneor <- rep(Inf, nrow(df))
    on_eor <- list(L = rep(NA_real_, nrow(df)))
    if (isTRUE(p$allow_eor) && "oneor_len_km" %in% names(df)) {
        on_eor <- pipe_leg("oneor")
        c_oneor <- on_eor$per_t + p$ccs_storage_cost - p$eor_credit_per_t
    }

    c_off <- rep(Inf, nrow(df))
    coast <- list(L = rep(NA_real_, nrow(df)))
    if ("offcoast_len_km" %in% names(df)) {
        coast <- pipe_leg("offcoast")
        sea_km <- if (isTRUE(p$allow_eor)) df$off_sea_km_any else df$off_sea_km_saline
        c_off <- coast$per_t + p$liquefaction_per_t + p$terminal_per_t +
            p$voyage_per_t_km * sea_km + p$cost_offshore_storage
        c_off[!is.finite(c_off)] <- Inf
    }

    C <- cbind(onsal = c_onsal, oneor = c_oneor, offshore = c_off)
    best <- max.col(-replace(C, !is.finite(C), 1e12), ties.method = "first")
    none <- rowSums(is.finite(C)) == 0
    cls <- colnames(C)[best]
    cls[none] <- NA_character_
    out <- data.frame(
        sink_class = cls,
        cost_per_t = ifelse(none, NA_real_, C[cbind(seq_len(nrow(C)), best)]),
        pipe_len_km = ifelse(cls == "onsal", on_sal$L, ifelse(cls == "oneor", on_eor$L, coast$L)),
        sink_is_offshore = cls == "offshore",
        cost_onsal = c_onsal, cost_oneor = c_oneor, cost_offshore = c_off
    )
    out
}

# ------------------------------------------------------------------------------
# 5. Toy example (placeholder numbers, NOT recommended values)
# ------------------------------------------------------------------------------
if (FALSE) {
    p <- list(
        discount_rate = 0.07, lifetime_yr = 25, om_frac = 0.025,
        # Placeholders: replace with your @tbl-parameters cost scaling
        feeder_per_km = function(m) 0.9e6 * (m / 1e6)^0.5,
        trunk_share_per_km = function(m) 2.0e6 * (m / 3e6),
        d_feeder_km = 50, d_booster_km = 700, P_booster = 2, terrain_share = 1,
        station_capex_fun = function(n, m) n * 8e6 * (m / 1e6)^0.6, # placeholder
        rho = 900, dP_station_MPa = 5, dP_margin_MPa = 1, pump_eff = 0.75,
        elec_price = 60, fractional_stations = TRUE,
        ccs_storage_cost = 10, cost_offshore_storage = 20,
        allow_eor = FALSE, eor_credit_per_t = 0,
        liquefaction_per_t = 12, terminal_per_t = 4, voyage_per_t_km = 0.01
    )
    df <- data.frame(
        mass_tpy = c(2e5, 2e5, 2e5),
        onsal_len_km = c(120, 480, 900), onsal_terrain_mult = c(1.02, 1.35, 1.10),
        onsal_hrel_max_m = c(40, 1600, 200),
        offcoast_len_km = c(300, 150, 250), offcoast_terrain_mult = c(1.0, 1.2, 1.05),
        offcoast_hrel_max_m = c(0, 300, 50),
        off_sea_km_saline = c(400, 600, 350)
    )
    print(select_sink_by_cost(df, p))
    # Hydraulic sanity check: 1,000 m of lift ~ 8.8 MPa and ~3.6 kWh/t
    str(elevation_booster(1000, 1e6, function(n, m) 0, pump_eff = 0.75))
}
# nolint end
