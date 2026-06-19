#' Run Spatial TEA Analysis
#'
#' Runs the Techno-Economic Assessment over a spatial grid defined by a template raster.
#' Calculates locally-specific metrics such as CO2 transport distance and plant scale
#' (based on biomass density).
#'
#' @param template_raster A `SpatRaster` (terra) defining the extent and resolution.
#' @param params A list of baseline parameters.
#' @param spatial_layers A named list of `SpatRaster` objects for spatially-varying parameters.
#'   Likely candidates: `soil_temp` (for Fperm), `biomass_density` (Mg/km2).
#' @param fun The TEA function to run (default: `calculate_beccs`).
#' @param collection_radius_km Radius of biomass collection zone to calculate plant scale
#'   (default: 50 km). Used with `biomass_density` to determine `plant_mw`.
#' @param use_flat_ci Logical. If TRUE, uses a flat rate for carbon intensity instead of spatial marginal CI.
#' @param flat_ci_gCO2_kWh Numeric. Flat carbon intensity rate in gCO2eq/kWh. Default is 12 (IPCC Nuclear).
#'
#' @return A `SpatRaster` with layers for key outputs (NPV, Total Cost, Abatement, etc.).
#' @export
#' @importFrom terra as.data.frame rast
# nolint start: indentation_linter, line_length_linter, object_usage_linter, commented_code_linter
run_spatial_tea <- function(template_raster, params, spatial_layers = list(),
                            fun = calculate_beccs, region = NULL, gis_dir = NULL) {
    if (!inherits(template_raster, "SpatRaster")) {
        stop("template_raster must be a terra SpatRaster object.")
    }

    # Determine tech name for resolving vector parameters
    tech_name <- NA
    if (identical(fun, BiocharAG::calculate_bes) || identical(fun, calculate_bes)) {
        tech_name <- "BES"
    } else if (identical(fun, BiocharAG::calculate_beccs) || identical(fun, calculate_beccs)) {
        tech_name <- "BECCS"
    } else if (identical(fun, BiocharAG::calculate_bebcs) || identical(fun, calculate_bebcs)) {
        tech_name <- "BEBCS"
    }

    plant_mw_th <- if (!is.null(params$plant_mw_th)) params$plant_mw_th else 50
    plant_mw_th <- resolve_plant_mw_th(plant_mw_th, tech_name)

    optimize_scale <- if (!is.null(params$optimize_scale)) params$optimize_scale else FALSE
    plant_sizes_mw_th <- if (!is.null(params$plant_sizes_mw_th)) params$plant_sizes_mw_th else c(5, 25, 50, 100, 250, 500)
    use_flat_ci <- if (!is.null(params$use_flat_ci)) params$use_flat_ci else FALSE
    flat_ci_tCO2_GJ <- if (!is.null(params$flat_ci_tCO2_GJ)) params$flat_ci_tCO2_GJ else 12 / 3600

    if (optimize_scale) {
        if (!"biomass_density" %in% names(spatial_layers)) {
            stop("biomass_density spatial layer is required for scale optimization.")
        }

        p <- params

        # Map spatial layers directly to SpatRaster parameters
        if ("soil_temp" %in% names(spatial_layers)) p$soil_temp <- spatial_layers$soil_temp
        if ("elec_price" %in% names(spatial_layers)) {
            factor <- if (!is.null(p$wholesale_discount_factor)) p$wholesale_discount_factor else 0.4
            p$elec_price <- spatial_layers$elec_price * factor
        }
        if ("soil_ph" %in% names(spatial_layers)) p$soil_ph <- spatial_layers$soil_ph
        if ("soil_cec" %in% names(spatial_layers)) p$soil_cec <- spatial_layers$soil_cec
        if ("dist_sink_km" %in% names(spatial_layers)) p$dist_sink_km <- spatial_layers$dist_sink_km
        if ("dist_sink_saline_km" %in% names(spatial_layers)) p$dist_sink_saline_km <- spatial_layers$dist_sink_saline_km
        if ("sink_is_offshore" %in% names(spatial_layers)) p$sink_is_offshore <- spatial_layers$sink_is_offshore

        if (use_flat_ci) {
            p$ff_c_intensity <- flat_ci_tCO2_GJ
        } else if ("ff_c_intensity" %in% names(spatial_layers)) {
            p$ff_c_intensity <- spatial_layers$ff_c_intensity
        }

        for (layer_name in c("cn_weather_risk", "cn_expansion_risk", "eu_base_eur", "us_base_cost")) {
            if (layer_name %in% names(spatial_layers)) p[[layer_name]] <- spatial_layers[[layer_name]]
        }

        dens <- spatial_layers$biomass_density

        if (is.null(p$bm_lhv)) p$bm_lhv <- 18.6
        capacity_factor <- 0.85

        npv_list <- list()
        tc_list <- list()
        abat_list <- list()
        ts_list <- list()

        message("Optimizing Scale for ", length(plant_sizes_mw_th), " sizes using raster algebra...")

        for (sz in plant_sizes_mw_th) {
            p_sz <- p
            p_sz$plant_mw_th <- sz

            # Use precalculated spatial transport distance
            dist_layer_name <- paste0("dist_", sz, "MWth")
            if (!dist_layer_name %in% names(spatial_layers)) {
                stop("Missing precalculated distance raster in spatial_layers: ", dist_layer_name)
            }
            p_sz$avg_dist <- spatial_layers[[dist_layer_name]]

            if (!is.null(region)) {
                p_sz$feedstock_cost <- calculate_regional_feedstock_cost(region, p_sz)
            }

            # Run TEA Math on SpatRasters
            res <- fun(p_sz)

            npv_list[[as.character(sz)]] <- if (inherits(res$net_value, "SpatRaster")) {
                res$net_value
            } else {
                terra::rast(template_raster, vals = res$net_value)
            }

            tc_list[[as.character(sz)]] <- if (inherits(res$total_cost, "SpatRaster")) {
                res$total_cost
            } else {
                terra::rast(template_raster, vals = res$total_cost)
            }

            abat_list[[as.character(sz)]] <- if (inherits(res$tot_c_abatement, "SpatRaster")) {
                res$tot_c_abatement
            } else {
                terra::rast(template_raster, vals = res$tot_c_abatement)
            }

            ts_list[[as.character(sz)]] <- if (!is.null(res$ts_cost)) {
                if (inherits(res$ts_cost, "SpatRaster")) {
                    res$ts_cost
                } else {
                    terra::rast(template_raster, vals = res$ts_cost)
                }
            } else {
                terra::rast(template_raster, nlyrs = 1, vals = NA)
            }
        }

        npv_stack <- terra::rast(npv_list)
        tc_stack <- terra::rast(tc_list)
        abat_stack <- terra::rast(abat_list)
        ts_stack <- terra::rast(ts_list)
        # Identify the index (1 to length(plant_sizes_mw_th)) of the layer with the maximum NPV for each pixel
        # This is the step that selects the optimal scale per pixel
        opt_idx <- terra::which.max(npv_stack)

        out_npv <- terra::rast(template_raster, nlyrs = 1, vals = NA)
        out_tc <- terra::rast(template_raster, nlyrs = 1, vals = NA)
        out_abat <- terra::rast(template_raster, nlyrs = 1, vals = NA)
        out_ts <- terra::rast(template_raster, nlyrs = 1, vals = NA)
        out_scale <- terra::rast(template_raster, nlyrs = 1, vals = NA)

        for (i in seq_along(plant_sizes_mw_th)) {
            sz <- plant_sizes_mw_th[i]
            mask_i <- (opt_idx == i)
            out_npv <- terra::ifel(mask_i, npv_stack[[i]], out_npv)
            out_tc <- terra::ifel(mask_i, tc_stack[[i]], out_tc)
            out_abat <- terra::ifel(mask_i, abat_stack[[i]], out_abat)
            out_ts <- terra::ifel(mask_i, ts_stack[[i]], out_ts)
            out_scale <- terra::ifel(mask_i, sz, out_scale)
        }

        out_r <- c(out_npv, out_tc, out_abat, out_ts, out_scale)
        names(out_r) <- c("Net_Value_USD", "Total_Cost_USD_Mg", "Abatement_tCO2", "Transport_Cost_USD_Mg", "Optimal_Plant_MW_th")

        # Apply Strict Biomass Mask (Removes Oceans, Lakes, and Zero-Biomass Deserts)
        bm_mask <- spatial_layers$biomass_density > 0
        out_r <- terra::mask(out_r, bm_mask, maskvalue = FALSE)
        return(out_r)
    }

    if (!optimize_scale) {
        # Determine target plant size, rounded to nearest 5 MW (min 5 MW)
        sz <- max(5, round(plant_mw_th / 5) * 5)
        params$plant_mw_th <- sz

        if ("biomass_density" %in% names(spatial_layers)) {
            dist_layer_name <- paste0("dist_", sz, "MWth")
            if (dist_layer_name %in% names(spatial_layers)) {
                spatial_layers$avg_dist <- spatial_layers[[dist_layer_name]]
            } else {
                if (is.null(region)) {
                    stop("region must be provided to dynamically generate a missing distance raster.")
                }
                # Generate missing distance raster dynamically
                spatial_layers$avg_dist <- calculate_distance_raster(
                    dens_wgs84 = spatial_layers$biomass_density,
                    target_mw_th = sz,
                    region = region,
                    gis_dir = gis_dir,
                    save_to_disk = !is.null(gis_dir)
                )
            }
        }
    }

    # Map spatial layers directly to SpatRaster parameters
    p <- params
    if ("soil_temp" %in% names(spatial_layers)) p$soil_temp <- spatial_layers$soil_temp
    if ("elec_price" %in% names(spatial_layers)) {
        factor <- if (!is.null(p$wholesale_discount_factor)) p$wholesale_discount_factor else 0.4
        p$elec_price <- spatial_layers$elec_price * factor
    }
    if ("soil_ph" %in% names(spatial_layers)) p$soil_ph <- spatial_layers$soil_ph
    if ("soil_cec" %in% names(spatial_layers)) p$soil_cec <- spatial_layers$soil_cec
    if ("dist_sink_km" %in% names(spatial_layers)) p$dist_sink_km <- spatial_layers$dist_sink_km
    if ("dist_sink_saline_km" %in% names(spatial_layers)) p$dist_sink_saline_km <- spatial_layers$dist_sink_saline_km
    if ("sink_is_offshore" %in% names(spatial_layers)) p$sink_is_offshore <- spatial_layers$sink_is_offshore
    if ("avg_dist" %in% names(spatial_layers)) p$avg_dist <- spatial_layers$avg_dist

    if (use_flat_ci) {
        p$ff_c_intensity <- flat_ci_tCO2_GJ
    } else if ("ff_c_intensity" %in% names(spatial_layers)) {
        p$ff_c_intensity <- spatial_layers$ff_c_intensity
    }

    # Map additional spatial layers for feedstock cost logic
    for (layer_name in c("cn_weather_risk", "cn_expansion_risk", "eu_base_eur", "us_base_cost")) {
        if (layer_name %in% names(spatial_layers)) p[[layer_name]] <- spatial_layers[[layer_name]]
    }

    if (!is.null(region)) {
        p$feedstock_cost <- calculate_regional_feedstock_cost(region, p)
    }

    # Run TEA Math on SpatRasters directly
    res <- fun(p)

    # Rasterize constants and extract results
    out_npv <- if (inherits(res$net_value, "SpatRaster")) res$net_value else terra::rast(template_raster, vals = res$net_value)
    out_tc <- if (inherits(res$total_cost, "SpatRaster")) res$total_cost else terra::rast(template_raster, vals = res$total_cost)
    out_abat <- if (inherits(res$tot_c_abatement, "SpatRaster")) res$tot_c_abatement else terra::rast(template_raster, vals = res$tot_c_abatement)
    out_ts <- if (!is.null(res$ts_cost)) {
        if (inherits(res$ts_cost, "SpatRaster")) res$ts_cost else terra::rast(template_raster, vals = res$ts_cost)
    } else {
        terra::rast(template_raster, nlyrs = 1, vals = NA)
    }

    out_r <- c(out_npv, out_tc, out_abat, out_ts)
    names(out_r) <- c("Net_Value_USD", "Total_Cost_USD_Mg", "Abatement_tCO2", "Transport_Cost_USD_Mg")

    # Apply Strict Biomass Mask (Removes Oceans, Lakes, and Zero-Biomass Deserts) if available
    if ("biomass_density" %in% names(spatial_layers)) {
        bm_mask <- spatial_layers$biomass_density > 0
        out_r <- terra::mask(out_r, bm_mask, maskvalue = FALSE)
    }

    return(out_r)
}

#' Calculate Spatially Explicit Feedstock Cost
#'
#' Implements regional pricing logic for crop residues based on spatial shadow pricing.
#' Converts regional currencies to a normalized USD/Mg value for NPV calculations.
#'
#' @param region Character string: "US", "EU", "India", or "China".
#' @param params List of parameters including base prices, distances, and local flags.
#' @return Delivered feedstock cost in USD/Mg.
#' @export
calculate_regional_feedstock_cost <- function(region, params) {
    # Standard Exchange Rates (Defaults to be overridden by global params if needed)
    xr_eur <- if (!is.null(params$xr_eur)) params$xr_eur else 1.10
    xr_inr <- if (!is.null(params$xr_inr)) params$xr_inr else 0.012 # ~1/83
    xr_cny <- if (!is.null(params$xr_cny)) params$xr_cny else 0.14 # ~1/7.2

    cost_usd <- 0

    if (region == "US") {
        # Baseline US Farm-gate
        base_cost <- if (!is.null(params$us_base_cost)) params$us_base_cost else 70.0

        # Nutrient Replacement (average ~$25.06/Mg for corn stover)
        nutrient_cost <- if (!is.null(params$us_nutrient_cost)) params$us_nutrient_cost else 25.06

        cost_usd <- base_cost + nutrient_cost
    } else if (region == "EU") {
        # Baseline NUTS-3 Road-side cost (EUR)
        base_eur <- if (!is.null(params$eu_base_eur)) params$eu_base_eur else 40.0

        # Temporal Volatility: Interim Storage Cost Multiplier
        # 3 months = +22.2%, 6 months = +36.4%
        storage_months <- if (!is.null(params$eu_storage_months)) params$eu_storage_months else 6
        storage_mult <- ifelse_raster(storage_months >= 6, 1.364, 1.222)

        cost_usd <- (base_eur * storage_mult) * xr_eur
    } else if (region == "India") {
        # Paddy straw focus. Excludes high-value wheat straw fodder.
        # Uses avg_dist generated by the distance rasters
        distance_km <- if (!is.null(params$avg_dist)) params$avg_dist else 25.0

        # Optimal zone vs Distance Penalty
        cost_inr_bale <- if (!is.null(params$inr_bale_cost)) params$inr_bale_cost else 2750
        cost_inr_pellet <- if (!is.null(params$inr_pellet_cost)) params$inr_pellet_cost else 5200

        cost_inr <- ifelse_raster(distance_km <= 50, cost_inr_bale, cost_inr_pellet)

        cost_usd <- cost_inr * xr_inr
    } else if (region == "China") {
        # Baseline plant-gate cost (CNY), incorporating rural broker discounts
        base_cny <- if (!is.null(params$cn_base_cny)) params$cn_base_cny else 250

        # Exogenous Risk Modifiers
        weather_risk_val <- if (!is.null(params$cn_weather_risk)) params$cn_weather_risk else FALSE
        expansion_risk_val <- if (!is.null(params$cn_expansion_risk)) params$cn_expansion_risk else FALSE

        weather_risk <- ifelse_raster(weather_risk_val, 1.13, 1.0)
        expansion_risk <- ifelse_raster(expansion_risk_val, 1.53, 1.0)

        cost_usd <- (base_cny * weather_risk * expansion_risk) * xr_cny
    } else {
        stop("Region not supported. Use US, EU, India, or China.")
    }

    return(cost_usd)
}
