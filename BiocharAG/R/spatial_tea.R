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
        for (nm in intersect(transport_layer_names(), names(spatial_layers))) p[[nm]] <- spatial_layers[[nm]]

        if (use_flat_ci) {
            p$ff_c_intensity <- flat_ci_tCO2_GJ
        } else if ("ff_c_intensity" %in% names(spatial_layers)) {
            p$ff_c_intensity <- spatial_layers$ff_c_intensity
        }

        for (layer_name in c("cn_weather_risk", "eu_feedstock_usd", "us_base_cost")) {
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
            p_sz <- attach_size_layers(p_sz, spatial_layers, sz)

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
    for (nm in intersect(transport_layer_names(), names(spatial_layers))) p[[nm]] <- spatial_layers[[nm]]
    if ("avg_dist" %in% names(spatial_layers)) p$avg_dist <- spatial_layers$avg_dist
    if (!optimize_scale) {
        for (nm in c("kt", "kd", "g")) {
            ln <- paste0("haul_", nm, "_", params$plant_mw_th)
            if (ln %in% names(spatial_layers)) p[[paste0("haul_", nm)]] <- spatial_layers[[ln]]
        }
    }

    if (use_flat_ci) {
        p$ff_c_intensity <- flat_ci_tCO2_GJ
    } else if ("ff_c_intensity" %in% names(spatial_layers)) {
        p$ff_c_intensity <- spatial_layers$ff_c_intensity
    }

    # Map additional spatial layers for feedstock cost logic
    for (layer_name in c("cn_weather_risk", "eu_feedstock_usd", "us_base_cost")) {
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

#' Calculate Regional Feedstock Cost
#'
#' Field-side (farm-gate or road-side) purchase price of crop residues plus interim storage, in 2024 USD
#' per Mg. Haulage from field to plant is costed separately (`biomass_logistics()`), so delivered or
#' plant-gate prices are not used here (issues #52, #53, #101). Sources and conversions:
#' - **US:** US DOE (2011) Billion-Ton Update (Perlack & Stokes): about 90% of the primary crop residue
#'   supply is profitable at a farm-gate price of $50 per dry short ton (2011 USD assumed) = $55.1/Mg x
#'   1.395 (CPI-U) = 76.9. The farm-gate price includes the grower payment for nutrient removal, so no
#'   separate nutrient charge is added.
#' - **Europe:** S2Biom road-side costs of cereal straw (Dees et al. 2017) aggregated to country level
#'   (EUR 20 / 37.5 / 62.5 per t dm, 2012 EUR; layer `eu_feedstock_usd` from data-raw/process_eu_feedstock.R)
#'   x 1.285 USD/EUR (2012) x 1.366 (CPI-U) = 35.1 / 65.8 / 109.7. Cells without a country value, and runs
#'   without the layer, use EUR 40/t dm = 70.2.
#' - **China:** field-side supply cost from the StrawFeed model (Wang et al. 2022; Nongan, Jilin, 2018-19):
#'   raking 1.0 + baling 84.3 + loading 14.3 = CNY 99.6/t (excluding transport, CNY 72.5/t), / 6.908 CNY/USD
#'   (2019) x 1.227 (CPI-U) = 17.7. Cost basis (no farmer or broker margin), consistent with the S2Biom
#'   (Europe) and Sokhansanj et al. (India) costs. Optional weather risk multiplier x1.13 (Wang et al. 2022).
#' - **India:** `india_feedstock_cost` = 34 $/Mg: baled paddy straw at the field side, dry basis (Sokhansanj
#'   et al. 2023, $33.14/t dm, 2023 USD), sampled 12-50 (paddy straw value where burned, Erenstein 2011, to
#'   fodder-market straw prices, Duncan et al. 2020; Lopes et al. 2023).
#' - **Storage (all regions):** `feedstock_storage_cost` ($/Mg, US/EU basis, about six months) scaled by
#'   `haulage_location_factor` (labour and equipment), see `parameters.csv`.
#'
#' @param region Character string: "US", "EU"/"Europe", "India", or "China".
#' @param params List of parameters; optional overrides `us_base_cost` ($/Mg, 2024 USD), `eu_base_eur`
#'   (EUR/t dm, 2012 EUR; fallback), `eu_feedstock_usd` (layer, USD/Mg), `cn_base_cny` (CNY/t, 2019), `cn_weather_risk`, `india_feedstock_cost` ($/Mg,
#'   2024 USD), `feedstock_storage_cost`, `haulage_location_factor`.
#' @return Field-side feedstock cost including storage, USD/Mg (2024 USD).
#' @export
calculate_regional_feedstock_cost <- function(region, params) {
    cost_usd <- 0

    if (region %in% c("US", "USA")) {
        cost_usd <- if (!is.null(params$us_base_cost)) params$us_base_cost else 76.9
    } else if (region %in% c("EU", "Europe")) {
        # Country-level S2Biom costs (layer eu_feedstock_usd, 2024 USD/Mg dm) where available; elsewhere
        # the default road-side cost of EUR 40/t dm (2012 EUR)
        base_eur <- if (!is.null(params$eu_base_eur)) params$eu_base_eur else 40.0
        cost_usd <- base_eur * 1.285 * 1.366 # 2012 EUR -> 2012 USD -> 2024 USD
        x <- params$eu_feedstock_usd
        if (!is.null(x)) {
            cost_usd <- if (inherits(x, "SpatRaster")) terra::ifel(is.na(x), cost_usd, x) else ifelse(is.na(x), cost_usd, x)
        }
    } else if (region == "India") {
        cost_usd <- if (!is.null(params$india_feedstock_cost)) params$india_feedstock_cost else 34
    } else if (region == "China") {
        base_cny <- if (!is.null(params$cn_base_cny)) params$cn_base_cny else 99.6 # CNY/t, 2019
        weather_risk_val <- if (!is.null(params$cn_weather_risk)) params$cn_weather_risk else FALSE
        weather_risk <- ifelse_raster(weather_risk_val, 1.13, 1.0)
        cost_usd <- base_cny * weather_risk / 6.908 * 1.227
    } else {
        stop("Region not supported: ", region, ". Use US, EU/Europe, India, or China.")
    }

    storage <- if (!is.null(params$feedstock_storage_cost)) params$feedstock_storage_cost else 15
    cost_usd + storage * location_factor(params, "haulage")
}

# nolint end
