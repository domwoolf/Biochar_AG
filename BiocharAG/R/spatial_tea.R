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
#'
#' @return A `SpatRaster` with layers for key outputs (NPV, Total Cost, Abatement, etc.).
#' @export
#' @importFrom terra as.data.frame rast
run_spatial_tea <- function(template_raster, params, spatial_layers = list(),
                            fun = calculate_beccs, collection_radius_km = 50,
                            optimize_scale = FALSE, plant_sizes_mw_th = c(5, 25, 50, 100, 250, 500)) {
    if (!inherits(template_raster, "SpatRaster")) {
        stop("template_raster must be a terra SpatRaster object.")
    }

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
        if ("dist_onshore" %in% names(spatial_layers)) p$dist_onshore <- spatial_layers$dist_onshore
        if ("dist_offshore" %in% names(spatial_layers)) p$dist_offshore <- spatial_layers$dist_offshore

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

    # 1. Prepare Data Frame from Raster Grid
    # Extract coordinates
    # We process as a data.frame for flexibility with 'sf' distance calcs logic.
    # For very large rasters, 'terra::app' or 'focal' is better, but this wrapper
    # is designed for complexity (calling full TEA models) rather than vectorized speed.

    df <- terra::as.data.frame(template_raster, xy = TRUE, cells = TRUE, na.rm = TRUE)

    # 2. Extract Spatial Layer Values
    for (layer_name in names(spatial_layers)) {
        # Extract values for these cells
        # Improve speed by assuming aligned rasters, or extracting by xy
        vals <- terra::extract(spatial_layers[[layer_name]], df[, c("x", "y")], ID = FALSE)
        df[[layer_name]] <- vals[, 1]
    }

    # 3. Iterate and Calculate
    # Initialize output vectors
    n <- nrow(df)
    net_value_vec <- numeric(n)
    total_cost_vec <- numeric(n)
    abatement_vec <- numeric(n)
    ts_cost_vec <- numeric(n) # NEW
    scale_vec <- numeric(n)

    for (i in seq_len(n)) {
        if (i %% 500 == 0) message("Processing cell ", i, " / ", n)
        # Copy baseline params
        p <- params

        # Update with spatial params if they exist
        p$lat <- df$y[i]
        p$lat <- df$y[i]
        p$lon <- df$x[i]
        p$collection_radius <- collection_radius_km

        # 3a. Soil Temp (Permenance)
        if ("soil_temp" %in% names(df)) {
            p$soil_temp <- df$soil_temp[i]
        }

        # 3b. Plant Scale from Biomass Density
        if ("biomass_density" %in% names(df)) {
            # Density in Mg/km2
            dens <- df$biomass_density[i]
            if (is.na(dens) || dens <= 0) dens <- 0.1 # Minimum

            # Calculate Annual Feedstock (Mg)
            # Area = pi * r^2
            area_km2 <- pi * collection_radius_km^2
            annual_biomass_feedstock <- dens * area_km2

            # Determine Plant MW Capacity from Feedstock
            # Reverse the logic: Annual = (MW * 8760 * CF) / ElecProd
            # MW = (Annual * ElecProd) / (8760 * CF)

            # We need ElecProd (MWh/Mg).
            # If not in params, use standard approximation.
            # BECCS/BES typical: ~1 MWh/Mg (very roughly).
            # Actually calculated inside the function (bm_lhv * eff * 0.277).
            # Let's pre-calc a reference elec_prod for sizing.
            if (is.null(p$bm_lhv)) p$bm_lhv <- 18.6
            if (is.null(p$bes_energy_efficiency)) p$bes_energy_efficiency <- 0.30
            ref_elec_prod <- p$bm_lhv * p$bes_energy_efficiency * 0.277778

            capacity_factor <- 0.85

            # Calculate and Assign Scaled Plant Capacity
            p$plant_mw <- (annual_biomass_feedstock * ref_elec_prod) / (8760 * capacity_factor)

            # Cap minimum size to avoid divide-by-zero or tiny plants (e.g. 1 MW min)
            if (p$plant_mw < 1) p$plant_mw <- 1
        }

        # 3c. Electricity Price
        if ("elec_price" %in% names(df)) {
            pv <- df$elec_price[i]
            # Apply Wholesale Discount Factor to convert Retail Map to Generator Revenue
            factor <- if (!is.null(p$wholesale_discount_factor)) p$wholesale_discount_factor else 0.4

            if (!is.na(pv) && pv > 0) p$elec_price <- pv * factor
        }

        # 3d. Advanced Ag Parameters (pH, CEC)
        if ("soil_ph" %in% names(df)) {
            val <- df$soil_ph[i]
            if (!is.na(val)) p$soil_ph <- val
        }
        if ("soil_cec" %in% names(df)) {
            val <- df$soil_cec[i]
            if (!is.na(val)) p$soil_cec <- val
        }

        # 3e. CCS Transport Parameters
        if ("dist_onshore" %in% names(df)) {
            val <- df$dist_onshore[i]
            if (!is.na(val)) p$dist_onshore <- val
        }
        if ("dist_offshore" %in% names(df)) {
            val <- df$dist_offshore[i]
            if (!is.na(val)) p$dist_offshore <- val
        }


        # Run TEA
        # Note: TEA function will handle finding nearest sink using p$lat/p$lon
        # if ccs_distance is NULL and it's BECCS.
        # Run TEA
        res <- tryCatch(fun(p), error = function(e) list(net_value = NA, total_cost = NA, tot_c_abatement = NA))

        net_value_vec[i] <- if (is.null(res$net_value)) NA else res$net_value
        total_cost_vec[i] <- if (is.null(res$total_cost)) NA else res$total_cost
        abatement_vec[i] <- if (is.null(res$tot_c_abatement)) NA else res$tot_c_abatement
        # Capture Transport Cost (specific to BECCS)
        ts_cost_vec[i] <- if (is.null(res$ts_cost)) NA else res$ts_cost

        # Store calculated scale/dist if relevant for debugging
        scale_vec[i] <- if (!is.null(p$plant_mw)) p$plant_mw else 50
    }

    # 4. Rasterize Results
    out_r <- terra::rast(template_raster, nlyrs = 4)
    names(out_r) <- c("Net_Value_USD", "Total_Cost_USD_Mg", "Abatement_tCO2", "Transport_Cost_USD_Mg")

    # Fill values
    # Using cell IDs to ensure alignment
    out_r[["Net_Value_USD"]][df$cell] <- net_value_vec
    out_r[["Total_Cost_USD_Mg"]][df$cell] <- total_cost_vec
    out_r[["Abatement_tCO2"]][df$cell] <- abatement_vec
    out_r[["Transport_Cost_USD_Mg"]][df$cell] <- ts_cost_vec

    return(out_r)
}
