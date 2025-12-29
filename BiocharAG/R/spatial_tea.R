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
                            fun = calculate_beccs, collection_radius_km = 50) {
    if (!inherits(template_raster, "SpatRaster")) {
        stop("template_raster must be a terra SpatRaster object.")
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
    dist_vec <- numeric(n)
    scale_vec <- numeric(n)

    # Lat/Lon detection
    # Assuming x = Lon, y = Lat (EPSG:4326). If projected, need conversion.
    # Check CRS? For now assume User provides Lat/Lon raster or acceptable coords.
    is_lonlat <- terra::is.lonlat(template_raster)

    for (i in seq_len(n)) {
        # Copy baseline params
        p <- params

        # Update with spatial params if they exist
        p$lat <- df$y[i]
        p$lon <- df$x[i]

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

            p$plant_mw <- (annual_biomass_feedstock * ref_elec_prod) / (8760 * capacity_factor)
        }

        # Run TEA
        # Note: TEA function will handle finding nearest sink using p$lat/p$lon
        # if ccs_distance is NULL and it's BECCS.
        res <- tryCatch(fun(p), error = function(e) list(net_value = NA, total_cost = NA, tot_c_abatement = NA))

        net_value_vec[i] <- if (is.null(res$net_value)) NA else res$net_value
        total_cost_vec[i] <- if (is.null(res$total_cost)) NA else res$total_cost
        abatement_vec[i] <- if (is.null(res$tot_c_abatement)) NA else res$tot_c_abatement

        # Store calculated scale/dist if relevant for debugging
        scale_vec[i] <- if (!is.null(p$plant_mw)) p$plant_mw else 50
        # Store CCS distance?
        # Not returned by all functions clearly, but could inspect p$ccs_distance if updated by ref?
        # (R passes by value, so p modification inside fun doesn't persist unless returned)
    }

    # 4. Rasterize Results
    out_r <- terra::rast(template_raster, nlyrs = 3)
    names(out_r) <- c("Net_Value_USD", "Total_Cost_USD_Mg", "Abatement_tCO2")

    # Fill values
    # Using cell IDs to ensure alignment
    out_r[["Net_Value_USD"]][df$cell] <- net_value_vec
    out_r[["Total_Cost_USD_Mg"]][df$cell] <- total_cost_vec
    out_r[["Abatement_tCO2"]][df$cell] <- abatement_vec

    return(out_r)
}
