#' Calculate and optionally save a distance raster for a given plant capacity
#'
#' @param dens_wgs84 SpatRaster of biomass density in WGS84
#' @param target_mw_th Target plant size in MWth
#' @param region Character string for region name (e.g., "us", "china")
#' @param gis_dir Optional directory path to save the raster
#' @param save_to_disk Logical, whether to save the raster to disk
#'
#' @return SpatRaster of average collection distance in km
#' @export
calculate_distance_raster <- function(dens_wgs84, target_mw_th, region, gis_dir = NULL, save_to_disk = TRUE) {
    if (!requireNamespace("terra", quietly = TRUE)) stop("terra package required.")

    radii_km <- c(5, 10, 25, 50, 100, 150, 250, 500)
    bm_lhv <- 18.6 # Default LHV
    capacity_factor <- 0.85

    # Equal-Area Projections for each region
    proj_dict <- list(
        us = "EPSG:5070",
        europe = "EPSG:3035",
        china = "+proj=aea +lat_1=25 +lat_2=47 +lat_0=30 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
        india = "+proj=aea +lat_1=12 +lat_2=28 +lat_0=24 +lon_0=80 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
    )

    if (is.null(region) || !region %in% names(proj_dict)) {
        stop("Region must be provided and one of: ", paste(names(proj_dict), collapse = ", "))
    }

    res_m <- 10000
    cell_area_km2 <- (res_m / 1000)^2

    # Reclassify values <= 0 and NAs to 0.0
    rcl <- matrix(c(-Inf, 0, 0, NA, NA, 0), ncol = 3, byrow = TRUE)
    dens_wgs84 <- terra::classify(dens_wgs84, rcl)

    # Project to Equal Area
    proj_str <- proj_dict[[region]]
    dens_ea <- terra::project(dens_wgs84, proj_str, res = res_m, method = "bilinear")

    # Calculate mass per cell (Mg)
    mass_ea <- dens_ea * cell_area_km2

    # Compute focal sums for all anchor radii
    focal_list <- list()
    message("  Computing focal sums for missing distance raster (", target_mw_th, " MWth)...")
    for (r_km in radii_km) {
        if (r_km <= (res_m / 2000)) {
            f_sum <- mass_ea
        } else {
            w <- terra::focalMat(mass_ea, r_km * 1000, type = "circle")
            w[w > 0] <- 1
            f_sum <- terra::focal(mass_ea, w = w, na.rm = TRUE)
        }
        names(f_sum) <- paste0("r_", r_km)
        focal_list[[paste0("r_", r_km)]] <- f_sum
    }
    focal_stack <- terra::rast(focal_list)

    message("  Interpolating for size: ", target_mw_th, " MW_th")
    target_mass <- (target_mw_th * 8760 * capacity_factor) / (bm_lhv * 0.277778)

    out_radius <- terra::rast(mass_ea, nlyrs = 1, vals = NA)
    rad_lower <- 0
    mass_lower <- mass_ea * 0

    for (i in seq_along(radii_km)) {
        rad_upper <- radii_km[i]
        mass_upper <- focal_stack[[i]]

        mask <- (target_mass > mass_lower) & (target_mass <= mass_upper)
        fraction <- (target_mass - mass_lower) / (mass_upper - mass_lower)
        fraction <- terra::ifel(mass_upper == mass_lower, 0, fraction)
        r_interp <- sqrt(rad_lower^2 + fraction * (rad_upper^2 - rad_lower^2))
        
        out_radius <- terra::ifel(mask, r_interp, out_radius)

        rad_lower <- rad_upper
        mass_lower <- mass_upper
    }

    # Convert to avg_dist (2/3 of collection radius for a circle)
    avg_dist_ea <- (2 / 3) * out_radius

    # Reproject back to WGS84 template
    avg_dist_wgs84 <- terra::project(avg_dist_ea, dens_wgs84, method = "bilinear")

    if (save_to_disk && !is.null(gis_dir)) {
        out_file <- file.path(gis_dir, paste0(region, "_dist_", target_mw_th, "MWth.tif"))
        terra::writeRaster(avg_dist_wgs84, out_file, overwrite = TRUE)
        message("    Saved ", out_file)
    }

    return(avg_dist_wgs84)
}
