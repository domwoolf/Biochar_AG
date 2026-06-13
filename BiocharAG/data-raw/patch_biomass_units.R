library(terra)

gis_dir <- "GIS/processed/"
regions <- c("us", "china", "europe", "india")

for (r in regions) {
    file_path <- file.path(gis_dir, paste0(r, "_biomass.tif"))
    if (!file.exists(file_path)) {
        message("File not found: ", file_path)
        next
    }
    
    message("Patching units for ", r, "...")
    bm <- terra::rast(file_path)
    
    # Check if we already patched this by looking at max values
    max_val <- max(terra::global(bm, "max", na.rm=TRUE)[[1]])
    
    if (max_val > 1000) {
        message("  Detected unpatched raster (max > 1000). Applying area division.")
        
        # Calculate latitude for every cell
        lat <- terra::init(bm, "y")
        
        # The area of the original 5-arcmin pixel at latitude `y`
        # 5-arcmin = 0.08333 degrees
        # Equator width = 111.32 km * (5/60) = 9.276 km
        # Equator area = 9.276 * 9.276 = 86.05 km2
        r_area <- 86.05 * cos(lat * pi / 180)
        
        # Convert Total Mg to Mg / km2
        bm_density <- bm / r_area
        
        # Write directly back to disk
        terra::writeRaster(bm_density, file_path, overwrite = TRUE)
        message("  -> Successfully converted to Mg/km2")
    } else {
        message("  Raster appears already patched (max <= 1000). Skipping.")
    }
}
message("Done.")
