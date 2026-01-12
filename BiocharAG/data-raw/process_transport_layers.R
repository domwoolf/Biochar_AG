library(terra)
library(sf)
library(dplyr)

# ==============================================================================
# Setup: Load Sinks and Define Regions
# ==============================================================================

# 1. Load the Sinks (generated in Step 1)
#    If not yet in package memory, load the object directly or source the script
#    devtools::load_all() # Best practice if inside the package project
if (!exists("co2_sinks")) {
    # Fallback if not loaded
    source("data-raw/generate_sinks.R")
}

# 2. Define File Paths
raw_dir <- "../GIS/raw"
proc_dir <- "../GIS/processed"
if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)

# ==============================================================================
# Helper Function: Process Region
# ==============================================================================
#' Generate Transport Layers for a Region
#' @param region_name String matching the 'Region' column in co2_sinks (e.g., "China")
#' @param template_path Path to the biomass template raster for this region
#' @param file_prefix Prefix for output files (e.g., "china")
process_transport_layers <- function(region_name, template_path, file_prefix) {
    message(paste0("\n=== Processing Transport Layers for: ", region_name, " ==="))

    # 1. Load Template
    if (!file.exists(template_path)) {
        warning(paste("Template not found:", template_path, "- Skipping."))
        return(NULL)
    }
    r_template <- terra::rast(template_path)

    # 2. Filter Sinks for this Region
    #    We accept sinks in the region AND global hubs that might be relevant?
    #    Strict filtering is usually safer for TEA unless cross-border transport is explicit.
    #    Note: For Europe, we treat the whole continent as one region.

    sinks_sub <- co2_sinks %>%
        filter(Region == region_name)

    if (nrow(sinks_sub) == 0) {
        warning("No sinks found for this region in co2_sinks database.")
        return(NULL)
    }

    message(paste0("Found ", nrow(sinks_sub), " sinks. Calculating distances..."))

    # 3. Calculate Geodesic Distance (km)
    #    terra::distance computes distance from each cell to the nearest geometry in 'y'
    #    For Lon/Lat (EPSG:4326), unit is meters.
    r_dist_m <- terra::distance(r_template, sinks_sub)
    r_dist_km <- r_dist_m / 1000

    names(r_dist_km) <- "dist_sink_km"

    # 4. Identification of Nearest Sink (for Offshore Classification)
    message("Identifying nearest sink types (Onshore vs. Offshore)...")

    # Use Voronoi polygons to partition space by nearest sink
    v_polys <- terra::voronoi(terra::vect(sinks_sub))

    # Rasterize these polygons onto the template grid
    # Field 'is_offshore' must be present in sinks_sub (derived from Type)

    # Ensure is_offshore column exists in the vector data
    # (Checking logical condition on Type)
    v_polys$is_offshore <- ifelse(v_polys$Type == "Offshore", 1, 0)

    # Rasterize
    r_offshore_flag <- terra::rasterize(v_polys, r_template, field = "is_offshore")
    names(r_offshore_flag) <- "sink_is_offshore"

    # 5. Calculate Distance to NON-EOR Sinks (Saline Only)
    message("Calculating distance to Saline (Non-EOR) sinks...")
    sinks_saline <- sinks_sub %>% filter(Is_EOR == FALSE)

    if (nrow(sinks_saline) > 0) {
        r_dist_saline_m <- terra::distance(r_template, sinks_saline)
        r_dist_saline_km <- r_dist_saline_m / 1000
    } else {
        # If no saline sinks exist in region, set distance to infinite/NA or very high?
        # Setting to NA might break calculations. Setting to arbitrary high (5000km).
        r_dist_saline_km <- terra::rast(r_template)
        values(r_dist_saline_km) <- 5000
    }
    names(r_dist_saline_km) <- "dist_sink_saline_km"

    # 6. Save Outputs
    out_dist <- file.path(proc_dir, paste0(file_prefix, "_dist_sink.tif"))
    out_dist_saline <- file.path(proc_dir, paste0(file_prefix, "_dist_sink_saline.tif"))
    out_type <- file.path(proc_dir, paste0(file_prefix, "_sink_type.tif"))

    terra::writeRaster(r_dist_km, out_dist, overwrite = TRUE)
    terra::writeRaster(r_dist_saline_km, out_dist_saline, overwrite = TRUE)
    terra::writeRaster(r_offshore_flag, out_type, overwrite = TRUE)

    message(paste("Saved:", out_dist))
    message(paste("Saved:", out_dist_saline))
    message(paste("Saved:", out_type))

    list(
        dist = r_dist_km,
        dist_saline = r_dist_saline_km,
        type = r_offshore_flag
    )
}

# ==============================================================================
# Execution: Run for All Study Areas
# ==============================================================================

# 1. United States (Coterminous)
#    Template: Assumed from process_local_spatial.R
process_transport_layers(
    region_name = "North America",
    template_path = "../GIS/processed/demo_biomass.tif", # Or us_biomass.tif
    file_prefix = "us"
)

# 2. China
#    Template: Assumed from a hypothetical process_china.R or similar
#    (Using the filename pattern you established in process_india.R)
process_transport_layers(
    region_name = "China",
    template_path = "../GIS/processed/china_biomass.tif",
    file_prefix = "china"
)

# 3. India
#    Template: Generated in process_india.R
process_transport_layers(
    region_name = "India",
    template_path = "../GIS/processed/india_biomass.tif",
    file_prefix = "india"
)

# 4. Europe
#    Template: Assumed standard EU grid
process_transport_layers(
    region_name = "Europe",
    template_path = "../GIS/processed/europe_biomass.tif",
    file_prefix = "europe"
)
