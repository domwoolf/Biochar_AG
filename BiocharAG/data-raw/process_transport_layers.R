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

    # 1. Load spatial template
    if (!file.exists(template_path)) {
        warning(paste("Template not found:", template_path, "- Skipping."))
        return(NULL)
    }
    r_template <- terra::rast(template_path)
    
    # 2. Filter Sinks for the Region
    region_sinks <- co2_sinks %>% filter(Region == region_name)
    
    if (nrow(region_sinks) == 0) {
        warning("No sinks found for this region in co2_sinks database.")
        return(NULL)
    }

    message(paste0("Found ", nrow(region_sinks), " sinks. Calculating distances..."))

    # Split into Onshore and Offshore
    onshore_sinks <- region_sinks %>% filter(Type != "Offshore")
    offshore_sinks <- region_sinks %>% filter(Type == "Offshore")
    
    # 3. Calculate Onshore Distance
    if (nrow(onshore_sinks) > 0) {
        r_dist_onshore <- terra::distance(r_template, terra::vect(onshore_sinks))
        r_dist_onshore_km <- r_dist_onshore / 1000
    } else {
        # If no onshore sinks exist in the region, create a raster of Inf
        r_dist_onshore_km <- terra::init(r_template, Inf)
    }
    
    # 4. Calculate Offshore Distance
    if (nrow(offshore_sinks) > 0) {
        r_dist_offshore <- terra::distance(r_template, terra::vect(offshore_sinks))
        r_dist_offshore_km <- r_dist_offshore / 1000
    } else {
        # If no offshore sinks exist, create a raster of Inf
        r_dist_offshore_km <- terra::init(r_template, Inf)
    }

    # 5. Define output paths
    out_dist_onshore <- file.path(proc_dir, paste0(file_prefix, "_dist_onshore.tif"))
    out_dist_offshore <- file.path(proc_dir, paste0(file_prefix, "_dist_offshore.tif"))

    # Name layers
    names(r_dist_onshore_km) <- "dist_onshore"
    names(r_dist_offshore_km) <- "dist_offshore"

    # Ensure output directory exists
    if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)

    # 6. Save Rasters
    terra::writeRaster(r_dist_onshore_km, out_dist_onshore, overwrite = TRUE)
    terra::writeRaster(r_dist_offshore_km, out_dist_offshore, overwrite = TRUE)

    message(paste("Saved:", out_dist_onshore))
    message(paste("Saved:", out_dist_offshore))

    # Return as list for immediate use if needed
    list(
        dist_onshore = r_dist_onshore_km,
        dist_offshore = r_dist_offshore_km
    )
}

# ==============================================================================
# Execution: Run for All Study Areas
# ==============================================================================

# 1. United States (Coterminous)
#    Template: Assumed from process_local_spatial.R
process_transport_layers(
    region_name = "North America",
    template_path = "../GIS/processed/us_biomass.tif",
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
