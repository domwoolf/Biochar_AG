library(terra)
library(sf)
library(dplyr)
library(tigris) # For fetching US county boundaries

# ==============================================================================
# Setup: Define Paths and Load Template
# ==============================================================================
raw_dir <- "../GIS/raw"
proc_dir <- "../GIS/processed"

if (!dir.exists(raw_dir)) dir.create(raw_dir, recursive = TRUE)
if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)

message("Loading US Biomass Template...")
template_path <- file.path(proc_dir, "us_biomass.tif")
if (!file.exists(template_path)) stop("Template raster not found.")
r_template <- terra::rast(template_path)

# ==============================================================================
# 1. Prepare Base County Geometries
# ==============================================================================
message("Fetching US County Boundaries...")
# Fetch US counties at 1:20 million scale for speed, omitting non-CONUS territories
us_counties <- tigris::counties(cb = TRUE, resolution = "20m", class = "sf") %>%
    dplyr::filter(!(STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")))

# Project counties to match the raster template (EPSG:5070 for US Equal Area)
us_counties_proj <- sf::st_transform(us_counties, terra::crs(r_template))

# Create a unique ID for joining (GEOID is standard: State FIPS + County FIPS)
# Ensure area is calculated for density metrics
us_counties_proj$area_km2 <- as.numeric(sf::st_area(us_counties_proj)) / 1e6

# ==============================================================================
# 2. Process Cattle Density (Opportunity Cost Trigger)
# ==============================================================================
message("Processing Cattle Density Mask...")

# NOTE: Download the USDA NASS Census data for "CATTLE, INCL CALVES - INVENTORY"
# Save it to GIS/raw/nass_cattle_inventory.csv
cattle_csv_path <- file.path(raw_dir, "nass_cattle_inventory.csv")

if (file.exists(cattle_csv_path)) {
    cattle_data <- read.csv(cattle_csv_path, stringsAsFactors = FALSE) %>%
        # Ensure GEOID matches tigris (string, 5 chars, leading zeros)
        dplyr::mutate(GEOID = sprintf("%02d%03d", State.ANSI, County.ANSI)) %>%
        dplyr::select(GEOID, inventory = Value) %>%
        # Clean numeric formatting (remove commas)
        dplyr::mutate(inventory = as.numeric(gsub(",", "", inventory)))
    
    # Join to spatial counties
    counties_cattle <- us_counties_proj %>%
        dplyr::left_join(cattle_data, by = "GEOID") %>%
        dplyr::mutate(
            inventory = ifelse(is.na(inventory), 0, inventory),
            density_head_per_km2 = inventory / area_km2
        )
    
    # Define threshold for "High Cattle" (e.g., top 25% of counties, or a hard limit like > 50 head/km2)
    threshold <- quantile(counties_cattle$density_head_per_km2, probs = 0.75, na.rm = TRUE)
    counties_cattle$is_high_cattle <- ifelse(counties_cattle$density_head_per_km2 >= threshold, 1, 0)
    
    # Rasterize
    r_high_cattle <- terra::rasterize(
        terra::vect(counties_cattle), 
        r_template, 
        field = "is_high_cattle", 
        background = 0
    )
    
    # Write to disk
    terra::writeRaster(r_high_cattle, file.path(proc_dir, "us_is_high_cattle.tif"), overwrite = TRUE)
    message("  -> Saved: us_is_high_cattle.tif")
} else {
    warning("Cattle inventory CSV not found. Skipping cattle density layer.")
}

# ==============================================================================
# 3. Process POLYSYS Baseline Cost
# ==============================================================================
message("Processing POLYSYS Baseline Costs...")

# NOTE: Download the Bioenergy KDF county-level supply curve data
# Save it to GIS/raw/polysys_stover_cost.csv
polysys_csv_path <- file.path(raw_dir, "polysys_stover_cost.csv")

if (file.exists(polysys_csv_path)) {
    cost_data <- read.csv(polysys_csv_path, stringsAsFactors = FALSE) %>%
        dplyr::mutate(GEOID = sprintf("%05d", FIPS)) %>%
        # Assuming you extract the baseline price column at your target supply volume
        # Rename your specific price column to 'base_cost_usd'
        dplyr::select(GEOID, base_cost_usd = Price_USD_per_Mg)
    
    # Join to spatial counties
    counties_cost <- us_counties_proj %>%
        dplyr::left_join(cost_data, by = "GEOID") %>%
        # Fill missing counties with the national default ($60) to prevent NAs from breaking the TEA
        dplyr::mutate(base_cost_usd = ifelse(is.na(base_cost_usd), 60.0, base_cost_usd))
    
    # Rasterize
    r_base_cost <- terra::rasterize(
        terra::vect(counties_cost), 
        r_template, 
        field = "base_cost_usd", 
        background = NA # Areas outside the US borders will be NA
    )
    
    # Mask out non-biomass pixels (oceans, lakes) using the template
    r_base_cost <- terra::mask(r_base_cost, r_template)
    
    # Write to disk
    terra::writeRaster(r_base_cost, file.path(proc_dir, "us_base_cost.tif"), overwrite = TRUE)
    message("  -> Saved: us_base_cost.tif")
} else {
    warning("POLYSYS cost CSV not found. Skipping baseline cost layer.")
}

message("=== US Feedstock Processing Complete ===")
