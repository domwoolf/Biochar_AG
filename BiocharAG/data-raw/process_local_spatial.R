# data-raw/process_local_spatial.R
library(terra)
library(sf)

# 1. Paths
# Input: Raw data in Root/GIS/raw/
# Output: Processed data in Root/GIS/processed/

raw_dir <- "../GIS/raw"
proc_dir <- "../GIS/processed"

if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)

# File names
biomass_file <- file.path(raw_dir, "res_avail.tif")
temp_file <- file.path(raw_dir, "SBIO1_0_5cm_Annual_Mean_Temperature.tif")

# 2. Check Exists
if (!file.exists(biomass_file) || !file.exists(temp_file)) {
    stop("Input files not found in ", raw_dir)
}

# 1. Prepare Template
message("Processing Layers from Local Source...")

# Create Template Grid (US Extent, ~20km ~ 0.2 deg)
# Approx US BBox: -125, 24, -66, 50
us_extent <- ext(-125, -66, 24, 50)
us_template <- rast(us_extent, res = 0.2, crs = "EPSG:4326")

processed_layers <- list()

# 2. Process Biomass
message("Processing Biomass...")
r_bm <- rast(biomass_file)
r_bm_us <- project(r_bm, us_template)

v_max <- global(r_bm_us, "max", na.rm = TRUE)$max
message("Biomass Max Value: ", v_max)

# Unit Conversion Logic (kg/ha or t/ha -> Mg/km2)
if (v_max > 10000 && v_max < 50000) {
    # Assuming kg/ha
    # 1 kg/ha = 0.1 Mg/km2
    r_bm_us <- r_bm_us * 0.1
    message("Converted kg/ha to Mg/km2 (Factor 0.1)")
} else if (v_max < 50) {
    # Assuming t/ha
    # 1 t/ha = 100 Mg/km2
    r_bm_us <- r_bm_us * 100
    message("Converted t/ha to Mg/km2 (Factor 100)")
}

names(r_bm_us) <- "biomass_density"
processed_layers$biomass_density <- r_bm_us

# 3. Process Soil Temp
message("Processing Soil Temp...")
r_st <- rast(temp_file)
r_st_us <- project(r_st, us_template)

v_mM <- minmax(r_st_us)
message("Soil Temp Range: ", v_mM[1], " - ", v_mM[2])

# Unit Check (x10 scaling common in bioclim)
if (v_mM[2] > 60) {
    r_st_us <- r_st_us / 10
    message("Dividing Soil Temp by 10 (Deci-degrees correction)")
}

names(r_st_us) <- "soil_temp"
processed_layers$soil_temp <- r_st_us

# 4. Save
# save(processed_layers, file = "data/spatial_demo_layers.rda") # Optional

# Export Tifs for quick check
terra::writeRaster(processed_layers$biomass_density, file.path(proc_dir, "demo_biomass.tif"), overwrite = TRUE)
terra::writeRaster(processed_layers$soil_temp, file.path(proc_dir, "demo_soil_temp.tif"), overwrite = TRUE)

message("Done. Layers saved to ", proc_dir)
