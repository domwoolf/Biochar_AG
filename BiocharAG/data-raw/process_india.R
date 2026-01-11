# Process Spatial Data for India (North-West)
# Comparison Scenario for BiocharAG

library(terra)

# 1. Define Paths
gis_proc <- "../GIS/processed"
gis_raw <- "../GIS/raw"
soil_raw <- "../GIS/raw/soilgrids/files.isric.org/soilgrids/latest/data"

# 2. Define Extent: Whole India
# Approx: 6N - 37N, 68E - 98E
e_india <- terra::ext(68, 98, 6, 38)

message("Processing India Region (Whole Country): ", e_india)

# 3. Process Biomass (Global Crop Residue)
bm_global_path <- file.path(gis_raw, "res_avail.tif")
if (!file.exists(bm_global_path)) stop("Biomass map not found: ", bm_global_path)

message("Loading Global Biomass...")
r_bm_global <- terra::rast(bm_global_path)
r_india_bm <- terra::crop(r_bm_global, e_india)

# Resample to coarser resolution for Interactive Demo Performance
# Target resolution: 0.1 degrees (~10km)
target_res <- 0.1
message("Resampling to ", target_res, " degree resolution...")
r_template <- terra::rast(e_india, res = target_res)
terra::crs(r_template) <- terra::crs(r_bm_global)

r_india_bm <- terra::resample(r_india_bm, r_template, method = "bilinear")

# Resample to match SoilGrids resolution (approx 0.0025 deg)?
# Or stick to Biomass resolution?
# SoilGrids is 250m. Beccs optimization is fast enough for fine resolution.
# Let's target the Biomass resolution or 1km to save demo time?
# Let's keep the native biomass resolution for now.

names(r_india_bm) <- "biomass_density"
# Handle Units: Dataverse dataset often Mg C / km2 or Mg / ha?
# User said "global extent". Assuming it is Mg/km2 or similar.
# We'll treat values as "Mg/km2" (feedstock density) for now.
# If values are too low/high we will know in the app.

out_bm <- file.path(gis_proc, "india_biomass.tif")
terra::writeRaster(r_india_bm, out_bm, overwrite = TRUE)
message("Saved: ", out_bm)

# Define Template from Biomass
template <- r_india_bm

# 4. Process Soil Layers (SoilGrids)
process_sg <- function(vrt_path, name, scaler = 0.1) {
    if (!file.exists(vrt_path)) {
        message("Skipping missing VRT: ", vrt_path)
        return(NULL)
    }
    r_vrt <- terra::rast(vrt_path)
    # Project to template (handles cropping and resolution)
    r_out <- terra::project(r_vrt, template)
    r_out <- r_out * scaler
    names(r_out) <- name

    out_p <- file.path(gis_proc, paste0("india_", name, ".tif"))
    terra::writeRaster(r_out, out_p, overwrite = TRUE)
    message("Saved: ", out_p)
}

cec_vrt <- file.path(soil_raw, "cec/cec_0-5cm_mean.vrt")
ph_vrt <- file.path(soil_raw, "phh2o/phh2o_0-5cm_mean.vrt")

process_sg(cec_vrt, "soil_cec", 0.1)
process_sg(ph_vrt, "soil_ph", 0.1)

# 5. Process Soil Temp (WorldClim / SBIO1)
temp_path <- file.path(gis_raw, "SBIO1_0_5cm_Annual_Mean_Temperature.tif")
if (file.exists(temp_path)) {
    r_temp <- terra::rast(temp_path)
    r_india_temp <- terra::project(r_temp, template)
    names(r_india_temp) <- "soil_temp"
    terra::writeRaster(r_india_temp, file.path(gis_proc, "india_soil_temp.tif"), overwrite = TRUE)
    message("Saved: india_soil_temp.tif")
} else {
    # Fallback: Constant 25C (Warm)
    r_india_temp <- terra::rast(template)
    values(r_india_temp) <- 25
    names(r_india_temp) <- "soil_temp"
    terra::writeRaster(r_india_temp, file.path(gis_proc, "india_soil_temp.tif"), overwrite = TRUE)
}

# 6. Generate APPC Electricity Price Layer
# Constant ~ $0.06/kWh = 60 $/MWh
# But parameters_india has this value, spatial_tea expects a map.
r_india_elec <- terra::rast(template)
values(r_india_elec) <- 60
names(r_india_elec) <- "elec_price"
terra::writeRaster(r_india_elec, file.path(gis_proc, "india_elec_price.tif"), overwrite = TRUE)
message("Saved: india_elec_price.tif")

message("India Data Processing Complete.")
