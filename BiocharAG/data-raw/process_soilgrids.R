# Process SoilGrids VRTs (CEC, pH) to match Project Grid
#
# Inputs:
# - GIS/processed/us_biomass.tif (Template)
# - GIS/raw/soilgrids/.../cec_0-5cm_mean.vrt
# - GIS/raw/soilgrids/.../phh2o_0-5cm_mean.vrt
#
# Outputs:
# - GIS/processed/soil_cec.tif (cmol/kg)
# - GIS/processed/soil_ph.tif (pH)

library(terra)

# 1. Define Paths
gis_proc <- "../GIS/processed"
gis_raw <- "../GIS/raw/soilgrids/files.isric.org/soilgrids/latest/data"

cec_vrt <- file.path(gis_raw, "cec/cec_0-5cm_mean.vrt")
ph_vrt <- file.path(gis_raw, "phh2o/phh2o_0-5cm_mean.vrt")

if (!file.exists(cec_vrt)) stop("CEC VRT not found at: ", cec_vrt)
if (!file.exists(ph_vrt)) stop("pH VRT not found at: ", ph_vrt)

regions <- c("us", "china", "india", "europe")

process_and_save <- function(r_in, r_template, out_name, scale_factor = 0.1, var_name) {
    message("  Processing ", var_name, "...")
    message("   - Projecting and Resampling (average)...")
    r_out <- terra::project(r_in, r_template, method = "average")
    message("   - Scaling units (x", scale_factor, ")...")
    r_out <- r_out * scale_factor
    r_out <- terra::mask(r_out, r_template)
    names(r_out) <- var_name
    
    out_path <- file.path(gis_proc, out_name)
    message("   - Saving to ", out_path)
    terra::writeRaster(r_out, out_path, overwrite = TRUE, gdal = c("COMPRESS=ZSTD", "PREDICTOR=2"))
    return(r_out)
}

# Load VRTs once
r_cec_raw <- terra::rast(cec_vrt)
r_ph_raw <- terra::rast(ph_vrt)

for (region in regions) {
    message("\n=== Processing SoilGrids for Region: ", toupper(region), " ===")
    template_path <- file.path(gis_proc, paste0(region, "_biomass.tif"))
    
    if (!file.exists(template_path)) {
        warning("Template raster missing for ", region, ": ", template_path, ". Skipping.")
        next
    }
    
    r_template <- terra::rast(template_path)
    message("  Template loaded: ", paste(dim(r_template), collapse = "x"), " | CRS: ", crs(r_template, proj = TRUE))
    
    # CEC: mmol(c)/kg -> cmol(+)/kg. Factor = 0.1
    process_and_save(r_cec_raw, r_template, paste0(region, "_soil_cec.tif"), 0.1, "soil_cec")
    
    # pH: pH*10 -> pH. Factor = 0.1
    process_and_save(r_ph_raw, r_template, paste0(region, "_soil_ph.tif"), 0.1, "soil_ph")
}

message("\nProcessing Complete for all regions.")
