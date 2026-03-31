# Process Spatial Data for China and Europe
# Extracts templates and base layers from global datasets

library(terra)

# 1. Define Paths
gis_proc <- "../GIS/processed"
gis_raw <- "../GIS/raw"
soil_raw <- "../GIS/raw/soilgrids/files.isric.org/soilgrids/latest/data"

# Global Biomass
bm_global_path <- file.path(gis_raw, "res_avail.tif")
if (!file.exists(bm_global_path)) stop("Global biomass map not found in: ", bm_global_path)

message("Loading Global Biomass...")
r_bm_global <- terra::rast(bm_global_path)

# Regions definition
regions <- list(
    China = list(
        prefix = "china",
        ext = terra::ext(73, 135, 18, 54) # Approx BBox
    ),
    Europe = list(
        prefix = "europe",
        ext = terra::ext(-11, 40, 35, 71) # Approx BBox
    )
)

for (r_name in names(regions)) {
    prefix <- regions[[r_name]]$prefix
    e_box <- regions[[r_name]]$ext
    
    message("\n==================================")
    message("Processing Region: ", r_name)
    message("Bounding Box: ", e_box)
    
    # Process Biomass / Template
    r_bm <- terra::crop(r_bm_global, e_box)
    
    target_res <- 0.1
    message("Resampling biomass to ", target_res, " degree resolution...")
    r_template <- terra::rast(e_box, res = target_res)
    terra::crs(r_template) <- terra::crs(r_bm_global)
    
    r_bm_resampled <- terra::resample(r_bm, r_template, method = "bilinear")
    names(r_bm_resampled) <- "biomass_density"
    
    out_bm <- file.path(gis_proc, paste0(prefix, "_biomass.tif"))
    terra::writeRaster(r_bm_resampled, out_bm, overwrite = TRUE)
    message("Saved: ", out_bm)
    
    template <- r_bm_resampled
    
    # Process SoilGrids (CEC and pH)
    process_sg <- function(vrt_path, name, scaler = 0.1) {
        if (!file.exists(vrt_path)) {
            message("  Skipping missing VRT: ", vrt_path)
            return(NULL)
        }
        message("  Processing Soil Grid: ", name)
        r_vrt <- terra::rast(vrt_path)
        r_out <- terra::project(r_vrt, template)
        r_out <- r_out * scaler
        names(r_out) <- name
        out_p <- file.path(gis_proc, paste0(prefix, "_", name, ".tif"))
        terra::writeRaster(r_out, out_p, overwrite = TRUE)
        message("  Saved: ", out_p)
    }
    
    cec_vrt <- file.path(soil_raw, "cec/cec_0-5cm_mean.vrt")
    ph_vrt <- file.path(soil_raw, "phh2o/phh2o_0-5cm_mean.vrt")
    process_sg(cec_vrt, "soil_cec", 0.1)
    process_sg(ph_vrt, "soil_ph", 0.1)
    
    # Process Soil Temp
    temp_path <- file.path(gis_raw, "SBIO1_0_5cm_Annual_Mean_Temperature.tif")
    if (file.exists(temp_path)) {
        message("  Processing Soil Temperature...")
        r_temp <- terra::rast(temp_path)
        r_temp_proj <- terra::project(r_temp, template)
        names(r_temp_proj) <- "soil_temp"
        out_temp <- file.path(gis_proc, paste0(prefix, "_soil_temp.tif"))
        terra::writeRaster(r_temp_proj, out_temp, overwrite = TRUE)
        message("  Saved: ", out_temp)
    } else {
        message("  Soil Temp not found, using constant fallback...")
        r_temp_proj <- terra::rast(template)
        values(r_temp_proj) <- if (r_name == "Europe") 10 else 15
        names(r_temp_proj) <- "soil_temp"
        terra::writeRaster(r_temp_proj, file.path(gis_proc, paste0(prefix, "_soil_temp.tif")), overwrite = TRUE)
    }
    
    # Generate constant Electricity Price Layer
    r_elec <- terra::rast(template)
    values(r_elec) <- if (r_name == "Europe") 150 else 60 # Default scalar guesses $/MWh
    names(r_elec) <- "elec_price"
    terra::writeRaster(r_elec, file.path(gis_proc, paste0(prefix, "_elec_price.tif")), overwrite = TRUE)
    message("  Saved: ", paste0(prefix, "_elec_price.tif"))
}

message("Finished generating templates for missing regions.")
