# BiocharAG/data-raw/process_regions.R
# Unified script to process raw spatial datasets into region-specific templates.
# Handles correct unit conversions exactly once.

library(terra)

# ==============================================================================
# 1. Define Paths & Parameters
# ==============================================================================
gis_proc <- "../../GIS/processed"
gis_raw <- "../../GIS/raw"
soil_raw <- file.path(gis_raw, "soilgrids/files.isric.org/soilgrids/latest/data")
params_csv <- "../inst/extdata/parameters.csv"

if (!dir.exists(gis_proc)) dir.create(gis_proc, recursive = TRUE)

message("Loading regional parameters from: ", params_csv)
params_df <- read.csv(params_csv, stringsAsFactors = FALSE)

# ==============================================================================
# 2. Define Regions
# ==============================================================================
regions <- list(
    us = list(
        prefix = "us",
        ext = terra::ext(-125, -66, 24, 50),
        res = 0.2, # Historically 0.2 for US
        param_col = "US"
    ),
    china = list(
        prefix = "china",
        ext = terra::ext(73, 135, 18, 54),
        res = 0.1,
        param_col = "China"
    ),
    europe = list(
        prefix = "europe",
        ext = terra::ext(-11, 40, 35, 71),
        res = 0.1,
        param_col = "Europe"
    ),
    india = list(
        prefix = "india",
        ext = terra::ext(68, 98, 6, 38),
        res = 0.1,
        param_col = "India"
    )
)

# ==============================================================================
# 3. Load Global Datasets
# ==============================================================================
# Biomass (Mg C / yr per pixel)
bm_global_path <- file.path(gis_raw, "res_avail.tif")
if (!file.exists(bm_global_path)) stop("Global biomass map not found: ", bm_global_path)
message("Loading Global Biomass...")
r_bm_global <- terra::rast(bm_global_path)

# Soil Temperature
temp_path <- file.path(gis_raw, "SBIO1_0_5cm_Annual_Mean_Temperature.tif")
r_temp_global <- if (file.exists(temp_path)) terra::rast(temp_path) else NULL

# SoilGrids (CEC and pH)
cec_vrt <- file.path(soil_raw, "cec/cec_0-5cm_mean.vrt")
ph_vrt <- file.path(soil_raw, "phh2o/phh2o_0-5cm_mean.vrt")
r_cec_raw <- if (file.exists(cec_vrt)) terra::rast(cec_vrt) else NULL
r_ph_raw <- if (file.exists(ph_vrt)) terra::rast(ph_vrt) else NULL

# ==============================================================================
# 4. Process Each Region
# ==============================================================================
for (r_name in names(regions)) {
    reg <- regions[[r_name]]
    prefix <- reg$prefix
    e_box <- reg$ext
    target_res <- reg$res
    p_col <- reg$param_col
    
    message("\n==================================")
    message("Processing Region: ", toupper(prefix))
    message("Bounding Box: ", e_box, " | Resolution: ", target_res)
    
    # Extract regional parameters
    bm_c <- as.numeric(params_df[params_df$name == "bm_c", p_col])
    bm_h2o <- as.numeric(params_df[params_df$name == "bm_h2o", p_col])
    bm_ash <- as.numeric(params_df[params_df$name == "bm_ash", p_col])
    
    message(sprintf("  Parameters -> bm_c: %s (DAF basis), bm_h2o: %s, bm_ash: %s", bm_c, bm_h2o, bm_ash))
    
    # --------------------------------------------------------------------------
    # A. Biomass Processing
    # --------------------------------------------------------------------------
    message("  Processing Biomass...")
    r_bm_crop <- terra::crop(r_bm_global, e_box)
    
    # Step 1: Convert Mg C -> Mg Moist Biomass
    # Moist Mass = (Mg C / bm_c) / (1 - bm_h2o - bm_ash)
    r_bm_moist <- (r_bm_crop / bm_c) / (1 - bm_h2o - bm_ash)
    
    # Step 2: Convert to Density at Native Resolution (Mg / km2)
    native_area_km2 <- terra::cellSize(r_bm_moist, unit = "km")
    r_bm_density <- r_bm_moist / native_area_km2
    
    # Step 3: Create Target Template and Resample
    r_template <- terra::rast(e_box, res = target_res)
    terra::crs(r_template) <- terra::crs(r_bm_global)
    
    r_bm_final <- terra::resample(r_bm_density, r_template, method = "bilinear")
    names(r_bm_final) <- "biomass_density"
    
    out_bm <- file.path(gis_proc, paste0(prefix, "_biomass.tif"))
    terra::writeRaster(r_bm_final, out_bm, overwrite = TRUE)
    message("    Saved: ", out_bm)
    
    # --------------------------------------------------------------------------
    # B. SoilGrids (CEC, pH)
    # --------------------------------------------------------------------------
    process_sg <- function(r_raw, name, scaler = 0.1) {
        if (is.null(r_raw)) return(NULL)
        message("  Processing ", name, "...")
        r_proj <- terra::project(r_raw, r_template, method = "average")
        r_proj <- r_proj * scaler
        r_proj <- terra::mask(r_proj, r_bm_final)
        names(r_proj) <- name
        out_p <- file.path(gis_proc, paste0(prefix, "_", name, ".tif"))
        terra::writeRaster(r_proj, out_p, overwrite = TRUE, gdal = c("COMPRESS=ZSTD", "PREDICTOR=2"))
        message("    Saved: ", out_p)
    }
    
    process_sg(r_cec_raw, "soil_cec", 0.1)
    process_sg(r_ph_raw, "soil_ph", 0.1)
    
    # --------------------------------------------------------------------------
    # C. Soil Temperature
    # --------------------------------------------------------------------------
    if (!is.null(r_temp_global)) {
        message("  Processing Soil Temperature...")
        r_temp_proj <- terra::project(r_temp_global, r_template, method = "average")
        r_temp_proj <- terra::mask(r_temp_proj, r_bm_final)
        
        # SBIO1 stores values as degC * 10. Check max value.
        v_mm <- terra::minmax(r_temp_proj)
        if (v_mm[2] > 60) {
            r_temp_proj <- r_temp_proj / 10
            message("    Dividing Soil Temp by 10 to convert to degC")
        }
        
        names(r_temp_proj) <- "soil_temp"
        out_temp <- file.path(gis_proc, paste0(prefix, "_soil_temp.tif"))
        terra::writeRaster(r_temp_proj, out_temp, overwrite = TRUE)
        message("    Saved: ", out_temp)
    } else {
        message("  Soil Temp global raster not found. Skipping.")
    }
}

message("\nAll regions processed successfully.")
