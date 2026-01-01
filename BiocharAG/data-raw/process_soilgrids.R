# Process SoilGrids VRTs (CEC, pH) to match Project Grid
#
# Inputs:
# - GIS/processed/demo_biomass.tif (Template)
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

template_path <- file.path(gis_proc, "demo_biomass.tif")
if (!file.exists(template_path)) stop("Template raster missing!")

cec_vrt <- file.path(gis_raw, "cec/cec_0-5cm_mean.vrt")
ph_vrt <- file.path(gis_raw, "phh2o/phh2o_0-5cm_mean.vrt")

if (!file.exists(cec_vrt)) stop("CEC VRT not found at: ", cec_vrt)
if (!file.exists(ph_vrt)) stop("pH VRT not found at: ", ph_vrt)

# 2. Load Template
r_template <- terra::rast(template_path)
message("Template loaded: ", paste(dim(r_template), collapse = "x"), " | CRS: ", crs(r_template, proj = TRUE))

process_and_save <- function(in_vrt, out_name, scale_factor = 0.1, var_name) {
    message("Processing ", var_name, "...")

    # Load VRT
    r_in <- terra::rast(in_vrt)

    # Crop first to roughly the target extent (projected)
    # Since VRT is likely Mollweide/Homolosine and Template is likely LonLat or Albers,
    # direct cropping might be tricky if regions distinct.
    # Safer: Project directly to template.

    # terra::project handles warping, resampling, and extent matching
    message(" - Projecting and Resampling...")
    r_out <- terra::project(r_in, r_template, method = "bilinear")

    # Scale Units
    # SoilGrids standard: Integer * 10. We want float real units.
    message(" - Scaling units (x", scale_factor, ")...")
    r_out <- r_out * scale_factor

    # Mask by template (ensure NA where biomass is NA?)
    # Optional, but keeps it clean
    r_out <- terra::mask(r_out, r_template)

    names(r_out) <- var_name

    # Save
    out_path <- file.path(gis_proc, out_name)
    message(" - Saving to ", out_path)
    terra::writeRaster(r_out, out_path, overwrite = TRUE, gdal = c("COMPRESS=ZSTD", "PREDICTOR=2"))
    return(r_out)
}

# 3. Process CEC
# Input: mmol(c)/kg (e.g. 150). Output: cmol(+)/kg (e.g. 15). Factor = 0.1
r_cec <- process_and_save(cec_vrt, "soil_cec.tif", 0.1, "soil_cec")

# 4. Process pH
# Input: pH*10 (e.g. 60). Output: pH (e.g. 6.0). Factor = 0.1
r_ph <- process_and_save(ph_vrt, "soil_ph.tif", 0.1, "soil_ph")

message("Processing Complete.")
plot(c(r_cec, r_ph), main = c("Processed CEC (cmol/kg)", "Processed pH"))
