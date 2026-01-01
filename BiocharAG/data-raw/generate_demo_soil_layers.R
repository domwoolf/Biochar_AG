# Generate Demo Soil Layers for Advanced Valuation
# Creating plausible synthetic data for US Demo
#
# Real implementation would use:
# geodata::soil_world(var="phh2o", depth=15, path="GIS/raw")
#
# Current Demo:
# pH: Gradient from East (Low/Acidic) to West (High/Alkaline)
# CEC: Random variation with spatial autocorrelation

library(terra)

# 1. Load Template
template_path <- "../GIS/processed/demo_biomass.tif"
if (file.exists(template_path)) {
    r_template <- terra::rast(template_path)
} else {
    stop("Template biomass raster not found. Run previous processing scripts.")
}

# 2. Generate pH Layer
# Hypothesis: US pH roughly correlates with longitude (Rainfall)
# East (Wet) -> Acidic (~5.5 - 6.5)
# West (Arid) -> Alkaline (~7.0 - 8.0)
coords <- terra::xyFromCell(r_template, 1:ncell(r_template))
lons <- coords[, 1]

# Normalize Lon: -125 (West) to -70 (East)
# Scale: (lon - min) / (max - min)
lon_min <- -125
lon_max <- -70
lon_norm <- (lons - lon_min) / (lon_max - lon_min) # 0 (West) to 1 (East)

# pH Model: West=7.5, East=5.5
# ph = 7.5 - (2.0 * lon_norm) + Noise
set.seed(42)
noise <- rnorm(length(lons), mean = 0, sd = 0.5)
ph_vals <- 7.5 - (2.0 * lon_norm) + noise

r_ph <- terra::rast(r_template)
values(r_ph) <- ph_vals
names(r_ph) <- "soil_ph"

# 3. Generate CEC Layer (cmol/kg)
# Range: Sand (~5) to Clay (~30-40)
# Make it somewhat spatially correlated (smooth noise)
r_noise <- terra::rast(r_template)
values(r_noise) <- rnorm(ncell(r_template))
# Smooth it to create "regions"
r_cec_spatial <- terra::focal(r_noise, w = 5, fun = mean)

# Normalize 0-1
v_min <- minmax(r_cec_spatial)[1]
v_max <- minmax(r_cec_spatial)[2]
r_cec_norm <- (r_cec_spatial - v_min) / (v_max - v_min)

# Scale to typical CEC 5 -> 30
r_cec <- 5 + (r_cec_norm * 25)
names(r_cec) <- "soil_cec"

# 4. Save
out_dir <- "../GIS/processed"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

terra::writeRaster(r_ph, file.path(out_dir, "demo_soil_ph.tif"), overwrite = TRUE)
terra::writeRaster(r_cec, file.path(out_dir, "demo_soil_cec.tif"), overwrite = TRUE)

message("Demo Soil Layers Generated: pH and CEC")
plot(c(r_ph, r_cec), main = c("Demo Soil pH", "Demo Soil CEC"))
