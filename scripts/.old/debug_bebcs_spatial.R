# debug_bebcs_spatial.R
devtools::load_all(".")
library(terra)

# 1. Load Data (TIFs)
bm <- terra::rast("data/demo_biomass.tif")
st <- terra::rast("data/demo_soil_temp.tif")

# 2. Pick a Valid Pixel
# Find a pixel with non-NA values in both
df <- as.data.frame(c(bm, st), xy = TRUE, cells = TRUE, na.rm = TRUE)
if (nrow(df) == 0) stop("No valid pixels found!")
idx <- 1 # Pick first valid pixel
row <- df[idx, ]

print(row)

# 3. Construct Params
params <- default_parameters()
params$c_price <- 100
params$bc_ag_value <- 50

# Spatial overrides
params$lat <- row$y
params$lon <- row$x
params$soil_temp <- row$soil_temp
params$biomass_density <- row$biomass_density

# Calc Plant Scale (replicate logic from spatial_tea)
collection_radius_km <- 50
area_km2 <- pi * collection_radius_km^2
annual_biomass_feedstock <- row$biomass_density * area_km2
ref_elec_prod <- 18.6 * 0.30 * 0.277778
params$plant_mw <- (annual_biomass_feedstock * ref_elec_prod) / (8760 * 0.85)

message("Testing calculate_bebcs with params:")
print(params$plant_mw)
print(params$soil_temp)

# 4. Run Function directly
res <- calculate_bebcs(params)
print(res)

message("Finished.")
