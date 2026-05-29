# debug_sensitivity_values.R
devtools::load_all(".")
library(terra)

# 1. Load Data
bm <- terra::rast("data/demo_biomass.tif")
st <- terra::rast("data/demo_soil_temp.tif")
processed_layers <- list(biomass_density = bm, soil_temp = st)
template <- bm

# 2. Setup Params
base_params <- default_parameters()
base_params$bc_ag_value <- 50
base_params$c_price <- 100 # Match the user's reference point

# 3. Run BECCS Only
message("Running BECCS Spatial for $100...")
beccs_res <- run_spatial_tea(template, base_params, processed_layers, fun = calculate_beccs)

# 4. Inspect Values
vals <- values(beccs_res[["Net_Value_USD"]], mat = FALSE)
vals <- vals[!is.na(vals)]

message("BECCS Net Value Summary ($100/t):")
print(summary(vals))
message("Standard Deviation: ", sd(vals))

# Check for Uniformity
if (sd(vals) < 1) {
    message("WARNING: Values are uniform! Something is wrong.")
    # Check if Distance is varying?
    # run_spatial_tea doesn't output distance by default, but we can infer from costs.
    cost_vals <- values(beccs_res[["Total_Cost_USD_Mg"]], mat = FALSE)
    message("Total Cost Summary:")
    print(summary(cost_vals))
} else {
    message("Values are varying spatially.")
}
