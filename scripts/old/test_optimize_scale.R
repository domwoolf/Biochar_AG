# tests_and_demos/test_optimize_scale.R
library(terra)
devtools::load_all(".")

# Create dummy rasters
r <- rast(matrix(1:100, 10, 10))
biomass_density <- r / 10 + 0.1 # 0.2 to 10.1 Mg/km2
soil_temp <- r / 10 + 10 # 10.1 to 20
elec_price <- r / 1000 + 0.05 # 0.051 to 0.15

spatial_layers <- list(
  biomass_density = biomass_density,
  soil_temp = soil_temp,
  elec_price = elec_price
)

# Precalculate dist rasters for the test
sizes <- c(5, 25, 50, 100, 250, 500)
bm_lhv <- 18.6
capacity_factor <- 0.85
for (sz in sizes) {
    annual_biomass <- (sz * 8760 * capacity_factor) / (bm_lhv * 0.277778)
    r_radius <- sqrt((annual_biomass / biomass_density) / pi)
    spatial_layers[[paste0("dist_", sz, "MWth")]] <- (2/3) * r_radius
}

params <- default_parameters()

message("Running with optimize_scale = FALSE")
t1 <- system.time({
  params_false <- params
  params_false$optimize_scale <- FALSE
  res_false <- run_spatial_tea(r, params_false, spatial_layers, fun = calculate_beccs)
})
print(t1)

message("Running with optimize_scale = TRUE")
t2 <- system.time({
  params_true <- params
  params_true$optimize_scale <- TRUE
  res_true <- run_spatial_tea(r, params_true, spatial_layers, fun = calculate_beccs)
})
print(t2)

print(res_true)
