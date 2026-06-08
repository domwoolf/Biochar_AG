library(terra)
library(sf)
library(dplyr)
library(geodata)

source("data-raw/generate_sinks.R")
r_template <- terra::rast("../GIS/processed/us_biomass.tif")
sinks_sub <- co2_sinks %>% dplyr::filter(Region == "North America")
r_dem <- geodata::elevation_global(res = 5, path = tempdir()) 
r_dem_proj <- terra::project(r_dem, r_template)
r_slope <- terra::terrain(r_dem_proj, v = "slope", unit = "degrees")
friction_surface <- terra::rast(r_template)
terra::values(friction_surface) <- 1 
slope_penalty <- terra::ifel(r_slope > 3, 1 + ((r_slope - 3) * 0.05), 1)
slope_penalty <- terra::subst(slope_penalty, NA, 1) 
friction_surface <- friction_surface * slope_penalty

dist_list <- list()
for (i in 1:2) {
    message("Iter ", i)
    sink_pt <- sinks_sub[i, ]
    message("CostDist")
    cost_m <- terra::costDist(friction_surface, target = sf::st_coordinates(sink_pt))
    message("Div")
    dist_list[[i]] <- cost_m / 1000
}
message("Done")
