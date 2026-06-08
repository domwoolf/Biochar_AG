library(terra)
library(sf)
source("data-raw/generate_sinks.R")
r_template <- terra::rast("../GIS/processed/us_biomass.tif")
sinks_sub <- co2_sinks[co2_sinks$Region == "North America", ]

friction_surface <- terra::rast(r_template)
terra::values(friction_surface) <- 1 

sink_pt <- sinks_sub[1, ]
coords <- sf::st_coordinates(sink_pt)
print(coords)

# 1. target as matrix of coords
print("Target as matrix:")
tryCatch({
    d1 <- terra::costDist(friction_surface, target = coords)
}, error=function(e) print(e))

# 2. target as cell number
print("Target as cell number:")
cell_num <- terra::cellFromXY(friction_surface, coords)
print(cell_num)
tryCatch({
    d2 <- terra::costDist(friction_surface, target = cell_num)
    print(d2)
}, error=function(e) print(e))

