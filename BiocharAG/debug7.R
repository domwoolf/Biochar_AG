library(terra)
library(sf)
source("data-raw/generate_sinks.R")
r_template <- terra::rast("../GIS/processed/us_biomass.tif")
sinks_sub <- co2_sinks[co2_sinks$Region == "North America", ]

friction_surface <- terra::rast(r_template)
terra::values(friction_surface) <- 1 

sink_pt <- sinks_sub[1, ]
sink_coords <- sf::st_coordinates(sink_pt)
print(sink_coords)

fs <- friction_surface
sink_cell <- terra::cellFromXY(fs, sink_coords)
print(sink_cell)

# What if sink_cell is NA because the point is outside?
# Let's see:
fs[sink_cell] <- 0
d <- terra::costDist(fs, target=0)
print(d)
print(minmax(d))
