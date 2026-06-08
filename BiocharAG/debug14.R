library(terra)
library(sf)
source("data-raw/generate_sinks.R")

r_template <- terra::rast("../GIS/processed/us_biomass.tif")
sinks_sub <- co2_sinks[co2_sinks$Region == "North America", ]

r_slope <- terra::rast(r_template)
values(r_slope) <- 0
friction_surface <- terra::rast(r_template)
terra::values(friction_surface) <- 1 

dist_list <- list()
for (i in 1:nrow(sinks_sub)) {
    sink_pt <- sinks_sub[i, ]
    sink_coords <- sf::st_coordinates(sink_pt)
    sink_cell <- terra::cellFromXY(friction_surface, sink_coords)
    if (is.na(sink_cell)) {
        e <- terra::ext(friction_surface)
        x <- max(min(sink_coords[1], e$xmax), e$xmin)
        y <- max(min(sink_coords[2], e$ymax), e$ymin)
        sink_cell <- terra::cellFromXY(friction_surface, cbind(x, y))
    }
    fs <- friction_surface
    fs[sink_cell] <- 0
    cost_m <- terra::costDist(fs, target = 0)
    dist_list[[i]] <- cost_m / 1000
}

dist_stack <- terra::rast(dist_list)
names(dist_stack) <- sinks_sub$Basin_Name

r_min_dist <- terra::app(dist_stack, min, na.rm = TRUE)
r_winner_idx <- terra::app(dist_stack, which.min)

print("r_winner_idx info:")
print(hasValues(r_winner_idx))
print(minmax(r_winner_idx))

r_offshore_flag <- terra::rast(r_template)

for (i in 1:nrow(sinks_sub)) {
    mask_winner <- (r_winner_idx == i)
    print(paste("Iter", i, "mask_winner hasValues:", hasValues(mask_winner)))
    print(minmax(mask_winner))
    
    r_offshore_flag[mask_winner] <- 0
}
