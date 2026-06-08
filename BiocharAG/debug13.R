options(error = function() traceback(2))
library(terra)
library(sf)
source("data-raw/generate_sinks.R")
process_transport_layers <- function(region_name, template_path, file_prefix) {
    message(paste0("\n=== Processing Terrain-Optimized Transport for: ", region_name, " ==="))

    r_template <- terra::rast(template_path)
    sinks_sub <- co2_sinks[co2_sinks$Region == region_name, ]
    message(paste0("Found ", nrow(sinks_sub), " sinks. Building Friction Surface..."))

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

    message("Determining optimal routing and sink allocations...")
    
    r_min_dist <- terra::app(dist_stack, min, na.rm = TRUE)
    names(r_min_dist) <- "dist_sink_km"
    r_winner_idx <- terra::app(dist_stack, which.min)

    r_offshore_flag <- terra::rast(r_template)
    r_saline_dist <- terra::rast(r_template)
    terra::values(r_saline_dist) <- NA 

    is_offshore_vec <- ifelse(sinks_sub$Type == "Offshore", 1, 0)
    is_eor_vec <- sinks_sub$Is_EOR
    
    for (i in 1:nrow(sinks_sub)) {
        mask_winner <- (r_winner_idx == i)
        r_offshore_flag[mask_winner] <- is_offshore_vec[i]
        if (is_eor_vec[i] == FALSE) {
            r_saline_dist[mask_winner] <- r_min_dist[mask_winner]
        }
    }
    
    names(r_offshore_flag) <- "sink_is_offshore"
    saline_indices <- which(sinks_sub$Is_EOR == FALSE)
    if (length(saline_indices) > 0) {
        saline_stack <- dist_stack[[saline_indices]]
        r_min_saline <- terra::app(saline_stack, min, na.rm = TRUE)
        mask_needs_saline <- is.na(r_saline_dist)
        r_saline_dist[mask_needs_saline] <- r_min_saline[mask_needs_saline]
    }
}
process_transport_layers("North America", "../GIS/processed/us_biomass.tif", "us")

