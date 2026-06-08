library(terra)
library(sf)
library(dplyr)
library(geodata)

# ==============================================================================
# Setup: Load Sinks and Define Regions
# ==============================================================================

# 1. Load the Sinks (generated in Step 1)
if (!exists("co2_sinks")) {
    source("data-raw/generate_sinks.R")
}

# 2. Define File Paths
raw_dir <- "../GIS/raw"
proc_dir <- "../GIS/processed"

if (!dir.exists(raw_dir)) dir.create(raw_dir, recursive = TRUE)
if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)

# ==============================================================================
# Helper Function: Process Region
# ==============================================================================
#' Generate Transport Layers for a Region (Terrain-Optimized)
#' @param region_name String matching the 'Region' column in co2_sinks
#' @param template_path Path to the biomass template raster for this region
#' @param file_prefix Prefix for output files (e.g., "us")
#' @param hires_factor Factor by which to disaggregate the grid for high-res routing. 
#'        e.g. fact=10 turns a 20km grid into a 2km grid. 1 = Native resolution.
#' @param pa_cost_multiplier Multiplier for WDPA Protected Areas. NA = absolute barrier.
process_transport_layers <- function(region_name, template_path, file_prefix, 
                                     hires_factor = 10, pa_cost_multiplier = NA) {
    message(paste0("\n=== Processing Terrain-Optimized Transport for: ", region_name, " ==="))

    # 1. Load Template
    if (!file.exists(template_path)) {
        warning(paste("Template not found:", template_path, "- Skipping."))
        return(NULL)
    }
    r_template <- terra::rast(template_path)

    # 2. Filter Sinks
    sinks_sub <- co2_sinks %>% dplyr::filter(Region == region_name)
    if (nrow(sinks_sub) == 0) {
        warning("No sinks found for this region in co2_sinks database.")
        return(NULL)
    }
    message(paste0("Found ", nrow(sinks_sub), " sinks."))

    # 2.5 WDPA Protected Areas
    pa_clean <- NULL
    if (hires_factor > 1) {
        message("Downloading/loading WDPA Protected Areas...")
        wdpa_dir <- file.path(raw_dir, "WDPA")
        zip_files <- list.files(wdpa_dir, pattern = "\\.zip$", full.names = TRUE)
        
        if (length(zip_files) > 0) {
            # Use the most recent zip file if there are multiple
            zip_file <- zip_files[which.max(file.info(zip_files)$mtime)]
            message("Loading local WDPA database from: ", zip_file)
            
            tryCatch({
                # Construct GDAL virtual file system path for the geodatabase inside the zip
                gdb_name <- gsub("\\.zip$", ".gdb", basename(zip_file))
                vsi_path <- paste0("/vsizip/", zip_file, "/", gdb_name)
                
                # Find the polygon layer automatically (usually WDPA_poly_MonYYYY)
                layers <- sf::st_layers(vsi_path)$name
                poly_layer <- layers[grepl("poly", layers, ignore.case = TRUE)][1]
                
                # Ensure the bbox matches the CRS of the WDPA database (EPSG:4326) for on-the-fly cropping
                message("Cropping WDPA polygons directly from disk (this is extremely fast)...")
                e_poly <- sf::st_as_sfc(sf::st_bbox(r_template))
                e_poly_wgs84 <- sf::st_transform(e_poly, 4326)
                
                # Read only the polygons within the bounding box
                pa_raw_cropped <- sf::st_read(vsi_path, layer = poly_layer, 
                                              wkt_filter = sf::st_as_text(e_poly_wgs84), 
                                              quiet = TRUE)
                
                # Project back to our native template CRS
                pa_raw_cropped <- sf::st_transform(pa_raw_cropped, terra::crs(r_template))
                
                message("Cleaning cropped WDPA data (this may take a minute)...")
                if (requireNamespace("wdpar", quietly = TRUE)) {
                    pa_clean <- wdpar::wdpa_clean(pa_raw_cropped)
                } else {
                    warning("wdpar package not installed. Using simple st_make_valid.")
                    pa_clean <- sf::st_make_valid(pa_raw_cropped)
                }
            }, error = function(e) {
                warning("Failed to load local WDPA data: ", e$message)
            })
        } else {
            warning(paste("No WDPA zip files found in", wdpa_dir, "- Skipping PA integration."))
        }
    }

    # 3. Create High-Resolution Base Grid
    # --------------------------------------------------------------------------
    if (hires_factor > 1) {
        message(paste0("Disaggregating grid by factor of ", hires_factor, " for micro-routing..."))
        r_base <- terra::disagg(r_template, fact = hires_factor)
    } else {
        r_base <- r_template
    }

    # 4. Generate Topographic Friction
    # --------------------------------------------------------------------------
    message("Fetching Global DEM and computing slope...")
    r_dem <- geodata::elevation_global(res = 5, path = raw_dir) 
    
    # Reproject DEM to match the template
    r_dem_proj <- terra::project(r_dem, r_base)
    
    # Calculate Slope (in degrees)
    r_slope <- terra::terrain(r_dem_proj, v = "slope", unit = "degrees")
    
    # Base friction is 1
    friction_surface <- terra::rast(r_base)
    terra::values(friction_surface) <- 1 
    
    # Add an exponential penalty for steep slopes: +5% cost per degree above 3 degrees.
    slope_penalty <- terra::ifel(r_slope > 3, 1 + ((r_slope - 3) * 0.05), 1)
    slope_penalty <- terra::subst(slope_penalty, NA, 1) 
    
    friction_surface <- friction_surface * slope_penalty

    # 5. Apply Protected Areas Penalty
    # --------------------------------------------------------------------------
    if (!is.null(pa_clean) && nrow(pa_clean) > 0) {
        message("Rasterizing Protected Areas to friction surface...")
        
        # Crop PA to the raster extent to save memory during rasterize
        pa_cropped <- sf::st_crop(pa_clean, terra::ext(r_base))
        
        if (nrow(pa_cropped) > 0) {
            r_pa <- terra::rasterize(terra::vect(pa_cropped), friction_surface, field = 1, background = 0)
            
            if (is.na(pa_cost_multiplier)) {
                # Absolute barrier
                friction_surface <- terra::ifel(r_pa == 1, NA, friction_surface)
            } else {
                # Cost multiplier
                friction_surface <- terra::ifel(r_pa == 1, friction_surface * pa_cost_multiplier, friction_surface)
            }
        }
    }

    # 6. Calculate Cost-Distance to EACH Sink Individually
    # --------------------------------------------------------------------------
    message("Calculating Least Cost Paths to individual sinks...")
    
    dist_list <- list()
    for (i in 1:nrow(sinks_sub)) {
        sink_pt <- sinks_sub[i, ]
        sink_coords <- sf::st_coordinates(sink_pt)
        sink_cell <- terra::cellFromXY(friction_surface, sink_coords)
        
        # If sink is slightly outside the bounding box, snap it to the nearest edge
        if (is.na(sink_cell)) {
            e <- terra::ext(friction_surface)
            x <- max(min(sink_coords[1], e$xmax), e$xmin)
            y <- max(min(sink_coords[2], e$ymax), e$ymin)
            sink_cell <- terra::cellFromXY(friction_surface, cbind(x, y))
        }
        
        # terra::costDist calculates distance to a target VALUE in the raster
        fs <- friction_surface
        fs[sink_cell] <- 0
        cost_m <- terra::costDist(fs, target = 0)
        
        # Convert to Kilometers
        cost_km <- cost_m / 1000
        
        # Aggregate back to native resolution if we used hires
        if (hires_factor > 1) {
            cost_km <- terra::aggregate(cost_km, fact = hires_factor, fun = mean, na.rm = TRUE)
            # Ensure perfect alignment
            cost_km <- terra::resample(cost_km, r_template, method = "bilinear")
        }
        
        dist_list[[i]] <- cost_km
    }
    
    # 7. Stack and Determine Winners
    # --------------------------------------------------------------------------
    dist_stack <- terra::rast(dist_list)
    names(dist_stack) <- sinks_sub$Basin_Name

    message("Determining optimal routing and sink allocations...")
    
    # Minimum cost-distance across all sinks
    r_min_dist <- terra::app(dist_stack, min, na.rm = TRUE)
    names(r_min_dist) <- "dist_sink_km"
    
    # Index of the winning sink
    r_winner_idx <- terra::app(dist_stack, which.min)

    # 8. Map Sink Properties based on the Winner
    # --------------------------------------------------------------------------
    # Initialize rasters for properties
    r_offshore_flag <- terra::rast(r_template)
    terra::values(r_offshore_flag) <- NA # Default NA
    r_saline_dist <- terra::rast(r_template)
    terra::values(r_saline_dist) <- NA # Default NA

    # Extract properties from the vector database
    is_offshore_vec <- ifelse(sinks_sub$Type == "Offshore", 1, 0)
    is_eor_vec <- sinks_sub$Is_EOR
    
    # Loop over the indices to populate the property maps
    for (i in 1:nrow(sinks_sub)) {
        mask_winner <- (r_winner_idx == i)
        
        # Assign Offshore Flag
        r_offshore_flag[mask_winner] <- is_offshore_vec[i]
        
        # Determine Saline Distance
        if (is_eor_vec[i] == FALSE) {
            r_saline_dist[mask_winner] <- r_min_dist[mask_winner]
        }
    }
    
    names(r_offshore_flag) <- "sink_is_offshore"

    # 9. Handle Saline routing for regions where EOR won
    # --------------------------------------------------------------------------
    saline_indices <- which(sinks_sub$Is_EOR == FALSE)
    if (length(saline_indices) > 0) {
        saline_stack <- dist_stack[[saline_indices]]
        r_min_saline <- terra::app(saline_stack, min, na.rm = TRUE)
        
        # Fill in the NA gaps in r_saline_dist where EOR won the primary routing
        mask_needs_saline <- is.na(r_saline_dist)
        r_saline_dist[mask_needs_saline] <- r_min_saline[mask_needs_saline]
    }
    names(r_saline_dist) <- "dist_sink_saline_km"

    # ==============================================================================
    # Final Output
    # ==============================================================================
    out_dist <- file.path(proc_dir, paste0(file_prefix, "_dist_sink.tif"))
    out_dist_saline <- file.path(proc_dir, paste0(file_prefix, "_dist_sink_saline.tif"))
    out_type <- file.path(proc_dir, paste0(file_prefix, "_sink_type.tif"))

    terra::writeRaster(r_min_dist, out_dist, overwrite = TRUE)
    terra::writeRaster(r_saline_dist, out_dist_saline, overwrite = TRUE)
    terra::writeRaster(r_offshore_flag, out_type, overwrite = TRUE)

    message(paste("Saved:", out_dist))
    return(list(dist = r_min_dist, dist_saline = r_saline_dist, type = r_offshore_flag))
}

# ==============================================================================
# Execution: Run for All Study Areas
# ==============================================================================

# 1. United States (Coterminous)
# Note: PA fetch and calculation might take ~10-15 minutes first time for USA due to WDPA size
process_transport_layers(
    region_name = "North America",
    template_path = "../GIS/processed/us_biomass.tif",
    file_prefix = "us",
    hires_factor = 10,
    pa_cost_multiplier = NA
)

# 2. China
process_transport_layers(
    region_name = "China",
    template_path = "../GIS/processed/china_biomass.tif",
    file_prefix = "china",
    hires_factor = 10,
    pa_cost_multiplier = NA
)

# 3. India
process_transport_layers(
    region_name = "India",
    template_path = "../GIS/processed/india_biomass.tif",
    file_prefix = "india",
    hires_factor = 10,
    pa_cost_multiplier = NA
)

# 4. Europe
process_transport_layers(
    region_name = "Europe",
    template_path = "../GIS/processed/europe_biomass.tif",
    file_prefix = "europe",
    hires_factor = 10,
    pa_cost_multiplier = NA
)

