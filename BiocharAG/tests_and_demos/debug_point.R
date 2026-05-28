library(terra)
library(dplyr)
devtools::load_all(".")

# --- Point to Audit (e.g., Middle of Illinois where BECCS dominates) ---
target_lon <- -89.0
target_lat <- 40.0
# -----------------------------------------------------------------------

message("Loading spatial data for US...")
gis_path <- "../GIS/processed/"
template <- terra::rast(file.path(gis_path, "us_biomass.tif"))
layers <- list(
    biomass_density = template,
    soil_temp = terra::rast(file.path(gis_path, "us_soil_temp.tif")),
    dist_onshore = terra::rast(file.path(gis_path, "us_dist_onshore.tif")),
    elec_price = terra::rast(file.path(gis_path, "us_elec_price.tif"))
)

sizes_mw_th <- c(5, 25, 50, 100, 250, 500)
for (sz in sizes_mw_th) {
    layers[[paste0("dist_", sz, "MWth")]] <- terra::rast(file.path(gis_path, paste0("us_dist_", sz, "MWth.tif")))
}

# Create a spatial point
pt <- terra::vect(data.frame(lon = target_lon, lat = target_lat), geom = c("lon", "lat"), crs = "EPSG:4326")

message("\n=== Diagnostic Report for Lon: ", target_lon, " Lat: ", target_lat, " ===")

# Extract raw spatial variables
cat("\n[Local Spatial Variables]\n")
cat("Biomass Density: ", terra::extract(layers$biomass_density, pt)[, 2], " Mg/km2\n")
cat("Dist to Sink:    ", terra::extract(layers$dist_onshore, pt)[, 2], " km\n")
cat("Elec Price:      $", terra::extract(layers$elec_price, pt)[, 2], " /MWh\n")

# Setup Params
p <- default_parameters()
p$c_price <- 150
p$discount_rate <- 0.08
p$dist_onshore <- terra::extract(layers$dist_onshore, pt)[, 2]
p$elec_price <- terra::extract(layers$elec_price, pt)[, 2] * p$wholesale_discount_factor

cat("\n[Scale Optimization for BECCS]\n")
results <- data.frame()

for (sz in sizes_mw_th) {
    p_sz <- p
    p_sz$plant_mw_th <- sz

    # Get pre-calc collection radius distance
    avg_dist <- terra::extract(layers[[paste0("dist_", sz, "MWth")]], pt)[, 2]
    p_sz$avg_dist <- avg_dist

    if (is.na(avg_dist)) {
        res_row <- data.frame(Scale_MWth = sz, Avg_Truck_km = NA, Truck_Cost = NA, Pipe_Cost = NA, Total_Cost = NA, NPV = NA)
    } else {
        res <- calculate_beccs(p_sz)
        truck_cost <- p_sz$bm_transport_fixed + (p_sz$bm_transport_var * avg_dist)

        res_row <- data.frame(
            Scale_MWth = sz,
            Avg_Truck_km = round(avg_dist, 1),
            Truck_Cost = round(truck_cost, 2),
            Pipe_Cost = round(res$ts_cost, 2),
            Total_Cost = round(res$total_cost, 2),
            NPV = round(res$net_value, 2)
        )
    }
    results <- bind_rows(results, res_row)
}

print(results)
