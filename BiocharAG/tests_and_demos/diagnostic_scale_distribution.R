library(terra)
library(dplyr)
library(tidyr)
devtools::load_all(".")

# --- Configuration ---
num_samples <- 500
test_c_price <- 150
test_discount_rate <- 0.08
# ---------------------

message("Loading spatial data for US...")
gis_path <- "../GIS/processed/"
# Load base template and mask out oceans/deserts (NA or 0)
bm_layer <- terra::rast(file.path(gis_path, "us_biomass.tif"))
bm_mask <- terra::ifel(bm_layer > 0, 1, NA)

message("Sampling ", num_samples, " valid land locations...")
set.seed(42) # For reproducibility
sample_pts <- terra::spatSample(bm_mask, size = num_samples, method = "random", na.rm = TRUE, xy = TRUE)

# Load context layers
layers <- list(
    biomass_density = bm_layer,
    soil_temp = terra::rast(file.path(gis_path, "us_soil_temp.tif")),
    dist_onshore = terra::rast(file.path(gis_path, "us_dist_sink.tif")),
    elec_price = terra::rast(file.path(gis_path, "us_elec_price.tif"))
)

sizes_mw_th <- c(5, 25, 50, 100, 250, 500)
for (sz in sizes_mw_th) {
    layers[[paste0("dist_", sz, "MWth")]] <- terra::rast(file.path(gis_path, paste0("us_dist_", sz, "MWth.tif")))
}

# Extract all layer data at the sample points
message("Extracting raster values at sample points...")
pt_data <- data.frame(lon = sample_pts$x, lat = sample_pts$y)
for (lyr_name in names(layers)) {
    extracted <- terra::extract(layers[[lyr_name]], pt_data[, c("lon", "lat")])
    pt_data[[lyr_name]] <- extracted[, 2]
}

# Clean any remaining NAs (e.g., points right on a coastline mismatch)
pt_data <- pt_data %>% filter(!is.na(biomass_density), !is.na(dist_onshore))

results_all <- list()
p_base <- default_parameters()
p_base$c_price <- test_c_price
p_base$discount_rate <- test_discount_rate

message(sprintf("Running optimization diagnostics on %d valid points...", nrow(pt_data)))

# Sweep locations
for (i in 1:nrow(pt_data)) {
    p <- p_base
    p$lat <- pt_data$lat[i]
    p$lon <- pt_data$lon[i]
    p$soil_temp <- pt_data$soil_temp[i]
    p$dist_onshore <- pt_data$dist_onshore[i]
    p$elec_price <- pt_data$elec_price[i] * p$wholesale_discount_factor

    for (tech in c("BES", "BECCS", "BEBCS")) {
        fun <- switch(tech,
            "BES" = calculate_bes,
            "BECCS" = calculate_beccs,
            "BEBCS" = calculate_bebcs
        )

        best_npv <- -Inf
        best_sz <- NA
        best_dist <- NA
        best_t_cost <- NA

        # Sweep sizes
        for (sz in sizes_mw_th) {
            p_sz <- p
            p_sz$plant_mw_th <- sz
            avg_dist <- pt_data[[paste0("dist_", sz, "MWth")]][i]

            # If the radius is NA, the region doesn't have enough biomass for this scale
            if (is.na(avg_dist)) next

            p_sz$avg_dist <- avg_dist

            # Run the specific TEA function
            res <- fun(p_sz)

            if (!is.na(res$net_value) && res$net_value > best_npv) {
                best_npv <- res$net_value
                best_sz <- sz
                # Calculate Effective Road Distance including tortuosity
                tort <- if (!is.null(p_sz$tortuosity)) p_sz$tortuosity else 1.3
                best_dist <- avg_dist * tort
                # Logistics Cost = fixed + (var * dist)
                best_t_cost <- p_sz$bm_transport_fixed + (p_sz$bm_transport_var * best_dist)
            }
        }

        if (!is.na(best_sz)) {
            results_all[[length(results_all) + 1]] <- data.frame(
                Point_ID = i,
                Tech = tech,
                Optimal_MWth = best_sz,
                Effective_Road_km = round(best_dist, 1),
                Logistics_Cost = round(best_t_cost, 2),
                NPV = round(best_npv, 2)
            )
        }
    }
}

df_res <- bind_rows(results_all)

# --- Generate Reports ---

message("\n=======================================================")
message("       OPTIMAL SCALE SELECTION FREQUENCY (%)           ")
message("=======================================================")
summary_table <- df_res %>%
    group_by(Tech, Optimal_MWth) %>%
    summarize(Count = n(), .groups = "drop") %>%
    group_by(Tech) %>%
    mutate(Percentage = round(100 * Count / sum(Count), 1)) %>%
    select(Tech, Optimal_MWth, Percentage, Count) %>%
    arrange(Tech, Optimal_MWth)

print(as.data.frame(summary_table))

message("\n=======================================================")
message("     AVERAGE LOGISTICS METRICS AT OPTIMAL SCALE        ")
message("=======================================================")
metrics_table <- df_res %>%
    group_by(Tech) %>%
    summarize(
        Avg_Road_Dist_km = round(mean(Effective_Road_km, na.rm = TRUE), 1),
        Max_Road_Dist_km = round(max(Effective_Road_km, na.rm = TRUE), 1),
        Avg_Logistics_Cost = round(mean(Logistics_Cost, na.rm = TRUE), 2)
    )
print(as.data.frame(metrics_table))
