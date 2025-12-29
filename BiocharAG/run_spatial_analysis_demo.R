# run_spatial_analysis_demo.R
library(terra)
# library(BiocharAG) # Install failed
devtools::load_all(".") # Load from source
library(ggplot2)
library(sf)

# 1. Load Data
# Cannot use .rda for SpatRaster (pointers invalid across sessions)
# Load from TIFs created by processing script
bm <- terra::rast("data/demo_biomass.tif")
st <- terra::rast("data/demo_soil_temp.tif")

processed_layers <- list(
    biomass_density = bm,
    soil_temp = st
)

# Create Template from one of the layers
template <- bm

# 2. Setup Parameters
params <- default_parameters()
# Ensure we have carbon price range or fixed?
# Fixed for map: $100/t CO2
params$c_price <- 100
params$bc_ag_value <- 50 # Assumption for BEBCS

# 3. Run Spatial TEA
message("Running BES Spatial Analysis...")
bes_res <- run_spatial_tea(template, params, processed_layers, fun = calculate_bes)
names(bes_res) <- paste0("BES_", names(bes_res))

message("Running BECCS Spatial Analysis...")
beccs_res <- run_spatial_tea(template, params, processed_layers, fun = calculate_beccs)
names(beccs_res) <- paste0("BECCS_", names(beccs_res))

message("Running BEBCS Spatial Analysis...")
bebcs_res <- run_spatial_tea(template, params, processed_layers, fun = calculate_bebcs)
names(bebcs_res) <- paste0("BEBCS_", names(bebcs_res))

# 4. Compare & Find Optimal
# Stack Net Values
net_val_stack <- c(
    bes_res[["BES_Net_Value_USD"]],
    beccs_res[["BECCS_Net_Value_USD"]],
    bebcs_res[["BEBCS_Net_Value_USD"]]
)

names(net_val_stack) <- c("BES", "BECCS", "BEBCS")

# Find Max Tech
# which.max returns index
opt_idx <- terra::app(net_val_stack, which.max)
# Map index to name
# 1=BES, 2=BECCS, 3=BEBCS

# 5. Save Outputs
terra::writeRaster(bes_res, "output_spatial_bes.tif", overwrite = TRUE)
terra::writeRaster(beccs_res, "output_spatial_beccs.tif", overwrite = TRUE)
terra::writeRaster(bebcs_res, "output_spatial_bebcs.tif", overwrite = TRUE)
terra::writeRaster(opt_idx, "output_spatial_optimal.tif", overwrite = TRUE)

# 6. simple Plot
png("spatial_results_map.png", width = 1000, height = 800)
par(mfrow = c(2, 2))
plot(net_val_stack[["BES"]], main = "BES Net Value ($/Mg)", range = c(-100, 300))
plot(net_val_stack[["BECCS"]], main = "BECCS Net Value ($/Mg)", range = c(-100, 300))
plot(net_val_stack[["BEBCS"]], main = "BEBCS Net Value ($/Mg)", range = c(-100, 300))

# Optimal Map with Categorical Legend
levels(opt_idx) <- data.frame(id = 1:3, technology = c("BES", "BECCS", "BEBCS"))
col_pal <- c("blue", "red", "green")
plot(opt_idx, main = "Optimal Tech ($100/t CO2)", col = col_pal)
dev.off()

message("Spatial Analysis Complete. Map saved to spatial_results_map.png")
