# run_spatial_analysis_demo.R
library(terra)
# library(BiocharAG) # Install failed
devtools::load_all(".") # Load from source
library(ggplot2)
library(sf)

# 1. Load Data
# Load from GIS folder (Root/GIS/processed/)
# Assuming working directory is BiocharAG/
bm_path <- "../GIS/processed/demo_biomass.tif"
st_path <- "../GIS/processed/demo_soil_temp.tif"
ep_path <- "../GIS/processed/demo_elec_price.tif"

bm <- terra::rast(bm_path)
st <- terra::rast(st_path)

processed_layers <- list(
    biomass_density = bm,
    soil_temp = st,
    elec_price = terra::rast(ep_path)
)

# Create Template from one of the layers
template <- bm

# 2. Setup Parameters
base_params <- default_parameters()
base_params$bc_ag_value <- 50 # Assumption for BEBCS

c_prices <- seq(0, 500, 100)
pdf("spatial_sensitivity.pdf", width = 12, height = 10)

for (cp in c_prices) {
    message("Running Spatial Analysis for Carbon Price: $", cp)
    params <- base_params
    params$c_price <- cp

    # 3. Run Spatial TEA
    message("  Running BES...")
    bes_res <- run_spatial_tea(template, params, processed_layers, fun = calculate_bes)

    message("  Running BECCS...")
    beccs_res <- run_spatial_tea(template, params, processed_layers, fun = calculate_beccs)

    message("  Running BEBCS...")
    bebcs_res <- run_spatial_tea(template, params, processed_layers, fun = calculate_bebcs)

    # 4. Compare & Find Optimal
    net_val_stack <- c(
        bes_res[["Net_Value_USD"]],
        beccs_res[["Net_Value_USD"]],
        bebcs_res[["Net_Value_USD"]]
    )
    names(net_val_stack) <- c("BES", "BECCS", "BEBCS")

    # Find Max Tech (1=BES, 2=BECCS, 3=BEBCS)
    opt_idx <- terra::app(net_val_stack, which.max)

    # 5. Plot
    par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))

    # Determine common range for Net Value plots for comparability across pages?
    # Or let them auto-scale? Auto-scale per page is safer for visibility.
    # But consistently across the 3 techs on one page is good.

    # v_range <- terra::minmax(net_val_stack)
    # zlim <- c(min(v_range[1, ], na.rm = TRUE), max(v_range[2, ], na.rm = TRUE))
    # User feedback: Shared scale hides spatial detail. Reverting to per-panel scaling.

    terra::plot(net_val_stack[["BES"]], main = "BES Net Value ($/Mg)", col = map.pal("viridis"))
    terra::plot(net_val_stack[["BECCS"]], main = "BECCS Net Value ($/Mg)", col = map.pal("viridis"))
    terra::plot(net_val_stack[["BEBCS"]], main = "BEBCS Net Value ($/Mg)", col = map.pal("viridis"))

    # Optimal Map
    # Create categorical raster for plotting
    # Explicitly set RAT (Raster Attribute Table) to ensure consistent colors
    levels(opt_idx) <- data.frame(id = 1:3, technology = c("BES", "BECCS", "BEBCS"))
    cols <- c("blue", "red", "green")

    terra::plot(opt_idx,
        main = paste0("Optimal Tech (C Price: $", cp, ")"),
        col = cols
    ) # Levels handled by object

    mtext(paste0("Spatial Sensitivity: Carbon Price $", cp, "/tCO2"), outer = TRUE, cex = 1.5)
}

dev.off()
message("Sensitivity Analysis Complete. Saved to spatial_sensitivity.pdf")
