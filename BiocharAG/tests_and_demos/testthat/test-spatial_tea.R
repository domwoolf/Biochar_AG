test_that("run_spatial_tea works with dummy raster", {
    skip_if_not_installed("terra")
    library(terra)

    # Create a small 2x2 raster
    r <- rast(nrows = 2, ncols = 2, xmin = -90, xmax = -88, ymin = 38, ymax = 40)
    values(r) <- 1:4

    # create biomass density layer
    bd <- r
    values(bd) <- c(100, 200, 50, 0) # Mg/km2

    spatial_layers <- list(biomass_density = bd)
    params <- default_parameters()

    # Run BECCS
    out <- run_spatial_tea(r, params, spatial_layers, fun = BiocharAG::calculate_beccs)

    expect_s4_class(out, "SpatRaster")
    d <- dim(out) # nrows, ncols, nlyrs
    expect_equal(d[3], 3) # Net_Value, Total_Cost, Abatement

    # Check values
    vals <- terra::values(out)
    # Cell 1: High density -> Larger Scale -> Lower Unit Cost
    # Cell 3: Low density -> Small Scale -> Higher Unit Cost
    # Cell 4: 0 density -> plant_mw calculated as approx 0? (Should be handled)

    expect_false(any(is.na(vals[1:3, ]))) # checking valid cells
})
