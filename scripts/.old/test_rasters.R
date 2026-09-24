source("manuscript_figures.R")

check_biomass_rasters <- function() {
  regions <- c("US", "China", "Europe", "India")
  
  cat("=== BIOMASS RASTER DIAGNOSTICS ===\n")
  for (r in regions) {
    dat <- load_region_data(r)
    bm_raster <- dat$layers$biomass_density
    
    # Extract active values
    vals <- terra::values(bm_raster, mat = FALSE)
    active_vals <- vals[!is.na(vals) & vals > 0]
    
    # Check for unit scaling (Max should be hundreds, not single digits)
    max_val <- max(active_vals, na.rm = TRUE)
    mean_val <- mean(active_vals, na.rm = TRUE)
    
    # Check for smearing (What % of active cells have near-zero density?)
    # E.g., less than 10 Mg/km2 (0.1 Mg/ha)
    ghost_cells <- sum(active_vals < 10) / length(active_vals)
    
    cat(sprintf("%-10s | Max: %8.2f | Mean: %8.2f | Cells < 10 Mg/km2: %5.1f%%\n", 
                r, max_val, mean_val, ghost_cells * 100))
  }
}
check_biomass_rasters()
