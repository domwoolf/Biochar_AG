# Diagnostic Script: Diagnose MACC vs. Spatial Map Allocation
source("scripts/manuscript_figures.R")

diagnose_region_allocation <- function(region_name = "China", c_price_test = 100, dr_test = 0.08) {
  dat <- load_region_data(region_name)
  cell_area <- terra::cellSize(dat$template, unit = "km")

  # 1. Parameter Check: ensure mechanistic ag valuation is active
  params <- BiocharAG::set_scenario(BiocharAG::scenarios[["default"]])
  params$region <- region_name
  params$discount_rate <- dr_test
  params$bc_valuation_method <- "advanced_mechanistic"

  base_res <- get_linear_baseline(dat$template, dat$layers, params, vec = dat$vec)

  stack_df <- terra::as.data.frame(
    c(dat$layers$biomass_density, cell_area, base_res$net, base_res$abate),
    na.rm = TRUE
  )
  names(stack_df)[1:8] <- c("bm_density", "area_km2", "NPV0_BES", "NPV0_BECCS", "NPV0_BEBCS", "A_BES", "A_BECCS", "A_BEBCS")
  stack_df$cell_bm <- stack_df$bm_density * stack_df$area_km2

  # Calculate NPV at target test price
  stack_df$val_bes <- stack_df$NPV0_BES + c_price_test * stack_df$A_BES
  stack_df$val_beccs <- stack_df$NPV0_BECCS + c_price_test * stack_df$A_BECCS
  stack_df$val_bebcs <- stack_df$NPV0_BEBCS + c_price_test * stack_df$A_BEBCS

  # Unconstrained Winner (as in Fig 3)
  stack_df$winner_unconstrained <- apply(stack_df[, c("val_bes", "val_beccs", "val_bebcs")], 1, function(x) c("BES", "BECCS", "BEBCS")[which.max(x)])

  # Constrained Winner with NPV >= 0 (as in Fig 6 MACC)
  stack_df$max_val <- pmax(stack_df$val_bes, stack_df$val_beccs, stack_df$val_bebcs)
  stack_df$winner_macc <- ifelse(stack_df$max_val >= 0, stack_df$winner_unconstrained, "None")

  # 2. Decompose allocations
  cat(sprintf("\n=== DIAGNOSTIC REPORT: %s at $%d/t CO2 (DR = %.1f%%) ===\n", region_name, c_price_test, dr_test * 100))
  cat(sprintf("Cells with NPV >= 0: %.2f%% of total active cells\n\n", 100 * mean(stack_df$winner_macc != "None")))

  summary_table <- data.frame(Technology = c("BES", "BECCS", "BEBCS"))

  # Metric A: Unconstrained Area Share (%)
  summary_table$Unconstrained_Area_Pct <- sapply(summary_table$Technology, function(t) {
    100 * sum(stack_df$area_km2[stack_df$winner_unconstrained == t]) / sum(stack_df$area_km2)
  })

  # Metric B: MACC Viable Area Share (%)
  viable_area <- sum(stack_df$area_km2[stack_df$winner_macc != "None"])
  summary_table$Viable_Area_Pct <- sapply(summary_table$Technology, function(t) {
    if (viable_area == 0) return(0)
    100 * sum(stack_df$area_km2[stack_df$winner_macc == t]) / viable_area
  })

  # Metric C: Biomass Processed Share (%)
  viable_bm <- sum(stack_df$cell_bm[stack_df$winner_macc != "None"])
  summary_table$Biomass_Share_Pct <- sapply(summary_table$Technology, function(t) {
    if (viable_bm == 0) return(0)
    100 * sum(stack_df$cell_bm[stack_df$winner_macc == t]) / viable_bm
  })

  # Metric D: Total Abatement Share (%) as shown on MACC
  stack_df$abatement_delivered <- 0
  stack_df$abatement_delivered[stack_df$winner_macc == "BES"] <-
    stack_df$cell_bm[stack_df$winner_macc == "BES"] * stack_df$A_BES[stack_df$winner_macc == "BES"]
  stack_df$abatement_delivered[stack_df$winner_macc == "BECCS"] <-
    stack_df$cell_bm[stack_df$winner_macc == "BECCS"] * stack_df$A_BECCS[stack_df$winner_macc == "BECCS"]
  stack_df$abatement_delivered[stack_df$winner_macc == "BEBCS"] <-
    stack_df$cell_bm[stack_df$winner_macc == "BEBCS"] * stack_df$A_BEBCS[stack_df$winner_macc == "BEBCS"]

  tot_abatement <- sum(stack_df$abatement_delivered)
  summary_table$MACC_Abatement_Share_Pct <- sapply(summary_table$Technology, function(t) {
    if (tot_abatement == 0) return(0)
    100 * sum(stack_df$abatement_delivered[stack_df$winner_macc == t]) / tot_abatement
  })

  summary_table[, -1] <- round(summary_table[, -1], 2)
  print(summary_table)
}

# Run diagnostics for China and US
diagnose_region_allocation("China", c_price_test = 100, dr_test = 0.08)
diagnose_region_allocation("US", c_price_test = 100, dr_test = 0.08)
