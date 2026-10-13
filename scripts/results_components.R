# scripts/results_components.R
# Per-tonne economics and abatement of each technology at the central regional parameters and a carbon
# price of 100 US$/t CO2e: biomass-weighted means over all grid cells of revenues, costs and abatement
# (per Mg dry, ash-free feedstock), including a decomposition of abatement into carbon stored, avoided soil
# N2O, grid credit, residue counterfactual and emissions. Used for the explanatory numbers in the Results.
#
# Output: results/ai_summaries/per_tonne_components.csv (long format: region, technology, quantity, value)

source("scripts/manuscript_figures.R") # load_all and helpers

C_PRICE <- 100
wmean <- function(x, bm) {
  x <- rep_len(as.numeric(x), length(bm))
  ok <- is.finite(x)
  sum((x * bm)[ok]) / sum(bm[ok])
}

rows <- list()
for (r in c("US", "Europe", "China", "India")) {
  d <- load_region_data(r)
  bm <- d$vec$layers$biomass_density * d$vec$cell_area
  p <- set_scenario(region = r)
  p$region <- r
  p$c_price <- C_PRICE
  q <- cell_params(p, d$vec)
  res <- list(BE = calculate_bes(q), BECCS = calculate_beccs(q), PyCCS = calculate_bebcs(q))
  qty <- c(abatement = "tot_c_abatement", net_value = "net_value", total_cost = "total_cost", capital_cost = "capital_cost_mg",
           energy_revenue = "energy_revenue_mg", agronomic_revenue = "agronomic_revenue_mg",
           co2_transport_storage = "co2_transport_cost_mg", carbon_stored = "c_sequestered",
           yield_value = "bc_yield_value_mg", mineral_value = "bc_mineral_value_mg", soil_n2o = "soil_n2o_abatement")
  for (tech in names(res)) {
    for (nm in names(qty)) {
      v <- res[[tech]][[qty[[nm]]]]
      if (is.null(v)) next
      rows[[length(rows) + 1]] <- data.frame(region = r, technology = tech, quantity = nm, value = wmean(v, bm))
    }
  }
  # Decomposition of abatement: grid credit (exported energy x displaced grid intensity at this price; zero
  # where PyCCS flares its surplus), residue counterfactual (avoided open burning), and emissions (haulage,
  # field operations and CO2 transport) as the residual, so that
  # abatement = carbon_stored + soil_n2o + grid_credit + residue_counterfactual - emissions
  mef <- displaced_grid_ci(q)
  res_cf <- BiocharAG:::residue_counterfactual_ghg(q)
  for (tech in names(res)) {
    r_t <- res[[tech]]
    grid <- r_t$energy_output * mef
    stored <- if (is.null(r_t$c_sequestered)) 0 else r_t$c_sequestered
    n2o <- if (is.null(r_t$soil_n2o_abatement)) 0 else r_t$soil_n2o_abatement
    emis <- stored + n2o + grid + res_cf - r_t$tot_c_abatement
    for (nm in c("grid_credit", "residue_counterfactual", "emissions")) {
      v <- switch(nm, grid_credit = grid, residue_counterfactual = res_cf, emissions = emis)
      rows[[length(rows) + 1]] <- data.frame(region = r, technology = tech, quantity = nm, value = wmean(v, bm))
    }
  }
  # Biochar reapplication on residue allocated to PyCCS (best and viable technology at this price): biomass-
  # weighted 10th, 50th and 90th percentiles of the interval between applications to the same field (years,
  # = number of cohorts) and the median dose per application (Mg biochar/ha)
  nvm <- cbind(res$BE$net_value, res$BECCS$net_value, res$PyCCS$net_value)
  nvm[!is.finite(nvm)] <- -Inf
  sel <- max.col(nvm, ties.method = "first") == 3 & apply(nvm, 1, max) >= 0 & is.finite(res$PyCCS$bc_cohorts)
  wq <- function(x, w, pr) {
    o <- order(x)
    cw <- cumsum(w[o]) / sum(w)
    x[o][which(cw >= pr)[1]]
  }
  if (any(sel)) {
    for (pr in c(0.1, 0.5, 0.9)) {
      rows[[length(rows) + 1]] <- data.frame(region = r, technology = "PyCCS", quantity = sprintf("reapply_interval_p%d", 100 * pr),
                                             value = wq(res$PyCCS$bc_cohorts[sel], bm[sel], pr))
    }
    rows[[length(rows) + 1]] <- data.frame(region = r, technology = "PyCCS", quantity = "dose_p50",
                                           value = wq(res$PyCCS$bc_dose_eff[sel], bm[sel], 0.5))
  }
  # Share of biomass where PyCCS exports no energy (surplus vapours flared)
  rows[[length(rows) + 1]] <- data.frame(region = r, technology = "PyCCS", quantity = "share_no_energy",
                                         value = wmean(as.numeric(res$PyCCS$bebcs_mode == 2), bm))
  rows[[length(rows) + 1]] <- data.frame(region = r, technology = "all", quantity = "feedstock_cost",
                                         value = wmean(q$feedstock_cost, bm))
}
out <- do.call(rbind, rows)
dir.create("results/ai_summaries", showWarnings = FALSE, recursive = TRUE)
write.csv(out, "results/ai_summaries/per_tonne_components.csv", row.names = FALSE)
message("Per-tonne components written: results/ai_summaries/per_tonne_components.csv")
