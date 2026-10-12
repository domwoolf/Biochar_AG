# scripts/results_components.R
# Per-tonne economics and abatement of each technology at the central regional parameters and a carbon
# price of 100 US$/t CO2e: biomass-weighted means over all grid cells of revenues, costs and abatement
# (per Mg dry, ash-free feedstock). Used for the explanatory numbers in the Results.
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
