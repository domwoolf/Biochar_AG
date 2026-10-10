library(BiocharAG)
library(terra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)

# 1) Manuscript main figures
#    Generates:
#    fig 3 evaporation maps
#    fig 6 MACC
#    fig 8 breakeven C price
{
  source("scripts/manuscript_figures.R")
  params <- BiocharAG::set_scenario()
  dir.create(out_dir, showWarnings = FALSE)
  .regions <- c("US", "China", "Europe", "India")
  # .scenarios <- c("default", "CP100_MW250", "CP100_MW250_reg", "EA_CP100_MW250", "EA_CP100_MW250_reg")
  .scenarios <- c("default")
  run_all_manuscript_figures(save_map = TRUE)
}

# 2) Structural variant sweeps (replaces the full factorial analysis)
#    Plant size 50/250 MWth, dedicated pipelines, CO2-EOR allowed; carbon-price sweeps at regional rates
#    Generates: results/variants/variant_macc_data.csv, results/figures/Variants_MACC.png
{
  source("scripts/variant_sweeps.R")
}

# 3) Monte Carlo Simulations
#    Generates:
#    "results/mc_analysis_results.csv"
#    (very slow, comment out if not needed)
{
  source("scripts/mc_analysis.R")
}

# 4) Monte Carlo Shap Analysis
#    Generates:
#    SHAP importance of the sampled parameters for the regional break-even, takeover and BE net-value
#    quantities, beeswarm plots, and percentiles of every recorded quantity
{
  source("scripts/MC_shap.R")
  run_mc_shap()
}

# 5) Spatial SHAP (regression on break-even and takeover prices; issue #111)
#    Generates:
#    importance, dependence and takeover-price maps (results/spatial_shap/)
{
  source("scripts/spatial_shap.R")
}

# 6) Heat sensitivity (issue #106): optional; BEBCS heat mode is not part of the results (Discussion only)
#    BEBCS selling heat instead of power, where year-round heat demand exists (reported per cell only)
#    Generates: results/heat_sensitivity/ (cell table, maps, distribution plot)
# {
#   source("scripts/heat_sensitivity.R")
# }
