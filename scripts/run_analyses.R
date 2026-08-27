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
    params <- load_parameters("/media/dominic/Data/git/Biochar_AG/parameters.csv")
    dir.create(out_dir, showWarnings = FALSE)
    .regions <- c("US", "China", "Europe", "India")
    .scenarios <- c("default", "CP100_MW250", "CP100_MW250_reg", "EA_CP100_MW250", "EA_CP100_MW250_reg")
    run_all_manuscript_figures(save_map = TRUE)
}

# 2) Factorial Analysis
#    Executes a full factorial spatial TEA across 960 factorial scenarios
#    Evaluates technologies competitively
#    Generates:
#      "results/factorial_analysis_results.csv"
{
    source("scripts/factorial_analysis.R")
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
#    Evolution Plot (shap value of highest features against carbon price)
#    Beeswarm of SHAP values
#    Dependence plots
{
    source("scripts/MC_shap.R")
    generate_evolution_plots()
    generate_global_beeswarm_plots()
}

# 5) Spatial sensitivity Analysis
#    Generates:
#    spatial_sensitivity_results.csv (economic and CO2 metrics for each grid cell)
#    spatial_shap_values_by_location.csv (shap values for each grid cell)
{
    source("scripts/spatial_sensitivity.R")
}

# 6) Spatial SHAP
#    Generates:
#    maps of dominant SHAP feature by location
{
    source("scripts/spatial_shap.R")
}
