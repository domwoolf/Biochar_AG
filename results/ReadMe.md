# Model results

All model outputs (data tables, figures and AI summary tables) are written here by the scripts in `scripts/` (run from the repository root, e.g. via `scripts/run_analyses.R`). Only this ReadMe is tracked by git; everything else is regenerated. The manuscript and SI read these files through the symlink `Article/results -> ../results`, so the documents always use the current outputs; do not copy figures into `Article/`.

| Path | Contents | Written by |
|---|---|---|
| `figures/` | Manuscript figures (evaporation maps, MACCs, break-even prices, extra figures) | `manuscript_figures.R`, `manuscript_figures_extra.R` |
| `factorial_analysis_results.csv` | Factorial spatial TEA results | `factorial_analysis.R` |
| `mc_analysis_results.csv` | Monte Carlo results | `mc_analysis.R` |
| `mc_shap/` | Monte Carlo SHAP plots (evolution, beeswarm, dependence) | `MC_shap.R` |
| `spatial_sensitivity_results.csv` | Per-cell inputs and results for the spatial sensitivity scenario | `spatial_sensitivity.R` |
| `spatial_shap/` | Spatial SHAP: global and regional beeswarm and dependence plots, dominant-driver maps, importance table, per-cell SHAP summary | `spatial_shap.R` |
| `ai_summaries/` | Compact CSV summaries of the above for AI-assisted review | all of the above |

## Column definitions (factorial and summary tables)

All fundamental revenues and costs are normalized to the dry biomass input to ensure they are perfectly additive (i.e., `capital_cost_mg` + `om_cost_mg` + `biomass_cost_mg` + `co2_transport_cost_mg` = `total_cost`). The `_mg` suffix stands for "per Mg of dry biomass." 

### Spatial & Volume Metrics
*   **`area_best_km2`**: $km^2$ (Total area where the technology has the highest NPV of the three options).
*   **`area_viable_km2`**: $km^2$ (Subset of the best area where NPV > 0).
*   **`biomass_processed_yr_mg`**: Mg (metric tonnes) of dry biomass processed per year across the `area_viable_km2`.

### Economic Metrics (Normalized to Biomass)
*These are all in **USD / Mg of dry biomass***
*   **`npv_min`**, **`npv_max`**, **`npv_mean`**: Net Present Value.
*   **`mean_capital_cost_mg`**: Levelized capital cost.
*   **`mean_om_cost_mg`**: Operations & Maintenance costs.
*   **`mean_biomass_cost_mg`**: Total biomass cost (farm-gate purchase + logistics).
*   **`mean_co2_transport_cost_mg`**: CO2 transport & storage cost (derived by taking the $/tCO2 pipeline/storage cost and multiplying it by the Mg of CO2 captured per Mg of biomass).
*   **`mean_carbon_removal_revenue_mg`**: Total carbon market revenue.
*   **`mean_electricity_revenue_mg`**: Total wholesale electricity revenue.
*   **`mean_agronomic_revenue_mg`**: Total biochar fertilizer-replacement revenue.

### Physical & Technical Metrics
*   **`mean_co2_transport_distance_km`**: km (Least-cost-path pipeline distance to the sink).
*   **`mean_biomass_transport_distance_km`**: km (Effective road logistics distance, including the tortuosity multiplier).
*   **`mean_net_cdr`**: Mg $CO_2e$ / Mg of dry biomass (Net abatement, subtracting supply-chain/transport emissions).
*   **`mean_electricity_production_mwh`**: MWh / Mg of dry biomass.
*   **`mean_abatement_efficiency`**: % (Ratio of Net CDR to Gross Sequestration; 0-1 scale).

### Alternative Economic Metrics
*   **`mean_lcoe_usd_mwh`**: USD / MWh (Levelized Cost of Electricity, accounting for biomass/CCS costs and biochar revenues).
*   **`mean_cost_of_co2_avoided`**: USD / Mg $CO_2e$ (Total cost divided by net abatement; the standard metric for carbon removal technologies).
*   **`mean_total_capex_m`**: Millions of USD (The absolute upfront capital expenditure for a *single plant* at the requested `plant_mw_th` scale).
