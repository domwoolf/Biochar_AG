# Results files read by the manuscript (`results-values` and `discussion-values` chunks)

All files are written by the model pipeline (`Article/pipeline.qmd`, one cached step per script) and read by
`Article/BiomassTEA.qmd`. Common conventions:

- Technology codes: `BES` = BE, `BECCS` = BECCS, `BEBCS` = PyCCS (manuscript display names in parentheses;
  `per_tonne_components.csv` already uses the display names).
- Feedstock quantities are per Mg of **dry, ash-free** residue. Costs in **2024 US$**. Carbon prices in
  **US$ Mg⁻¹ CO₂e**. Regions: `US`, `Europe`, `China`, `India`.
- "Best" technology in a grid cell = highest net value at the given carbon price; "viable" = that net value > 0.
- Unless stated otherwise: central (deterministic) regional parameters, regional market discount rates,
  125 MWₜₕ plants, shared CO₂ trunklines, no EOR sinks.

---

## 1. `results/ai_summaries/evaporation_grid_summary_default.csv`

Share of residue allocated to each technology, by carbon price. From `run_all_manuscript_figures()`
(scripts/manuscript_figures.R): the full model is run in every cell at each carbon price; each cell is assigned to
its best technology and flagged viable or not. Basis of @fig-evap and @fig-si-evap-social.

```
134 obs. of 9 variables:
 $ region       : chr  "China" ...
 $ dr           : chr  "market" | "social"        # regional market discount rate | 2% social rate
 $ cp           : int  0, 50, 100, 150, 200, 250   # carbon price (US$ Mg-1 CO2e)
 $ technology   : chr  "BES" | "BECCS" | "BEBCS"  # best technology in the cell
 $ viable       : logi                              # best technology has net value > 0
 $ cells        : int                               # number of grid cells
 $ biomass_Mg   : num                               # residue in those cells (Mg yr-1)
 $ biomass_share: num                               # fraction of the region's residue
 $ cell_share   : num                               # fraction of the region's cells
```

## 2. `results/ai_summaries/macc_data_default.csv`

Marginal abatement cost curves (@fig-macc, @fig-si-macc-biomass). From the carbon-price sweep
(`run_price_sweep`, 0–250 in steps of 5 and 275–500 in steps of 25 US$ Mg⁻¹ CO₂e): at each price, each cell is
assigned to its best technology if viable, and abatement, residue and cropland area are summed by technology.

```
9036 obs. of 5 variables:
 $ Price     : int   # carbon price (US$ Mg-1 CO2e)
 $ Metric    : chr   "Abatement" (Tg CO2e yr-1) | "Biomass" (Tg yr-1) | "Area" (Mha)
 $ Technology: chr   "BES" | "BECCS" | "BEBCS"
 $ Value     : num   # in the unit of Metric
 $ Region    : chr
```

## 3. `results/ai_summaries/per_tonne_components.csv`

Per-tonne economics and abatement (scripts/results_components.R): each technology evaluated in every cell at
100 US$ Mg⁻¹ CO₂e with central parameters; biomass-weighted means over **all** cells (not only cells where the
technology is best).

```
152 obs. of 4 variables (long format):
 $ region    : chr
 $ technology: chr  "BE" | "BECCS" | "PyCCS" | "all"
 $ quantity  : chr
 $ value     : num
```

| quantity | unit | meaning |
|---|---|---|
| `abatement` | Mg CO₂e Mg⁻¹ | net GHG abatement (eq-abatement) |
| `net_value` | US$ Mg⁻¹ | levelized net value at 100 US$ Mg⁻¹ CO₂e (eq-net-value) |
| `total_cost` | US$ Mg⁻¹ | all costs |
| `capital_cost` | US$ Mg⁻¹ | annualized CAPEX |
| `energy_revenue` | US$ Mg⁻¹ | electricity sales |
| `agronomic_revenue` | US$ Mg⁻¹ | value of biochar or ash to crops (lime, P, K, yield for PyCCS) |
| `yield_value`, `mineral_value` | US$ Mg⁻¹ | PyCCS: crop-yield and lime/P/K parts of `agronomic_revenue` |
| `co2_transport_storage` | US$ Mg⁻¹ | BECCS CO₂ transport and storage |
| `carbon_stored` | Mg CO₂ Mg⁻¹ | BECCS captured CO₂; PyCCS permanent biochar carbon |
| `soil_n2o` | Mg CO₂e Mg⁻¹ | PyCCS avoided soil N₂O |
| `grid_credit` | Mg CO₂e Mg⁻¹ | exported electricity × displaced grid intensity at 100 US$ Mg⁻¹ CO₂e (zero where PyCCS flares its surplus) |
| `residue_counterfactual` | Mg CO₂e Mg⁻¹ | avoided emissions of the counterfactual residue fate (open burning); same for all technologies |
| `emissions` | Mg CO₂e Mg⁻¹ | haulage, field operations and CO₂ transport; residual, so that `abatement` = `carbon_stored` + `soil_n2o` + `grid_credit` + `residue_counterfactual` − `emissions` |
| `share_no_energy` | fraction | PyCCS: biomass share where the power block is omitted (surplus gas flared) |
| `feedstock_cost` (technology `all`) | US$ Mg⁻¹ | field-side feedstock price incl. storage |

## 4. `results/mc_analysis_results.csv`

Monte Carlo draws (scripts/mc_analysis.R). Each row is one draw for one region: sampled parameters (PERT,
uniform or normal; correlated pairs via a Gaussian copula) and regional summary quantities from a full
carbon-price sweep of every cell (grid 0–200 by 25, then 250, 300, 400). Current file: production run,
5,000 draws × 4 regions, all cells (`cell_fraction = 1`).

```
20000 obs. of 109 variables:
 $ mc_run_id    : int
 $ region       : chr
 $ cell_fraction: num   # 1 = all cells; 0.25 = quick/revision runs on a stratified cell sample
 $ <78 sampled parameters>: num   # names and units as in parameters.csv (tbl-si-mc);
                                  # elec_price_multiplier, ff_ci_multiplier scale the spatial layers
 $ be_PyCCS, be_BECCS : num  # biomass-weighted median break-even price (US$ Mg-1 CO2e), censored at 400
 $ takeover           : num  # biomass-weighted median price at which BECCS becomes best and viable (censored at 400)
 $ n0_BE              : num  # biomass-weighted mean net value of BE without carbon revenue (US$ Mg-1)
 $ share_<tech>_<cp>  : num  # fraction of residue where <tech> (BE, BECCS, PyCCS) is best and viable, cp = 0..250 by 50
 $ abate_<cp>         : num  # abatement of the allocated cells (Tg CO2e yr-1)
```

## 5. `results/ai_summaries/variant_summary.csv`

Structural variants (scripts/variant_sweeps.R; @fig-si-variants): the carbon-price sweep repeated with one
structural assumption changed at a time; cells assigned to their best viable technology.

```
180 obs. of 8 variables:
 $ Region         : chr
 $ Variant        : chr  "Default (125 MWth)" | "50 MWth plants" | "250 MWth plants" |
                         "Dedicated pipelines" (no shared trunklines) | "CO2-EOR allowed" (EOR sites as sinks)
 $ Price          : int  100, 150, 200   # US$ Mg-1 CO2e
 $ Technology     : chr  "BES" | "BECCS" | "BEBCS"
 $ Abatement      : num  # Tg CO2e yr-1
 $ Biomass        : num  # Tg yr-1
 $ Total_abatement: num  # sum over technologies (Tg CO2e yr-1)
 $ Total_biomass  : num  # sum over technologies (Tg yr-1)
```

## 6. `results/ai_summaries/spatial_shap_importance.csv`

Spatial drivers (scripts/spatial_shap.R; @fig-shap-importance, @tbl-si-shap-importance): per region, an XGBoost
regression of each cell-level quantity on the spatial inputs used by the relevant technologies; importance =
share of the mean absolute SHAP value.

```
146 obs. of 6 variables:
 $ target         : chr  "be_PyCCS" | "be_BECCS" (break-even, US$ Mg-1 CO2e) | "n0_BE" (US$ Mg-1) | "takeover_BECCS" (US$ Mg-1 CO2e)
 $ region         : chr
 $ feature        : chr  # spatial input (units in section 8)
 $ share_pct      : num  # % of the model's total mean |SHAP|
 $ holdout_r2     : num  # variance explained on 20% held-out cells
 $ share_undefined: num  # fraction of cells excluded because the target is undefined
```

## 7. `results/mc_shap/mc_shap_importance.csv`

Parameter drivers of uncertainty (scripts/MC_shap.R): per region and quantity, an XGBoost regression of the
Monte Carlo quantity (section 4) on the sampled parameters; SHAP importance.

```
1232 obs. of 8 variables:
 $ parameter    : chr   # sampled parameter, or a group (bc_yield_amplitude = bc_yield_b0 + bc_yield_bcec)
 $ region       : chr
 $ quantity     : chr   "be_PyCCS" | "be_BECCS" | "takeover" (US$ Mg-1 CO2e) | "n0_BE" (US$ Mg-1)
 $ mean_abs_shap: num   # in the unit of the quantity
 $ share        : num   # mean_abs_shap / sum of mean_abs_shap over individual parameters; for a group,
                        # mean_abs_shap = mean |sum of the members' SHAP values| (less than the members' sum
                        # when their effects offset)
 $ r2_test      : num   # variance explained on 20% held-out draws (same for all rows of a model)
 $ group        : logi  # TRUE for parameter groups
 $ description  : chr   # from parameters.csv
```

## 8. `results/spatial_shap/shap_targets_by_cell.csv.gz`

Cell-level targets and spatial inputs used by the spatial SHAP models (central parameters, market discount
rates). One row per grid cell with residue.

```
155778 obs. of 26 variables:
 $ region, x, y     : chr, num, num   # region; cell centre (decimal degrees, WGS84)
 $ cell_area_km2    : num   # km2
 $ be_PyCCS, be_BECCS: num  # break-even carbon price (US$ Mg-1 CO2e); negative = profitable without a carbon price; NA = never
 $ n0_BE            : num   # net value of BE without carbon revenue (US$ Mg-1)
 $ takeover_BECCS   : num   # price at which BECCS becomes best and viable (US$ Mg-1 CO2e); NA = not below 500
 $ biomass_density  : num   # available residue (Mg km-2 yr-1, dry ash-free); weight = biomass_density * cell_area_km2
 $ dist_BE_base, dist_BE_flex, dist_BECCS, dist_PyCCS: num  # mean collection distance at each technology's capacity factor (km)
 $ haul_mult        : num   # terrain and road-access multiplier of haulage cost (regional mean 1)
 $ burn_frac        : num   # share of residue otherwise burned in the field (fraction)
 $ feedstock_cost   : num   # field-side feedstock price incl. storage (US$ Mg-1)
 $ elec_price       : num   # wholesale electricity price (US$ MWh-1)
 $ ff_c_intensity   : num   # displaced grid intensity at today's carbon price (Mg CO2e GJ-1; x 3600 = g kWh-1)
 $ soil_temp        : num   # mean annual soil temperature, 0-5 cm (deg C)
 $ soil_ph          : num   # pH
 $ soil_cec         : num   # cation exchange capacity (cmolc kg-1)
 $ bc_supply        : num   # biochar supply per hectare of cropland (Mg ha-1 yr-1)
 $ crop_value       : num   # crop value per hectare of cropland (US$ ha-1 yr-1)
 $ co2_route_km     : num   # length of the chosen CO2 route (km)
 $ co2_offshore     : int   # 1 = route ends at an offshore sink
 $ co2_storage_cost : num   # storage cost at the chosen sink (US$ Mg-1 CO2)
```

## 9. `results/ai_summaries/heat_sensitivity_quantiles.csv` (Discussion)

Exploratory heat-export scenario (scripts/heat_sensitivity.R): PyCCS sells its surplus heat (heat-only block)
instead of electricity; differences from the power configuration across cells.

```
12 obs. of 6 variables:
 $ region          : chr
 $ c_price         : int  50, 100, 200   # US$ Mg-1 CO2e
 $ delta_p10, delta_p50, delta_p90: num  # change in PyCCS net value, heat minus power (US$ Mg-1): 10th/50th/90th percentile over cells
 $ abate_change_p50: num                 # median change in abatement (Mg CO2e Mg-1)
```
