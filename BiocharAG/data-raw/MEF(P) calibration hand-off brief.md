# MEF(P) calibration: hand-off brief

Sep 27, 2026 · @Dominic

## Purpose and context

Your task is to calibrate a price-dependent marginal emission factor curve, MEF(P), for four regions (USA, EU, China, India) by fitting it to NGFS scenario data. The output feeds a techno-economic assessment (TEA) of three biomass options: BECCS, biochar and unabated bioenergy.

The TEA takes the carbon price P as an exogenous input. A given P implies a position along the decarbonization pathway, so the grid electricity that a bio plant displaces must get cleaner as P rises. MEF(P) encodes that link. It stops the TEA from running implausible combinations such as a high carbon price with a coal-dominated margin. It also replaces the earlier two-scenario framing (empirical baseline vs Paris-aligned) with a single continuous framework.

The curve has a Hill (log-logistic) form:

```latex
MEF(P) = MEF_{min} + (MEF_{max} - MEF_{min}) \cdot H(P), \qquad H(P) = \frac{1}{1 + (P/P_{50})^{k}}
```

- **MEF\_max**: the high-intensity end. It is back-calculated so the curve passes through the study team's empirical MEF at today's effective carbon price.
- **MEF\_min**: the floor, about 0.02 tCO2/MWh on a life-cycle basis. It does not go below zero.
- **P50 and k**: the midpoint price and steepness. These come from NGFS.
- **Optional variants**: a two-stage version (coal to gas, then gas to clean) and a version where P50 drifts down over time. Both are selected by AIC.

The NGFS models contribute only the shape of the decline, not its level. Each model's build-margin intensity is normalised by that model's own low-price build margin, then transferred onto the empirical anchor.

## What is being handed over

You are receiving a single script, `ngfs_mef_fit.py`. It is tested on synthetic data only and has never been run against real NGFS data.

| Component | What it does |
| --- | --- |
| Loaders | Pulls data with pyam from the IIASA NGFS database, or reads an IAMC-format CSV/XLSX export. `--cache` saves the raw pull. |
| Region resolver | Maps each model's native regions to USA, EU, China and India by regex. Aggregates EU sub-regions. Supports `region_overrides`. |
| Build-margin estimator | Uses capacity additions × implied capacity factor where reported. Otherwise uses positive generation increments by technology. The fleet average is kept only as a diagnostic. |
| Normaliser | Divides by each model's low-price reference build margin (price ≤ $15/t, year ≤ 2035). |
| Fitter | Fits single-stage, time-drift and two-stage Hill curves with robust least squares, per model and pooled with equal model weights. Selects by AIC (needs >2 improvement to add complexity). |
| Bootstrap | Resamples models and scenarios (300 draws by default). |
| Anchoring | `mef_curve()` puts the curve through the empirical point. `mef_lookup()` interpolates the output table for the TEA. |
| Self-test | `--selftest` checks that known parameters are recovered from synthetic data. |

**Tests passed so far:**

- The self-test recovered P50 within 5% and k within 11% for three synthetic models.
- One synthetic model reported prices in US$2010, which checked deflation. Another reported only total coal, which checked the derived "w/o CCS" path.
- A second test covered the capacity-additions method and EU-12 + EU-15 aggregation. It also covered two-stage detection, recovering P50 ≈ 30 and 105 against true values of 30 and 120.

**Outputs, written to `--out`:**

- Build-margin points: `points_raw.csv` and `points_normalised.csv`.
- Coverage and emissions checks: `diagnostics.csv`.
- Fits and draws: `fit_params.csv` and `bootstrap_draws.csv`.
- Curves: `shape_curves.csv` (H(P) only), `mef_curves.csv` (anchored, with 5–95% bands) and `fit_<region>.png`.
- The configuration actually used: `run_config.json`.

## Inputs the study team must supply

Get these from the study team before you fit anything. Do not invent values. If an input is missing, run the fit and deliver shape curves only (`shape_curves.csv`).

| Input | Where it goes in CONFIG | Notes |
| --- | --- | --- |
| Empirical MEF per region (tCO2/MWh) | `anchors[region]["mef_emp"]` | From the team's empirical baseline (the deployment-based build margin). |
| Current effective carbon price per region | `anchors[region]["p_now"]` | Effective price, not only the explicit ETS price. Must be in the target currency year. |
| Emission-factor basis (direct or life-cycle) | `ef_basis`, `mef_min` | Must match the basis of `mef_emp`. The default is life-cycle with a 0.02 floor. |
| Target currency year and deflator | `deflators` | The defaults are US CPI-U ratios to US$2024. Confirm the TEA's currency year. |
| Technology emission factors | `techs` | The defaults are typical values. Replace them if the team has regional factors. |
| Scenario exclusions, if any | `scenario_exclude` | For example, whether to keep the Low Demand scenario. |

The effective price should include the implicit value of non-price policies such as subsidies and mandates. If the team has no estimate, run P\_now as a sensitivity: explicit price only, and explicit price plus an assumed implicit component. Record which case the final curves use.

## Calibration tasks, step by step

Work through these steps in order. Save every intermediate file. After each step, record in the run log what you did and any deviations.

1. **Set up the environment.**
   - Install the dependencies: `pip install pyam-iamc numpy pandas scipy matplotlib`.
   - Run `python ngfs_mef_fit.py --selftest --out selftest`. It must print `SELF-TEST PASSED` before you continue.
2. **Identify the current NGFS database.**
   - Run `import pyam; pyam.iiasa.Connection().valid_connections` and pick the latest NGFS phase. The script defaults to `ngfs_phase_5`.
   - Record the database name, the date of the pull, and the model versions.
   - If pyam access fails, export the variables from the NGFS Scenario Explorer as IAMC CSV and use `--source file`.
3. **Pull and cache the raw data.**
   - Run `python ngfs_mef_fit.py --source pyam --db <name> --cache ngfs_raw.csv --list-regions`.
   - Keep `ngfs_raw.csv` under version control, or archive it with a checksum.
4. **Fix the region mapping.**
   - From the `--list-regions` output, confirm that each model maps to the right native region for USA, EU, China and India.
   - Add `region_overrides` wherever the regex fails. MESSAGE's R12 regions are known to have no "EU"; Western + Eastern Europe is the likely substitute, but that choice must be escalated.
   - Check that the right sub-regions are merged for the EU, for example GCAM EU-12 + EU-15.
5. **Check variable coverage.**
   - For each model and region, confirm that the price, total generation and generation-by-technology variables are present.
   - Note which models report `Capacity Additions|Electricity|*`, since this decides between the capacity and increment build-margin methods.
6. **Fill in CONFIG.** Enter the inputs from the previous section: anchors, EF basis, deflators and exclusions. Leave the fitting settings at their defaults for the first run.
7. **Run a first fit.**
   - Run `python ngfs_mef_fit.py --cache ngfs_raw.csv --out run1`.
   - Review `diagnostics.csv` and the plots against the quality checks in the next section before doing anything else.
8. **Run the sensitivity runs.** Change one setting at a time and save each run to its own folder:
   - `bm_method` set to increment vs capacity, where both are available;
   - `price_timing` set to mid vs end;
   - `ref_price_max` of 10, 15 and 25;
   - `weight_power` of 0, 0.5 and 1;
   - `bootstrap_models` on vs off;
   - the P\_now cases from the inputs section.
9. **Bottom-up check of the coal-to-gas step.** For USA and EU, compute the fuel-switching price from the TEA's own fuel-price scenarios: P\_switch = (SRMC\_gas − SRMC\_coal) / (EF\_coal − EF\_gas). Compare it with the fitted P50 (single-stage) or P50\_1 (two-stage). Report any difference greater than a factor of 2.
10. **Freeze the curves.**
    - Choose the final specification per region, justifying any departure from the AIC choice.
    - Rerun with `bootstrap` set to 1000.
    - Deliver the outputs listed under Deliverables.

## Quality checks and acceptance criteria

A regional curve is accepted only if it passes every check below. When one fails, fix the cause or escalate. Do not loosen the threshold. These thresholds are the agreed defaults; if development shows tighter tolerances are needed, propose them to the study lead.

| Check | Where to look | Pass condition |
| --- | --- | --- |
| Technology coverage | `diagnostics.csv` → `median_coverage` | 0.95–1.05 (the technologies captured account for total generation) |
| Emissions reconstruction | `diagnostics.csv` → `median_emis_ratio` | 0.85–1.15 (direct EFs × generation ≈ the model's reported power CO2) |
| Enough points | `diagnostics.csv` → `n_points` | ≥ 20 per model-region, spread over at least 3 scenarios |
| Price range | `points_normalised.csv` | Data span from < $15/t to > $150/t |
| Reference level | `points_normalised.csv` → `mef_ref` | Plausible for the region; a warning appears if it is within 0.05 of the floor |
| Fit quality | `fit_params.csv` → `rmse` | Pooled RMSE < 0.20 in normalised units |
| Parameter bounds | `fit_params.csv` | No parameter on a bound (for example k = 0.5 or 8, P50 = 2 or 3000) |
| Model agreement | `fit_params.csv`, per-model rows | Per-model P50 values within a factor of 3 of each other, or the disagreement is explained |
| Anchor consistency | `mef_curves.csv` → `mef_max_central` | ≤ 1.15 tCO2/MWh; the script warns above this |
| Monotonic curve | `mef_curves.csv` | MEF falls as price rises, with no increases |
| Band width | `mef_curves.csv` | 5–95% band reported at $50, $100 and $200/t |
| Visual check | `fit_<region>.png` | Fitted curve follows the scatter, with no systematic drift at high or low prices |

## Known pitfalls and judgment calls to escalate

**Escalate these to the study lead rather than deciding them yourself.**

- **Anchors far along the curve.** If `mef_max_central` exceeds 1.15, today's effective price is already well into the decline. The EU is the likely case. The fix is `clamp_below_now = True`, which holds MEF at the empirical value for prices below P\_now, but this changes the curve's meaning for low-price scenarios. Get sign-off.
- **Non-price policy.** In the US and China, clean capacity is being built at low explicit carbon prices. A curve anchored on the explicit price will look too steep. Escalate the choice of P\_now.
- **Region substitutes.** MESSAGE R12 has no EU region, and some models may define China including Taiwan. Record every substitute used.
- **Two-stage curves in India or China.** Coal-to-gas switching is limited there by gas availability and LNG costs, so a two-stage curve is physically doubtful even if AIC selects it. Default to single-stage unless the team agrees otherwise.
- **Choosing time drift.** If the drift variant wins, the curve then depends on the year as well as the price. The TEA must pass a year to `mef_lookup()`. Confirm the TEA can do this before adopting it.

**Pitfalls to handle yourself:**

- **Currency.** NGFS prices are usually in US$2010. A missing or wrong deflator shifts P50 by about 40%. Check the unit strings in the raw pull.
- **Model variants.** Exclude NGFS variants that include physical-damage feedbacks (`model_exclude`). Otherwise they duplicate scenarios.
- **Increment method.** With positive increments, like-for-like replacement is invisible, which biases the build margin towards growing technologies. Prefer the capacity method wherever it is available, and compare the two.
- **Early steps with no new generation.** A step where new generation falls below 0.5% of the total is dropped. If many steps are dropped, lower `min_new_frac` and document it.
- **Biomass in the build margin.** Biomass EFs are set to 0 by design (the floor does not go below zero, and bio emissions are handled in the TEA). Do not change this without discussion.
- **Operating margin.** The script fits the build margin only. The combined-margin weighting is a TEA decision and out of scope here.

The script has never run on real NGFS data, so expect small bugs. Examples include unit strings the script does not recognise, variable names that differ from the IAMC template, and pandas version issues. Fix them in a branch, add a regression case to the self-test, and list each fix in the run log.

## Deliverables expected back

Deliver a single archive containing the final run folder and a short calibration note of 2–3 pages.

- [ ] The final `mef_curves.csv` for every region that has an anchor, and `shape_curves.csv` for all regions
- [ ] `fit_params.csv` and `bootstrap_draws.csv` (1000 draws) for the TEA Monte Carlo
- [ ] `fit_<region>.png` for each region
- [ ] `run_config.json` for the final run, and the cached `ngfs_raw.csv` with its checksum
- [ ] A summary of the sensitivity runs: P50, k and MEF at $50, $100 and $200/t for each setting, by region
- [ ] The bottom-up check of the coal-to-gas switching price for USA and EU
- [ ] A run log listing the database name, pull date, model versions, region mappings and overrides, code fixes, and the outcome of every quality check
- [ ] A list of open judgment calls, each with a recommended option

## Key references

- [NGFS–IIASA Scenario Explorer](https://iiasa.ac.at/models-and-data/ngfs-iiasa-scenario-explorer): the data source, with the GCAM, MESSAGEix-GLOBIOM and REMIND-MAgPIE models.
- [NGFS scenarios: narratives and key findings](https://www.ngfs.net/system/files/2025-11/NGFS%20scenarios%20narratives%20and%20key%20findings_0.pdf): how the carbon-price paths differ across models.
- [IPCC AR6 WGIII Chapter 6](https://www.ipcc.ch/report/ar6/wg3/chapter/chapter-6/): electricity reaches net zero CO2 in 2045–2055 (1.5°C pathways) and 2050–2080 (2°C). Low-carbon sources supply 93–97% of electricity by 2050 in 2°C pathways.
- [Pehl et al. 2017, Nature Energy](https://link.springer.com/article/10.1038/s41560-017-0032-9): life-cycle emissions in 2050 of 3.5–12 g/kWh for nuclear, wind and solar, and 78–110 g/kWh for fossil CCS. The basis for the MEF\_min floor.
- [CDM TOOL07](https://cdm.unfccc.int/methodologies/PAmethodologies/tools/am-tool-07-v7.0.pdf) and the [GHG Protocol consequential-accounting draft](https://ghgprotocol.org/sites/default/files/2025-05/S2-consequential-Meeting5-Presentation-20250501.pdf): definitions of operating, build and combined margins, and the default weights.
