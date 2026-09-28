# MEF(P) calibration note and run log

27 September 2026. Calibration of the price-dependent marginal emission factor curve MEF(P) for the
Biochar_AG TEA, following `MEF(P) calibration hand-off brief.md`. Script: `ngfs_mef_fit.py`;
sensitivity driver: `ngfs_mef_sensitivity.py`. TEA implementation: `R/grid_intensity.R`
(`displaced_grid_ci()`).

## Data

- **Source:** NGFS Phase 5, Version 5.1, Zenodo doi:10.5281/zenodo.17901363, `IAM_data.xlsx` (downloaded by the study lead; pyam was not used).
- **Checksums:** SHA-256 in `GIS/raw/ngfs/checksums.sha256`:
  - zip: `acae7160…d5ad7c31b`
  - `IAM_data.xlsx`: `33e4fbb8…f76e`
  - parquet cache: `468f90ae…9860ea8`
- **Models:** GCAM 6.0 NGFS, MESSAGEix-GLOBIOM 2.0-M-R12-NGFS, REMIND-MAgPIE 3.3-4.8. The REMIND IntegratedPhysicalDamages variant is excluded by `model_exclude`.
- **Scenarios (7, all kept, to reconfirm):** Below 2°C, Current Policies, Delayed transition, Fragmented World, Low demand, NDCs, Net Zero 2050.
- **Units:** carbon prices are US$2010/t CO2, deflated to US$2024 by 1.44 (US CPI-U). Generation is in EJ/yr.

## Region mapping

| Region | GCAM | MESSAGE (R12) | REMIND |
|---|---|---|---|
| USA | USA | North America (substitute) | United States of America |
| EU | EU-12 + EU-15 | Western + Eastern Europe (substitute) | EU 28 |
| China | China | China | China |
| India | India | South Asia (substitute) | India |

The MESSAGE substitutes were signed off by the study lead on 27 Sep 2026.

## Configuration decisions (study lead, 27 Sep 2026)

- **Emission factors:**
  - IPCC life-cycle values, matching the TEA's empirical anchor: coal 0.82, gas 0.49, oil 0.70, nuclear 0.012, hydro 0.024, wind 0.0115, solar 0.048, geothermal 0.038 t/MWh.
  - Biomass is 0 in both the curve and the anchor.
  - CCS rows keep the script defaults (no IPCC value).
- **Floor:** 0.02 t/MWh (life-cycle). **Currency:** US$2024. **Margin:** the TEA anchor is the empirical build margin.
- **Shape quantity:** fleet-average intensity (`bm_method = "average"`), not the model build margin (see Escalations).
- **Specs:** single- and two-stage only (no time drift; the TEA has no year dimension). Single-stage is forced for China and India.
- **Anchoring:** per grid cell, with MEF_cell(P) = floor + (MCI_cell − floor) · H(P)/H(P_now). The script therefore writes shape curves only; the anchoring is done in R.
- **P_now (explicit prices):** EU 80, China 12, USA 0, India 0 $/t at the first calibration; updated to EU 70 and China 14 (2024 averages; see "P_now decision").

## Code changes to the script

- `load`: the IAMC export is filtered to the required variables. The full file carries hundreds of other variables with unrecognised units.
- CLI: added `--bm-method`, `--model-exclude` and `--bootstrap`.
- `spec_force` adds a per-region override of the AIC spec choice.
- Self-test pinned to `bm_method = "increment"` and no spec forcing, which is what the synthetic data were built for. `SELF-TEST PASSED` under pandas 3.0.6 / numpy 2.5.3 / scipy 1.18.1.
- The plot labels now name the intensity method.

## Escalations and outcomes

1. **Model build margins are near the floor at zero carbon price (run1, capacity method).**
   - GCAM and REMIND build mostly renewables even in Current Policies: build margin 0.06–0.13 t/MWh at < $5/t.
   - Normalisation dropped most US and EU model-regions (reference within 0.05 of the floor).
   - The remaining fits were fragile (China P50 20, k 0.87; USA k on its bound).
   - Outcome: switch to the fleet-average shape (study lead, option C).
2. **REMIND fails the fit-quality and model-agreement checks (run2).**
   - Pooled RMSE: China 0.22, India 0.20, USA 0.33 (threshold 0.20).
   - REMIND's P50 is 12 (China) and 2 (India, on bound) versus 30–80 for GCAM and MESSAGE.
   - Cause: every REMIND scenario starts at a positive (implicit) price, and its grids decarbonise over time even at low prices, so price and time are confounded.
   - Recommendation: exclude REMIND (the TEA default, `mef_include_remind = FALSE`). **Signed off by the study lead, 27 Sep 2026.** The all-model fit is kept as `mef_shape_draws_all_models.csv`.
3. **Anchors far along the curve (EU).** Not triggered: the implied MEF_max is 0.05 t/MWh at the median EU anchor and at most ≈ 0.37 for the dirtiest cells (≤ 1.15). No clamp is used. Cells below today's EU price rise above their anchor, capped in the TEA at coal (0.82).

## Quality checks (final run, GCAM + MESSAGE, 1,000 bootstrap draws)

| Check | Result |
|---|---|
| Technology coverage | 1.00 for all model-regions ✓ |
| Emissions reconstruction | 0.86–1.10 ✓ (GCAM India 0.86 is near the lower limit) |
| Points | 56 per model-region, 7 scenarios ✓ |
| Price range | $0 to > $880/t ✓ |
| Reference level | fleet average 0.16–0.52 t/MWh; none near the floor ✓ |
| Pooled RMSE | USA 0.147, China 0.140, India 0.126, EU 0.137 ✓ |
| Parameter bounds | pooled fits within bounds ✓ (EU k2 = 7.6, below the bound of 8) |
| Model agreement (P50, GCAM vs MESSAGE) | USA 61/106, China 81/42, India 52/31, EU 52/112 (factor < 3) ✓ |
| Monotonic | ✓ by construction |
| Visual | curves follow the scatter; no systematic drift ✓ |

## Final curves (US$2024)

| Region | Spec | Parameters | MEF at $50 / $100 / $200/t for a representative anchor* |
|---|---|---|---|
| USA | one | P50 94.7, k 1.80 | 0.264 / 0.173 / 0.086 (anchor 0.341) |
| EU | two | P50_1 63.6, k1 1.18; P50_2 137.5, k2 7.61; s 0.28 | 0.041 / 0.036 / 0.025 (anchor 0.038) |
| China | one | P50 53.4, k 1.19 | 0.238 / 0.155 / 0.092 (anchor 0.378) |
| India | one | P50 40.5, k 1.25 | 0.261 / 0.155 / 0.086 (anchor 0.574) |

\*The biomass-weighted median cell anchor, in life-cycle t/MWh. Outputs are in `GIS/raw/ngfs/final_noremind/` and `final_all/`; the draws for the TEA are in `inst/extdata/mef_shape_draws*.csv` (draw 0 = central).

## Sensitivity runs

These use 60 draws each; full table in `GIS/raw/ngfs/sens/sensitivity_summary.csv`. Changing `ref_price_max` (10/15/25), `weight_power` (0/0.5/1) or `bootstrap_models` changes P50 by < 10% and MEF at $50–200 by < 0.02 t/MWh. Other settings matter more:

- **`price_timing = "end"`:** raises P50 by 18–22% (USA 115, China 63, India 46). The EU becomes single-stage (P50 109). MEF at $100 rises by 0.02–0.03 t/MWh.
- **The build-margin (capacity) method:** fails the checks (see Escalation 1).
- **P_now:** the explicit/implicit cases act in the TEA through `grid_p_now`, not in the fit. REMIND's current-policy implicit prices (US ≈ $32, EU ≈ $40, China ≈ $8, India ≈ $2 in US$2024; corrected 28 Sep, previously double-deflated; GCAM and MESSAGE report 0) indicate the scale of the implicit component.

## Bottom-up coal-to-gas check

- **Method:** NGFS Current Policies fuel prices, 2025–2030, US$2024. SRMC = fuel price / efficiency (coal 0.38, gas 0.55). EF uses direct values (coal 0.95, gas 0.37 t/MWh).
- **EU:** P_switch is GCAM $64–65, MESSAGE $6–11 and REMIND $26–27, against a fitted first stage P50_1 of $64. That matches GCAM; MESSAGE and REMIND are more than a factor of 2 lower.
- **USA:** P_switch is GCAM $40–43, MESSAGE −$5 to −7 (gas already cheaper than coal) and REMIND $26–27. The US fit is single-stage (P50 $95), which describes the full transition rather than coal-to-gas, consistent with no distinct switching stage.
- The TEA has no fuel-price scenarios of its own, so NGFS prices were used instead.

## Open judgment calls (with recommendation)

1. **REMIND:** excluded (signed off 27 Sep 2026).
2. **P_now:** explicit only (current) or explicit plus implicit. Recommended: run both as a TEA sensitivity. REMIND's implicit prices are a candidate for the implicit case.
3. **Price timing:** mid-period (current; the brief's default) or end-of-period (+20% P50). Recommended: keep mid.
4. **Low Demand scenario:** kept (to reconfirm).
5. **Anchor versus shape quantity:** the anchor is the empirical build margin and the shape is the fleet average, a deliberate mismatch documented in the Methods.

## Revision 2 (28 Sep 2026): downscaled MESSAGE regions

- **What changed:** MESSAGE's USA, India and EU fits now use the NGFS downscaled country data (`Downscaled_MESSAGEix-GLOBIOM 2.0-M-R12-NGFS_data.xlsx`: USA, IND, and the sum of the EU-27 member states) in place of the R12 substitutes. China keeps the native R12 region. Inputs are built by `mef_prepare_inputs.py`; the fit runs `ngfs_mef_fit.py --file ngfs_central_input.parquet`.
- **Data limits of the downscaled file:**
  - Generation is reported to 2050 only (7 five-year steps).
  - There is no CCS split: coal and gas are divided using the parent R12 region's CCS shares by scenario and year (effect on P50: USA −5%, India −1%; without the split the EU fit becomes single-stage at P50 92).
  - There is no power-sector CO2, so the emissions reconstruction check does not apply to these rows. Coverage is 1.00.
  - The EU-27 carbon price is the generation-weighted mean of member-state prices.
- **Config:** `region_overrides` removed; `model_exclude` defaults to `(?i)damage|REMIND` (REMIND exclusion signed off 27 Sep 2026).
- **Final central curves** (GCAM + MESSAGE, 1,000 bootstrap draws; `GIS/raw/ngfs/final_v2/`; with REMIND: `final_v2_all/`):

  | Region | Spec | Parameters | RMSE | Per-model P50 (GCAM / MESSAGE) |
  |---|---|---|---|---|
  | USA | one | P50 103.2, k 1.78 | 0.137 | 61 / 116 |
  | EU | two | P50_1 45.4, k1 0.95; P50_2 137.8, k2 6.85; s 0.30 | 0.136 | 52 / 100 (single-stage) |
  | China | one | P50 53.4, k 1.19 | 0.140 | 81 / 42 |
  | India | one | P50 43.7, k 1.25 | 0.129 | 52 / 36 |

- **MC spread:** the NGFS bootstrap from this fit (`mef_shape_draws.csv`, draws 1–1000). A pooled NGFS + IPCC AR6 R10 spread, re-centred on the NGFS central curves, was tried and dropped. Its bootstrap of the pooled fit reflects uncertainty in the ensemble mean, which narrowed the bands. The alternative, inter-model spread, would have required a post-hoc harmonisation. See `bak/mef_spread_ar6.py`.

## P_now decision (28 Sep 2026)

P_now is today's **explicit** carbon price on power emissions: EU 70 (ETS), China 14 (national ETS), USA 0, India 0 (US$2024). These are 2024 annual averages: EU ETS €65/t at 1.08 USD/EUR; China national ETS ¥98/t at 7.20 CNY/USD. They replace the earlier estimates of 80 and 12. The TEA's carbon price is interpreted as an explicit price.

Alternatives were assessed and rejected:

1. **Model-consistent reading.** GCAM and MESSAGE report a Current Policies price of 0 in 2025 in every region because they model current policies as constraints. REMIND reports US 32, EU 40, China 8 and India 2, but it is excluded from the fit.
2. **Inversion from AR6 no-policy baselines.** The 2025–2030 baseline build margins (P1a/P0_1a; 12 models, 27 scenarios) have medians of USA 0.16, EU 0.25, China 0.47 and India 0.55 t/MWh. These give China P_now ≈ $17/t, no solution for the USA and India (their empirical build margins exceed the baseline), and an implausibly high value for the EU (empirical 0.04 against baseline 0.25).
3. **Bottom-up effective prices.** Not pursued; this measures a different axis.

Anchoring at explicit prices embeds today's non-price policies in the empirical anchor rather than as a shift along the curve.
