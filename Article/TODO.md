# TODO

Open modelling issues to resolve before the final re-run of results.

## Status (26 September 2026)

Done since the last commit:
- Haulage terrain and road-access factors (Weiss et al. 2020 friction surface), with the trucking cost split into time, fuel and other shares after ATRI (2024). Layers are built for all regions.
- `bm_transport_var` = 0.19 $/Mg/km (Searcy et al. 2007, in 2026 USD). Manuscript Methods and bibliography updated.
- Biochar return haulage charged as a backhaul (handling only). Ash return haulage omitted (below 1% of costs).
- Plant-side site factor (handoff item 6) dropped by decision: the cropland mask already excludes implausible sites, and terrain enters through haulage instead.

Open, with owner:
1. **O&M levels (done, 27 September 2026).** Europe's best BES cell had become unprofitable at low carbon prices after the O&M bug fix (commit 4f5ef6c): the old code applied `bes_om_factor` to *annualised* CAPEX, which understated O&M about 16-fold. Changes made:
   - **One all-in O&M rate for every technology:** `plant_om_factor` = 0.04 of total CAPEX per year (fixed + variable), sampled once in the MC (PERT 0.75–1.35 relative). It replaces `bes_om_factor`, `beccs_om_factor`, `py_om_factor`, `bebcs_power_om_factor` (was 0.05) and `bebcs_heat_om_factor` (was 0.03). A technology-specific value is used only if supplied explicitly. Sources: EIA AEO2023 (50 MW biomass: $141.5/kW-yr + $5.44/MWh ≈ 3.6%/yr at 85% capacity factor); IRENA (2012) (stoker/BFB/CFB boilers: 3.2–4.2% + $3.8–4.7/MWh); Danish Energy Agency catalogue v0009 (80 MW feed CHP: straw 4.4%, wood pellets 4.6%/yr all-in).
   - **High-ash O&M multiplier removed** (was ×1.2 medium, ×1.5 high ash). The ash CAPEX and efficiency penalties remain. Manuscript corrected: it previously described a single 25% CAPEX / 50% O&M penalty, but the code has two ash bands.
   - **`om_location_factor`:** US 1.0, Europe 1.0, China 0.67, India 0.56 (was 1.125, 1.125, 0.75, 0.625). Correlation rows for the removed O&M parameters deleted.
   - **Result with default parameters** (O&M and soil GHG penalty changes combined). Biomass in profitable cells: $50/t: US 0%, Europe 0.1%, China 48.9%, India 49.3%; $150/t: US 82.4%, Europe 79.9%, China 57.2%, India 70.1%. Europe's best BES cell is −$13.5/Mg at $0/t and +$6.3/Mg at $50/t. BES is still rarely profitable at low carbon prices; CHP heat revenue is not modelled.
   - `Article/parameters_table.csv` still listed old O&M values but is not used by the manuscript (the parameter table is built from `inst/extdata/parameters.csv`). `parameters_table.csv` deleted as no longer needed.
2. **Residue soil GHG penalty (done, 27 September 2026).** The SOC-only penalty (0.19 Mg CO2e/Mg feed) is replaced by a net soil GHG penalty, `residue_soil_ghg_penalty`, which defaults to 0 and is not sampled in the Monte Carlo. The basis is McClelland et al. (2025): the SOC gain and N2O increase from residue retention approximately cancel through 2100 almost everywhere except the Brazilian Cerrado. The former `residue_c_retention` formulation is recorded in the parameter note for sensitivity runs. The per-cell SOC/NPP equation is no longer needed. Avoided burning emissions are still credited. Diversion of residues from livestock feed (especially India) is handled qualitatively in a new manuscript subsection (Competing Uses of Crop Residues), which carries a TODO to confirm whether the Karan et al. (2023) supply already excludes fodder use.
3. **Grid displacement intensity: price-dependent MEF(P) (implemented, 27 September 2026).** `displaced_grid_ci()` (R/grid_intensity.R) scales each cell's empirical build margin with a regional Hill curve fitted to NGFS Phase 5 V5.1 (`data-raw/ngfs_mef_fit.py`). MEF_cell(P) = floor + (MCI_cell − floor) · H(P)/H(P_now); floor 0.02 t/MWh; capped at coal. It is the default for electricity and BEBCS heat. `use_flat_ci` is kept as a control option (not in sensitivity or MC). The MC samples `mef_draw_u` over 1,000 precomputed bootstrap draws. Full calibration record: `data-raw/MEF(P) calibration note.md`.
   - **Fit:**
     - Fleet-average shape (option C), GCAM + MESSAGE.
     - Single-stage for USA (P50 95, k 1.8), China (53, 1.2) and India (41, 1.2); two-stage for the EU (64 and 138).
     - All acceptance checks pass. MESSAGE region substitutes signed off.
   - **Effect (default parameters, static → MEF):**
     - US biomass in profitable cells at $150/t falls from 82% to 50%.
     - India at $50/t: 49% → 34%, with BES → BEBCS.
     - Europe barely changes (anchor already ≈ 0.04 t/MWh).
     - BEBCS gains share at $100–200/t in every region.
   - **Anchors:** recomputed with biomass = 0 (99 of 278 values fell slightly). The Methods text was corrected (the window is 2019–2024, not 2018–2023; values updated).
   - **Open, to check:**
     - (a) **P_now values** (explicit prices EU 80, China 12, US 0, India 0 $/t); consider an implicit-price sensitivity (REMIND implicit US ≈ 46, EU ≈ 58).
     - (b) REMIND exclusion: **signed off 27 Sep 2026** (fails RMSE and model agreement; all-model draws kept, `mef_include_remind`).
     - (c) **Low Demand scenario kept:** to reconfirm.
     - (d) **Currency audit to 2024 USD** of all cost parameters (e.g. `bm_transport_var` 0.19 is 2026 USD, EIA costs 2022 USD).
     - (e) **The source and records for the empirical build-margin method** (user).
     - (f) **NGFS citation year** (`ngfs2026`) to verify.
3b. **MEF(P) revision 2 (done, 28 September 2026): what changed, and how to revert.** The baseline is commit `aaddc48` (NGFS fit with native MESSAGE R12 substitutes).
   - **Central curves and MC spread: NGFS only** (GCAM + MESSAGE; REMIND excluded, now the CONFIG default).
   - **MESSAGE uses the NGFS downscaled country data** for the USA, India and the EU-27 (`data-raw/mef_prepare_inputs.py`) instead of the R12 substitutes. China keeps the native region. The downscaled file has no CCS split, so the parent R12 region's CCS shares are applied (effect: USA P50 −5%, India −1%). It covers 2020–2050, giving 7 steps per scenario; there is no power-sector CO2, so the emissions check does not apply.
   - **Refitted central curves:**
     - USA P50 95 → 103 (k 1.78)
     - India 41 → 44 (k 1.25)
     - EU two-stage 64/138 → 45/138 (k1 0.96, k2 6.85, s 0.30)
     - China unchanged (53, k 1.19)
     - All checks pass (RMSE 0.13–0.14; GCAM/MESSAGE within a factor of 2).
   - **MC draws:** the 1,000-draw NGFS bootstrap from the same fit (as before, no re-centring).
   - **AR6 not used:** a pooled NGFS + AR6 spread (re-centred on NGFS) was tried and dropped. Its bootstrap narrowed rather than widened the bands, and using it would have required a post-hoc harmonisation. The script is in `bak/mef_spread_ar6.py`; the AR6 cache is in `GIS/raw/ar6/`.
   - **Files changed:**
     - `inst/extdata/mef_shape_draws*.csv`
     - `data-raw/ngfs_mef_fit.py` (region overrides removed, REMIND excluded by default)
     - new `data-raw/mef_prepare_inputs.py`
     - calibration note
     - Methods
   - **To revert:** `git checkout aaddc48 -- BiocharAG/inst/extdata/mef_shape_draws.csv BiocharAG/inst/extdata/mef_shape_draws_all_models.csv`. The R code is unchanged.

4. **Regional haulage level.** Implied collection speeds (US 56, Europe 45, India 30, China 26 km/h at 125 MWth) versus `haulage_location_factor` from long-haul freight rates (see section 2).
5. **Remaining handoff items:** 7 (calibrate modelled corridors against existing pipelines) and 8 (update the manuscript Methods for v2 routing, the hub-and-spoke model, lift cost and sink/port/landfall choice).
6. **Full re-run** of MC, SHAP and figures: MEF(P) is now implemented; pending the checks in item 3 and the currency audit.

## 1. CO2 transport routing

### Current algorithm

Distance layers were built by the v1 script (now `bak/process_transport_layers_v1.R`) for each region:

1. Slope is derived from the global 30 arc-second DEM (`geodata::elevation_global`), aggregated to a grid 10× finer than the model grid using the 95th-percentile slope in each block.
2. Friction is set to $M(\theta) = \exp(0.25\,\theta)$ (θ in degrees). Water cells (NA in the DEM, which includes inland lakes) get friction 5, and WDPA protected areas are absolute barriers.
3. `terra::costDist` from each sink gives a friction-weighted distance ("effective km") to every cell. Each cell is assigned the sink with the minimum effective distance (`*_dist_sink`), and separately the nearest saline sink (`*_dist_sink_saline`), with matching onshore/offshore flags.

In the model (`calculate_ccs_transport()`):

- Onshore sinks are costed by hub-and-spoke pipeline over the effective distance.
- Offshore sinks are costed as liquefaction + terminal + 0.035 $/t/km × effective distance.
- Already fixed: the 1.25 tortuosity multiplier was removed (it double-counted routing), and shipping is no longer a cost ceiling for onshore sinks.

### Assessment

1. **Effective km are costed as physical km.** The friction multiplier is ×12 at 10°, ×148 at 20° and ×1,800 at 30° of 95th-percentile slope. The weighted distances are then passed to the pipeline cost function as kilometres, which inflates pipeline costs in hilly terrain:

   | Region | Median (effective km) | 95th percentile | Max |
   |---|---|---|---|
   | US | 1,048 | 3,690 | 11,955 |
   | Europe | 1,964 | 5,326 | 51,029 |
   | India | 1,494 | 17,766 | 121,206 |
   | China | 4,374 | 11,478 | 118,572 |

   These are distances to the nearest saline sink over biomass cells. A China median of 4,374 km is not physically plausible, and it drives BECCS T&S costs of roughly $100–200/t CO2 in China and India. The function was chosen empirically so that routes don't cross the Rockies or Himalayas when a lowland sink on the same side is available. That behaviour is worth keeping for path choice, but the same number should not also set the cost.

2. **Water friction of 5.** Distances to offshore sinks are inflated 5× over water. This penalises offshore sinks in the sink assignment and inflates their voyage cost, which is priced on the same weighted distance.

3. **Sinks are assigned by distance, not cost.** Pipeline and ship routes, and the different injection costs, are never compared, so a cell may be assigned a sink that isn't its cheapest option.

4. **No inland leg for shipping.** An inland source going to an offshore sink pays no pipeline cost to reach a port. A nearest-coast port was prototyped and rejected:
   - In India, west-coast sources then sail about 3,000 km (median 2,961 km) around the peninsula to Krishna-Godavari instead of piping east.
   - In China, about 28,000 cells were routed to inland lakes (DEM-NA water not connected to the ocean) and got no sea route.

5. **Protected areas were never applied.** In every v1 run, loading WDPA failed inside the tryCatch, so routes ignored protected areas entirely: `wdpar::wdpa_clean()` needs a projected CRS but was given lon/lat (China, India, US), and Europe hit a GEOS topology error. No `*_debug_pa.tif` files exist. The manuscript's statement that WDPA areas are "treated as absolute barriers" doesn't describe the current results. The v2 script (`BiocharAG/data-raw/process_transport_layers.R`; v1 moved to `bak/process_transport_layers_v1.R`) now uses status and UNESCO-MAB filters plus `st_make_valid` by default, with `wdpa_clean()` optional in a projected CRS.

6. **v2 routing status (September 2026).** `BiocharAG/data-raw/process_transport_layers.R` (v2) implements A, C and E. It has been tested on a Rockies box (onshore only), a coastal India box (Krishna-Godavari) and a North Sea box. `calculate_ccs_transport()` and `calculate_beccs()` now read `<prefix>_transport_layers.tif` when it exists (D: sink class chosen by transport + storage cost at run time). The v1 layers are used otherwise. Still open:
   - **Done: port choice.** Each coastal cell seeds a multi-source Dijkstra (C++, `dijkstra_offsets()`) with its sea leg priced at `sea_route_weight_ship` = 0.7 flat-pipeline km per sea km. Port and land route are therefore chosen jointly on land + sea cost; terminal and liquefaction are fixed per tonne and don't affect the choice.
   - **Done: near-shore pipeline option.** Offshore sinks can also be reached by an onshore pipeline to a landfall plus a subsea leg (`offpipe_*` layers; landfall chosen with `sea_route_weight_pipe` = 1.5; costed in the TEA with `co2_subsea_capex_factor` = 1.5). In a North Sea test, 25% of cells chose the subsea pipeline, 1.3% ship and 73% onshore saline.
   - **Sea-leg weights are fixed in the GIS step.** Port and landfall choice uses fixed weights, whereas the TEA's actual costs depend on the plant's CO2 flow and on the sampled `co2_subsea_capex_factor`. `sea_route_weight_pipe` should be kept equal to the default of that parameter.
   - **Sink point locations:** Krishna-Godavari is a point essentially on the coast, so its subsea leg is about 0 km. Basin polygons, or points at the actual storage sites, would give more realistic offshore distances.
   - **Done: `r_max` ensemble test (handoff item 5).** Layers were regenerated with `route_risk_max` = 0, 0.5 and 1 and compared with production (2), using default parameters with BECCS transport and storage costed per candidate route. The ensemble keeps the cheapest candidate per cell (λ = 0).

     | Region | BECCS-preferred cells (default, $150/t) | Of those, optimal tech changes at r_max = 0 | All cells: optimal tech changes, ensemble vs default | Ensemble T&S saving, p95 / max ($/t CO2) |
     |---|---|---|---|---|
     | US | 6,635 | 0.015% | 0.09% | 1.9 / 6.5 |
     | Europe | 16,830 | 0.018% | 0.17% | 1.5 / 20 |
     | China | 15,608 | 0 | 0.35% | 2.6 / 24 |
     | India | 17,411 | 0 | 0.05% | 0.4 / 8 |

     - At $50/t, BECCS is almost never preferred (0–1 cells per region), so no flips occur.
     - The median T&S change between r_max = 0 and the default is under $0.25 per Mg feed in every region.
     - The ensemble picks r_max = 0 in 26–38% of cells and the default (or an identical route) in 42–59%. The r_max = 0 route isn't always cheapest because the TEA cost (hub-and-spoke breakpoints, plant CO2 flow, lift, sink class) differs from the GIS cost surface.
     - **Decision:** keep the single default route in production. Report this as a robustness result in the manuscript: the routing premium does not drive the conclusions. This is a single deterministic run, not across MC draws.
   - The plant-side site factor, calibration against existing pipelines and the manuscript Methods (handoff items 6–8).

7. **Other issues.**
   - The US sink database has no offshore sinks (e.g. Gulf of Mexico offshore saline storage).
   - The whole global DEM is slope-aggregated before cropping, so each region takes 15–19 min single-threaded.
   - `hires_factor = 1` fails in the aggregation step.

### Options

- **A. Separate path choice from cost.** Keep $\exp(0.25\,\theta)$ as the routing friction, and cost the physical length of the chosen path times a bounded terrain cost multiplier.
  - The physical length can be recovered without path tracing. Run `costDist` a second time with friction $M + \varepsilon$. Because the optimal path is unchanged for small ε, $(D_\varepsilon - D_0)/\varepsilon$ equals its physical length.
  - The terrain multiplier should come from the pipeline-costing literature (typically about 1.0–1.5 on hilly ground, 2–3 in mountains), either as the length-weighted mean along the path or by slope class.
- **B. Replace the friction function outright** with a literature-based bounded multiplier for both routing and cost, plus an impassable threshold for extreme slopes (e.g. above 30°) to keep mountain-range avoidance. This is simpler, but the avoidance behaviour then depends on the threshold.
- **C. Port choice by least total cost.** For each offshore sink, run `costDist` over a combined surface: sea cells at 1 per km, land at the terrain friction, and only ocean-connected water counted as sea (use `terra::patches`). At trunk scale, pipeline and ship cost per t-km are similar (about $0.04 vs $0.035/t/km), so equal weights are a reasonable approximation. A second run with sea friction 1+ε splits each route into land and sea legs, giving `dist_coast` and `dist_sea`. The model code already accepts these (`calculate_ccs_transport(dist_coast, dist_sea)`) and falls back to the current behaviour when the layers are absent.
- **D. Assign sinks by cost at run time.** Save per-sink layers: physical length and terrain factor for pipelines, and land/sea legs for offshore sinks. Then, in `calculate_beccs()`, choose the sink with the lowest transport + injection cost for the actual CO2 flow and plant size. That's 5–9 sinks per region, so the extra data is modest.
- **E. Performance.** Crop the DEM to the region (with a buffer aligned to the aggregation blocks) before computing slope, and run regions in parallel.

### Recommendation

1. **A:** keep the empirical friction for path choice only, and cost physical path length × a cited bounded terrain multiplier. As a sanity check, the ratio of physical length to straight-line distance should mostly fall around 1.1–1.4.
2. **C and D together:** save per-sink route components and assign sinks by total cost at run time. This fixes the port choice and the sink assignment in one step, and removes the water-friction-5 problem for offshore routes.
3. **E:** add this before the next regeneration, since A, C and D all need the transport layers rebuilt (about 20 min per region, or less once cropped).
4. **US offshore sinks:** add Gulf of Mexico offshore storage to the US sink set, or document its absence as a limitation.

## 2. Other open items

- **Injection cost citation:** the manuscript's CO2 Injection paragraph has a hidden TODO for the source of the onshore ($12/t) and offshore ($40/t) injection cost defaults.
- **Haulage factors:** `haulage_location_factor` values (US 1.0, Europe 1.0, China 0.75, India 0.65) are estimates from relative road-freight rates (India about $0.047/t-km vs about $0.07/t-km for US/EU truckload) and need a citable source. The variable haulage cost is now sourced (0.19 $/Mg/km, Searcy et al. 2007); the fixed $5/Mg is still unsourced.
- **CO2 transport emissions:** emissions from CO2 transport (pipeline compression, liquefaction, shipping fuel) are not deducted from BECCS abatement; only biomass trucking emissions are.
- **Stale tests:** `test-calculations.R` checks the old output name `res$elec_prod` (now `energy_prod`). `test-spatial_tea.R` only provides a 50 MWth distance layer, but the default plant is now 125 MWth.
- **Full re-run:** the MC, SHAP analysis and manuscript figures need a full re-run once the code fixes are complete.
- **Biochar nutrient and liming parameters:** `bc_p_content` and `bc_cce` are per Mg of biochar rather than derived from feedstock ash and P by mass balance. At the default 5% feedstock ash, the ash-recycling credit for BES/BECCS matches the biochar liming + P value per Mg feed. At 15% ash (China, India), ash is worth more, because the biochar values don't scale with feedstock ash. Both pathways should draw on shared feedstock P, K and alkalinity. The implied feedstock K (about 0.15%, from `bc_k_content`) is also low for crop residues (typically about 1%).
- **K and P units:** `price_k` and `price_p` are in $/kg K2O and $/kg P2O5, but the biochar and ash nutrient contents are applied as elemental K and P, with no oxide conversion (×1.2 for K2O, ×2.29 for P2O5).
- **K in ash:** most K volatilises in combustion but condenses in fly ash, so ash recycling that includes fly ash would recover some of it; K is currently not credited.
- **Residue soil GHG penalty:** resolved; see Status item 2.
- **Residue burning shares:** `residue_burn_fraction` for the US (0.02) and China (0.10) are estimates; India (0.16) is from Jain et al. (2014), and the EU (0.01) reflects the burning ban. Black carbon from burning is not counted.
- **Biochar logistics costs:** biochar is returned to fields as a backhaul in the feedstock trucks, charged loading and handling only (`bc_return_haul`; about $1.5–2/Mg feed). Field application of biochar (`bc_field_cost` in nets1.xlsm) is still not costed.
- **Ash return haulage: not included.** Back-of-envelope with default parameters: as a backhaul in the returning feedstock trucks (handling only), ash costs 0.2–0.8% of BES total cost per Mg feed (US 0.25, Europe 0.25, China 0.56, India 0.49 $/Mg). As a dedicated haul it would be 0.5–1.4%, exceeding 1% only in China (15% ash). Below the 1% threshold as a backhaul, so omitted. Ash spreading is also not costed.
- **Grid displacement intensity:** superseded by MEF(P); see Status item 3.
- **Haulage terrain factors (done, September 2026).** `data-raw/generate_logistics_layers.R` routes every field-to-plant trip on the Weiss et al. (2020) motorised friction surface and gives per-size time (`kt`), road-distance (`kd`) and climb-fuel (`g`) factors, each normalised to a regional biomass-weighted mean of 1. Variable trucking cost is split into time (64%), fuel (24%) and other distance costs (12%) after ATRI (2024); the Methods (Biomass Transport Costs and Emissions) describe this.
  - Effect with default parameters (flat 0.15 rate and no biochar charge → Searcy rate, terrain factors and biochar backhaul): feedstock haulage costs the same per Mg for all technologies, so the rate and terrain factors change no rankings. They change viability: BES net value moves −$4 to −$10/Mg (p5) with a median of −$0.3 to −$1.4/Mg. The biochar handling charge moves 0–3% of cells from BEBCS to BES or BECCS. The share of biomass in profitable cells falls by up to 4.6 points (Europe at $150/t: 39.7% → 35.1%; China at $50/t: 43.8% → 41.7%; India at $50/t: 37.0% → 35.5%).
  - **Open: regional level of haulage cost.** Implied average collection speeds at 125 MWth are 56 km/h (US), 45 (Europe), 30 (India) and 26 (China). `haulage_location_factor` (China 0.75, India 0.65) comes from long-haul truckload rates, which may understate rural collection costs where speeds are half the US level. Consider deriving the regional level from the friction-surface speeds combined with regional wage and fuel costs.
  - **Open: the climb-fuel factor** assumes 50% of descent energy offsets climbs on the same leg (1 km DEM). It is normalised within region, so only its spatial pattern matters.
  - **Done: haulage cost basis.** `bm_transport_var` = 0.19 $/Mg/km (Searcy et al. 2007: 0.12 $/Mg/km for straw and stover, in 2026 USD; per one-way km including the empty return). `bm_transport_fixed` ($5/Mg) still needs a source.
- **Avoided liming emissions:** substituting biochar or ash for agricultural lime avoids the CO2 released when lime dissolves (IPCC Tier 1: 0.12 t C per t limestone), which is not credited.
-**crs** co2_sinks in data has crs=4326.  Check that this is reprojected to regional crs for analysis when being used with equal area projections.

