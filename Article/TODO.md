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
     - (a) **P_now = today's explicit carbon price (decision 28 Sep 2026)**: EU 70, China 14, US 0, India 0 $/t (US$2024; checked 28 Sep against 2024 averages: EU ETS €65/t, China national ETS ¥98/t). The TEA's carbon price is interpreted as an explicit price, so the curve predicts the displaced intensity at an explicit price X. Alternatives considered and rejected:
       - Model-consistent P_now = 0: GCAM and MESSAGE report a Current Policies price of 0 because they model current policies as constraints.
       - Inversion from AR6 no-policy baselines: gives China ≈ $17/t, no solution for the US and India, and an implausible value for the EU.
       - Bottom-up effective prices.

       The low BES NPV at low carbon prices is accepted as realistic: there has been little residue-fired power build-out outside Danish straw CHP.
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
4. **US offshore sinks:** done (Gulf of Mexico offshore added 28 Sep 2026).

## 2. Other open items

- **Injection cost citation: resolved (28 Sep 2026).** `ccs_storage_cost` changed from 12 to 10 $/t (PERT 2–33) and `cost_offshore_storage` from 40 to 20 $/t (PERT 5–50), both 2024 USD, covering exploration to 50-year post-closure monitoring. Sources:
  - Global CCS Institute (2025): onshore 2–15 (open boundary), 5–33 (moderate closed), up to 58; offshore 5–31, 8–50, up to 147 $/t.
  - ZEP (2011): onshore saline €2–12/t, offshore saline €6–20/t.

  Cited in the Methods (`gccsi2025`, `zep2011`). The lower offshore cost makes offshore sinks more competitive.
- **Haulage factors:** `haulage_location_factor` values (US 1.0, Europe 1.0, China 0.75, India 0.65) are estimates from relative road-freight rates (India about $0.047/t-km vs about $0.07/t-km for US/EU truckload) and need a citable source. The variable haulage cost is now sourced (0.19 $/Mg/km, Searcy et al. 2007); the fixed $5/Mg is still unsourced. **Low priority.**
- **CO2 transport emissions: resolved (28 Sep 2026).** The BECCS efficiency penalty covers capture and compression at the plant only, so transport emissions are now deducted from BECCS abatement for the chosen route (new output `co2_transport_emissions`):
  - **Ship:** `co2_ship_emis_fixed` = 0.022 t/t (liquefaction, loading) + `co2_ship_emis_per_km` = 1.3e-5 t/t/km, from Al Baroudi et al. (2021): ~2.5% of CO2 carried at 200 km and ~18% at 12,000 km.
  - **Pipeline:** lift-pumping electricity at the displaced grid intensity (negligible).
  - **Not counted:** friction boosters (cost only).
- **Stale tests: resolved (26 Sep 2026).** `test-calculations.R` now checks `energy_prod`, and `test-spatial_tea.R` builds the distance layer for the default plant size.
- **Full re-run:** the MC, SHAP analysis and manuscript figures need a full re-run once the code fixes are complete.
- **Biochar nutrient and liming parameters: resolved.** The higher feedstock ash in China and India (15%) is largely silica from rice residues, so biochar P and liming values should not scale with ash content.
- **K and P units: resolved (fixed 28 Sep 2026).** No oxide conversion existed anywhere (code, data-raw or parameter history), and the price levels confirm they are oxide prices (US potash $0.82/kg K2O ≈ 2024 market). Elemental contents are now converted before pricing (P ×2.291 to P2O5, K ×1.205 to K2O) in `calculate_biochar_value()` and `calculate_ash_value()`. The effect is small (a few $/Mg feed).
- **K in ash:** most K volatilises in combustion but condenses in fly ash, so ash recycling that includes fly ash would recover some of it; K is currently not credited. **Low priority.**
- **Residue soil GHG penalty:** resolved; see Status item 2.
- **Residue burning shares:** `residue_burn_fraction` for the US (0.02) and China (0.10) are estimates; India (0.16) is from Jain et al. (2014), and the EU (0.01) reflects the burning ban. Black carbon from burning is not counted. **Open, low priority:** the estimates are acceptable and unlikely to change before publication; residue burning is not an important factor in the sensitivities.
- **Biochar logistics costs:** biochar is returned to fields as a backhaul in the feedstock trucks, charged loading and handling only (`bc_return_haul`; about $1.5–2/Mg feed). **Field application: added (28 Sep 2026).**
  - **`bc_field_cost` = 116 $/ha (PERT 0–233):** nets1.xlsm central 87.8, range 0–175.6 $/ha, × 1.325 from 2014 to 2024 USD. **The base year of nets1 is to confirm.** The cost is applied per Mg biochar at the rate `bc_app_rate_c` / biochar C content and scaled by the haulage location factor; ≈ $2.6/Mg feed in the US (nets1 gave $2.58).
  - **`bc_field_diesel` = 10 L/ha at 2.68 kg CO2/L:** an estimate (to check); nets1 has no tractor emissions. Its magnitude is negligible (~1.5 kg CO2/Mg biochar).
- **Ash return haulage: resolved (excluded as negligible).** As a backhaul in the returning feedstock trucks (handling only), ash costs 0.2–0.8% of BES total cost per Mg feed (US 0.25, Europe 0.25, China 0.56, India 0.49 $/Mg). As a dedicated haul it would be 0.5–1.4%. Ash spreading is not costed.
- **Grid displacement intensity:** superseded by MEF(P); see Status item 3.
- **Haulage terrain factors (done, September 2026).** `data-raw/generate_logistics_layers.R` routes every field-to-plant trip on the Weiss et al. (2020) motorised friction surface and gives per-size time (`kt`), road-distance (`kd`) and climb-fuel (`g`) factors, each normalised to a regional biomass-weighted mean of 1. Variable trucking cost is split into time (64%), fuel (24%) and other distance costs (12%) after ATRI (2024); the Methods (Biomass Transport Costs and Emissions) describe this.
  - Effect with default parameters (flat 0.15 rate and no biochar charge → Searcy rate, terrain factors and biochar backhaul): feedstock haulage costs the same per Mg for all technologies, so the rate and terrain factors change no rankings. They change viability: BES net value moves −$4 to −$10/Mg (p5) with a median of −$0.3 to −$1.4/Mg. The biochar handling charge moves 0–3% of cells from BEBCS to BES or BECCS. The share of biomass in profitable cells falls by up to 4.6 points (Europe at $150/t: 39.7% → 35.1%; China at $50/t: 43.8% → 41.7%; India at $50/t: 37.0% → 35.5%).
  - **Open, low priority: regional level of haulage cost.** Implied average collection speeds at 125 MWth are 56 km/h (US), 45 (Europe), 30 (India) and 26 (China). `haulage_location_factor` (China 0.75, India 0.65) comes from long-haul truckload rates, which may understate rural collection costs where speeds are half the US level. Consider deriving the regional level from the friction-surface speeds combined with regional wage and fuel costs.
  - **Open, low priority: the climb-fuel factor** assumes 50% of descent energy offsets climbs on the same leg (1 km DEM). It is normalised within region, so only its spatial pattern matters.
  - **Done: haulage cost basis.** `bm_transport_var` = 0.19 $/Mg/km (Searcy et al. 2007: 0.12 $/Mg/km for straw and stover, in 2026 USD; per one-way km including the empty return). `bm_transport_fixed` ($5/Mg) still needs a source.
- **Avoided liming emissions: resolved (not credited).** (a) Recycled ash also contains carbonates, so substituting ash or biochar for lime does not simply remove a lime CO2 source. (b) Whether agricultural liming is a net CO2 source or sink is uncertain and spatially variable: the sink term depends on how much carbonate dissolves to bicarbonate and is leached to the oceans before it reprecipitates.
- **CRS: checked (28 Sep 2026), no problem found.** Every layer in `GIS/processed/` (rasters and gpkg) is EPSG:4326, with one consistent grid per region (US 0.2°, others 0.1°; routing grids 0.02°/0.01°). `co2_sinks` is EPSG:4326. The v2 transport script transforms sinks to the routing-grid CRS and computes step lengths geodesically, and the distance rasters project to equal area internally. No layer relies on planar distances in degrees.
- **CO2 sink coverage (checked 28 Sep 2026; decision needed).** `co2_sinks` is 30 hand-entered basin centroids. Sinks are selected by region label, not clipped by bounding box, so no listed sink is lost at region edges. Major omissions:
  - **US:** Gulf of Mexico offshore (no US offshore sink at all); California (Sacramento–San Joaquin basins; first Class VI permits); Denver–Julesburg and Greater Green River / Wind River (Wyoming). The Alberta Basin (Canada, adjacent jurisdiction) was in the older list.
  - **Europe:** Adriatic/Ravenna (Eni offshore depleted gas; injection since 2024; was in the older list); East Irish Sea (HyNet, Liverpool Bay); Danish North Sea (Greensand); Prinos (Greece); Iberian onshore saline (Duero, Ebro); Polish Permian saline. Southern and eastern European biomass currently routes to the North Sea or the Pannonian and Paris basins.
  - **China:** the Sichuan Basin (near large Sichuan biomass); Jianghan; the Beibu Gulf and East China Sea offshore.
  - **India:** Mumbai offshore (Bombay High; was in the older list); the west coast has no offshore sink.

  Adding sinks requires regenerating the transport layers (~15 min per region, in parallel).
  - **Sinks added (28 Sep 2026)** in `data-raw/generate_sinks.R`, 30 → 46 entries:
    - US: Gulf of Mexico offshore, San Joaquin, Sacramento, Denver-Julesburg, Greater Green River.
    - Europe: Adriatic/Ravenna, East Irish Sea, Danish North Sea (Greensand), Prinos, Duero, Polish Lowlands.
    - China: Sichuan, Jianghan (EOR), Beibu Gulf, East China Sea.
    - India: Mumbai Offshore.

    The Alberta Basin was not added: it lies outside the US routing grid, so routes would be snapped to the border. **Done (28 Sep 2026):** `co2_sinks` rebuilt; transport layers regenerated. Sink counts: US 9 onshore saline / 4 EOR / 1 offshore; Europe 4 / 1 / 8; China 2 / 5 / 4; India 2 / 3 / 2. Tests pass.

# Open Manuscript issues
## Methods

Review of the Methods section by subsection (28 Sep 2026): consistency with the code, completeness, and citation support. Items marked **Done** were fixed in `BiomassTEA.qmd`. **Reference required** items give suggested sources where a good one is known.

### Code issue found during the review

- **M0. MACC and break-even figures assumed net value is linear in the carbon price (DONE 28 Sep 2026).** `get_linear_baseline()` computed Net(C) = Net(0) + C × Abatement(0), which overstates the grid-displacement credit at high prices now that abatement depends on C through MEF(P). **Fix applied:** new `BiocharAG/R/price_sweep.R`. Net value is exactly N0 + C·A(C), with N0 independent of C (checked to 1e-13), so `run_price_sweep()` runs the model at C = 0–250 by 5 and 275–500 by 25 $/t (about 10 s per region) and stores A. `sweep_abate()`/`sweep_net()` interpolate A linearly (exact below 0, where MEF is flat; A held at its 500 $/t value above the grid); interpolation error against direct runs is < 0.05 $/Mg within the grid. `sweep_breakeven()`/`price_root()` find the lowest price where net value turns non-negative. `get_linear_baseline()` removed from `scripts/manuscript_figures.R` and `scripts/manuscript_figures_extra.R`; the MACC, break-even and Fig. 5 (switch-away-from-BES threshold) now use the sweep, and the MACC abatement now uses A(C) rather than A(0). Tests in `tests_and_demos/testthat/test-price_sweep.R`. Methods text (Economic Evaluation) updated; it also now describes the MACC as the code builds it (highest-net-value technology per cell at each price) rather than a ranking by break-even price. Effect (default scenario): adopted abatement falls by 10–45% (e.g. China 1196 → 936 Mt/yr and India 783 → 582 Mt/yr at $100; US 46 → 11 Mt/yr at $100); median BES break-even rises sharply (e.g. China 71 → 393 $/t). Also deleted stray `params <- set_scenario(..., region = r)` lines in six single-region functions of `manuscript_figures_extra.R`, which referenced an undefined `r` and, in Fig. 7, discarded the `c_price` argument.

### Model Architecture

- **M1. Done: grid and CRS statement was wrong.** The text said all inputs were projected to equal-area CRSs at 0.1°. In fact all layers are WGS84 at 0.1° (US 0.2°), and equal-area projections are used only for the collection-distance focal sums. Corrected.
- **M2. Done: "NPV" definition.** The model computes a levelized annual net value per Mg of dry, ash-free feed (the NPV annualized with the CRF), not a project NPV. Clarified; "NPV" is kept as shorthand.
- **M3. Done: naming.** "Bioenergy (BE)" changed to BES, consistent with the rest of the text. The table caption said "BiocharAG framework"; it now says C-SCAPE, and the Methods state that BiocharAG is the R package implementing it.
- **M4. Done: typo** ("assumptions,m").
- **M5. Done: terra citation added** (`hijmans2025terra`).

### Biomass Feedstock Sourcing and Logistics

- **M6. Currency year of the haulage rate (open).** The text says 0.19 $/Mg/km is "escalated to 2026 USD", but the study now uses 2024 USD (≈ 0.18). Part of the pending currency audit; update text and parameter together.
- **M7. Reference required: truck emission factor** (0.0001 t CO2e per t·km). Suggested: GLEC Framework v3 (Smart Freight Centre, 2023) default intensities for heavy trucks.
- **M8. Reference required: fixed loading and handling cost** ($5/Mg); see the existing item under Other open items.
- **M9. Regional feedstock pricing is not spatial, and may double-count haulage (open; needs discussion).**
  - The Methods describe spatially explicit feedstock prices (US farm-gate plus nutrient replacement, EU NUTS-3 roadside costs, Chinese risk multipliers). But the spatial layers (`us_base_cost`, `eu_base_eur`, `cn_*_risk`) are neither loaded by `load_region_data()` nor present in `GIS/processed/`, so every region uses scalar defaults: US 95.06, EU 40 € × 1.364 × 1.10 ≈ 60, China ¥250 ≈ 35 $/Mg (risk flags off), India ₹2,750 (bale, ≤ 50 km) or ₹5,200 (pellet).
  - The Chinese value is a *plant-gate* (delivered) cost and the Indian values are described as bale *transport* costs, yet haulage is added on top.
  - Decide whether to restore the spatial layers or describe scalar prices, and whether to net out transport from the Chinese and Indian prices.
- **M10. Reference required: feedstock prices and exchange rates:**
  - US: $70/Mg farm-gate; $25.06/Mg nutrient replacement (corn stover).
  - EU: €40/Mg NUTS-3 roadside; storage premium +22.2% (3 months) / +36.4% (6 months).
  - India: ₹2,750 bale; ₹5,200 pellet; 50 km threshold.
  - China: ¥250 plant-gate; weather ×1.13; expansion ×1.53.
  - Exchange rates: 1.10 $/€; 0.012 $/₹; 0.14 $/¥.
- **M11. Reference required: fuel-quality penalty magnitudes.** The cited papers (Baxter 1998; García 2017; Luan 2025) describe the ash-related problems but not the specific values: CAPEX +25% / +10%, efficiency −10% / −5%, ash thresholds 5% and 2%. The Danish Energy Agency data give straw CHP investment about 20% above wood pellets (80 MW feed), which supports the order of magnitude.

### Technology Pathways (BES, BECCS)

- **M12. Done: default plant size.** The text said 250 MWth; the default is 125 MWth.
- **M13. Reference required: BES techno-economics.** Values: CAPEX 3,000 $/kWe at the base location, efficiency 30%, capacity factor 85%, lifetime 30 years, scaling exponent 0.7. Suggested: EIA AEO2023 (50 MW biomass: $4,996/kW, 2022 USD; heat rate 13,500 Btu/kWh ≈ 25% efficiency) and IRENA, *Renewable Power Generation Costs in 2023* (2024).
- **M14. Reference required: biomass properties** (C 48%, LHV 18.6 GJ/Mg daf, ash 5% and 15%). Suggested: the Phyllis2 database (TNO) for cereal straw, corn stover and rice straw.
- **M15. Reference required: BECCS efficiency penalty (8 percentage points) and capture rate (90%).** Suggested: IEAGHG (2009), already cited for the CAPEX premium, if its efficiency drop matches; otherwise Bhave et al. (2017).

### Carbon Transport and Geological Storage

- **M16. Done: Pipeline Routing described the v1 algorithm** (GDEM slope aggregated at the 95th percentile, exp(0.25θ) friction, WDPA as absolute barriers, least-cost distance costed as km). Rewritten for v2: Geomorpho90m 250 m slope; separate cost and routing surfaces; tiered protected areas; physical length × route-average terrain multiplier; the r_max robustness result. New citations: `amatulli2020`, `wdpa2026`.
- **M17. Done: hub-and-spoke section updated.** Physical length replaces the "effective distance"; the terrain multiplier, reference cost (US$50M per 100 km at 1 Mt/yr), scaling exponent 0.6, O&M 4% and booster pumping for lift (`mccollum2006`) are now stated.
- **M18. Done: shipping section** said CO2 goes to the "nearest coastal port" and had no subsea option. Rewritten: joint land and sea port or landfall choice by weighted multi-source least-cost search, subsea pipelines (1.5 × CAPEX), and liquefaction, terminal and voyage costs.
- **M19. Done: sink choice.** The text said the nearest saline sink is used. The model chooses, per cell, the class with the lowest transport-and-storage cost. A new subsection "Storage Sites and Sink Choice" describes the 46 basins and the four classes.
- **M20. Reference required: sink database.** The 46 basin centroids (`data-raw/generate_sinks.R`) cite an unidentified "Global Geologic Carbon Storage Assessment". Suggested: OGCI CO2 Storage Resource Catalogue, Cycle 4 (2024); USGS (2013) National Assessment of Geologic Carbon Dioxide Storage Resources; project sources for the 16 basins added in September 2026.
- **M21. Reference required: pipeline cost model parameters** (US$50M per 100 km at 1 Mt/yr, exponent 0.6, 3 Mt/yr trunk, 50 km feeder, 700 km booster threshold, penalty 2, O&M 4%). Suggested: ZEP (2011) *The Costs of CO2 Transport*; IEAGHG (2005) *Building the Cost Curves for CO2 Storage*; McCollum & Ogden (2006).
- **M22. Reference required: terrain and altitude construction-cost multipliers and the routing premium.** Suggested: IEA GHG (2002) *Pipeline Transmission of CO2 and Energy* (terrain cost factors).
- **M23. Reference required: shipping cost parameters** (liquefaction $20/t, terminal $15/t, voyage $0.035/t/km, sea-leg weights 0.7 and 1.5, subsea CAPEX 1.5×). Suggested: IEAGHG (2020) *The Status and Challenges of CO2 Shipping Infrastructures* (TR 2020-10); ZEP (2011) *The Costs of CO2 Transport*.

### BEBCS

- **M24. Reference required: pyrolysis and power-block techno-economics** (pyrolysis CAPEX $500 per Mg feed/yr, power block $1,500/kWe, efficiency 35%, parasitic 0.07 GJe/Mg, heater efficiency 0.8, lifetimes 20 and 25 years, py_temp 500 °C). Suggested: Woolf et al. (2016) Nature Communications (the nets1 model) where values come from it; otherwise original sources.
- **M25. Done: agronomic valuation.**
  - The text said physical *and nutrient* benefits are annuitized over 10 years; in the code only the physical benefit is annuitized (liming and nutrients are one-off credits). Corrected.
  - Nutrient availability factors (N 10%, P 50%, K 80%) added.
  - Lime price now refers to the regional table.
- **M26. Reference required: biochar agronomic parameters** (CCE 15%; N, P and K contents; availability factors; 10-year impact duration).
- **M27. The CEC value function is a heuristic (open; needs discussion).** The model assumes $50/Mg/yr in soil with CEC 5, falling linearly to $0 at CEC 30. It needs support: yield-response meta-analyses (e.g. Jeffery et al. 2011, *Agric. Ecosyst. Environ.*; Jeffery et al. 2017, *Environ. Res. Lett.*) or presentation as an illustrative assumption with a sensitivity range.
- **M28. Reference required: regional fertilizer and lime prices** (`price_n`, `price_p`, `price_k`, `price_lime`).

### Baseline Energy System and Offsets

- **M29. Done: net abatement balances** now include CO2 transport emissions (BECCS) and field-application diesel (BEBCS), with a pointer to the residue counterfactual.
- **M30. Done: grid displacement.**
  - "Today's effective carbon price" changed to explicit, consistent with the P_now decision.
  - Ember (`ember2025`) and IPCC AR5 Annex III (`schlomer2014`) cited for generation data and life-cycle intensities.
- **M31. Reference required: oil life-cycle intensity (700 g/kWh).** IPCC AR5 Annex III does not list oil-fired generation; a source is needed (e.g. UNECE 2021 *Life Cycle Assessment of Electricity Generation Options*, if it covers oil).

### Regional Cost Adjustment and Economics

- **M32. Reference required: regional CAPEX location factors** (US 1.25, Europe 1.15, China 0.7, India 0.65) and the O&M location factors derived from them.
- **M33. Reference required: regional discount rates** (US 5%, Europe 4.5%, China 4.5%, India 10%). Suggested: Steffen (2020) *Energy Economics* 88:104783 (cost of capital for renewable energy projects by country); IEA *Cost of Capital Observatory*.

### Sections missing from the Methods

- **M34. Drafted, to review: new subsections.**
  - "Spatial Input Data": SoilGrids 2.0 (`poggio2021`), soil temperature (`lembrechts2022`), electricity prices.
  - "Economic Evaluation and Technology Selection": net value, CRF, lifetimes, technology choice, break-even price, MACCs.
  - "Uncertainty and Sensitivity Analyses": Monte Carlo design, XGBoost (`chen2016xgboost`) and SHAP (`lundberg2017`), factorial design.

  These describe what the code does. Check that the factorial design matches the manuscript runs: `run_analyses.R` comments say 960 scenarios, while `factorial_analysis.R` gives 720.
- **M35. Reference required: electricity prices** (EIA, Eurostat, NDRC, CERC industrial prices 2024–25) and the wholesale factor 0.4.
- **M36. Bibliography: `lembrechts2022`** uses "and others" for its long author list; complete it or use the journal's recommended short form.

### Housekeeping done during the review

- **M37. Done: `data-raw/generate_us_soil_layers.R` moved to `bak/generate_us_soil_layers_demo.R`.** It generates *synthetic* demo soil layers and writes `us_soil_ph.tif` and `us_soil_cec.tif`, so running it would overwrite the real SoilGrids layers. The current layers match SoilGrids: US pH 4.1–9.0, median 6.3, spatially patchy like Europe. The orphaned demo outputs `GIS/processed/soil_ph.tif` and `soil_cec.tif` (January 2026) are unused and can be deleted.



