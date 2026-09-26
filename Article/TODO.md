# TODO

Open modelling issues to resolve before the final re-run of results.

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
- **Haulage factors:** `haulage_location_factor` values (US 1.0, Europe 1.0, China 0.75, India 0.65) are estimates from relative road-freight rates (India about $0.047/t-km vs about $0.07/t-km for US/EU truckload) and need a citable source. The base haulage costs (5 $/Mg + 0.15 $/Mg/km) are also unsourced.
- **CO2 transport emissions:** emissions from CO2 transport (pipeline compression, liquefaction, shipping fuel) are not deducted from BECCS abatement; only biomass trucking emissions are.
- **Stale tests:** `test-calculations.R` checks the old output name `res$elec_prod` (now `energy_prod`). `test-spatial_tea.R` only provides a 50 MWth distance layer, but the default plant is now 125 MWth.
- **Full re-run:** the MC, SHAP analysis and manuscript figures need a full re-run once the code fixes are complete.
- **Biochar nutrient and liming parameters:** `bc_p_content` and `bc_cce` are per Mg of biochar rather than derived from feedstock ash and P by mass balance. At the default 5% feedstock ash, the ash-recycling credit for BES/BECCS matches the biochar liming + P value per Mg feed. At 15% ash (China, India), ash is worth more, because the biochar values don't scale with feedstock ash. Both pathways should draw on shared feedstock P, K and alkalinity. The implied feedstock K (about 0.15%, from `bc_k_content`) is also low for crop residues (typically about 1%).
- **K and P units:** `price_k` and `price_p` are in $/kg K2O and $/kg P2O5, but the biochar and ash nutrient contents are applied as elemental K and P, with no oxide conversion (×1.2 for K2O, ×2.29 for P2O5).
- **K in ash:** most K volatilises in combustion but condenses in fly ash, so ash recycling that includes fly ash would recover some of it; K is currently not credited.
- **Residue SOC loss:** confirm the source for `residue_c_retention` = 0.11 (long-term retention efficiency of residue C in soil). Bolinder et al. (2020) is cited for the effect size of residue retention on SOC. The term is about −0.19 t CO2e per Mg feed, which is large relative to other abatement terms.
- **Residue burning shares:** `residue_burn_fraction` for the US (0.02) and China (0.10) are estimates; India (0.16) is from Jain et al. (2014), and the EU (0.01) reflects the burning ban. Black carbon from burning is not counted.
- **Biochar logistics costs:** BEBCS costs don't include biochar haulage and field application (the nets1.xlsm model included `bc_haul_cost` and `bc_field_cost`), and ash spreading is likewise not costed for BES/BECCS.
- **Avoided liming emissions:** substituting biochar or ash for agricultural lime avoids the CO2 released when lime dissolves (IPCC Tier 1: 0.12 t C per t limestone), which is not credited.
-**crs** co2_sinks in data has crs=4326.  Check that this is reprojected to regional crs for analysis when being used with equal area projections.

