# Issue tracker

Open modelling and manuscript issues are tracked as **GitHub Issues**: <https://github.com/domwoolf/Biochar_AG/issues>.

Useful views:
- [Open, high priority](https://github.com/domwoolf/Biochar_AG/issues?q=is%3Aissue+is%3Aopen+label%3Apriority%3Ahigh)
- [Open, needs a decision](https://github.com/domwoolf/Biochar_AG/issues?q=is%3Aissue+is%3Aopen+label%3Aneeds-decision)
- [Open, reference required](https://github.com/domwoolf/Biochar_AG/issues?q=is%3Aissue+is%3Aopen+label%3Areference-required)
- [Currency audit](https://github.com/domwoolf/Biochar_AG/issues?q=is%3Aissue+label%3Acurrency-audit)
- [Recently closed](https://github.com/domwoolf/Biochar_AG/issues?q=is%3Aissue+is%3Aclosed+sort%3Aupdated-desc)

Each issue has a priority label (`priority:high`, `priority:medium`, `priority:low`) and area labels. From the command line: `gh issue list --label priority:high`.
Reference issues in commit messages (e.g. "Fixes #16") to close them automatically, and in code comments as `TODO(#16)`.

Long design notes live in `docs/design/` (e.g. [CO2 transport routing](../docs/design/co2_transport_routing.md)).

## Legacy IDs

This file held the issue list until 2026-09-30, when every item was migrated to GitHub Issues (resolved items as closed issues). Issue titles keep the old IDs in square brackets. T = modelling issues, M = Methods review (28 Sep 2026), C = currency audit.

| Legacy ID | Issue | Status at migration | Title |
|---|---|---|---|
| T1 | [#1](https://github.com/domwoolf/Biochar_AG/issues/1) | closed | O&M levels: single all-in O&M rate for all technologies |
| T2 | [#2](https://github.com/domwoolf/Biochar_AG/issues/2) | closed | Residue soil GHG penalty replaces SOC-only penalty |
| T3 | [#3](https://github.com/domwoolf/Biochar_AG/issues/3) | closed | Price-dependent grid displacement intensity MEF(P) |
| T4 | [#4](https://github.com/domwoolf/Biochar_AG/issues/4) | open | MEF(P): reconfirm keeping the Low Demand scenario |
| T5 | [#5](https://github.com/domwoolf/Biochar_AG/issues/5) | open | MEF(P): source and records for the empirical build-margin method |
| T6 | [#6](https://github.com/domwoolf/Biochar_AG/issues/6) | open | MEF(P): verify the NGFS citation year (ngfs2026) |
| T7 | [#7](https://github.com/domwoolf/Biochar_AG/issues/7) | closed | MEF(P) revision 2: downscaled MESSAGE regions, NGFS-only spread |
| T8 | [#8](https://github.com/domwoolf/Biochar_AG/issues/8) | closed | Haulage terrain and road-access factors (Weiss 2020 friction surface) |
| T9 | [#9](https://github.com/domwoolf/Biochar_AG/issues/9) | closed | Variable haulage rate from Searcy et al. (2007) |
| T10 | [#10](https://github.com/domwoolf/Biochar_AG/issues/10) | closed | Biochar return haulage as a backhaul; ash return omitted |
| T11 | [#11](https://github.com/domwoolf/Biochar_AG/issues/11) | closed | Plant-side site cost factor (handoff item 6): dropped |
| T12 | [#12](https://github.com/domwoolf/Biochar_AG/issues/12) | open | Regional level of haulage cost (collection speeds vs freight-rate factors) |
| T13 | [#13](https://github.com/domwoolf/Biochar_AG/issues/13) | open | Climb-fuel factor assumes 50% descent energy recovery |
| T14 | [#14](https://github.com/domwoolf/Biochar_AG/issues/14) | open | Calibrate modelled CO2 corridors against existing pipelines (handoff item 7) |
| T15 | [#15](https://github.com/domwoolf/Biochar_AG/issues/15) | closed | Manuscript Methods for v2 routing (handoff item 8) |
| T16 | [#16](https://github.com/domwoolf/Biochar_AG/issues/16) | open | Full re-run of Monte Carlo, SHAP and manuscript figures |
| T17 | [#17](https://github.com/domwoolf/Biochar_AG/issues/17) | closed | CO2 transport routing v2 (path choice separated from cost; cost-based sink, port and landfall choice) |
| T18 | [#18](https://github.com/domwoolf/Biochar_AG/issues/18) | closed | Protected areas were never applied in v1 routing |
| T19 | [#19](https://github.com/domwoolf/Biochar_AG/issues/19) | open | Sea-leg weights are fixed in the GIS step |
| T20 | [#20](https://github.com/domwoolf/Biochar_AG/issues/20) | open | Sink point locations: use basin polygons or actual storage sites |
| T21 | [#21](https://github.com/domwoolf/Biochar_AG/issues/21) | closed | CO2 sink coverage: 16 sinks added (30 → 46), incl. US offshore |
| T22 | [#22](https://github.com/domwoolf/Biochar_AG/issues/22) | closed | v1: whole global DEM slope-aggregated before cropping (slow) |
| T23 | [#23](https://github.com/domwoolf/Biochar_AG/issues/23) | open | hires_factor = 1 fails in the aggregation step |
| T24 | [#24](https://github.com/domwoolf/Biochar_AG/issues/24) | closed | CO2 injection (storage) cost citation |
| T25 | [#25](https://github.com/domwoolf/Biochar_AG/issues/25) | open | Haulage location factors need a citable source |
| T26 | [#26](https://github.com/domwoolf/Biochar_AG/issues/26) | closed | Deduct CO2 transport emissions from BECCS abatement |
| T27 | [#27](https://github.com/domwoolf/Biochar_AG/issues/27) | closed | Stale unit tests |
| T28 | [#28](https://github.com/domwoolf/Biochar_AG/issues/28) | closed | Biochar nutrient and liming values should not scale with (silica) ash |
| T29 | [#29](https://github.com/domwoolf/Biochar_AG/issues/29) | closed | K and P units: convert elemental contents to oxides before pricing |
| T30 | [#30](https://github.com/domwoolf/Biochar_AG/issues/30) | open | Potassium in fly ash is not credited |
| T31 | [#31](https://github.com/domwoolf/Biochar_AG/issues/31) | open | Residue burning shares for the US and China are estimates |
| T32 | [#32](https://github.com/domwoolf/Biochar_AG/issues/32) | closed | Biochar field application cost and tractor diesel |
| T33 | [#33](https://github.com/domwoolf/Biochar_AG/issues/33) | closed | Check the biochar spreading diesel estimate (10 L/ha) |
| T34 | [#34](https://github.com/domwoolf/Biochar_AG/issues/34) | closed | Ash return haulage excluded as negligible |
| T35 | [#35](https://github.com/domwoolf/Biochar_AG/issues/35) | closed | Avoided liming emissions not credited |
| T36 | [#36](https://github.com/domwoolf/Biochar_AG/issues/36) | closed | CRS consistency check across all layers |
| T37 | [#37](https://github.com/domwoolf/Biochar_AG/issues/37) | open | Soil temperature has a surprisingly high SHAP value |
| T38 | [#38](https://github.com/domwoolf/Biochar_AG/issues/38) | open | Confirm whether the Karan et al. (2023) residue supply excludes fodder use |
| T39 | [#39](https://github.com/domwoolf/Biochar_AG/issues/39) | open | Move the unused legacy calc_transport_cost() to bak/ |
| T40 | [#40](https://github.com/domwoolf/Biochar_AG/issues/40) | open | Align evaporation-map discount rates with the factorial set |
| T41 | [#41](https://github.com/domwoolf/Biochar_AG/issues/41) | open | Parameter table does not show the Monte Carlo ranges cited in the Methods |
| T42 | [#42](https://github.com/domwoolf/Biochar_AG/issues/42) | open | Delete orphaned demo soil layers in GIS/processed |
| M0 | [#43](https://github.com/domwoolf/Biochar_AG/issues/43) | closed | MACC and break-even figures assumed net value linear in the carbon price |
| M1 | [#44](https://github.com/domwoolf/Biochar_AG/issues/44) | closed | Grid and CRS statement in Model Architecture |
| M2 | [#45](https://github.com/domwoolf/Biochar_AG/issues/45) | closed | Define the levelised annual net value ("NPV") |
| M3 | [#46](https://github.com/domwoolf/Biochar_AG/issues/46) | closed | Naming: BES and C-SCAPE |
| M4 | [#47](https://github.com/domwoolf/Biochar_AG/issues/47) | closed | Typo "assumptions,m" |
| M5 | [#48](https://github.com/domwoolf/Biochar_AG/issues/48) | closed | Cite terra |
| M6 | [#49](https://github.com/domwoolf/Biochar_AG/issues/49) | open | Currency audit: all monetary values to 2024 USD |
| M7 | [#50](https://github.com/domwoolf/Biochar_AG/issues/50) | open | Reference required: truck emission factor |
| M8 | [#51](https://github.com/domwoolf/Biochar_AG/issues/51) | open | Reference required: fixed loading and handling cost ($5/Mg) |
| M9 | [#52](https://github.com/domwoolf/Biochar_AG/issues/52) | open | Regional feedstock pricing is not spatial and may double-count haulage |
| M10 | [#53](https://github.com/domwoolf/Biochar_AG/issues/53) | open | Reference required: feedstock prices and exchange rates |
| M11 | [#54](https://github.com/domwoolf/Biochar_AG/issues/54) | open | Reference required: fuel-quality penalty magnitudes |
| M12 | [#55](https://github.com/domwoolf/Biochar_AG/issues/55) | closed | Default plant size stated as 250 MWth |
| M13 | [#56](https://github.com/domwoolf/Biochar_AG/issues/56) | open | Reference required: BES techno-economics |
| M14 | [#57](https://github.com/domwoolf/Biochar_AG/issues/57) | open | Reference required: biomass properties |
| M15 | [#58](https://github.com/domwoolf/Biochar_AG/issues/58) | open | Reference required: BECCS efficiency penalty and capture rate |
| M16 | [#59](https://github.com/domwoolf/Biochar_AG/issues/59) | closed | Pipeline Routing section described the v1 algorithm |
| M17 | [#60](https://github.com/domwoolf/Biochar_AG/issues/60) | closed | Hub-and-spoke pipeline section |
| M18 | [#61](https://github.com/domwoolf/Biochar_AG/issues/61) | closed | Shipping section: port choice and subsea option |
| M19 | [#62](https://github.com/domwoolf/Biochar_AG/issues/62) | closed | Sink choice by lowest transport-and-storage cost |
| M20 | [#63](https://github.com/domwoolf/Biochar_AG/issues/63) | open | Reference required: CO2 sink database |
| M21 | [#64](https://github.com/domwoolf/Biochar_AG/issues/64) | open | Reference required: pipeline cost model parameters |
| M22 | [#65](https://github.com/domwoolf/Biochar_AG/issues/65) | open | Reference required: terrain and altitude cost multipliers and routing premium |
| M23 | [#66](https://github.com/domwoolf/Biochar_AG/issues/66) | open | Reference required: CO2 shipping cost parameters |
| M24 | [#67](https://github.com/domwoolf/Biochar_AG/issues/67) | closed | Reference required: pyrolysis and power-block techno-economics |
| M25 | [#68](https://github.com/domwoolf/Biochar_AG/issues/68) | closed | Agronomic valuation text vs code |
| M26 | [#69](https://github.com/domwoolf/Biochar_AG/issues/69) | open | Reference required: biochar agronomic parameters |
| M27 | [#70](https://github.com/domwoolf/Biochar_AG/issues/70) | open | CEC value function is a heuristic |
| M28 | [#71](https://github.com/domwoolf/Biochar_AG/issues/71) | closed | Reference required: fertilizer and lime prices |
| M29 | [#72](https://github.com/domwoolf/Biochar_AG/issues/72) | closed | Net abatement balances in the Methods |
| M30 | [#73](https://github.com/domwoolf/Biochar_AG/issues/73) | closed | Grid displacement text and citations |
| M31 | [#74](https://github.com/domwoolf/Biochar_AG/issues/74) | open | Reference required: oil life-cycle intensity (700 g/kWh) |
| M32 | [#75](https://github.com/domwoolf/Biochar_AG/issues/75) | open | Reference required: regional CAPEX location factors |
| M33 | [#76](https://github.com/domwoolf/Biochar_AG/issues/76) | closed | Reference required: regional discount rates |
| M34 | [#77](https://github.com/domwoolf/Biochar_AG/issues/77) | open | Review drafted Methods subsections |
| M35 | [#78](https://github.com/domwoolf/Biochar_AG/issues/78) | open | Reference required: electricity prices and wholesale factor |
| M36 | [#79](https://github.com/domwoolf/Biochar_AG/issues/79) | open | Bibliography: complete the lembrechts2022 author list |
| M37 | [#80](https://github.com/domwoolf/Biochar_AG/issues/80) | closed | Synthetic US soil-layer demo script moved to bak/ |
| M38 | [#81](https://github.com/domwoolf/Biochar_AG/issues/81) | open | BECCS CAPEX premium should represent mature technology |
| C1 | [#82](https://github.com/domwoolf/Biochar_AG/issues/82) | closed | BES CAPEX: documented regional values |
| C2 | [#83](https://github.com/domwoolf/Biochar_AG/issues/83) | closed | Pyrolysis unit CAPEX |
| C3 | [#84](https://github.com/domwoolf/Biochar_AG/issues/84) | closed | BEBCS power block CAPEX |
| C4 | [#85](https://github.com/domwoolf/Biochar_AG/issues/85) | open | BEBCS heat block CAPEX and heat price: source and year |
| C5 | [#86](https://github.com/domwoolf/Biochar_AG/issues/86) | open | CO2 pipeline reference CAPEX: source and year |
| C6 | [#87](https://github.com/domwoolf/Biochar_AG/issues/87) | open | CO2 liquefaction, terminal and voyage costs: source and year |
| C7 | [#88](https://github.com/domwoolf/Biochar_AG/issues/88) | open | CO2 storage costs: currency year of GCCSI (2025) |
| C8 | [#89](https://github.com/domwoolf/Biochar_AG/issues/89) | closed | Fertilizer prices: world market prices (World Bank Pink Sheet) |
| C9 | [#90](https://github.com/domwoolf/Biochar_AG/issues/90) | open | Lime price: better source needed |
| C10 | [#91](https://github.com/domwoolf/Biochar_AG/issues/91) | closed | CEC physical benefit: sourced value, valued as a perpetuity |
| C11 | [#92](https://github.com/domwoolf/Biochar_AG/issues/92) | open | Biochar price (sales method): currency year |
| C12 | [#93](https://github.com/domwoolf/Biochar_AG/issues/93) | open | Field application cost: confirm nets1 base year |
| C13 | [#94](https://github.com/domwoolf/Biochar_AG/issues/94) | open | Loading/handling cost ($5/Mg): source and year |
| C14 | [#95](https://github.com/domwoolf/Biochar_AG/issues/95) | open | Wholesale electricity price layers: source and year |
| C15 | [#96](https://github.com/domwoolf/Biochar_AG/issues/96) | open | US feedstock cost: source and year |
| C16 | [#97](https://github.com/domwoolf/Biochar_AG/issues/97) | open | EU feedstock cost: source and year |
| C17 | [#98](https://github.com/domwoolf/Biochar_AG/issues/98) | open | China feedstock cost: source and year |
| C18 | [#99](https://github.com/domwoolf/Biochar_AG/issues/99) | open | India feedstock cost: source and year |
| C19 | [#100](https://github.com/domwoolf/Biochar_AG/issues/100) | closed | Discount rates: real, after-tax WACC (Hatton et al. 2025) |
