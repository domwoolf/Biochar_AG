# Revision spec: biochar field application, agronomic value, N₂O, and nutrient accounting

Handover for implementation in C-SCAPE (`BiocharAG/R`, `BiocharAG/inst/extdata/parameters.csv`, Methods in `BiomassTEA.qmd`).
Status: design agreed with the PI, 2026-10-08. Evidence base: analyses in `biochar_agronomy_analysis_v2.zip` (Python; `run_all.sh` gives the order). A reference implementation with test values is in `agro_ref.py` / `agro_ref_testvalues.txt`.

## 1. What changes, in one paragraph

The yield benefit of biochar is currently linear in biochar carbon (21.1 US\$ Mg⁻¹ C yr⁻¹, scaled linearly from CEC 5 to 30 cmol꜀ kg⁻¹, as a perpetuity), and the N₂O credit, nutrient credits and spreading costs are not tied to how much biochar each hectare receives or how often. The revision makes the field-application strategy explicit: each cell's biochar supply per hectare of its own cropland is applied in rotating cohorts at a chosen dose, the yield response is a concave function of the cumulative biochar stock per hectare, N₂O reductions depend on how recently each hectare was treated, and spreading cost is charged per pass. Nutrient and liming credits are put on a consistent counterfactual basis by charging every pathway for the nutrients and alkalinity removed with the residue and crediting each technology for what it returns, on the same availability basis.

## 2. Evidence base (for the Methods and SI)

Datasets: Woolf et al. 2016 training data (`BCYield_data2.csv`), Xu et al. 2021 (*Soil Tillage Res.* 213:105125; sheets "All" and "Interaction"), Zhang et al. 2023 (*Sci. Total Environ.* 905:167290).

Cleaning (all affect results; report in SI):
- **Control matching.** In Xu et al., treatments combining biochar with another input (BN, BNPK, BM, …) share the biochar-only control (CK) in 27 of 33 studies, so "BX vs CK" includes the input's own effect (mean lnRR +0.24 to +0.38 vs +0.15 for clean contrasts). Kept only where CK differs from the study's biochar-only CK; otherwise rebuilt as BX vs X from the Interaction sheet (94 contrasts). Three Zhang studies whose control yield does not vary with N level were dropped. The 2016 data are assumed to use fertilizer-matched controls (PI to confirm).
- **Duplicates.** 15 studies appear in more than one dataset (matched on first-author surname, year, dose, site); precedence Xu > Zhang > 2016 for field rows. Identical rows re-entered under different study names removed.
- **Pot vs field.** Xu labels all trials "field"; four studies coded as pot trials in the 2016 data (Chan 2007, 2008; Wang 2012; Xie 2013) and one column trial were recoded as pot. Pot trials excluded throughout (their dose response differs completely).
- Final field yield data: 673 dose-means in 251 multi-dose experiments (115 studies) for dose shape; 1,264 observations (191 studies) for amplitude.

Results used below (90% CIs by bootstrap over studies unless stated):

| Quantity | Estimate | Basis |
|---|---|---|
| Yield dose shape, power exponent *p*, all field | 0.50 (0.29–0.66) | within-experiment fits, experiment-specific amplitudes, inverse-variance weights |
| *p*, soil pH ≥ 6.5 (CEC component) | 0.30 (0.16–0.46) | as above, 100 experiments, 50 studies |
| *p*, soil pH < 6.0 | 0.64 (0.38–0.77) | 99 experiments, 41 studies (liming adds a near-linear term) |
| Yield amplitude at 10 Mg biochar ha⁻¹, lnRR | a₁₀ = 0.3607 + 0.0696·max(0, 6.5 − pH) − 0.1008·ln CEC | multilevel meta-regression, 434 obs, 51 studies with pH and CEC; SEs 0.076, 0.029, 0.025 |
| CEC at which the CEC yield response is zero | ≈ 36 cmol꜀ kg⁻¹ | from a₁₀ (cf. 30 in the current heuristic) |
| Out-of-study skill of amplitude models (study-level R²) | meta-regression 0.20; monotone boosted trees 0.25; RF all covariates 0.32; random (leaky) CV 0.38 | 10-fold CV grouped by study, 5 repeats |
| N₂O reduction, field | 20% (11–29%); lnRR −0.224 ± 0.068 (SE) | multilevel random-effects model, 160 obs, 36 studies, clean contrasts |
| N₂O dose dependence, 2.2–72 Mg ha⁻¹ | none detectable (log-dose slope 0.001 ± 0.052) | across and within experiments |
| N₂O aging | not identifiable (25 obs > 1 yr, 6 studies) | — |
| Persistence of yield effect | not identifiable; k = 0.14 yr⁻¹ (−0.43 to 0.31) | 70 repeated-measure series, 23 studies, median follow-up 1 yr |

Caveat to state: in pH ≥ 6.5 soils alone (19 studies) the CEC slope is half as large and not significant (−0.049 ± 0.042), so the CEC signal is identified largely from acid soils, where low CEC and low pH co-occur. Used as the pessimistic bound in the Monte Carlo (§6).

## 3. Feedstock price and nutrient/alkalinity accounting

### 3.1 Feedstock price
- United States: replace 76.9 US\$ Mg⁻¹ with the price excluding the grower's nutrient payment, ≈ 37 US\$ Mg⁻¹ (collection + grower premium; PI has the decomposition from @perlack2011). Delete the sentence "so we add no separate nutrient-replacement charge" (Methods, Feedstock prices).
- Other regions unchanged.

### 3.2 Removal charge (all pathways, all regions)
Per Mg dry ash-free feedstock:

C_rem = (1 − f_burn) · f_N · n_N · p_N + n_P · p_P + n_K · p_K + 𝟙[pH < pH_target] · A_res · p_lime

- n_N, n_P, n_K: residue N, P, K (the regional `bm_n`, `bm_p`, `bm_k`), priced per element via oxide/urea conversions as now.
- **P and K at total content** (long-run equivalence: they remain in the soil system whether returned as residue, biochar or ash). The first-year availability factors (0.1, 0.5, 0.8) are removed everywhere; this also removes the unreferenced TODO.
- **N at a fertilizer-replacement fraction f_N** (new parameter; see §6). Charged only on the retained share (1 − f_burn): burning returns no N.
- **Alkalinity**: A_res is @eq-anc evaluated on the residue (all Cl and S present), charged at the lime price where soil pH is below the target pH, the same condition as the liming credit. Under burning the ash returns the alkalinity, so no burning adjustment is needed for A (P, K likewise).
- The same world fertilizer prices as the credits.

### 3.3 Return credits
- **PyCCS**: biochar returns all P and K (credit at total content), ≈ 0 N (heterocyclic; credit 0), and alkalinity A_bc (@eq-anc with 45% Cl, 40% S retained) at lime price where pH < target.
- **BE/BECCS**: bottom ash returns its P, K and A_bottom; fly ash, with returned share s_fly (existing MC 0–1), returns its K and P (its ANC ≈ 0). N credit 0.
- Net effect: relative to retention, PyCCS ≈ zero net P and K, a small positive alkalinity credit (volatilised Cl and S), and a cost of f_N·n_N; BE/BECCS lose the fly-ash K, Cl and S unless returned. Only differences between pathways affect technology choice; the common removal charge shifts all MACCs.
- Optional (low priority after netting): cap each alkalinity credit at the lime requirement.

### 3.4 Replacement-fertilizer emissions (small; check, then document)
If removed N is replaced with mineral N, its upstream emissions are a cost; field N₂O from residue N removed and fertilizer N added roughly cancel. If implemented, apply the same upstream factor to avoided fertilizer credited to biochar/ash. At f_N ≤ 0.5 this is expected to be small; a sentence in Methods suffices if not implemented.

## 4. Field application model (PyCCS biochar; same machinery available for ash)

### 4.1 Supply per physical hectare
- b_h: biochar produced per harvested ha per yr in the cell (Mg biochar ha⁻¹ yr⁻¹) — available residue per harvested area (MapSPAM basis) × biochar yield. PI has computed this (`biochar_production_histograms_harvested.csv`).
- CI: cropping intensity (harvests per physical ha per yr), regional parameter (§6).
- b = CI · b_h: biochar per **physical** ha per yr; m = b / Y_bc: feedstock per physical ha per yr.
- Doses are in **Mg biochar ha⁻¹** (the units of the field data), not Mg C. Convert from the model's C basis with the biochar C fraction.

### 4.2 Dose options and cohorts
- Dose options D ∈ {annual, 2.5, 5, 10, 20} Mg biochar ha⁻¹ (annual means D = b).
- n = max(1, ⌈D/b⌉) cohorts; each cohort is treated every n years with D_eff = n·b, for t = 1…T (plant life; 20 yr for the pyrolysis unit).
- Treat the dose options like the PyCCS power-block configurations: evaluate each, and at each carbon price use the one with the highest net value (the N₂O credit is valued at the carbon price, so the choice can change with price).
- Hauling biochar beyond the source fields (D < b at extra haulage cost) is **not** in this round; mention as an extension.

### 4.3 Effective stock and persistence
B_{j,t} = Σ_{applications i ≤ t} D_eff · exp(−(k_y + k_C)(t − t_i)), for cohort j, t = 1…H (H = 100 yr; stock outlives the plant).
- k_C = −ln F_perm / 100 (existing).
- k_y: decay of the yield effect beyond carbon loss. Central 0 (mechanistic: biochar CEC rises with age); MC range in §6.

### 4.4 Yield response and value
- g(B) = (B/10)^p for B ≥ B_min; g(B) = (B_min/10)^p · B / B_min for B < B_min (linear through the origin; do **not** extrapolate the power law below the lowest well-tested dose — it makes per-tonne efficiency explode and the optimiser will exploit it).
- p = p_CEC (pH ≥ 6.5 shape; the liming part is valued separately).
- a₁₀,CEC = max(0, β₀ + β_CEC · ln CEC) — the acid term is **not** used for yield, because the pH effect is credited as avoided lime.
- Yield value per physical ha in year t: Y_t = CI · V_crop · mean_j[exp(a₁₀,CEC · g(B_{j,t})) − 1]
- V_crop: value of crop production per harvested ha (US\$ ha⁻¹ yr⁻¹) at production-weighted international commodity prices for major row crops (PI's existing basis; MapSPAM production × fixed international prices). This replaces the per-Mg-C value and its 2008–2013 price base.
- Assumption to state: a series of small applications acts like one application of the same cumulative stock (supported indirectly: time since application has ~zero importance in the 2016 random forest).

### 4.5 Liming
Net alkalinity credit per §3 at lime price where pH < target. Unchanged in form, but now net of the removal charge.

### 4.6 N₂O
- Direct emissions on treated land: E_dir = CI · N_app · EF₁ (per physical ha), with N_app per harvest as now.
- Reduction R applied to cohorts in proportion to an activity factor that restarts at each application: act_{j,t} = exp(−λ (t − t_last,j)), λ = ln 2 / half-life.
- Doses below the lowest tested dose scale linearly: R_eff = R · min(1, D_eff / D_min,N₂O), D_min,N₂O = 2.2 Mg ha⁻¹.
- Avoided N₂O per physical ha in year t: E_dir · R_eff · mean_j(act_{j,t}) · 44/28 · 10⁻³ (Mg N₂O), × GWP₁₀₀.
- **Abatement per Mg feedstock is undiscounted**: Σ_{t=1}^{H} avoided_t · GWP / (m · T), consistent with the undiscounted treatment of biochar carbon storage.
- Optional (cheap, makes N₂O spatially explicit): disaggregated IPCC 2019 EF₁ (wet-climate synthetic N, dry climate, flooded rice) instead of the aggregate 0.010. Verify the values in the 2019 Refinement before use.
- Replaces: 23% at > 10 Mg C ha⁻¹, a_bc, t_N₂O.

### 4.7 Spreading cost and emissions
- Passes per physical ha per yr = 1/n for t ≤ T.
- Split the current 17.5 L ha⁻¹ into spreading and incorporation; cost per pass c_pass = c_spread + s_inc · c_inc, with s_inc ∈ [0, 1] in the MC (incorporation may coincide with existing tillage, or biochar may be banded with fertilizer). PI to set the split from @roberts2010, @pujolpereira2016, @leppakoski2021, @cheng2025.
- Diesel emissions per Mg feedstock: Σ passes · L_pass · EF_diesel / (m · T).

### 4.8 Levelisation (economic terms)
V = CRF(r, T) · Σ_{t=1}^{H} (1 + r)⁻ᵗ · X_t / m, for X = yield value, −spreading cost (per physical ha).
This replaces the perpetuity "annual benefit / (r + k)".

Total agronomic value of PyCCS per Mg feedstock: V_ag = V_yield + V_lime,net + V_nut,net − V_spread − C_rem (C_rem common to all pathways).

## 5. Implementation notes

- Vectorise over cells × dose options × years (H = 100). Cohort count can be large when b is small (e.g. 48 cohorts at b = 0.42, D = 20); compute cohort-mean quantities analytically or cap n at H.
- For the Monte Carlo, consider precomputing per-cell results on a small grid of (p, k_y, λ) and interpolating, if a direct evaluation per draw is too slow.
- Sample the amplitude coefficients jointly (multivariate normal; §6), not independently.
- **Unit tests**: port `agro_ref.py` cases; R results must match `agro_ref_testvalues.txt` to ~0.1%. The test inputs are illustrative cells, not model outputs; c_pass = 25 US\$ ha⁻¹ is a placeholder.
- Expected direction of change vs current model: per-tonne yield value falls where biochar supply per hectare is high (much of the US Corn Belt, NE China) and rises where it is low; biochar-weighted regional effect of saturation alone is roughly 0.4–0.8 of a linear valuation (China lowest, Europe highest), before the change of amplitude model.

## 6. Parameters (add/replace in `parameters.csv`)

| Parameter | Central | MC distribution | Notes |
|---|---|---|---|
| p_CEC (dose exponent) | 0.30 | PERT(0.16, 0.30, 0.46) | pH ≥ 6.5 within-experiment fit |
| β₀, β_acid, β_CEC | 0.3607, 0.0696, −0.1008 | MVN, cov = [[0.005808, −0.001184, −0.001671], [−0.001184, 0.000831, 0.000199], [−0.001671, 0.000199, 0.000634]] | β_acid used only for reporting; pessimistic scenario: β₀ = 0.2127, β_CEC = −0.0491 (pH ≥ 6.5 only) |
| B_min | 2.5 Mg ha⁻¹ | fixed (sensitivity 1–5) | floor on power law |
| k_y | 0 yr⁻¹ | PERT(0, 0, 0.23) | upper = 3-yr half-life; **PI decision** |
| R_N₂O | 0.20 | lnRR ~ N(−0.224, 0.068) | |
| N₂O half-life | **PI decision** | e.g. PERT(1, 3, 20 yr) | data uninformative beyond 1 yr |
| D_min,N₂O | 2.2 Mg ha⁻¹ | fixed | lowest tested dose |
| f_N | **PI to source** | e.g. PERT(0, 0.2, 0.5) | long-run fertilizer replacement value of residue N |
| CI | US 1.0, Europe 1.0, China/India **to source** | uniform over FAOSTAT-based range | harvested ÷ physical cropland area |
| c_spread, c_inc, s_inc | **PI to set** | s_inc U(0, 1) | split of 17.5 L ha⁻¹ |
| US feedstock price | 37 US\$ Mg⁻¹ | existing range rescaled | excludes nutrient payment |
| Dose options | {annual, 2.5, 5, 10, 20} | — | choose per carbon price |

Remove: 21.1 US\$ Mg⁻¹ C yr⁻¹ CEC value and its linear CEC scaling; availability factors 0.1/0.5/0.8; a_bc (10 Mg C ha⁻¹) and t_N₂O; the perpetuity capitalisation.

## 7. Text changes in `BiomassTEA.qmd`

Methods:
- Rewrite "Soil N₂O emissions" and "Biochar agronomic valuation" from §§4.4–4.6; new subsection "Field application of biochar" (supply per hectare, cohorts, dose options, spreading).
- "Feedstock prices and storage": US price and the nutrient-charge sentence (§3.1).
- "Residue minerals and ash return": availability factors removed; removal charge and counterfactual (§3.2–3.3).
- SI: new text and table for the meta-analysis (datasets, cleaning, dose fits, amplitude model with grouped CV, N₂O model, persistence), and figure `dose_response_v2.png`.

Results (inconsistencies already present, fix regardless):
- "amortized over a 10-year agronomic impact duration" contradicts the Methods; rewrite after rerun.
- India paragraph attributes the result to fertilizer subsidies, but the model uses world prices and excludes subsidies; re-explain after rerun (high-ash rice straw, low biochar yield, supply per hectare).
- Southern China SHAP narrative ("model scales yield gains inversely to CEC"): revisit after rerun; the new amplitude model is logarithmic in CEC and the per-tonne value now also depends on supply per hectare.

## 8. Decisions still open (PI)

1. N₂O half-life central value and range (current Methods assume short-lived; data cannot decide).
2. k_y range (persistence of the yield effect).
3. f_N central value and source; CI values for China and India.
4. Spreading vs incorporation split of the 17.5 L ha⁻¹ and their costs.
5. Confirm 2016 data used fertilizer-matched controls.
6. Whether to use disaggregated EF₁.
