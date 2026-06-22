# Parameter Review & Validation Audit

This document outlines the findings from an audit of the constants defined in `BiocharAG/R/parameters.R`. The goal is to identify parameters that currently use a global constant but should ideally vary spatially, differ by region, or require stronger empirical validation to ensure robust results.

## 1. Parameters Requiring Spatial Variation

The following parameters are currently defined as global constants but are inherently spatial in nature. While some (like `elec_price`) have spatial overrides in `spatial_tea.R`, others do not.

| Parameter | Current Default | Recommendation |
| :--- | :--- | :--- |
| `ccs_storage_cost` | `$15 / Mg CO2` | **High Priority**. Geologic storage costs vary drastically based on basin depth, onshore vs. offshore location, and whether the sink is a saline aquifer or Depleted Oil/Gas (EOR) site. We should consider replacing this with a spatial cost raster or a look-up table mapped to the `co2_sinks` database. |
| `ag_impact_duration`| `10 Years` | **Medium Priority**. The duration of biochar's agronomic benefit (liming and nutrient release) depends heavily on local climate (weathering and leaching rates) and soil buffering capacity. A flat 10-year assumption across all global soils is likely poorly validated. |
| `bm_lhv` & `bm_ash` | `18.6 GJ/Mg` / `0.05` | **Medium Priority**. Biomass properties vary significantly depending on the dominant regional feedstock (e.g., high-ash rice straw in India/China vs. forestry residues vs. corn stover in the US). If we know the dominant feedstock per region, we should map these properties accordingly. |

## 2. Parameters Requiring Regional/Country Overrides

Similar to the logic applied in `parameters_india.R`, several financial and market parameters should not be global constants due to macro-economic differences between the US, Europe, China, and India.

| Parameter | Current Default | Recommendation |
| :--- | :--- | :--- |
| `discount_rate` | `0.08` (8%) | **High Priority**. Cost of capital varies wildly by country risk profiles. Developing markets (India, China) typically face higher discount rates than established western markets. We should establish explicit regional discount rates. |
| `price_n`, `price_p`, `price_k` | `$0.92`, `$1.10`, `$0.62 / kg` | **High Priority**. Fertilizer prices are subject to massive regional variations due to government subsidies (e.g., heavy urea subsidies in India), import tariffs, and local supply chains. |
| `price_lime` | `$60 / Mg` | **Low Priority**. Bulk ag lime prices vary by local transport distance since it is cheap but heavy. However, a regional average may suffice if data is sparse. |
| `bes_capital_cost` / `beccs_capital_cost` | `$3000` / `$4000 / kW` | **Medium Priority**. Capital construction costs differ significantly due to local labor and material costs. We currently have a 0.7x factor for India, but China and Europe should also have their own specific construction cost modifiers. |
| `bes_om_factor` / `beccs_om_factor` | `0.04` / `0.05` | **Medium Priority**. O&M is highly labor-dependent. Regional modifiers should be applied. |

## 3. Parameters Requiring Stronger Empirical Validation

The following assumptions and technical constants may need to be double-checked against recent literature or flagged for sensitivity analysis.

- **`bc_stab_factor = 4.6`**: This stability factor calculation dictates the permanence of biochar. Ensure this aligns precisely with the latest IPCC or verified carbon standard methodologies for biochar permanence.
- **`bc_cce = 0.15` (15% Calcium Carbonate Equivalent)**: This is a critical driver for the agronomic value in acidic soils. Does a 15% CCE hold true across the diverse feedstocks represented globally, or should this scale with the `bm_ash` parameter?
- **`capture_rate = 0.90` (90%)**: While 90% is the standard target for BECCS, real-world deployment data is sparse. A sensitivity analysis around this (e.g., 85% to 95%) might be necessary.
- **`n2o_factor = 0.01` (1%)**: The 1% default emission factor for nitrogen application is standard (IPCC Tier 1), but biochar is known to interact with N2O emissions. Ensure the model accounts for any biochar-induced N2O suppression if applicable.

---

### Suggested Next Actions

1. **Review and Advise**: Please review the above lists. Let me know which of the **High Priority** items you would like to actively tackle (e.g., creating region-specific configuration files like `parameters_china.R` and `parameters_europe.R`, or converting `ccs_storage_cost` to a spatial layer).
2. **Sensitivity Variables**: If any poorly validated parameters lack solid empirical data, we can flag them to be included in a Monte Carlo or sensitivity analysis step later.
