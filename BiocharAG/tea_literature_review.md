# Techno-Economic Assessment (TEA) Literature Review & Parameter Derivation

This document summarizes recent literature (2020-2024) regarding the costs, efficiencies, and operational parameters for Bioenergy Systems (BES) and Bioenergy with Carbon Capture and Storage (BECCS).

## 1. Literature Sources & Key Values (BECCS)

### A. Global CCS Institute (2023) / IEA Bioenergy
*   **Context**: Global assessment of CCS status and costs.
*   **Cost of CO2 Avoided**: Establishes a broad range of **$60 - $250 USD / tCO2**.
*   **Capital Cost**: **$3,000 - $5,000 / kW_e** (Rising inflation).

### B. Drax Power Station Reports (UK/US, 2023-2024)
*   **Context**: Real-world dedicated biomass power station retrofitting for CCS.
*   **Efficiency Penalty**: **8-11 percentage points** drop (from ~39% to ~28-30%).

### C. Zero Emissions Platform (ZEP) & NETL Cost Models
*   **Transport Mode**: **Pipelines** scale with capacity ($Cost \propto Capacity^{0.6}$).

---

## 2. Literature Sources & Key Values (BES - No CCS)

### D. IRENA Renewable Power Generation Costs (2023-2024)
*   **Capital Cost**: **~$3,000 / kW_e** (2024 projection).
*   **LCOE**: Rose to $0.072/kWh in 2023.

### E. IEA & NREL Annual Technology Baseline (2024)
*   **Efficiency**: **30%** (Electricity Only, Dedicated Biomass).
*   **O&M Costs**: **4%** of CAPEX annually.

---

## 3. Parameter Derivation for BiocharAG

### Efficiency
*   **BES (Standard)**: `bes_energy_efficiency` = **0.30 (30%)**.
*   **BECCS**: `beccs_efficiency` = **0.28 (28%)** (Optimistic modern BECCS).

### Capital Cost (CAPEX)
*   **BES (Standard)**: `bes_capital_cost` = **$3,000 / kW_e**.
*   **BECCS**: `beccs_capital_cost` = **$4,000 / kW_e**.

### O&M
*   **Factor**: `bes_om_factor` = **0.04 (4%)**.

### CO2 Transport (Geospatial)
*   **Method**: `calculate_ccs_transport` ($Cost \propto Distance \times Capacity^{0.6}$).

---

## 4. Geospatial Data Sources for CO2 Sinks (Built-in Dataset)

*   **North America**: NETL (NATCARB).
*   **Europe**: CO2StoP.
*   **Asia**: Regional Surveys / CEEW.

---

## 5. Spatial TEA Framework

To perform spatially explicit analysis, the `run_spatial_tea` function accepts specific raster layers to override default parameters. This allows for scale-dependent and location-dependent cost/benefit analysis.

### A. Spatially Varying Parameters (Input Layers)
These parameters strongly influence the viability of a project at a specific location and are expected to be provided as raster layers in future analyses.

1.  **Biomass Feedstock Density** (`biomass_density`, $Mg \cdot km^{-2}$)
    *   **Function**: Determines the **Plant Scale** and thus the **Unit Capital Cost** & **Transport Unit Cost**.
    *   **Logic**: $PlantCapacity \propto Density \times \pi \cdot r_{collection}^2$. High density $\rightarrow$ Large Plant $\rightarrow$ Economies of Scale $\rightarrow$ Lower Costs.
    *   **Source**: Future integration with *Global Forest Watch*, *FAOSTAT*, or *Billion Ton Study* (US).

2.  **Soil Temperature** (`soil_temp`, $^{\circ}C$)
    *   **Function**: Determines **Biochar Stability** ($F_{perm}$).
    *   **Logic**: Higher temperature $\rightarrow$ Faster decay $\rightarrow$ Lower long-term sequestration value.
    *   **Source**: *WorldClim* or *CRU TS*.

3.  **Project Location** (Lat/Lon)
    *   **Function**: Determines **Transport Distance** to nearest Sink.
    *   **Logic**: Derived automatically from raster cell coordinates.

4.  **Electricity Price** (`elec_price`, $\$ \cdot MWh^{-1}$)
    *   **Function**: Determines **Revenue** from electricity sales for BES, BECCS, and BEBCS.
    *   **Logic**: Prices vary significantly by region/grid.
    *   **Source**: *EIA Annual Average Retail Price by State* (2023).
    *   **Adjustment (Wholesale)**: The model applies a **Wholesale Discount Factor** (Default: 0.4) to this retail layer.
        *   *Reasoning*: Bioenergy plants act as **Generators** selling at wholesale/busbar rates. Retail prices include ~60% overhead for Transmission, Distribution (T&D), taxes, and utility profit, which the generator does not receive.
        *   *Formula*: $P_{generator} = P_{retail} \times 0.4$.

---

## 6. Agricultural Substitute Data (2024/2025)

Used for the "Advanced Valuation" mechanistic model. Prices reflect US Market averages.

### A. Liming Agents
*   **Ag Lime (Bulk)**: **~$60 USD / ton**.
    *   *Range*: $30 - $90 depending on transport.
    *   *Source*: Regional quarry price lists (2024/2025).

### B. Fertilizers
*   **Urea (46-0-0)**: **~$425 USD / ton**.
    *   Derived N Price: **$0.92 / kg N**.
*   **DAP (18-46-0)**: **~$675 USD / ton**.
    *   Derived P2O5 Price: **$1.10 / kg P2O5** (Allocating N value first).
*   **Potash (0-0-60)**: **~$375 USD / ton**.
    *   Derived K2O Price: **$0.62 / kg K2O**.

### C. Biochar Ag Properties (Defaults)
*   **Liming Equivalence (CCE)**: **15% (0.15)**.
    *   *Context*: Typical for woody biochar pyrolyzed >500°C. Range 5-20%[1].
*   **Nutrient Content**: Conservative generic defaults.
    *   N: 0.5%, P: 0.2%, K: 0.5%.


### B. Constant Parameters (Non-Spatial)
These parameters are assumed to be dependent on the technology selected rather than the location, or spatial data is not yet granular enough to be useful.

1.  **Pyrolysis / Reaction Temperature** (`py_temp`, default $500^{\circ}C$): Operational choice.
2.  **Conversion Efficiency** (`bes_energy_efficiency`, default $0.30$): Technology constant.
3.  **Capital Cost Basis** (`bes_capital_cost`): Base technology cost ($/kW) is constant, though realized unit cost ($/Mg) varies with scale (spatial).
4.  **Carbon Price** (`c_price`): Market assumption (global/regional).
