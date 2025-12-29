# BECCS Cost & Efficiency: Literature Review and Parameter Derivation

This document summarizes recent literature (2020-2024) regarding the Techno-Economic Assessment (TEA) of Bioenergy with Carbon Capture and Storage (BECCS). These findings form the basis for the modernized parameters in the `BiocharAG` package.

## 1. Literature Sources & Key Values

### A. Global CCS Institute (2023) / IEA Bioenergy
*   **Context**: Global assessment of CCS status and costs.
*   **Cost of CO2 Avoided**: Establishes a broad range of **$60 - $250 USD / tCO2**.
    *   Lower end ($60-100) represents high-purity sources (e.g., ethanol fermentation).
    *   Higher end ($150-250) represents dilute streams like power generation flue gas.
*   **Capital Cost**: Trends show significant inflation in recent years (post-COVID supply chain).
    *   Estimates for new-build power BECCS have risen to **$3,000 - $5,000 / kW_e** depending on scale and location (NOAK - Nth of A Kind).

### B. Drax Power Station Reports (UK/US, 2023-2024)
*   **Context**: Real-world dedicated biomass power station retrofitting for CCS.
*   **Efficiency Penalty**:
    *   Capture units consume significant steam and electricity (parasitic load).
    *   Reported efficiency drop is approximately **8-11 percentage points**.
    *   *Baseline Biomass Efficiency*: ~38-40%.
    *   *BECCS Efficiency*: **27-30%**.
*   **Capture Rate**: Targeted capture rates are effectively standard at **90% or 95%**.

### C. Zero Emissions Platform (ZEP) & NETL Cost Models
*   **Context**: Engineering cost models for CO2 Transport.
*   **Transport Mode**:
    *   **Pipelines**: Strong economies of scale. Scale factor $\approx 0.6$.
        *   Costs drop below $10/t for large volumes (>2 Mt/y) over medium distances.
        *   Costs rise significantly for small volumes (Biochar/Small BECCS scale < 0.1 Mt/y).
    *   **Trucking**: Viable only for very small scales/short distances.

---

## 2. Parameter Derivation for BiocharAG

Based on the above sources, the following default values were derived for `R/parameters.R` and `R/beccs.R`.

### Efficiency
*   **Parameter**: `beccs_efficiency`
*   **Value**: **0.28 (28%)**
*   **Derivation**:
    *   Started with `bes_efficiency` (Standard Biomass Power) = 0.39 (39%) [Woolf et al. 2016 original].
    *   Applied a conservative **11% penalty** for solvent regeneration heat and compression energy (based on Drax/IEA data for amine capture).
    *   $0.39 - 0.11 = 0.28$.

### Capital Cost (CAPEX)
*   **Parameter**: `beccs_capital_cost`
*   **Value**: **$4,000 / kW_e**
*   **Derivation**:
    *   Historic values (2016) were often optimistic (~$2,000/kW).
    *   Adjusted for 2024 inflation and recent project FID (Final Investment Decision) estimates.
    *   Selected $4,000/kW as a representative midpoint for Nth-of-a-kind plants in developed markets.

### Capture Rate
*   **Parameter**: `capture_rate`
*   **Value**: **0.90 (90%)**
*   **Derivation**: Standard industry guarantee for amine-based post-combustion capture systems.

### CO2 Transport
*   **Method**: `calculate_ccs_transport`
*   **Model**: $Cost \propto Distance \times Capacity^{0.6}$
*   **Derivation**:
    *   Standard engineering "six-tenths rule" for pipeline diameter scaling (NETL/ZEP).
    *   Ensures that small biomass plants (typical of local distributed systems) face realistically high per-ton transport costs ($20-50/t), while large centralized hubs face low costs ($5-10/t).

### Storage Cost
*   **Parameter**: `ccs_storage_cost`
*   **Value**: **$15 / Mg CO2**
*   **Derivation**:
    *   Represents monitoring and injection costs into saline aquifers or depleted oil/gas fields.
    *   Literature range: $8 - $20 / tCO2. $15 selected as conservative central estimate.

---

## 3. Geospatial Data Sources for CO2 Sinks

To facilitate explicit transport distance calculations, `BiocharAG` includes a built-in dataset (`co2_sinks`) representing the centroids of major global geological storage basins.

### A. North America
*   **Basins Included**: Illinois, Permian, Gulf Coast, Williston, Alberta.
*   **Primary Source**: **NETL (National Energy Technology Laboratory) - NATCARB**.
    *   The *National Carbon Sequestration Database* provides shapefiles for assessed saline and coal storage basins across the US and Canada.
    *   **Processing**: Centroids were approximated from the major storage formations identified in the **2015 Carbon Storage Atlas (Atlas V)**.

### B. Europe
*   **Basins Included**: North Sea (Sleipner/Aurora), Rotterdam (Porthos), Adriatic.
*   **Primary Source**: **CO2StoP (CO2 Storage Potential in Europe)** & **EU Projects**.
    *   Data derived from the EU JRC's CO2StoP database and active project documentation (e.g., Northern Lights, Porthos).
    *   **Processing**: Locations represent key offshore storage hubs currently under development or operation.

### C. Asia (China/India)
*   **Basins Included**: Ordos, Songliao (China); Cambay, Bombay High (India).
*   **Primary Sources**:
    *   *China*: **Regional Geological Surveys** & PNNL Assessment (2009). The Ordos and Songliao basins are widely cited as having the highest onshore potential.
    *   *India*: **CEEW (Council on Energy, Environment and Water)** & **IEA GHG**. Cambay and Bombay High are identified as primary sinks due to proximity to industrial clusters.
    *   **Processing**: Coordinates correspond to the optimal injection zones identified in regional storage capacity maps.
