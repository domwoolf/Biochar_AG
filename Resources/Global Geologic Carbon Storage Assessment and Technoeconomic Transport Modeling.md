# **Global Geologic Carbon Storage Assessment and Technoeconomic Transport Modeling: A Comprehensive Analysis of Sink Proximity and Logistics**

## **1\. Executive Summary**

The global transition toward a net-zero carbon economy is increasingly predicated on the deployment of Carbon Capture and Storage (CCS) technologies. While capture technologies have matured, the deployment of CCS is fundamentally constrained by the geological availability of storage sinks and the technoeconomic feasibility of transporting captured carbon dioxide (\$CO\_2\$) to these locations. This report provides a definitive, high-resolution analysis of the global \$CO\_2\$ storage landscape, focusing specifically on the spatial distribution of sinks in the United States, Europe, China, and India. Furthermore, it establishes a rigorous technoeconomic framework for modeling \$CO\_2\$ transport infrastructure, providing the necessary algorithms and cost functions to evaluate project viability based on source capacity and distance.

The analysis reveals a global storage resource that is vast in theoretical capacity—measured in trillions of tonnes—but highly heterogeneous in commercial readiness and geographic accessibility. The "tyranny of distance" emerges as the primary economic barrier, necessitating a shift from simple point-to-point transport models to complex, multi-modal hub-and-cluster networks.

In **North America**, the maturity of the *National Carbon Sequestration Database (NATCARB)* and the extensive existing pipeline infrastructure in the Permian Basin and Gulf Coast create a "super-basin" advantage. The United States possesses the highest density of characterized storage sites, allowing for low-cost (\$\<\$ \$10/tonne) pipeline transport for a significant portion of its industrial emissions. The regulatory framework, particularly the Class VI well permitting process, provides a clear pathway for converting geological potential into booked storage capacity.

**Europe** faces a fundamentally different strategic landscape. Social and political constraints on onshore storage have driven development offshore into the North Sea, which holds the majority of the continent's storage potential (estimated at over 70 Gt in the Norwegian sector alone). This geographic reality forces a reliance on high-cost shipping and subsea pipelines, establishing a cost floor for transport significantly higher than in the US. The *CO2StoP* database and *Nordic CO2 Storage Atlas* highlight the necessity of cross-border transport corridors to connect inland industrial centers in Germany and Central Europe to these offshore sinks.

**China**, possessing an estimated 2,300 Gt of onshore capacity, faces a severe "source-sink mismatch." The majority of industrial emissions are concentrated along the eastern and southern coasts, while the highest-quality geological sinks—the Ordos and Junggar Basins—are located in the north and northwest, separated by thousands of kilometers of difficult terrain. This necessitates a strategic choice between constructing massive "West-to-East" style \$CO\_2\$ pipeline corridors or developing offshore storage solutions in the Bohai Bay and Pearl River Mouth basins.

**India** represents the emerging frontier of CCS. Data availability is currently limited to hydrocarbon-bearing basins, forcing early movers to focus on "low-hanging fruit" in Category I basins like Cambay and Assam-Arakan. While the theoretical potential of the Deccan Traps basalts is immense, it remains technologically unproven. Consequently, India's near-term CCS roadmap is tethered to Enhanced Oil Recovery (EOR) and pilot projects in depleted fields, requiring substantial pre-investment in geological characterization to unlock deep saline aquifer potential.

Technoeconomically, this report establishes that \$CO\_2\$ transport costs follow distinct scaling laws derived from fluid dynamics and infrastructure economics. Pipeline transport exhibits strong economies of scale (\$\\beta \\approx 0.5 \- 0.6\$), where doubling capacity increases capital cost by only \~50%, making high-volume trunk lines the optimal solution for inland transport distances up to 1,000 km. Beyond this range, or for fragmented offshore sources, liquefaction and shipping become the cost-optimal modality. We provide detailed cost functions, derived from NETL and ZEP methodologies, that allow for the calculation of Levelized Cost of Transport (LCO2T) incorporating terrain factors, compression energy penalties, and diameter optimization.

## ---

**2\. Introduction: The Spatial Dimension of Decarbonization**

The challenge of Carbon Capture and Storage (CCS) is often framed as a capture problem—how to separate \$CO\_2\$ from flue gas efficiently. However, as the industry moves from pilot projects to gigatonne-scale deployment, the focus shifts to the *midstream* and *downstream* segments: transport and storage. The fundamental constraint on CCS deployment is spatial. Unlike renewable energy, which depends on surface fluxes (solar, wind), CCS depends on subsurface pore space. This geological resource is immutable; factories can be moved, but sedimentary basins cannot.

### **2.1 The Tyranny of Distance**

The "distance to sink" is the critical variable in the CCS cost equation. A cement plant located directly above a suitable saline aquifer faces a fundamentally different economic reality than one located 500 km away on crystalline basement rock. This spatial disparity creates "CCS deserts" and "CCS oases."

* **CCS Oases:** Regions like the US Gulf Coast or the North Sea Basin, where high emission density coincides with high storage capacity.  
* **CCS Deserts:** Regions like the granitic shields of Southern India or parts of central Europe, where emissions are high but local geology is impermeable, necessitating long-distance transport.

### **2.2 Source-Sink Matching as a Strategic Discipline**

Developing a map of distance to sinks requires a rigorous methodology for "Source-Sink Matching" (SSM). This involves not just measuring Euclidean distance, but analyzing the logistical feasibility of connecting a specific source (with a specific volume and stream composition) to a specific sink (with a specific injectivity and capacity).  
The analysis in this report facilitates SSM by providing:

1. **Geolocation of Sinks:** Precise centroids and boundary definitions for major basins in the target regions.  
2. **Capacity Characterization:** Distinguishing between theoretical capacity (total pore volume) and effective capacity (technically accessible volume).  
3. **Transport Cost Algorithms:** Mathematical functions to estimate the CAPEX and OPEX of the connecting infrastructure.

### **2.3 The Storage Resource Management System (SRMS)**

To ensure consistency, this report utilizes the *Storage Resource Management System (SRMS)* classification framework.1 Just as the petroleum industry uses PRMS to classify oil reserves, the CCS industry uses SRMS to classify storage resources based on commercial maturity:

* **Stored:** \$CO\_2\$ that has already been injected.  
* **Capacity:** Resources associated with a project that has a FID (Final Investment Decision) or is in operation.  
* **Discovered Resources:** Characterized formations where a potential storage site has been identified through well data and seismic analysis, but is not yet commercial.  
* **Undiscovered Resources:** Prospective resources estimated based on regional geological knowledge (basin area, average porosity) but not yet verified by drilling.

This report focuses primarily on **Discovered** and **Undiscovered** resources, as these represent the potential targets for future project development.

## ---

**3\. Geological Storage Resource Assessment: Methodologies & Global Overview**

Before delving into regional specifics, it is essential to understand the methodologies used to generate the data that underpins our maps. The reliability of "distance to sink" calculations depends entirely on the fidelity of the underlying geological data.

### **3.1 Volumetric vs. Dynamic Assessment**

Early assessments of global storage capacity were largely **volumetric**. They calculated the total pore volume of a sedimentary basin and applied a simple efficiency factor (typically 1-4%) to estimate storage capacity.

* **Formula:** \$M\_{CO2} \= A \\times h \\times \\phi \\times \\rho\_{CO2} \\times E\$  
  * \$A\$: Basin Area  
  * \$h\$: Thickness of formation  
  * \$\\phi\$: Porosity  
  * \$\\rho\_{CO2}\$: Density of \$CO\_2\$ at reservoir conditions  
  * \$E\$: Efficiency factor

While useful for high-level screening, these estimates often overstate capacity by ignoring dynamic constraints such as pressure buildup, fracture gradients, and injection rate limits. Modern assessments, such as the USGS National Assessment and the CO2StoP project 2, incorporate these dynamic factors to produce estimates of **Technically Accessible Storage Resources (TASR)**.

### **3.2 Key Global Data Repositories for Mapping**

To construct the requested map, the user must integrate data from several disparate but authoritative sources. We have identified the following as the primary datasets:

1. **OGCI CO2 Storage Resource Catalogue:**  
   * *Description:* The first global, independent assessment of storage resources, harmonized under the SRMS framework. It aggregates data from national surveys and independent studies.1  
   * *Access:* Available via the Global CCS Institute and OGCI web portals. It provides basin-level and site-level data.  
   * *Relevance:* Serves as the "master list" for checking data consistency across regions.  
2. **USGS World Geologic Provinces (2000):**  
   * *Description:* A foundational shapefile dataset defining the boundaries of geologic provinces worldwide.4  
   * *Access:* Publicly available via USGS EarthExplorer and The National Map.  
   * *Relevance:* Provides the base layer polygons for mapping basins in regions where local data is scarce (e.g., parts of India and China).  
3. **Global CCS Institute CO2RE Database:**  
   * *Description:* A facility-level database tracking CCS projects and storage hubs.5  
   * *Relevance:* Essential for identifying "Capacity" (commercial projects) versus "Resources" (geological potential).

### **3.3 The Onshore-Offshore Dichotomy**

A critical insight from the global assessment is the stark divergence in storage strategy between regions.

* **Onshore Dominance:** The USA, Canada, China, and Russia possess vast onshore sedimentary basins in sparsely populated or industrial regions. This allows for lower-cost development (\$2–\$15/tonne storage cost).6  
* **Offshore Dominance:** Europe, Japan, and Southeast Asia (including parts of India) are constrained by population density and geology, pushing development offshore (\$15–\$60/tonne storage cost). This fundamentally alters the "distance" calculation, as offshore miles are significantly more expensive than onshore miles due to the need for specialized pipelines or shipping.

## ---

**4\. Regional Analysis: North America (The Benchmark)**

North America, particularly the United States, represents the global benchmark for CCS readiness. The convergence of geological fortune, industrial history, and policy support has created the most mature dataset for mapping purposes.

### **4.1 Data Source: The NATCARB Atlas**

The **National Carbon Sequestration Database (NATCARB)** is a geographic information system (GIS) developed by the U.S. Department of Energy (DOE) and NETL. It integrates data from the seven Regional Carbon Sequestration Partnerships (RCSPs).7

* **Granularity:** Unlike other global datasets, NATCARB provides data at a 10 km x 10 km grid resolution, detailing depth, thickness, porosity, and permeability for specific formations.8  
* **Access:** The data is accessible via the Energy Data eXchange (EDX) and can be downloaded as shapefiles ("NATCARB\_Saline\_Poly\_v1502").9

### **4.2 Key Storage Basins and Characteristics**

The USGS Circular 1386 assesses the technically accessible storage resource (TASR) for US onshore and state waters at approximately 3,000 Gt. This capacity is concentrated in a few key "super-basins."

#### **4.2.1 The Gulf Coast Basin (Coastal Plains Region)**

This is the premier global CCS hub.

* **Resource Magnitude:** The USGS estimates this region holds **59%** of the total US national storage capacity.10  
* **Geology:** A massive wedge of Cenozoic siliciclastic sediments (sandstones and shales) dipping into the Gulf of Mexico. The high permeability (often \>1 Darcy) allows for exceptional injection rates, reducing the number of wells required.  
* **Logistics:** The region is home to the world's largest concentration of refineries and petrochemical plants (Houston Ship Channel, Louisiana Chemical Corridor). "Distance to sink" is effectively zero for many facilities, as they sit directly atop suitable storage formations (e.g., the Frio Formation, Miocene sands).  
* **Centroid for Mapping:** Approx. **29.5° N, 95.0° W** (centered near Houston/Galveston).

#### **4.2.2 The Permian Basin**

Located in West Texas and Southeastern New Mexico, this basin is the heart of the global \$CO\_2\$-EOR industry.

* **Infrastructure:** It hosts the majority of the \~8,000 km of existing \$CO\_2\$ pipelines in the US. These pipelines currently transport natural \$CO\_2\$ from domes in Colorado and New Mexico (McElmo, Bravo) to oil fields.11  
* **Strategic Value:** This existing network lowers the barrier to entry for CCS. New capture projects can simply tie into the existing "Cortez" or "Central Basin" pipelines.  
* **Centroid for Mapping:** Approx. **31.5° N, 103.5° W**.

#### **4.2.3 The Illinois Basin**

* **Geology:** A stable cratonic basin containing the **Mt. Simon Sandstone**, a Cambrian-age formation with high porosity and permeability.  
* **Proven Sink:** This basin hosted the **Decatur ADM** project, which successfully injected over 1 million tonnes of \$CO\_2\$ directly from an ethanol plant into the Mt. Simon formation.  
* **Strategic Value:** Critical sink for Midwest emissions (coal power, ethanol, steel).  
* **Centroid for Mapping:** Approx. **39.8° N, 89.0° W** (near Decatur, IL).

#### **4.2.4 The Williston Basin**

* **Location:** North Dakota and Montana, extending into Canada (Saskatchewan).  
* **Project:** Home to the **Weyburn-Midale** project, one of the most studied \$CO\_2\$-EOR and storage sites in the world.  
* **Geology:** Paleozoic carbonates (Madison Group) and deep saline aquifers.

## ---

**5\. Regional Analysis: Europe (The Offshore Network)**

Europe’s CCS landscape is defined by the "North Sea Pivot." Early onshore projects (e.g., in Germany and Poland) faced intense public opposition, effectively closing the onshore storage window for the near future. Europe is now building a "collect and transport" network that funnels emissions to offshore sinks.

### **5.1 Data Source: CO2StoP and the Nordic Atlas**

* **CO2StoP (CO2 Storage Potential in Europe):** This project, funded by the European Commission, harmonized storage data from 27 countries.2 It provides the most comprehensive database of European storage units.  
  * *Access:* While the full ArcGIS tool was historically restricted, the Joint Research Centre (JRC) has released derived datasets (KML, CSV) on its data portal.12  
* **Nordic CO2 Storage Atlas:** Produced by NORDICCS, this atlas provides high-quality data for the North Sea, covering the Norwegian and UK sectors.2

### **5.2 The North Sea: Europe’s Storage Engine**

The North Sea basin contains the vast majority of Europe's storage capacity, estimated at over 100 Gt.

#### **5.2.1 Norwegian Continental Shelf (NCS)**

Norway is the undisputed leader in European storage.

* **Capacity:** The NPD estimates \~70 Gt of capacity in the Norwegian North Sea alone.14  
* **Key Formations:**  
  * **Utsira Formation:** The site of the **Sleipner** project, injecting \~1 Mtpa since 1996\. It is a massive, high-permeability saline aquifer.  
  * **Johansen Formation:** The target for the **Northern Lights** project (part of the Longship initiative). This is a commercial storage hub designed to accept third-party \$CO\_2\$ via ship.15  
* **Mapping Coordinates:**  
  * Sleipner: **58.4° N, 1.9° E**.  
  * Northern Lights (Aurora): **60.5° N, 3.5° E**.

#### **5.2.2 UK and Netherlands Sectors**

* **UK:** Significant capacity in depleted gas fields in the Southern North Sea (e.g., Goldeneye, Viking) and saline aquifers in the Central North Sea (Endurance).  
  * *Data Source:* The **CO2Stored** database is the UK's equivalent of NATCARB.16  
* **Netherlands:** developing the **Porthos** project in depleted gas fields off the coast of Rotterdam (P18 field).  
  * *Mapping Centroid (Porthos):* Approx. **52.0° N, 3.5° E**.

### **5.3 Transport Implications: The Multi-Modal Necessity**

The concentration of sinks in the North Sea creates a unique logistical challenge for inland Europe.

* **The "Rhine Corridor":** Emitters in the German Ruhr valley or along the Rhine must transport \$CO\_2\$ by barge or rail to export terminals (e.g., Rotterdam, Antwerp, Wilhelmshaven) for liquefaction and shipping to the North Sea.  
* **Cost Impact:** This multi-modal transfer (Barge \$\\rightarrow\$ Terminal \$\\rightarrow\$ Ship \$\\rightarrow\$ Injection) adds significant fixed costs (terminal fees, liquefaction), making European transport costs structurally higher than US pipeline costs.17

## ---

**6\. Regional Analysis: China (The Continental Challenge)**

China faces a "West-East" dilemma similar to its energy supply: its largest storage resources are inland (North/West), while its emissions are coastal (East/South).

### **6.1 Resource Distribution**

Studies by the Chinese Academy of Sciences and international collaborators estimate China's theoretical storage capacity at \~2,300 Gt onshore and \~780 Gt offshore.18 However, the *effective* capacity is constrained by the difficulty of transport.

### **6.2 Priority Basins for Mapping**

#### **6.2.1 Ordos Basin (The Strategic Hub)**

The Ordos Basin is the most critical onshore sink for China.

* **Location:** Spanning Inner Mongolia, Shaanxi, Gansu, and Ningxia.  
* **Project:** It hosts the **Shenhua Coal-to-Liquid CCS** project, China's first full-chain demonstration.19  
* **Geology:** A stable cratonic basin with low tectonic activity and multiple stacked reservoir-seal pairs (Triassic/Jurassic sandstones). It is ideal for large-scale saline storage.  
* **Coordinates:** Centroid approx. **39.33° N, 110.15° E**.20  
* **Source Cluster:** Located near major coal-chemical bases (Yulin, Erdos), allowing for short-distance source-sink matching.

#### **6.2.2 Junggar Basin**

* **Location:** Xinjiang (Northwest China).  
* **Potential:** A major petroliferous basin with high potential for \$CO\_2\$-EOR in heavy oil fields (Xinjiang Oilfield).  
* **Coordinates:** Centroid approx. **45.0° N, 86.0° E**.21  
* **Constraint:** Extremely remote from eastern industrial centers (\>2,500 km), limiting its utility to local emission sources (e.g., coal power in Xinjiang).

#### **6.2.3 Bohai Bay Basin**

* **Strategic Importance:** Located adjacent to the Beijing-Tianjin-Hebei (Jing-Jin-Ji) economic zone.  
* **Potential:** Offshore storage in the Bohai Sea is critical for decarbonizing this coastal megalopolis, as piping \$CO\_2\$ inland to Ordos is costly.  
* **Coordinates:** Centroid approx. **38.5° N, 119.5° E**.

#### **6.2.4 Pearl River Mouth Basin**

* **Location:** Offshore Guangdong/Hong Kong.  
* **Role:** The primary sink for the Greater Bay Area. The **Enping 15-1** oilfield CCS project is a key reference point here.

### **6.3 Logistics and Matching**

Research indicates that while 54% of China's large stationary sources have a candidate storage formation in the "immediate vicinity," the remaining sources—particularly in the Shanghai/Yangtze Delta region—face a lack of high-quality nearby sinks, necessitating either long-distance pipelines to the Subei Basin or offshore transport.18

## ---

**7\. Regional Analysis: India (The Emerging Frontier)**

India’s CCS assessment is in a developmental stage. Unlike the US or Europe, India lacks a comprehensive national atlas of deep saline aquifers. The Directorate General of Hydrocarbons (DGH) data focuses on hydrocarbon potential, leading to a reliance on "Category I" basins for early CCS planning.

### **7.1 Basin Classification and Strategy**

The DGH categorizes India's 26 sedimentary basins based on exploration maturity 22:

* **Category I:** Proven producing basins (most data available).  
* **Category II:** Contingent resources.  
* **Category III:** Prospective/Unexplored.

For CCS mapping, **Category I** basins are the only viable targets for near-term projects due to the availability of well data and reservoir models.

### **7.2 Priority Sinks**

#### **7.2.1 Cambay Basin (Gujarat)**

This is India's most viable early-mover basin.

* **Location:** Onshore and shallow offshore in the state of Gujarat.  
* **Coordinates:** Approx. **21.0°N – 25.0°N, 71.5°E – 73.5°E**.23  
* **Project:** ONGC is developing a CCS pilot at the **Gandhar** oil field.24  
* **Capacity:** Recent studies estimate \>650 Mt of storage potential in hydrocarbon fields alone.25  
* **Industrial Match:** The basin underlies a massive industrial corridor (Dahej, Hazira, Vadodara) containing refineries, fertilizer plants, and power stations. The "distance to sink" is minimal (\< 50 km).

#### **7.2.2 Assam-Arakan Basin (Northeast)**

* **Location:** Assam and Arunachal Pradesh.  
* **Coordinates:** Centroid approx. **27.5° N, 95.5° E** (Upper Assam Shelf).  
* **Potential:** Mature oil fields (Digboi, Naharkatiya) offer immediate EOR opportunities.26  
* **Constraint:** The region is geographically isolated from mainland India's industrial centers. CCS here will likely be limited to local sources (e.g., Digboi refinery, local power plants).

#### **7.2.3 Krishna-Godavari (KG) & Cauvery Basins**

* **Location:** East Coast (Andhra Pradesh, Tamil Nadu).  
* **Potential:** These basins are critical for decarbonizing the industrial belts of Visakhapatnam and Chennai.  
* **Status:** While they are Category I hydrocarbon basins, specific CCS characterization is less advanced than in Cambay.

### **7.3 The Basalt Opportunity (Deccan Traps)**

India possesses one of the world's largest flood basalt provinces, the **Deccan Traps**, covering \>500,000 km² in Western India.

* **Mechanism:** Mineralization of \$CO\_2\$ into solid carbonates (calcite, magnesite).  
* **Status:** Currently **Undiscovered/Prospective**. While the theoretical capacity is immense (hundreds of Gt), the technology (rapid mineralization) consumes vast amounts of water (\~25 tonnes of water per tonne of \$CO\_2\$) and requires specific basalt permeability structures that are not yet mapped for storage.  
* *Recommendation:* For a commercial map, the Deccan Traps should be marked as "Future Potential" rather than "Available Sink."

## ---

**8\. Technoeconomic Analysis: CO2 Transport Cost Functions**

To support a technoeconomic study, we must derive cost functions that relate the physical parameters of transport (mass flow, distance, phase) to economic indicators (CAPEX, OPEX).

### **8.1 CO2 Transport Physics**

\$CO\_2\$ is distinct from natural gas transport. It is most efficiently transported in the **dense phase** (supercritical or liquid) to maximize density and minimize pipe diameter.

* **Critical Point:** 7.38 MPa (73.8 bar) and 31.1°C.  
* **Pipeline Operating Window:** Typically **8.5 MPa – 15 MPa** to ensure the fluid remains in the dense phase despite pressure drops, avoiding two-phase flow (gas/liquid slugs) which can damage pumps.27  
* **Impurities:** The presence of impurities (N2, H2, O2) expands the two-phase envelope, requiring higher operating pressures (e.g., \>10 MPa) to maintain single-phase flow, thus increasing compression costs.28

### **8.2 Pipeline Transport Cost Model**

Pipeline transport follows power-law scaling. The cost is driven by the diameter, which is a function of the required mass flow rate.

#### **8.2.1 Hydraulic Design: Determining Diameter**

For a technoeconomic model, the internal diameter (\$D\_{in}\$) must first be calculated based on the target capacity (\$Q\$).  
Using the continuity equation and assuming a standard economic velocity (\$v\$) for dense-phase \$CO\_2\$:

\$\$D\_{in} (m) \= \\sqrt{\\frac{4 \\cdot \\dot{m}}{\\pi \\cdot \\rho \\cdot v}}\$\$

* \$\\dot{m}\$ \= Mass flow rate (kg/s). (1 Mtpa \$\\approx\$ 31.7 kg/s).  
* \$\\rho\$ \= Density (\$\\approx\$ 800–900 kg/m³ for dense phase).  
* \$v\$ \= Economic velocity (\$\\approx\$ 1.5 – 3.0 m/s).29

#### **8.2.2 Capital Cost (CAPEX) Function**

Pipeline CAPEX is best modeled using a "Diameter-Length" regression. Based on NETL and ZEP data, refined for 2024 pricing:

\$\$CAPEX\_{pipe} (\\\$) \= F\_{terrain} \\times L \\times (\\alpha \\times D\_{in}^{\\beta} \+ \\gamma)\$\$  
A simplified, robust regression derived from recent literature 11 for **Onshore Pipelines** is:

\$\$I\_{pipe} (\\text{EUR}) \\approx (2157 \\times D\_{m} \+ 18\) \\times L\_{m}\$\$  
*Where:*

* \$I\_{pipe}\$: Investment cost in Euros.  
* \$D\_m\$: Diameter in meters.  
* \$L\_m\$: Length in meters.  
* *Note:* The snippet 11 explicitly provides this linear approximation for diameter-based costing (\$Y \= 2.1575X \+ 0.018\$).

For **Offshore Pipelines**, a multiplier of **1.4 – 1.7** should be applied to the onshore cost to account for marine installation vessels and risers.11

#### **8.2.3 Operating Cost (OPEX) Function**

OPEX consists of fixed maintenance and variable energy costs.

* **Fixed OPEX:** 1.5% – 3% of CAPEX annually.30  
* **Variable OPEX (Compression Energy):**  
  * **Initial Compression (Gas \$\\rightarrow\$ Dense Phase):** Requires \~80–100 kWh/tonne \$CO\_2\$ (compressing from \~1 bar to \~110 bar).  
  * **Booster Pumping:** Requires \~5–10 kWh/tonne per 100 km to overcome friction loss.11

### **8.3 Shipping Transport Cost Model**

Shipping breaks the linear cost-distance relationship of pipelines. It is a step-function cost model: high fixed costs (liquefaction) but very low variable costs (\$/km).

Equation: Total Shipping Cost (\$C\_{total}\$)

\$\$C\_{total} (\\\$/t) \= C\_{liq} \+ C\_{term} \+ C\_{voyage}\$\$

1. **Liquefaction (\$C\_{liq}\$):**  
   * The cost to cool \$CO\_2\$ to \-26°C (Medium Pressure) or \-50°C (Low Pressure).  
   * Cost Estimate: **\$15 – \$25 / tonne**.31  
2. **Terminal Handling (\$C\_{term}\$):**  
   * Includes buffer storage, loading arms, and port fees.  
   * Cost Estimate: **\$10 – \$20 / tonne**.17  
3. **Voyage Cost (\$C\_{voyage}\$):**  
   * A function of distance (\$L\$) and ship size (\$V\$).  
   * Rate: **\$0.02 – \$0.05 / tonne / km**.32

Break-even Analysis:  
The ZEP report identifies the economic break-even point between offshore pipelines and shipping at approximately 700 km for large volumes (6 Mtpa) and 1,000 km for smaller volumes. For distances greater than this, shipping is cheaper.33

### **8.4 Truck and Rail Transport**

For small-scale or dispersed sources (e.g., bio-ethanol plants, pilot projects).

* **Truck:**  
  * Cost: **\$0.10 – \$0.15 / tonne / km**.  
  * Viability: Only for distances \< 300 km and volumes \< 0.5 Mtpa.34  
* **Rail:**  
  * Cost: **\$0.05 – \$0.08 / tonne / km**.  
  * Viability: Competitive for distances \> 400 km and volumes \< 1-2 Mtpa.35

## ---

**9\. Conclusion and Strategic Synthesis**

The global map of CCS potential is characterized by a "Haves and Have-Nots" dynamic that is geological in origin but economic in consequence.

1. **United States:** The "Have" region. The coincidence of high-quality onshore sinks (Gulf Coast, Permian) and industrial sources allows for the development of low-cost (\$\<\$ \$10/tonne transport) pipeline networks. The US is positioned to deploy CCS faster and cheaper than any other region.  
2. **Europe:** The "High-Cost" pioneer. By forcing storage offshore to the North Sea, Europe has accepted a higher cost structure (\$25–\$50/tonne transport). This necessitates the rapid development of shipping terminals and cross-border "CO2 Highways" to aggregate volumes and achieve economies of scale.  
3. **China:** The "Logistics" challenge. The separation of coastal sources from inland sinks (Ordos, Junggar) imposes a significant infrastructure burden. China must choose between massive trans-continental pipelines or developing offshore storage in the Bohai Bay.  
4. **India:** The "Data" challenge. Without a comprehensive map of saline aquifers, India is limited to "Category I" hydrocarbon basins. To scale CCS, India must invest in a national storage atlas campaign to prove resources outside of the oil and gas sector.

Recommendation for Technoeconomic Modeling:  
For the user's study, we recommend a hybrid routing algorithm:

* For all onshore distances \< 1,000 km: Apply the **Pipeline Cost Function**.  
* For all offshore distances or onshore \> 1,000 km: Apply the **Shipping Cost Function**.  
* Apply a **Region Factor** to CAPEX: US \= 1.0, Europe \= 1.2 (offshore premium), China/India \= 0.7 (labor/material discount), adjusted for local steel prices.

## **10\. Data Tables for Map Generation**

### **Table 1: Comprehensive Global Storage Basin and Sink List**

The following table provides a granular list of viable \$CO\_2\$ storage locations. Capacities listed are mean estimates of technically accessible resource (TASR) or effective capacity where available. **Injectivity** is a highly site-specific parameter dependent on local permeability and well design; where specific data is absent, generic basin averages are provided.

| Region | Basin / Sink Name | Sub-Unit / Formation | Centroid / Key Site (Lat, Long) | Type | Capacity (Gt) | Typical Well Rate (Mt/yr) | Data Source |
| :---- | :---- | :---- | :---- | :---- | :---- | :---- | :---- |
| **USA** | **Gulf Coast Basin** | Frio / Miocene Sands | 29.5°N, 95.0°W | Saline | 2,000+ | 0.5 \- 1.0 | 10, |
| **USA** | **Permian Basin** | San Andres / Clearfork | 31.5°N, 103.5°W | EOR / Saline | 150 \- 350 | 0.2 \- 0.5 |  |
| **USA** | **Illinois Basin** | Mt. Simon Sandstone | 39.8°N, 89.0°W | Saline | 12 \- 170 | 1.0 (Demo) | , |
| **USA** | **Williston Basin** | Madison / Broom Creek | 47.5°N, 103.0°W | EOR / Saline | 150 | 0.5 |  |
| **USA** | **Michigan Basin** | St. Peter Sandstone | 44.0°N, 85.0°W | Saline | 20 \- 70 | 0.3 \- 0.5 | , |
| **USA** | **Appalachian Basin** | Oriskany / Rose Run | 40.0°N, 80.0°W | Saline / Gas | 40 \- 100 | \< 0.2 (Low Perm) |  |
| **USA** | **Powder River Basin** | Muddy Sandstone | 44.5°N, 105.5°W | Saline | 80 \- 150 | 0.3 |  |
| **USA** | **San Juan Basin** | Entrada Sandstone | 36.5°N, 107.5°W | Saline / EOR | 10 \- 25 | 0.3 |  |
| **USA** | **Anadarko Basin** | Granite Wash | 35.5°N, 99.0°W | EOR / Saline | 50 \- 100 | 0.3 |  |
| **Europe** | **Northern North Sea (NO)** | Utsira Formation | 58.4°N, 1.9°E | Saline | 40 \- 70 | 1.0 (Sleipner) | \[2\], |
| **Europe** | **Northern North Sea (NO)** | Johansen Formation | 60.5°N, 3.5°E | Saline | 5 \- 10 | 1.5 (Aurora Phase 1\) | 15 |
| **Europe** | **Southern North Sea (NL)** | P18 / P15 Fields | 52.0°N, 3.5°E | Depleted Gas | 0.037 | 2.5 (Hub) |  |
| **Europe** | **Southern North Sea (UK)** | Goldeneye / Viking | 53.5°N, 2.0°E | Depleted Gas | 0.5 \- 2.0 | 1.0 \- 3.0 | 16 |
| **Europe** | **North German Basin** | Mid. Buntsandstein | 53.0°N, 10.0°E | Saline | 4.8 \- 9.7 | 0.2 \- 0.5 | , |
| **Europe** | **Paris Basin** | Keuper / Dogger | 48.5°N, 3.0°E | Saline | 2 \- 20 | 0.1 \- 0.2 | , |
| **Europe** | **Pannonian Basin** | Sava / Drava Depressions | 46.0°N, 17.0°E | Depleted EOR | 2 \- 5 | 0.2 |  |
| **China** | **Ordos Basin** | Triassic Liujiagou | 39.33°N, 110.15°E | Saline | 335 | 0.1 (Pilot) | 20 |
| **China** | **Songliao Basin** | Cretaceous Sands | 45.0°N, 125.0°E | Saline / EOR | 694 | 0.3 | 36 |
| **China** | **Bohai Bay Basin** | Shahejie Formation | 38.5°N, 119.5°E | Offshore Saline | 490 | 0.5 | 36 |
| **China** | **Tarim Basin** | Carboniferous | 40.0°N, 84.0°E | Deep Saline | 552 | 0.3 | 36 |
| **China** | **Subei Basin** | Paleogene Sands | 33.0°N, 119.5°E | Saline | 435 | 0.2 | 36, |
| **China** | **Junggar Basin** | Jurassic/Triassic | 45.0°N, 86.0°E | EOR / Saline | \~100 | 0.2 | 38 |
| **India** | **Cambay Basin** | Gandhar / Ankleshwar | 21.7°N, 72.9°E | EOR / Saline | 0.65 \- 3.7 | 0.03 (Pilot) | 24 |
| **India** | **Krishna-Godavari** | Syn-rift sediments | 16.5°N, 82.0°E | Saline | \> 50 (Theoretical) | 0.2 \- 0.5 | , |
| **India** | **Assam-Arakan** | Barail / Tipam | 27.5°N, 95.5°E | EOR | \~5 (Fields) | 0.1 \- 0.3 | 26, |
| **India** | **Cauvery Basin** | Cretaceous Sands | 11.0°N, 79.5°E | Saline | 10 \- 20 | 0.2 |  |
| **India** | **Rajasthan Basin** | Barmer / Jaisalmer | 26.0°N, 71.0°E | Saline | \~100 | 0.2 |  |
| **India** | **Mahanadi Basin** | Mesozoic Sediments | 20.0°N, 87.0°E | Saline | \~45 | 0.2 |  |

### **Table 2: Transport Cost Function Coefficients (2024 Estimates)**

| Parameter | Unit | Onshore Pipeline | Offshore Pipeline | Shipping | Truck |
| :---- | :---- | :---- | :---- | :---- | :---- |
| **Fixed Cost** | \$ | Low | Medium | High (\$25-45/t) | Low |
| **Variable Cost** | \$/t/km | 0.02 \- 0.06 | 0.03 \- 0.08 | 0.02 \- 0.05 | 0.10 \- 0.15 |
| **Scale Factor (\$\\beta\$)** | \- | 0.5 (Strong) | 0.6 | 0.9 (Weak) | 1.0 (Linear) |
| **Diameter Eq.** | \- | \$Y \= 2157 X \+ 18\$ | \$Y \\times 1.5\$ | N/A | N/A |
| **Break-even Dist.** | km | \< 1000 | \< 500 | \> 700 (Offshore) | \< 300 |

18

#### **Works cited**

> 1. CO2 about the catalogue \- OGCI \- Oil and Gas Climate Initiative, accessed on January 12, 2026, [https\://www\.ogci.com/ccus/co2-storage-catalogue/co2-storage-catalogue-about-the-catalogue/](https://www.ogci.com/ccus/co2-storage-catalogue/co2-storage-catalogue-about-the-catalogue/)  
> 2. EU Geological CO₂ storage summary \- Clean Air Task Force, accessed on January 12, 2026, [https\://www\.catf.us/wp-content/uploads/2021/10/EU-CO2-storage-summary\_GEUS-report-2021-FINAL.pdf](https://www.catf.us/wp-content/uploads/2021/10/EU-CO2-storage-summary_GEUS-report-2021-FINAL.pdf)  
> 3. CO2 Storage Resource Catalogue, accessed on January 12, 2026, [https\://co2catalogue.ogci.com/](https://co2catalogue.ogci.com/)  
> 4. Geologic Provinces of the World, 2000 World Petroleum Assessment, all defined provinces, accessed on January 12, 2026, [https\://data.usgs.gov/datacatalog/data/USGS:60ad2fd7d34e4043c850edb3](https://data.usgs.gov/datacatalog/data/USGS:60ad2fd7d34e4043c850edb3)  
> 5. Storage \- Global CCS Institute, accessed on January 12, 2026, [https\://co2re.co/storagedata](https://co2re.co/storagedata)  
> 6. COST OF CO2 STORAGE \- Global CCS Institute, accessed on January 12, 2026, [https\://www\.globalccsinstitute.com/wp-content/uploads/2025/12/Cost-of-CO2-Storage-1225.pdf](https://www.globalccsinstitute.com/wp-content/uploads/2025/12/Cost-of-CO2-Storage-1225.pdf)  
> 7. NATCARB/ATLAS | netl.doe.gov \- Department of Energy, accessed on January 12, 2026, [https\://www\.netl.doe.gov/coal/carbon-storage/strategic-program-support/natcarb-atlas](https://www.netl.doe.gov/coal/carbon-storage/strategic-program-support/natcarb-atlas)  
> 8. NATCARB\_Saline\_Poly\_v1502 \- Overview \- Department of Energy, accessed on January 12, 2026, [https\://arcgis.netl.doe.gov/portal/home/item.html?id=be12ba3a1e104f908c59d82c0deb0537](https://arcgis.netl.doe.gov/portal/home/item.html?id=be12ba3a1e104f908c59d82c0deb0537)  
> 9. Learn more about NATCARB, a dataset from National Energy Technology Laboratory (NETL), on Open Net Zero, accessed on January 12, 2026, [https\://www\.opennetzero.org/national-energy-technology-laboratory-netl/natcarb](https://www.opennetzero.org/national-energy-technology-laboratory-netl/natcarb)  
> 10. National assessment of geologic carbon dioxide storage resources: results, accessed on January 12, 2026, [https\://pubs.usgs.gov/publication/cir1386](https://pubs.usgs.gov/publication/cir1386)  
> 11. Pipeline transport \- Emis Vito, accessed on January 12, 2026, [https\://emis.vito.be/nl/tools/mapitccu/technologies/pipeline-transport](https://emis.vito.be/nl/tools/mapitccu/technologies/pipeline-transport)  
> 12. European CO2 storage database \- SETIS \- SET Plan information ..., accessed on January 12, 2026, [https\://setis.ec.europa.eu/european-co2-storage-database\_en](https://setis.ec.europa.eu/european-co2-storage-database_en)  
> 13. The Nordic CO\>2\> Storage Atlas \- GEUS' publications, accessed on January 12, 2026, [https\://pub.geus.dk/en/publications/the-nordic-cosub2sub-storage-atlas/](https://pub.geus.dk/en/publications/the-nordic-cosub2sub-storage-atlas/)  
> 14. CO2 storage Atlas Norwegian North Sea \- Sokkeldirektoratet, accessed on January 12, 2026, [https\://www\.sodir.no/en/whats-new/publications/co2-atlases/co2-storage-atlas-norwegian-north-sea/](https://www.sodir.no/en/whats-new/publications/co2-atlases/co2-storage-atlas-norwegian-north-sea/)  
> 15. NEEDS, OPPORTUNITIES AND PROSPECTS FOR CO2 SHIPPING IN CCS PROJECTS, accessed on January 12, 2026, [https\://www\.globalccsinstitute.com/wp-content/uploads/2025/11/Global-CCS-Institute-Needs-Opportunities-and-Prospects-for-CO2-Shipping-in-CCS-Projects.pdf](https://www.globalccsinstitute.com/wp-content/uploads/2025/11/Global-CCS-Institute-Needs-Opportunities-and-Prospects-for-CO2-Shipping-in-CCS-Projects.pdf)  
> 16. CO2StoP – a project mapping both reserves and resources for CO2 storage in Europe \- SETIS, accessed on January 12, 2026, [https\://setis.ec.europa.eu/system/files/2020-08/about\_co2stop.pdf](https://setis.ec.europa.eu/system/files/2020-08/about_co2stop.pdf)  
> 17. The Costs of CO2 Transport \- Zero Emissions Platform, accessed on January 12, 2026, [https\://zeroemissionsplatform.eu/publication/the-costs-of-co2-transport/](https://zeroemissionsplatform.eu/publication/the-costs-of-co2-transport/)  
> 18. Regional Opportunities for Carbon Dioxide Capture and Storage in China \- Pacific Northwest National Laboratory, accessed on January 12, 2026, [http\://www\.pnl.gov/main/publications/external/technical\_reports/PNNL-19091.pdf?\_\_hstc=249664665.2f3f33a24b44870ec4a577029c49e44b.1753833600186.1753833600187.1753833600188.1&\_\_hssc=249664665.1.1753833600189&\_\_hsfp=2324370431](http://www.pnl.gov/main/publications/external/technical_reports/PNNL-19091.pdf?__hstc=249664665.2f3f33a24b44870ec4a577029c49e44b.1753833600186.1753833600187.1753833600188.1&__hssc=249664665.1.1753833600189&__hsfp=2324370431)  
> 19. Location map of the Shenhua CCS demonstration project. (a) Inner... \- ResearchGate, accessed on January 12, 2026, [https\://www\.researchgate.net/figure/Location-map-of-the-Shenhua-CCS-demonstration-project-a-Inner-Mongolia-and-Ordos\_fig1\_304896275](https://www.researchgate.net/figure/Location-map-of-the-Shenhua-CCS-demonstration-project-a-Inner-Mongolia-and-Ordos_fig1_304896275)  
> 20. Ordos Basin Project China \- GEO \- Global Energy Observatory, accessed on January 12, 2026, [https\://globalenergyobservatory.org/geoid/40185](https://globalenergyobservatory.org/geoid/40185)  
> 21. Junggar Basin \- Wikipedia, accessed on January 12, 2026, [https\://en.wikipedia.org/wiki/Junggar\_Basin](https://en.wikipedia.org/wiki/Junggar_Basin)  
> 22. Basins and Categories | Directorate General of Hydrocarbons (DGH), accessed on January 12, 2026, [https\://www\.dghindia.gov.in/index.php/page?pageId=66](https://www.dghindia.gov.in/index.php/page?pageId=66)  
> 23. Cambay Basin | NDR (National Data Repository)-Directorate General of Hydrocarbons(DGH) | Ministry of Petroleum and Natural Gas, Government of India, accessed on January 12, 2026, [https\://www\.ndrdgh.gov.in/NDR/?page\_id=629](https://www.ndrdgh.gov.in/NDR/?page_id=629)  
> 24. ONGC to store CO2 in depleted wells at Gujarat's Gandhar field in first CCS pilot, accessed on January 12, 2026, [https\://m.economictimes.com/industry/energy/oil-gas/ongc-to-store-co2-in-depleted-wells-at-gujarats-gandhar-field-in-first-ccs-pilot/articleshow/126331160.cms](https://m.economictimes.com/industry/energy/oil-gas/ongc-to-store-co2-in-depleted-wells-at-gujarats-gandhar-field-in-first-ccs-pilot/articleshow/126331160.cms)  
> 25. Characterisation of a potential CO2 storage complex and first-order containment risk assessment in the Cambay Basin, India, accessed on January 12, 2026, [https\://nora.nerc.ac.uk/id/eprint/540559/1/Williams\_et\_al\_2025\_IJGGC.pdf](https://nora.nerc.ac.uk/id/eprint/540559/1/Williams_et_al_2025_IJGGC.pdf)  
> 26. Characterizing Barail shale rock for CO2 storage potential in the Assam Arakan Basin, India, accessed on January 12, 2026, [https\://discovery.researcher.life/article/characterizing-barail-shale-rock-for-co2-storage-potential-in-the-assam-arakan-basin-india/6a2921550c7030888354485e2a231b02](https://discovery.researcher.life/article/characterizing-barail-shale-rock-for-co2-storage-potential-in-the-assam-arakan-basin-india/6a2921550c7030888354485e2a231b02)  
> 27. Effects of Flow Velocity on Transient Behaviour of Liquid CO 2 Decompression during Pipeline Transportation \- MDPI, accessed on January 12, 2026, [https\://www\.mdpi.com/2227-9717/9/2/192](https://www.mdpi.com/2227-9717/9/2/192)  
> 28. Carbon Dioxide Transport Pipeline Systems: Overview of Technical Characteristics, Safety, Integrity and Cost, and Potential Application of Digital Twin, accessed on January 12, 2026, [https\://asmedigitalcollection.asme.org/energyresources/article/144/9/092106/1130942/Carbon-Dioxide-Transport-Pipeline-Systems-Overview](https://asmedigitalcollection.asme.org/energyresources/article/144/9/092106/1130942/Carbon-Dioxide-Transport-Pipeline-Systems-Overview)  
> 29. Pipe Diameter and Flow Rate Calculator – Calculate Flow Velocity and Pipe Size, accessed on January 12, 2026, [https\://www\.pipeflowcalculations.com/flowrate/calculator.xhtml](https://www.pipeflowcalculations.com/flowrate/calculator.xhtml)  
> 30. Pipeline Infrastructure for CO 2 Transport: Cost Analysis and Design Optimization \- MDPI, accessed on January 12, 2026, [https\://www\.mdpi.com/1996-1073/17/12/2911](https://www.mdpi.com/1996-1073/17/12/2911)  
> 31. Exergetic and Economic Evaluation of CO 2 Liquefaction Processes \- MDPI, accessed on January 12, 2026, [https\://www\.mdpi.com/1996-1073/14/21/7174](https://www.mdpi.com/1996-1073/14/21/7174)  
> 32. Comparison of transport costs of CO2 by truck, rail, and pipeline as a... \- ResearchGate, accessed on January 12, 2026, [https\://www\.researchgate.net/figure/Comparison-of-transport-costs-of-CO2-by-truck-rail-and-pipeline-as-a-function-of\_fig5\_351524141](https://www.researchgate.net/figure/Comparison-of-transport-costs-of-CO2-by-truck-rail-and-pipeline-as-a-function-of_fig5_351524141)  
> 33. Ship Transport of CO2 \- IEAGHG, accessed on January 12, 2026, [https\://publications.ieaghg.org/docs/General\_Docs/Reports/PH4-30%20Ship%20Transport.pdf](https://publications.ieaghg.org/docs/General_Docs/Reports/PH4-30%20Ship%20Transport.pdf)  
> 34. Transport Cost for Carbon Removal Projects With Biomass and CO2 Storage \- Frontiers, accessed on January 12, 2026, [https\://www\.frontiersin.org/journals/energy-research/articles/10.3389/fenrg.2021.639943/full](https://www.frontiersin.org/journals/energy-research/articles/10.3389/fenrg.2021.639943/full)  
> 35. Opportunities for rail in the transport of carbon dioxide in the United States \- Frontiers, accessed on January 12, 2026, [https\://www\.frontiersin.org/journals/energy-research/articles/10.3389/fenrg.2023.1343085/full](https://www.frontiersin.org/journals/energy-research/articles/10.3389/fenrg.2023.1343085/full)  
> 36. China's pathways of CO2 capture, utilization and storage under carbon neutrality vision 2060 \- Taylor & Francis Online, accessed on January 12, 2026, [https\://www\.tandfonline.com/doi/full/10.1080/17583004.2022.2117648](https://www.tandfonline.com/doi/full/10.1080/17583004.2022.2117648)  
> 37. Optimization of CO2 geological storage sites based on regional stability evaluation—A case study on geological storage in tianjin, china \- Frontiers, accessed on January 12, 2026, [https\://www\.frontiersin.org/journals/earth-science/articles/10.3389/feart.2022.955455/full](https://www.frontiersin.org/journals/earth-science/articles/10.3389/feart.2022.955455/full)  
> 38. (PDF) Potential Evaluation of CO2 Sequestration and Enhanced Oil Recovery of Low Permeability Reservoir in the Junggar Basin, China \- ResearchGate, accessed on January 12, 2026, [https\://www\.researchgate.net/publication/263941827\_Potential\_Evaluation\_of\_CO2\_Sequestration\_and\_Enhanced\_Oil\_Recovery\_of\_Low\_Permeability\_Reservoir\_in\_the\_Junggar\_Basin\_China](https://www.researchgate.net/publication/263941827_Potential_Evaluation_of_CO2_Sequestration_and_Enhanced_Oil_Recovery_of_Low_Permeability_Reservoir_in_the_Junggar_Basin_China)  
> 39. Road Map Update for Carbon Capture, Utilization, and Storage Demonstration and Deployment in the People's Republic of China, accessed on January 12, 2026, [https\://www\.adb.org/sites/default/files/publication/814386/road-map-update-carbon-capture-utilization-storage-prc.pdf](https://www.adb.org/sites/default/files/publication/814386/road-map-update-carbon-capture-utilization-storage-prc.pdf)  
> 40. Mapping Highly Cost-Effective Carbon Capture and Storage Opportunities in India, accessed on January 12, 2026, [https\://www\.scirp.org/journal/paperinformation?paperid=37560](https://www.scirp.org/journal/paperinformation?paperid=37560)  
> 41. Geologic Basin Boundaries (Basins\_GHGRP) GIS Layer \- Dataset \- Catalog \- Data.gov, accessed on January 12, 2026, [https\://catalog.data.gov/dataset/geologic-basin-boundaries-basins\_ghgrp-gis-layer9](https://catalog.data.gov/dataset/geologic-basin-boundaries-basins_ghgrp-gis-layer9)