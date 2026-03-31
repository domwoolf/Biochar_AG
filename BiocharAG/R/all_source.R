#' Calculate Biochar-Energy (BEBCS) Metrics
#'
#' @param params A list of parameters.
#' @return A list of calculated metrics for BEBCS.
#' @export
calculate_bebcs <- function(params) {
  # Ensure optional parameters exist
  h_c_org <- if (!is.null(params$h_c_org)) params$h_c_org else 0.35
  soil_temp <- if (!is.null(params$soil_temp)) params$soil_temp else 14.9

  with(params, {
    # Pyrolysis Physics (Mass & Energy Balance)
    # Uses sophisticated Woolf et al. (2016) / Excel logic
    phys <- calculate_pyrolysis_physics(
      py_temp = py_temp,
      lignin = lignin,
      bm_lhv = bm_lhv,
      moisture = if (exists("bm_h2o")) bm_h2o else 0.1,
      ash = if (exists("bm_ash")) bm_ash else 0.05
    )

    bc_yield <- phys$yield_bc
    bc_c_content <- phys$bc_c_content_final

    # Stable Fraction using Fperm (Woolf et al. 2021)
    bc_stability <- calculate_fperm_approx(h_c_org, method = "HC", soil_temp = soil_temp)
    # Carbon Sequestration
    # C sequestered = bc_yield * bc_c_content * bc_stability
    c_sequestered <- bc_yield * bc_c_content * bc_stability

    # Energy Output
    # phys$energy_net is the Net Fuel Energy available (GJ/Mg feedstock)
    # after satisfying process heat requirements.
    # Convert to Electricity using BES efficiency.
    energy_output <- phys$energy_net * bes_energy_efficiency

    elec_prod <- energy_output * 0.277778
    elec_revenue <- elec_prod * elec_price

    # Biochar Revenue
    bc_revenue <- bc_yield * bc_price

    # Costs
    # Pyrolysis Plant Cost (py_cc) + Power Plant Cost.
    # Assume 50 MW Scale reference for unit cost derivation
    # (Consistent with BES/BECCS modern logic)

    # 1. Pyrolysis Unit Cost
    # Assume py_cc is capital cost per Mg capacity? Or $/kW equiv?
    # Original logic used py_cc directly. Let's keep py_cc as $/Mg annual capacity?
    # Or modernize: Pyrolysis Plant Cost ~ $500-800 / annual ton?
    # For now, let's treat py_cc as $/Mg Annual Capacity constant.

    annuity_fac_py <- calculate_annuity_factor(discount_rate, py_life)
    annual_capex_py <- py_cc / annuity_fac_py

    # 2. Power Generation Unit Cost (for Volatiles)
    # Volatiles Mass = (1 - bc_yield) * Feed
    # We use bes_capital_cost ($/kW) to find cost per Mg.

    # Calculate Power Capex per Mg Biomass (same logic as BES)
    # Calculate Power Capex per Mg Biomass (same logic as BES)
    plant_mw <- if (!is.null(params$plant_mw)) params$plant_mw else 50
    capacity_factor <- 0.85
    # Elec Prod for pure BES:
    bes_elec_prod_ref <- bm_lhv * bes_energy_efficiency * 0.277778
    ref_annual_biomass <- (plant_mw * 8760 * capacity_factor) / bes_elec_prod_ref

    today_bes_capex <- bes_capital_cost * plant_mw * 1000
    # Apply Scaling Factor (Economies of Scale)
    scaling_factor <- 0.7
    base_cost_ref <- bes_capital_cost * 50 * 1000

    total_bes_capex <- base_cost_ref * ((plant_mw / 50)^scaling_factor)
    # Note: We use the base 'bes_capital_cost' (Wood/Stoker reference) here.
    # We do NOT apply the 'high ash' penalty from adjust_costs_for_fuel() because
    # BEBCS combusts syngas (clean), leaving the ash in the biochar.

    annuity_fac_bes <- calculate_annuity_factor(discount_rate, bes_life)
    annual_bes_payment <- total_bes_capex / annuity_fac_bes

    # Base Power Capex per Mg Input
    base_power_capex_per_mg <- annual_bes_payment / ref_annual_biomass

    # BEBCS Power Unit handles only (1 - bc_yield) fraction?
    # Or is it sized for the volatiles energy?
    # Volatiles Energy approx proportional to mass? (Simplification)
    # Let's scale by mass fraction of volatiles.
    annual_capex_power <- base_power_capex_per_mg * (1 - bc_yield)

    # O&M
    # Pyrolysis O&M + Power O&M
    annual_om <- (py_cc * O_M_factor) + (base_power_capex_per_mg * (1 - bc_yield) * bes_om_factor)

    # Logistics Cost (Biomass Transport)
    radius <- if (!is.null(params$collection_radius)) params$collection_radius else 50
    avg_dist <- (2 / 3) * radius
    tf <- if (!is.null(params$bm_transport_fixed)) params$bm_transport_fixed else 5.0
    tv <- if (!is.null(params$bm_transport_var)) params$bm_transport_var else 0.15
    logistics_cost <- tf + (tv * avg_dist)

    total_cost <- annual_capex_py + annual_capex_power + annual_om + logistics_cost

    # Abatement
    # C sequestered + C displaced by energy
    c_displaced <- energy_output * ff_c_intensity

    # Soil GHG benefits (N2O suppression)
    # Excel J23: Annual N2O benefit
    # Paper uses GWP_N2O (approx 298 or similar from AR5)
    # n_app_rate (kg N/ha), n2o_factor (% emitted)
    # BEBCS reduces emissions by avoiding fertilizer or reducing N2O rate?
    # Excel calculates N2O benefit based on n_app_rate * n2o_factor.
    # Simplified 'soil_ghg_abatement' parameter for C eq per Mg Biochar.
    soil_ghg_abatement <- 0.1 # Placeholder Mg CO2e/Mg BC

    tot_c_abatement <- c_sequestered + c_displaced + soil_ghg_abatement
    abatement_value <- tot_c_abatement * c_price

    # Crop Yield Benefits (Nbcf)
    # Equation 13: value / (discount + decay_rate)
    # value = crop_price * yield_increase
    # We add this as 'ag_value' separate from carbon market.

    # Calculate decay rate from biochar half-life or stability
    # Paper: T1/2. Excel: J8 = 10^(bc_stab_factor * ...)
    # Assume mean residence time (MRT) = 1/decay_rate.
    # Excel J12: EXP(-time * (LN(2)/J8)). So J8 is Half Life?
    # Yes, Half Life T1/2.
    # Biochar Valuation
    # Refactored to separate function (handling Market vs Ag Value switch)
    bc_val_res <- calculate_biochar_value(params, bc_yield)

    # This value is already in $/Mg Feedstock
    biochar_economic_value <- bc_val_res$value_usd_per_mg_feedstock

    # Note: For strict financial accounting, 'nbcf' (Ag Value) might be considered separate from 'sales revenue'
    # but for TEA Net Value comparison, we sum them based on the selected mode.

    total_revenue <- elec_revenue + biochar_economic_value + abatement_value
    net_value <- total_revenue - total_cost

    list(
      technology = "BEBCS",
      bc_yield = bc_yield,
      bc_c_content = bc_c_content,
      energy_output = energy_output,
      elec_prod = elec_prod,
      c_sequestered = c_sequestered,
      tot_c_abatement = tot_c_abatement,
      total_cost = total_cost,
      total_revenue = total_revenue,
      biochar_value = biochar_economic_value,
      val_method = bc_val_res$method_used,
      net_value = net_value
    )
  })
}
#' Calculate Bioenergy Carbon Capture and Storage (BECCS) Metrics
#'
#' Modernizated logic (2024 Basis):
#' - Uses distinct BECCS efficiency (accounting for capture penalty).
#' - Explicitly calculates Transport & Storage costs based on CO2 mass and distance.
#' - Updated Capital Costs estimates.
#'
#' @param params A list of parameters.
#' @return A list of calculated metrics for BECCS.
#' @export
calculate_beccs <- function(params) {
  # Default to modern params if not present
  if (is.null(params$beccs_efficiency)) params$beccs_efficiency <- 0.28
  if (is.null(params$capture_rate)) params$capture_rate <- 0.90
  # Determine CCS Distance
  # Priority: 1. Spatial Raster (dist_sink_km) if passed
  #           2. Explicit params$ccs_distance (if not the default placeholder or if meant to override)
  #           3. Nearest Sink Calculation (if Lat/Lon provided)
  #           4. Default (100 km)

  # Check for EOR Toggle
  allow_eor <- if (!is.null(params$allow_eor)) as.logical(params$allow_eor) else TRUE

  # Check for Spatial Inputs
  dist_spatial <- NULL
  if (allow_eor) {
    if (!is.null(params$dist_sink_km)) dist_spatial <- params$dist_sink_km
  } else {
    if (!is.null(params$dist_sink_saline_km)) dist_spatial <- params$dist_sink_saline_km
  }

  if (!is.null(dist_spatial)) {
    # Spatial Input Wins
    params$ccs_distance <- dist_spatial
  } else if (is.null(params$ccs_distance)) {
    # No spatial input AND no manual distance -> Calc from Lat/Lon or Default
    if (!is.null(params$lat) && !is.null(params$lon)) {
      geo <- find_nearest_sink(params$lat, params$lon)
      params$ccs_distance <- geo$distance_km
    } else {
      params$ccs_distance <- 100
    }
  }
  # Else: params$ccs_distance exists and no spatial input -> Use it (Default behavior is 100)
  # WARNING: default_parameters() sets ccs_distance = 100.
  # If spatial inputs ARE provided (dist_spatial != NULL), we overwrite it above.
  # This works.
  if (is.null(params$ccs_storage_cost)) params$ccs_storage_cost <- 15
  if (is.null(params$beccs_capital_cost)) params$beccs_capital_cost <- 4000

  # Apply Fuel Quality Penalties (High Ash -> Higher Cost)
  params <- adjust_costs_for_fuel(params)

  with(params, {
    # 1. Energy Output
    # BECCS efficiency (e.g., 28%)
    energy_output <- bm_lhv * beccs_efficiency
    elec_prod <- energy_output * 0.277778 # MWh / Mg biomass

    # 2. Carbon Capture
    # CO2 Produced = C_content * (44/12)
    # CO2 Captured = Produced * Capture Rate
    co2_produced <- bm_c * (44 / 12)
    co2_captured <- co2_produced * capture_rate # Mg CO2 / Mg Biomass

    # 3. Transport & Storage Costs
    # Annual CO2 Flow: depend on plant capacity.
    # We operate on "Per Mg Biomass" basis, but Transport Cost scales with Total Mass.
    # Assess Total Annual Biomass Throughput to find Scale.
    # Assume: Plant Capacity (MW_e) -> Biomass Input
    # This is circular if we don't define Plant Size.
    # Let's assume a reference commercial scale plant input for scale calculation?
    # Or add 'plant_capacity_mw' to parameters?
    # Defaulting to a standard 50 MW plant scale for cost estimation if not provided.

    # Scale calculation
    # Allow user to specify plant_mw (e.g. from spatial analysis of feedstock availability)
    plant_mw <- if (!is.null(params$plant_mw)) params$plant_mw else 50

    # Annual Biomass = (MW * 8760 * CapacityFactor) / (MWh/Mg)
    # Elec_Prod (MWh/Mg)
    capacity_factor <- 0.85
    annual_biomass <- (plant_mw * 8760 * capacity_factor) / elec_prod

    annual_co2_total <- annual_biomass * co2_captured

    # Explicit Transport Cost ($/Mg CO2)
    # Extract is_offshore from params (populated by run_spatial_tea)
    is_offshore <- if (!is.null(params$sink_is_offshore)) params$sink_is_offshore else FALSE

    transport_cost_per_ton <- calculate_ccs_transport(
      co2_mass = annual_co2_total,
      distance = ccs_distance,
      is_offshore = is_offshore,
      discount_rate = discount_rate,
      lifetime = bes_life
    )

    # Total T&S Cost ($/Mg Biomass)
    # (Transport + Storage) * CO2_Captured
    ts_cost <- (transport_cost_per_ton + ccs_storage_cost) * co2_captured

    # 4. Plant Costs (CAPEX/OPEX)
    # Scale calculation methodology identical to BECCS for consistency
    # Capital Cost parameter is usually $/kW (installed capacity).
    # Convert $/kW to $/Mg Biomass/yr ??
    # Or calculate Annual Capex Payment per Mg Biomass processed.

    # Total Capex ($)
    # Apply Scaling Factor (Economies of Scale)
    scaling_factor <- 0.7
    base_cost_beccs <- beccs_capital_cost * 50 * 1000

    total_capex <- base_cost_beccs * ((plant_mw / 50)^scaling_factor)

    # Annual Capex ($/yr)
    annuity_fac <- calculate_annuity_factor(discount_rate, bes_life)
    annual_capex_payment <- total_capex / annuity_fac

    # Capex per Mg Biomass
    capex_per_mg <- annual_capex_payment / annual_biomass

    # OPEX ($/Mg)
    # Fixed % of Capex (converted to per Mg) + Variable?
    # Simplified: % of Capex per Mg
    opex_per_mg <- capex_per_mg * 0.05 # 5% O&M

    # Logistics Cost (Biomass Transport)
    radius <- if (!is.null(params$collection_radius)) params$collection_radius else 50
    avg_dist <- (2 / 3) * radius
    tf <- if (!is.null(params$bm_transport_fixed)) params$bm_transport_fixed else 5.0
    tv <- if (!is.null(params$bm_transport_var)) params$bm_transport_var else 0.15
    logistics_cost <- tf + (tv * avg_dist)

    total_cost <- capex_per_mg + opex_per_mg + ts_cost + logistics_cost

    # 5. Revenue & Value
    elec_revenue <- elec_prod * elec_price

    # Carbon Abatement
    c_sequestered <- bm_c * capture_rate # C equivalent
    c_displaced <- energy_output * ff_c_intensity
    tot_c_abatement <- c_sequestered + c_displaced
    abatement_value <- tot_c_abatement * c_price

    total_revenue <- elec_revenue + abatement_value
    net_value <- total_revenue - total_cost

    list(
      technology = "BECCS",
      energy_output = energy_output,
      elec_prod = elec_prod,
      c_sequestered = c_sequestered,
      tot_c_abatement = tot_c_abatement,
      total_cost = total_cost,
      ts_cost = ts_cost, # Breakout for analysis
      total_revenue = total_revenue,
      net_value = net_value
    )
  })
}
#' Calculate Bioenergy System (BES) Metrics
#'
#' Modernizated logic (2024 Basis):
#' - Uses modernized Capital Cost ($3,000/kW) and Efficiency (30%) defaults.
#' - Calculates Levelized Cost of Electricity components (CAPEX/OPEX).
#'
#' @param params A list of parameters.
#' @return A list of calculated metrics for BES.
#' @export
calculate_bes <- function(params) {
  # Default to modern params if not present
  if (is.null(params$bes_capital_cost)) params$bes_capital_cost <- 3000
  if (is.null(params$bes_energy_efficiency)) params$bes_energy_efficiency <- 0.30
  if (is.null(params$bes_om_factor)) params$bes_om_factor <- 0.04
  if (is.null(params$bes_life)) params$bes_life <- 30

  # Apply Fuel Quality Penalties (High Ash -> Higher Cost)
  params <- adjust_costs_for_fuel(params)

  with(params, {
    # 1. Energy Output
    energy_output <- bm_lhv * bes_energy_efficiency
    elec_prod <- energy_output * 0.277778 # MWh / Mg biomass

    # 2. Plant Costs (CAPEX/OPEX)
    # Scale calculation methodology identical to BECCS for consistency
    plant_mw <- if (!is.null(params$plant_mw)) params$plant_mw else 50
    capacity_factor <- 0.85
    annual_biomass <- (plant_mw * 8760 * capacity_factor) / elec_prod

    # Total Capex ($)
    # Apply Scaling Factor (Economies of Scale)
    # Reference: 50 MW
    # Scaling Factor: 0.7 (typical for thermal plants)
    scaling_factor <- 0.7
    base_cost <- bes_capital_cost * 50 * 1000 # Cost of 50 MW plant

    total_capex <- base_cost * ((plant_mw / 50)^scaling_factor)

    # Annual Capex ($/yr)
    annuity_fac <- calculate_annuity_factor(discount_rate, bes_life)
    annual_capex_payment <- total_capex / annuity_fac

    # Capex per Mg Biomass
    capex_per_mg <- annual_capex_payment / annual_biomass

    # OPEX ($/Mg)
    opex_per_mg <- capex_per_mg * bes_om_factor

    # Logistics Cost (Biomass Transport)
    # Default radius 50km if not provided
    radius <- if (!is.null(params$collection_radius)) params$collection_radius else 50
    avg_dist <- (2 / 3) * radius

    # Defaults in case params missing
    tf <- if (!is.null(params$bm_transport_fixed)) params$bm_transport_fixed else 5.0
    tv <- if (!is.null(params$bm_transport_var)) params$bm_transport_var else 0.15

    logistics_cost <- tf + (tv * avg_dist)

    total_cost <- capex_per_mg + opex_per_mg + logistics_cost

    # 3. Revenue & Value
    elec_revenue <- elec_prod * elec_price

    # Carbon Abatement (No Sequestration, only displacement)
    c_sequestered <- 0
    c_displaced <- energy_output * ff_c_intensity
    tot_c_abatement <- c_sequestered + c_displaced
    abatement_value <- tot_c_abatement * c_price

    total_revenue <- elec_revenue + abatement_value
    net_value <- total_revenue - total_cost

    list(
      technology = "BES",
      energy_output = energy_output,
      elec_prod = elec_prod,
      c_sequestered = c_sequestered,
      tot_c_abatement = tot_c_abatement,
      total_cost = total_cost,
      total_revenue = total_revenue,
      net_value = net_value
    )
  })
}
#' Calculate Biochar Economic Value
#'
#' Determines the economic value of the biochar fraction based on the selected valuation method.
#' Handles the mutually exclusive logic between Market Sales (Revenue) and Agronomic Value (Shadow Price).
#' ensuring values are normalized to $/Mg Feedstock.
#'
#' @param params List of parameters including `bc_valuation_method`, `bc_price`, `bc_ag_value`, etc.
#' @param bc_yield Numeric. Biochar yield fraction (Mg Biochar / Mg Feedstock).
#'
#' @return A list containing:
#' \item{value_usd_per_mg_feedstock}{Total economic value per Mg of biomass feedstock.}
#' \item{method_used}{Character string indicating the method ("market_price" or "ag_value").}
#' \item{detail}{Intermediate values (e.g. unit price per Mg char).}
#' @export
calculate_biochar_value <- function(params, bc_yield) {
    method <- if (!is.null(params$bc_valuation_method)) params$bc_valuation_method else "ag_value"

    # Initialize
    val_per_mg_feedstock <- 0
    detail <- list()

    if (method == "market_price") {
        # Method A: Market Sale
        bc_price <- if (!is.null(params$bc_price)) params$bc_price else 0
        val_per_mg_feedstock <- bc_yield * bc_price

        detail <- list(unit_price_char = bc_price, type = "Sales Revenue")
    } else if (method == "ag_value") {
        # Method B: Legacy Simple Ag Value
        bc_ag_value <- if (!is.null(params$bc_ag_value)) params$bc_ag_value else 0
        discount_rate <- if (!is.null(params$discount_rate)) params$discount_rate else 0.1
        # Use simple decay model
        bc_stab_factor <- if (!is.null(params$bc_stab_factor)) params$bc_stab_factor else 4.6
        bc_half_life <- 10^(bc_stab_factor * 0.9)
        decay_rate <- log(2) / bc_half_life

        nbcf_per_mg_char <- bc_ag_value / (discount_rate + decay_rate)
        val_per_mg_feedstock <- bc_yield * nbcf_per_mg_char

        detail <- list(type = "Static Ag Value", value = nbcf_per_mg_char)
    } else if (method == "advanced_mechanistic") {
        # Method C: Mechanistic Substitution Model (Advanced)

        # 1. Liming Value (Substitution)
        soil_ph <- if (!is.null(params$soil_ph)) params$soil_ph else 6.5
        target_ph <- 6.5
        price_lime <- if (!is.null(params$price_lime)) params$price_lime else 60
        bc_cce <- if (!is.null(params$bc_cce)) params$bc_cce else 0.15

        v_lime_per_mg_char <- 0
        if (soil_ph < target_ph) {
            v_lime_per_mg_char <- bc_cce * price_lime
        }

        # 2. Nutrient Value (Substitution)
        p_n <- if (!is.null(params$price_n)) params$price_n else 0.92
        p_p <- if (!is.null(params$price_p)) params$price_p else 1.10
        p_k <- if (!is.null(params$price_k)) params$price_k else 0.62

        c_n <- if (!is.null(params$bc_n_content)) params$bc_n_content else 0.005
        c_p <- if (!is.null(params$bc_p_content)) params$bc_p_content else 0.002
        c_k <- if (!is.null(params$bc_k_content)) params$bc_k_content else 0.005

        # Availability Factors
        avail_n <- 0.1
        avail_p <- 0.5
        avail_k <- 0.8

        v_nut_per_mg_char <- (c_n * avail_n * p_n * 1000) +
            (c_p * avail_p * p_p * 1000) +
            (c_k * avail_k * p_k * 1000)

        # 3. Physical/CEC Value (Yield Efficiency)
        soil_cec <- if (!is.null(params$soil_cec)) params$soil_cec else 20
        # Heuristic: Value is proportional to CEC deficit (Sandier = More value)
        # Assume $50/Mg annual benefit in pure sand (CEC=5), $0 in clay (CEC>30)
        # Linear ramp: (30 - CEC) * 2
        cec_val_annual <- max(0, (30 - soil_cec) * 2)

        # Discounted over impact duration
        dur <- if (!is.null(params$ag_impact_duration)) params$ag_impact_duration else 10
        dr <- if (!is.null(params$discount_rate)) params$discount_rate else 0.1
        apv <- (1 - (1 + dr)^-dur) / dr

        v_phys_per_mg_char <- cec_val_annual * apv

        # Total
        total_val_per_mg_char <- v_lime_per_mg_char + v_nut_per_mg_char + v_phys_per_mg_char
        val_per_mg_feedstock <- bc_yield * total_val_per_mg_char

        detail <- list(
            v_lime = v_lime_per_mg_char,
            v_nut = v_nut_per_mg_char,
            v_phys = v_phys_per_mg_char,
            type = "Mechanistic Substitutes"
        )
    }

    list(
        value_usd_per_mg_feedstock = val_per_mg_feedstock,
        method_used = method,
        detail = detail
    )
}
#' Calculate Relative Present Value (RPV)
#'
#' @param results_list List of result objects from calculate_bes, calculate_beccs, calculate_bebcs.
#' @return Data frame with comparison.
#' @export
calculate_rpv <- function(results_list) {
    # Extract NPVs
    npvs <- sapply(results_list, function(x) x$net_value)
    names(npvs) <- sapply(results_list, function(x) x$technology)

    # Find best alternative for each
    rpv_res <- list()
    for (tech in names(npvs)) {
        others <- npvs[names(npvs) != tech]
        oc <- max(others) # Opportunity Cost is the max of alternatives
        rpv <- npvs[tech] - oc
        rpv_res[[tech]] <- rpv
    }

    data.frame(
        Technology = names(npvs),
        NPV = npvs,
        Best_Alternative_NPV = sapply(names(npvs), function(x) max(npvs[names(npvs) != x])),
        RPV = unlist(rpv_res)
    )
}
#' Biochar Permanence Reference Data
#'
#' A dataset containing experimental biochar stability data from Woolf et al. (2021).
#' Used by `calculate_fperm` to predict biochar permanence based on H:C ratios or Pyrolysis Temperature.
#'
#' @format A data frame with 89 rows and 25 variables:
#' \describe{
#'   \item{Reference}{Source of the data}
#'   \item{Feedstock}{Biochar feedstock type}
#'   \item{Pyrolysis_temperature}{Temperature of pyrolysis (Celsius)}
#'   \item{H_to_Corg}{Molar Hydrogen to Organic Carbon ratio}
#'   \item{Temp_of_experiment}{Temperature at which the incubation experiment was conducted}
#'   \item{k1, k2, k3}{Decay rates for the three-pool model}
#'   \item{C1, C2, C3}{Carbon fractions for the three-pool model (Percentages)}
#'   ...
#' }
#' @source Woolf et al. (2021)
#' Biochar Permanence Look-Up Table (Approximate)
#'
#' A list containing pre-calculated Fperm matrices for fast approximation.
#' Generated for Soil Temps (-55 to 40 C), H:C (0-0.7), and PyTemp (350-1000 C).
#'
#' @format A list with components:
#' \describe{
#'   \item{hc_grid}{Matrix of Fperm values (Rows: H:C, Cols: SoilTemp)}
#'   \item{temp_grid}{Matrix of Fperm values (Rows: PyTemp, Cols: SoilTemp)}
#'   \item{soil_temps}{Vector of soil temperatures}
#'   \item{hc_vals}{Vector of H:C ratios}
#'   \item{py_temps}{Vector of pyrolysis temperatures}
#' }
"fperm_lut"
#' Adjust TEA Costs based on Fuel Quality (Ash Content)
#'
#' Applies cost penalties for high-ash biomass (e.g., crop residues) which require
#' more expensive boilers (CFB vs Stoker) and have higher O&M/Lower Efficiency.
#'
#' @param params A list of TEA parameters including `bm_ash` (fraction).
#' @return The modified parameter list with updated `bes_capital_cost`, `beccs_capital_cost`,
#' `bes_om_factor`, `beccs_om_factor`, `bes_energy_efficiency`, and `beccs_efficiency`.
#' @export
adjust_costs_for_fuel <- function(params) {
    # Default ash to low (wood chip) if missing
    ash <- if (!is.null(params$bm_ash)) params$bm_ash else 0.01

    # Base Multipliers (1.0 = No Penalty)
    capex_mult <- 1.0
    om_mult <- 1.0
    eff_mult <- 1.0

    # Thresholds based on Literature (Wood < 1-2%, Straw > 5-10%)
    if (ash > 0.05) {
        # High Ash Regime (Straw, Corn Stover)
        # Significant fouling, slagging risk. Requires Fluidized Bed (CFB).
        capex_mult <- 1.25
        om_mult <- 1.50
        eff_mult <- 0.90
    } else if (ash > 0.02) {
        # Medium Ash (Bark, Forest Residues with dirt)
        capex_mult <- 1.10
        om_mult <- 1.20
        eff_mult <- 0.95
    }

    # Apply Multipliers to BES
    if (!is.null(params$bes_capital_cost)) {
        params$bes_capital_cost <- params$bes_capital_cost * capex_mult
    }
    if (!is.null(params$bes_om_factor)) {
        params$bes_om_factor <- params$bes_om_factor * om_mult
    }
    if (!is.null(params$bes_energy_efficiency)) {
        params$bes_energy_efficiency <- params$bes_energy_efficiency * eff_mult
    }

    # Apply Multipliers to BECCS
    if (!is.null(params$beccs_capital_cost)) {
        params$beccs_capital_cost <- params$beccs_capital_cost * capex_mult
    }
    if (!is.null(params$beccs_om_factor)) {
        params$beccs_om_factor <- params$beccs_om_factor * om_mult
    }
    if (!is.null(params$beccs_efficiency)) {
        params$beccs_efficiency <- params$beccs_efficiency * eff_mult
    }

    # Store multipliers for transparency/debugging if needed
    params$fuel_penalty_capex <- capex_mult

    params
}
#' Find Nearest CO2 Sink
#'
#' Calculates the geodesic distance from a given projected location to the nearest
#' potential CO2 storage basin in the database.
#'
#' @param lat Latitude of the project location (decimal degrees).
#' @param lon Longitude of the project location (decimal degrees).
#' @return A list containing:
#'   - `distance_km`: Distance to the nearest sink (numeric).
#'   - `sink_name`: Name of the nearest sink (character).
#'   - `sink_region`: Region of the nearest sink (character).
#' @export
#' @importFrom sf st_as_sf st_nearest_feature st_distance
find_nearest_sink <- function(lat, lon) {
    # Load internal data
    if (!exists("co2_sinks")) {
        try(utils::data("co2_sinks", package = "BiocharAG", envir = environment()), silent = TRUE)
    }

    if (!exists("co2_sinks")) {
        warning("Dataset 'co2_sinks' not found. Using default distance of 100 km.")
        return(list(distance_km = 100, sink_name = "Default", sink_region = "Unknown"))
    }

    # Create point from input
    pt <- sf::st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326)

    # Find nearest feature calculation
    # st_nearest_feature returns index
    nearest_idx <- sf::st_nearest_feature(pt, co2_sinks)
    nearest_sink <- co2_sinks[nearest_idx, ]

    # Calculate distance
    dist_m <- sf::st_distance(pt, nearest_sink)
    dist_km <- as.numeric(dist_m) / 1000

    list(
        distance_km = dist_km,
        sink_name = nearest_sink$Basin,
        sink_region = nearest_sink$Region
    )
}
#' Calculate Annuity Factor
#'
#' Calculates the Present Value of an Annuity Factor.
#' @param discount_rate Discount rate (decimal).
#' @param lifetime Lifetime in years.
#' @return The annuity factor.
#' @export
calculate_annuity_factor <- function(discount_rate, lifetime) {
  if (discount_rate == 0) return(lifetime)
  (1 - (1 / ((1 + discount_rate)^lifetime))) / discount_rate
}

#' Calculate Net Present Value
#'
#' @param cash_flows Vector of cash flows.
#' @param discount_rate Discount rate.
#' @return NPV.
#' @export
calculate_npv <- function(cash_flows, discount_rate) {
  t <- seq_along(cash_flows) - 1 # Assuming start at year 0 or 1? Excel formula suggests annuity.
  # If using annuity factor, we deal with annualized costs.
  # This function is a placeholder for direct cash flow streams if needed.
  sum(cash_flows / (1 + discount_rate)^t)
}
#' Default Parameters for India (North-West)
#'
#' Returns a list of parameters customized for the Indian context (Punjab/Haryana).
#' Key Differences from US:
#' - Lower Labor Costs (affects O&M)
#' - Lower Capital Costs (Construction factor ~0.7)
#' - High Discount Rate (Developing market risk)
#' - Fertilizer Subsidies (Low N price)
#' - Negative/Zero Feedstock Cost (Crop Residue burning prevention)
#'
#' @return A named list of parameters.
#' @export
parameters_india <- function() {
    p <- default_parameters()

    # 1. Financial
    # Higher cost of capital in India
    p$discount_rate <- 0.12

    # 2. Technology / CAPEX
    # Construction in India is cheaper, technology might be imported or domestic.
    # Assume 70% of US Capital Cost context.
    capex_factor <- 0.7
    p$bes_capital_cost <- 3000 * capex_factor
    p$beccs_capital_cost <- 4000 * capex_factor

    # O&M: significantly lower due to labor
    p$bes_om_factor <- 0.025 # vs 0.04
    p$beccs_om_factor <- 0.03 # vs 0.05

    # 3. Biomass Feedstock
    # KEY ASSUMPTION: Crop residue is a nuisance (Stubble Burning).
    # Farmers burn it to clear fields quickly.
    # Cost structure: Collection & Transport only.
    # Payment to farmer might be 0 or negative (subsidy to remove).
    # We assume $0 'stumpage' but standard transport cost logic applies.
    # In our model, 'bc_price' is sale price.
    # Feedstock cost in spatial_tea usually comes from 'feedstock_cost' map or fixed.
    # If we want to simulate negative cost, we can treat it later.
    # For now, let's assume the "Price of Biomass" at field edge is $0.

    # 4. Electricity
    # India Wholesale (APPC): ~ Rs 4-6 / kWh -> ~$0.05 - $0.07 / kWh
    p$elec_price <- 0.06 * 1000 # $/MWh = 60
    # Wholesale factor is 1.0 because we are inputting the Generator price directly
    p$wholesale_discount_factor <- 1.0

    # 5. Fertilizer Substitutes (Subsidized)
    # Urea is heavily subsidized in India. Market price might be $400, Farmer pays $70.
    # Substitution value to farmer is based on SUBSIDIZED price (low).
    # Substitution value to Society (Social Cost) is full price.
    # Let's assess Private Value first (Farmer perspective).
    p$price_n <- 0.30 # $/kg N (Very low due to subsidy)
    p$price_p <- 0.80 # $/kg P
    p$price_k <- 0.40 # $/kg K
    p$price_lime <- 40 # $/Mg (Locally available)

    # 6. Soil
    # Soils in Punjab are often alkaline (pH > 7) -> No Liming Value!
    # But they are low in Organics.
    p$soil_ph_target <- 6.5 # If soil is 7.5, value is 0.

    # 7. Feedstock Characteristics (Rice Straw)
    p$bm_ash <- 0.15 # High Ash triggers BES Cost Penalty (but not BEBCS)

    return(p)
}
#' Default Parameters
#'
#' Returns a list of default parameters used in the BiocharAG model.
#' Values inferred from op_space_2.41.xlsm.
#'
#' @return A named list of parameters.
#' @export
default_parameters <- function() {
  list(
    # BECCS / CCS
    beccs_efficiency = 0.28, # Lower than BES due to capture penalty (35% -> 28%)
    capture_rate = 0.90, # 90% capture efficiency
    ccs_distance = 100, # Transport distance (km)
    ccs_storage_cost = 15, # Injection/Monitoring cost ($/Mg CO2)
    beccs_capital_cost = 4000, # Updated 2024 estimate ($/kW)
    beccs_om_factor = 0.05,

    # Financial
    discount_rate = 0.10, 3, # Excel F9

    # Biomass
    bm_lhv = 18.6, # GJ/Mg (deduced from BES B7 ~ 7.3/0.39 approx or directly input?)
    # Wait, B7 = bm_lhv * eff. If B7=7.32 and eff=0.39 (BECCS B4), then bm_lhv = 7.32/0.39 = 18.76?
    # Actually let's trust common values or precise extraction later.
    # Sheet "bebcs" B4 uses "lignin".
    # For now I will put placeholders closer to typical values.
    bm_c = 0.48, # Carbon fraction
    bm_transport_fixed = 5.0, # $/Mg (Loading/Handling)
    bm_transport_var = 0.15, # $/Mg/km (Trucking)
    bm_ash = 0.05,
    bm_h2o = 0.1, # Moisture content?
    bm_feed_rate = 250, # kg/hr?

    # Prices
    elec_price = 100, # $/MWh ? (In formula F14 it's ElecProd * Price)
    wholesale_discount_factor = 0.4, # Ratio of Wholesale to Retail (Generator Revenue / Retail Rate)
    c_price = 50, # $/tCO2e
    bc_price = 100, # $/t Biochar

    # Operations
    O_M_factor = 0.04, # % of Capex? or similar

    # BES (Modernized 2024 Basis)
    bes_life = 30,
    bes_energy_efficiency = 0.30, # Updated from 0.39 to standard 30% for dedicated biomass
    bes_capital_cost = 3000, # Updated to $3,000/kW (IRENA 2023/24)
    bes_om_factor = 0.04, # 4% of Capex
    ff_c_intensity = 0.05,
    rebound = 0.0,

    # BECCS
    beccs_available = TRUE,
    beccs_eff_penalty = 0.08, # efficiency penalty
    ccs_cc = 500, # CCS capital cost
    beccs_seq_fraction = 0.9,

    # BEBCS (Pyrolysis)
    py_temp = 500,
    py_life = 20,
    py_cc = 500,
    lignin = 0.2, # Fraction
    time_frame = 100, # Years for stability
    bc_stab_factor = 4.6, # From Excel formula J6/J8 logic

    # Advanced Biochar Valuation (Substitutes)
    # Prices based on 2024/2025 Market Averages (Source: tea_literature_review.md)
    price_lime = 60, # $/Mg (Bulk Ag Lime)
    price_n = 0.92, # $/kg N (Derived from Urea ~$425/t)
    price_p = 1.10, # $/kg P2O5 (Derived from DAP ~$675/t)
    price_k = 0.62, # $/kg K2O (Derived from Potash ~$375/t)

    # Biochar Ag Properties (Defaults)
    bc_cce = 0.15, # Calcium Carbonate Equivalent (15%)
    bc_n_content = 0.005, # 0.5% N (Low availability often)
    bc_p_content = 0.002, # 0.2% P (Available)
    bc_k_content = 0.005, # 0.5% K
    ag_impact_duration = 10, # Years (Liming/Nutrient effect duration, < Stability)

    bc_ag_value = 0, # Figure 1 Base Case assumes 0 or low mean.
    bc_valuation_method = "ag_value", # Options: "ag_value" (Shadow Price) or "market_price" (Sale)
    h_c_org = 0.35, # Molar ratio, typical for ~500-600C pyrolysis.

    # Soil / Ag factors
    n_app_rate = 100,
    n2o_factor = 0.01
  )
}
#' Calculate Biochar Permanence (Fperm)
#'
#' Calculates the fraction of biochar carbon remaining after a specified time frame (Fperm),
#' using the method described by Woolf et al. (2021).
#' The function dynamically adjusts reference decay rates to the local soil temperature
#' before fitting a regression model (H:C ratio or Pyrolysis Temperature) to predict stability.
#'
#' @param val The independent variable value (H/Corg ratio or Pyrolysis Temperature).
#' @param method Character string specifying the method: "HC" (Hydrogen:Organic Carbon ratio) or "Temp" (Pyrolysis Temperature). Default is "HC".
#' @param soil_temp Mean annual soil temperature (Celsius). Default is 14.9.
#' @param time_years Time frame for permanence calculation (years). Default is 100.
#' @return Numeric fraction (0 to 1).
#' @importFrom utils read.csv
#' @importFrom stats lm predict coefficients
#' @export
calculate_fperm <- function(val, method = "HC", soil_temp = 14.9, time_years = 100) {
    # 1. Load Reference Data
    # Access the lazy-loaded dataset 'fperm_data'
    # In some testing contexts, it might need explicit loading
    if (!exists("fperm_data")) {
        try(utils::data("fperm_data", package = "BiocharAG", envir = environment()), silent = TRUE)
    }

    # Check if loaded successfully
    if (!exists("fperm_data")) {
        stop("Dataset 'fperm_data' not found. Ensure BiocharAG is properly loaded.")
    }

    # 2. Adjust Reference Data to Target Soil Temperature
    # Q10 function: Q10(T) = 1.1 + 12.0 * exp(-0.19 * T)
    # We need to shift from T_expt to soil_temp.

    # Check if T_expt column exists, default to 20 or similar if missing?
    # The CSV has "Temp_of_experiment".
    t_expt <- fperm_data$Temp_of_experiment
    # Handle NAs? Rmd didn't show NAs handling for T_expt but let's assume valid.

    # Vectorized calculation for efficiency (avoid data.table dependency for now)

    # Calculate Q10_avg (q10)
    # q10 = (Integral(T_soil to T_expt) / (T_expt - T_soil))
    # Integral: 1.1*T - (12/0.19)*exp(-0.19*T)
    # Term at T: 1.1*T + 63.1579*exp(-0.19*T) ... wait sign.
    # Rmd: 1.1(T2 - T1) - 63.1579(exp(-0.19T2) - exp(-0.19T1))
    # Rmd q10 formula line 88:
    # (1.1 *(T_expt-soil_temperature) - 63.1579 * exp(-0.19*T_expt) + 63.1579 *exp(-0.19*soil_temperature)) / (T_expt-soil_temperature)

    delta_t <- t_expt - soil_temp

    # Handle case where delta_t is close to 0 to avoid division by zero
    q10 <- ifelse(abs(delta_t) < 0.001,
        1.1 + 12.0 * exp(-0.19 * soil_temp), # Limit as T_expt -> soil_temp
        (1.1 * delta_t - 63.15789 * (exp(-0.19 * t_expt) - exp(-0.19 * soil_temp))) / delta_t
    )

    # Shift factor fT
    # fT = exp(log(q10) * (soil_temp - T_expt) / 10)
    # Note: shift FROM expt TO soil.
    # If soil < expt (cooling), rate should decrease. Q10 > 1. (Soil - Expt) is negative.
    fT <- exp(log(q10) * (soil_temp - t_expt) / 10)

    # Adjust decay rates k (k1, k2, k3) parameters in CSV
    # Note: C1, C2, C3 are percentages in CSV (based on Rmd line 80 dividing by 100).
    c1 <- fperm_data$C1 / 100
    c2 <- fperm_data$C2 / 100
    c3 <- fperm_data$C3 / 100

    k1_adj <- fperm_data$k1 * fT
    k2_adj <- fperm_data$k2 * fT
    k3_adj <- fperm_data$k3 * fT

    # Calculate Reference Fperm at t = time_years
    fperm_ref <- c1 * exp(-k1_adj * time_years) +
        c2 * exp(-k2_adj * time_years) +
        c3 * exp(-k3_adj * time_years)

    # 3. Fit Regression Model
    if (method == "HC") {
        # Use H:C ratio (Column "H_to_Corg" or "H_C_used"?)
        # Rmd line 79: H_to_Corg renamed to H_C.
        # We'll use H_to_Corg.
        x_var <- fperm_data$H_to_Corg
        valid <- !is.na(x_var) & !is.na(fperm_ref)

        model <- lm(fperm_ref[valid] ~ x_var[valid])

        # Predict for input val
        pred <- coef(model)[1] + coef(model)[2] * val
    } else if (method == "Temp") {
        # Use Pyrolysis Temperature
        x_var <- fperm_data$Pyrolysis_temperature

        # Rmd filtered for Temp >= 350 for regression logic usually?
        # Rmd line 143: Pyrolysis_temperature > 350
        valid <- !is.na(x_var) & !is.na(fperm_ref) & x_var >= 350

        model <- lm(fperm_ref[valid] ~ x_var[valid])

        pred <- coef(model)[1] + coef(model)[2] * val
    } else {
        stop("Invalid method. Choose 'HC' or 'Temp'.")
    }

    # 4. Return Result (clamped 0-1)
    result <- as.numeric(pred) # Drop names
    result <- pmin(1.0, pmax(0.0, result))

    return(result)
}

#' Calculate Approximate Biochar Permanence (Fast)
#'
#' Calculates Fperm using bilinear interpolation over pre-calculated look-up tables (LUTs).
#' This function is significantly faster than `calculate_fperm` but approximates the result.
#'
#' @param val The independent variable value (H/Corg ratio or Pyrolysis Temperature).
#' @param method Character string specifying the method: "HC" (Hydrogen:Organic Carbon ratio) or "Temp" (Pyrolysis Temperature). Default is "HC".
#' @param soil_temp Mean annual soil temperature (Celsius).
#' @return Numeric fraction (0 to 1).
#' @export
calculate_fperm_approx <- function(val, method = "HC", soil_temp) {
    # Ensure LUT is loaded
    if (!exists("fperm_lut")) {
        try(utils::data("fperm_lut", package = "BiocharAG", envir = environment()), silent = TRUE)
    }

    if (!exists("fperm_lut")) {
        stop("Dataset 'fperm_lut' not found. Ensure BiocharAG is properly loaded or data/fperm_lut.rda exists.")
    }

    lut <- fperm_lut

    # Select Grid
    if (method == "HC") {
        grid_z <- lut$hc_grid
        x_grid <- lut$hc_vals

        # Bound input val (H:C should be 0 - 0.7 roughly)
        # If val is outside range, we clamp it to the nearest edge
        # Warning: extrapolation is dangerous, clamping is safer for Fperm.
        val <- pmax(min(x_grid), pmin(max(x_grid), val))
    } else if (method == "Temp") {
        grid_z <- lut$temp_grid
        x_grid <- lut$py_temps

        val <- pmax(min(x_grid), pmin(max(x_grid), val))
    } else {
        stop("Invalid method. Choose 'HC' or 'Temp'.")
    }

    y_grid <- lut$soil_temps
    soil_temp <- pmax(min(y_grid), pmin(max(y_grid), soil_temp))

    # Bilinear Interpolation

    # Find indices
    # findInterval returns index i such that vec[i] <= x < vec[i+1]
    # We need x0 and x1

    idx_x <- findInterval(val, x_grid, all.inside = TRUE)
    idx_y <- findInterval(soil_temp, y_grid, all.inside = TRUE)

    x0 <- x_grid[idx_x]
    x1 <- x_grid[idx_x + 1]

    y0 <- y_grid[idx_y]
    y1 <- y_grid[idx_y + 1]

    # Values at 4 corners
    # Grid structure: Rows = X (Val), Cols = Y (Temp)
    q11 <- grid_z[idx_x, idx_y] # x0, y0
    q21 <- grid_z[idx_x + 1, idx_y] # x1, y0
    q12 <- grid_z[idx_x, idx_y + 1] # x0, y1
    q22 <- grid_z[idx_x + 1, idx_y + 1] # x1, y1

    # Weights
    wx <- (val - x0) / (x1 - x0)
    wy <- (soil_temp - y0) / (y1 - y0)

    # Interpolate
    # Linear in X at y0
    r1 <- q11 * (1 - wx) + q21 * wx
    # Linear in X at y1
    r2 <- q12 * (1 - wx) + q22 * wx

    # Linear in Y
    res <- r1 * (1 - wy) + r2 * wy

    return(as.numeric(res))
}

#' Prepare Fperm 1D Vector (Optimization)
#'
#' Pre-calculates a 1D vector of Fperm values across the entire soil temperature range
#' for a fixed input value (H:C ratio or Pyrolysis Temperature). This object can be
#' passed to `calculate_fperm_vectorized` for highly efficient repeated calculations.
#'
#' @param val The fixed independent variable value (H/Corg ratio or Pyrolysis Temperature).
#' @param method Character string: "HC" or "Temp". Default is "HC".
#' @return A list containing the `vec` (Fperm values) and `y_grid` (Soil Temperatures).
#' @export
prep_fperm_vector <- function(val, method = "HC") {
    # Ensure LUT is loaded
    if (!exists("fperm_lut")) {
        try(utils::data("fperm_lut", package = "BiocharAG", envir = environment()), silent = TRUE)
    }

    if (!exists("fperm_lut")) {
        stop("Dataset 'fperm_lut' not found.")
    }

    lut <- fperm_lut

    if (method == "HC") {
        grid_z <- lut$hc_grid
        x_grid <- lut$hc_vals
    } else if (method == "Temp") {
        grid_z <- lut$temp_grid
        x_grid <- lut$py_temps
    } else {
        stop("Invalid method.")
    }

    # Clamp val to grid bounds
    val <- pmax(min(x_grid), pmin(max(x_grid), val))

    # Find X Interval (once)
    idx_x <- findInterval(val, x_grid, all.inside = TRUE)
    x0 <- x_grid[idx_x]
    x1 <- x_grid[idx_x + 1]
    wx <- (val - x0) / (x1 - x0)

    # Interpolate the entire column vector for this val
    # grid_z is [X, Y], so we take rows idx_x and idx_x+1
    col0 <- grid_z[idx_x, ]
    col1 <- grid_z[idx_x + 1, ]

    fperm_vec <- col0 * (1 - wx) + col1 * wx

    list(vec = fperm_vec, y_grid = lut$soil_temps)
}

#' Calculate Fperm from Pre-calculated Vector (Optimization)
#'
#' Calculates Fperm for a vector of soil temperatures using 1D linear interpolation
#' on a pre-calculated Fperm vector (created by `prep_fperm_vector`).
#'
#' @param prep A list object returned by `prep_fperm_vector`.
#' @param soil_temp Numeric vector of soil temperatures (Celsius).
#' @return Numeric vector of Fperm fractions.
#' @export
calculate_fperm_vectorized <- function(prep, soil_temp) {
    y_grid <- prep$y_grid
    vec <- prep$vec

    # Clamp Temp
    soil_temp <- pmax(min(y_grid), pmin(max(y_grid), soil_temp))

    # 1D Linear Interpolation
    idx_y <- findInterval(soil_temp, y_grid, all.inside = TRUE)

    y0 <- y_grid[idx_y]
    y1 <- y_grid[idx_y + 1]

    # Calculate weight
    wy <- (soil_temp - y0) / (y1 - y0)

    # Interpolate values
    v0 <- vec[idx_y]
    v1 <- vec[idx_y + 1]

    res <- v0 * (1 - wy) + v1 * wy
    return(as.numeric(res))
}
#' Plot RPV vs Carbon Price
#'
#' Generates a plot of Net Present Value (or RPV) for BES, BECCS, and BEBCS
#' across a range of carbon prices, similar to Figure 1 in the article.
#'
#' @param params A list of default parameters.
#' @param c_price_range A numeric vector of carbon prices to simulate (e.g., seq(0, 150, 10)).
#' @param metric Character string, either "RPV" (Relative Present Value) or "NPV" (Net Present Value). Default is "RPV".
#' @return A ggplot object.
#' @import ggplot2
#' @importFrom dplyr bind_rows
#' @export
plot_rpv_vs_c_price <- function(params, c_price_range = seq(0, 150, 10), metric = "RPV") {
    results_df <- data.frame()

    for (cp in c_price_range) {
        # Update carbon price in parameters
        p <- params
        p$c_price <- cp

        # Run models
        bes <- calculate_bes(p)
        beccs <- calculate_beccs(p)
        bebcs <- calculate_bebcs(p)

        # Calculate RPV
        rpv_res <- calculate_rpv(list(bes, beccs, bebcs))
        rpv_res$Carbon_Price <- cp

        results_df <- rbind(results_df, rpv_res)
    }

    # Prepare for plotting
    if (metric == "RPV") {
        y_var <- "RPV"
        y_label <- "Relative Present Value ($/Mg Biomass)"
    } else {
        y_var <- "NPV"
        y_label <- "Net Present Value ($/Mg Biomass)"
    }

    p <- ggplot(results_df, aes(x = Carbon_Price, y = .data[[y_var]], color = Technology)) +
        geom_line(size = 1.2) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        labs(
            title = paste(y_label, "vs Carbon Price"),
            x = "Carbon Price ($/tCO2e)",
            y = y_label
        ) +
        theme_minimal() +
        theme(
            legend.position = "bottom",
            text = element_text(size = 14)
        )

    return(p)
}
#' Calculate Pyrolysis Yields and Energy Balance (Woolf et al. 2016)
#'
#' Implements the sophisticated mass and energy balance from op_space_2.41.xlsm.
#' Mass balance is constrained by elemental conservation (C, H, O).
#' Bio-oil, CO2, and H2O yields are solved to close the balance.
#'
#' @param py_temp Pyrolysis temperature (Celsius).
#' @param lignin Lignin fraction of biomass.
#' @param bm_lhv Biomass LHV (GJ/Mg).
#' @param moisture Biomass moisture fraction.
#' @param ash Biomass ash fraction.
#' @param feed_c Feedstock Carbon fraction (DAF). Default 0.50.
#' @param feed_h Feedstock Hydrogen fraction (DAF). Default 0.06.
#' @param feed_o Feedstock Oxygen fraction (DAF). Default 0.44.
#' @return A list containing yields, HHVs, and net energy flux.
#' @export
calculate_pyrolysis_physics <- function(py_temp, lignin, bm_lhv, moisture = 0.1, ash = 0.05,
                                        feed_c = 0.50, feed_h = 0.06, feed_o = 0.44) {
    T_k <- py_temp + 273.15

    # --- 1. Biochar Yield & Composition ---
    # Yield (DAF basis) - Row 4
    yield_bc <- 0.1260917 + 0.27332 * lignin + 0.5391409 * exp(-0.004 * py_temp)

    # Composition - Rows 5, 6, 7
    bc_c <- 0.99 - 0.78 * exp(-0.0042 * py_temp)
    bc_h <- -0.0041 + 0.1 * exp(-0.0024 * py_temp)
    bc_o <- 1 - bc_c - bc_h

    # --- 2. Gas Yields (Explicit) ---
    # Rows 19, 20, 22, 23
    y_h2 <- 0.029528 * (1 - exp(-0.003496 * T_k))^62.980403
    y_co <- (0.043) / (1 + exp(-0.03 * T_k + 17.2)) + (0.36 - 0.043) / (1 + exp(-0.01 * T_k + 10.9))
    y_ch4 <- 0.07818 * (1 - exp(-0.0033788 * T_k))^30.14865
    y_c2h4 <- 0.035637 * (1 - exp(-0.005221 * T_k))^154.974

    # --- 3. Unaccounted Element Balance (Uc, Uh, Uo) ---
    # "Available" for Oil, CO2, H2O

    # Molar masses
    mm_c <- 12
    mm_h <- 1
    mm_o <- 16
    mm_co <- 28
    mm_ch4 <- 16
    mm_c2h4 <- 28
    mm_co2 <- 44
    mm_h2o <- 18

    # Uc (Row 26) = FeedC - CharC - CO_C - CH4_C - C2H4_C
    uc <- feed_c - (yield_bc * bc_c) -
        (y_co * 12 / 28) - (y_ch4 * 12 / 16) - (y_c2h4 * 24 / 28)

    # Uh (Row 27) = FeedH - CharH - H2 - CH4_H - C2H4_H
    uh <- feed_h - (yield_bc * bc_h) -
        y_h2 - (y_ch4 * 4 / 16) - (y_c2h4 * 4 / 28)

    # Uo (Row 28) = FeedO - CharO - CO_O
    uo <- feed_o - (yield_bc * bc_o) - (y_co * 16 / 28)

    # --- 4. Solve for Bio-oil, CO2, H2O ---
    # Bio-oil Composition (Fixed - Rows 11, 12, 13)
    bo_c <- 0.625
    bo_h <- 0.0756
    bo_o <- 0.2994

    # Bio-oil Yield (Row 10)
    # Y_oil = (Uo - Uc*32/12 - Uh*16/2) / (Oil_O - Oil_C*32/12 - Oil_H*16/2)
    num <- uo - uc * (32 / 12) - uh * (16 / 2)
    den <- bo_o - bo_c * (32 / 12) - bo_h * (16 / 2)
    yield_bo <- num / den

    # CO2 Yield (Row 21)
    # CO2_Y = (Uc - Oil_C * Y_oil) / (12/44)
    yield_co2 <- (uc - bo_c * yield_bo) / (12 / 44)

    # H2O Yield (Row 15 - Reaction Water)
    # H2O_Y = (Uh - Oil_H * Y_oil) / (2/18)
    yield_h2o_rxn <- (uh - bo_h * yield_bo) / (2 / 18)

    # Verify Balance
    # Check if yields are positive. If physics breaks (extrapolation), clamp?
    yield_bc <- pmax(0, yield_bc)
    yield_bo <- pmax(0, yield_bo)
    yield_co2 <- pmax(0, yield_co2)
    # yield_h2o_rxn can be close to 0

    yield_gas_total <- y_h2 + y_co + y_ch4 + y_c2h4 + yield_co2

    # --- 5. Ash & Moisture Adjustment ---
    feed_daf <- 1 - moisture - ash
    mass_bc <- yield_bc * feed_daf + ash # Ash reports to char
    mass_bo <- yield_bo * feed_daf
    mass_gas <- yield_gas_total * feed_daf
    mass_h2o <- (yield_h2o_rxn * feed_daf) + moisture

    # --- 6. Energy Balance ---

    # HHVs
    # Biochar HHV (Dulong) - Row 279
    bc_hhv <- (0.3491 * bc_c + 1.1783 * bc_h - 0.1034 * bc_o) * 100 # MJ/kg = GJ/Mg

    # Bio-oil HHV (Constant) - Row 271 ~ 27.6
    bo_hhv <- 27.63

    # Gas HHV (Weighted Sum) - Row 265
    # Heat of Comb (MJ/kg): H2=141.8, CH4=55.5, CO=10.1, C2H4=50.33
    energy_gas_components <- (y_h2 * 141.8 + y_ch4 * 55.5 + y_co * 10.1 + y_c2h4 * 50.33) # MJ/kg_feedDAF
    # Note: CO2 contributes 0 energy.

    # Total Product Energy (Enthalpy Out per kg Feed DAF)
    # We work in GJ/Mg (same as MJ/kg)

    # Enthalpy of Products (DAF basis * DAF mass)
    e_bc <- yield_bc * bc_hhv * feed_daf
    e_bo <- yield_bo * bo_hhv * feed_daf
    e_gas <- energy_gas_components * feed_daf

    # Heat Losses (Row 31)
    heat_loss <- 2.35 # GJ/Mg feed

    # Process Heat Required
    # Heat_In = H_products + Losses - H_feed
    # H_feed ~ LHV_biomass
    e_products_total <- e_bc + e_bo + e_gas
    heat_required <- e_products_total + heat_loss - bm_lhv

    # Net Energy
    # Try to meet load with Gas + Oil
    e_avail_fuel <- e_gas + e_bo

    if (e_avail_fuel >= heat_required) {
        e_net_fuel <- e_avail_fuel - heat_required
        e_net_bc <- e_bc # All char conserved
    } else {
        e_net_fuel <- 0
        deficit <- heat_required - e_avail_fuel
        # Burn char to meet deficit?
        # Usually avoided, but consistent physics requires checks.
        # Assuming deficit met by char or external.
        e_net_bc <- max(0, e_bc - deficit)
    }

    list(
        yield_bc = mass_bc, # As received mass fraction
        bc_c_content = bc_c, # C fraction of the organic part (simplified)
        # Note: bc_c_content usually refers to C fraction of the final char (including ash).
        # Recalculate C content of final char:
        # Mass C = yield_bc * bc_c * feed_daf
        # Mass Char = mass_bc
        # Final C% = (yield_bc * bc_c * feed_daf) / mass_bc
        bc_c_content_final = (yield_bc * bc_c * feed_daf) / mass_bc,
        energy_net = e_net_fuel,
        energy_char = e_net_bc
    )
}
#' Run Spatial TEA Analysis
#'
#' Runs the Techno-Economic Assessment over a spatial grid defined by a template raster.
#' Calculates locally-specific metrics such as CO2 transport distance and plant scale
#' (based on biomass density).
#'
#' @param template_raster A `SpatRaster` (terra) defining the extent and resolution.
#' @param params A list of baseline parameters.
#' @param spatial_layers A named list of `SpatRaster` objects for spatially-varying parameters.
#'   Likely candidates: `soil_temp` (for Fperm), `biomass_density` (Mg/km2).
#' @param fun The TEA function to run (default: `calculate_beccs`).
#' @param collection_radius_km Radius of biomass collection zone to calculate plant scale
#'   (default: 50 km). Used with `biomass_density` to determine `plant_mw`.
#'
#' @return A `SpatRaster` with layers for key outputs (NPV, Total Cost, Abatement, etc.).
#' @export
#' @importFrom terra as.data.frame rast
run_spatial_tea <- function(template_raster, params, spatial_layers = list(),
                            fun = calculate_beccs, collection_radius_km = 50) {
    if (!inherits(template_raster, "SpatRaster")) {
        stop("template_raster must be a terra SpatRaster object.")
    }

    # 1. Prepare Data Frame from Raster Grid
    # Extract coordinates
    # We process as a data.frame for flexibility with 'sf' distance calcs logic.
    # For very large rasters, 'terra::app' or 'focal' is better, but this wrapper
    # is designed for complexity (calling full TEA models) rather than vectorized speed.

    df <- terra::as.data.frame(template_raster, xy = TRUE, cells = TRUE, na.rm = TRUE)

    # 2. Extract Spatial Layer Values
    for (layer_name in names(spatial_layers)) {
        # Extract values for these cells
        # Improve speed by assuming aligned rasters, or extracting by xy
        vals <- terra::extract(spatial_layers[[layer_name]], df[, c("x", "y")], ID = FALSE)
        df[[layer_name]] <- vals[, 1]
    }

    # 3. Iterate and Calculate
    # Initialize output vectors
    n <- nrow(df)
    net_value_vec <- numeric(n)
    total_cost_vec <- numeric(n)
    abatement_vec <- numeric(n)
    ts_cost_vec <- numeric(n) # NEW
    dist_vec <- numeric(n)
    scale_vec <- numeric(n)

    # Lat/Lon detection
    # Assuming x = Lon, y = Lat (EPSG:4326). If projected, need conversion.
    # Check CRS? For now assume User provides Lat/Lon raster or acceptable coords.
    is_lonlat <- terra::is.lonlat(template_raster)

    for (i in seq_len(n)) {
        if (i %% 500 == 0) message("Processing cell ", i, " / ", n)
        # Copy baseline params
        p <- params

        # Update with spatial params if they exist
        p$lat <- df$y[i]
        p$lat <- df$y[i]
        p$lon <- df$x[i]
        p$collection_radius <- collection_radius_km

        # 3a. Soil Temp (Permenance)
        if ("soil_temp" %in% names(df)) {
            p$soil_temp <- df$soil_temp[i]
        }

        # 3b. Plant Scale from Biomass Density
        if ("biomass_density" %in% names(df)) {
            # Density in Mg/km2
            dens <- df$biomass_density[i]
            if (is.na(dens) || dens <= 0) dens <- 0.1 # Minimum

            # Calculate Annual Feedstock (Mg)
            # Area = pi * r^2
            area_km2 <- pi * collection_radius_km^2
            annual_biomass_feedstock <- dens * area_km2

            # Determine Plant MW Capacity from Feedstock
            # Reverse the logic: Annual = (MW * 8760 * CF) / ElecProd
            # MW = (Annual * ElecProd) / (8760 * CF)

            # We need ElecProd (MWh/Mg).
            # If not in params, use standard approximation.
            # BECCS/BES typical: ~1 MWh/Mg (very roughly).
            # Actually calculated inside the function (bm_lhv * eff * 0.277).
            # Let's pre-calc a reference elec_prod for sizing.
            if (is.null(p$bm_lhv)) p$bm_lhv <- 18.6
            if (is.null(p$bes_energy_efficiency)) p$bes_energy_efficiency <- 0.30
            ref_elec_prod <- p$bm_lhv * p$bes_energy_efficiency * 0.277778

            capacity_factor <- 0.85

            # Calculate and Assign Scaled Plant Capacity
            p$plant_mw <- (annual_biomass_feedstock * ref_elec_prod) / (8760 * capacity_factor)

            # Cap minimum size to avoid divide-by-zero or tiny plants (e.g. 1 MW min)
            if (p$plant_mw < 1) p$plant_mw <- 1
        }

        # 3c. Electricity Price
        if ("elec_price" %in% names(df)) {
            pv <- df$elec_price[i]
            # Apply Wholesale Discount Factor to convert Retail Map to Generator Revenue
            factor <- if (!is.null(p$wholesale_discount_factor)) p$wholesale_discount_factor else 0.4

            if (!is.na(pv) && pv > 0) p$elec_price <- pv * factor
        }

        # 3d. Advanced Ag Parameters (pH, CEC)
        if ("soil_ph" %in% names(df)) {
            val <- df$soil_ph[i]
            if (!is.na(val)) p$soil_ph <- val
        }
        if ("soil_cec" %in% names(df)) {
            val <- df$soil_cec[i]
            if (!is.na(val)) p$soil_cec <- val
        }

        # 3e. CCS Transport Parameters
        if ("dist_sink_km" %in% names(df)) {
            val <- df$dist_sink_km[i]
            if (!is.na(val)) p$dist_sink_km <- val
        }
        if ("dist_sink_saline_km" %in% names(df)) {
            val <- df$dist_sink_saline_km[i]
            if (!is.na(val)) p$dist_sink_saline_km <- val
        }
        if ("sink_is_offshore" %in% names(df)) {
            val <- df$sink_is_offshore[i]
            # Ensure logical
            if (!is.na(val)) p$sink_is_offshore <- as.logical(val)
        }


        # Run TEA
        # Note: TEA function will handle finding nearest sink using p$lat/p$lon
        # if ccs_distance is NULL and it's BECCS.
        # Run TEA
        res <- tryCatch(fun(p), error = function(e) list(net_value = NA, total_cost = NA, tot_c_abatement = NA))

        net_value_vec[i] <- if (is.null(res$net_value)) NA else res$net_value
        total_cost_vec[i] <- if (is.null(res$total_cost)) NA else res$total_cost
        abatement_vec[i] <- if (is.null(res$tot_c_abatement)) NA else res$tot_c_abatement
        # Capture Transport Cost (specific to BECCS)
        ts_cost_vec[i] <- if (is.null(res$ts_cost)) NA else res$ts_cost

        # Store calculated scale/dist if relevant for debugging
        scale_vec[i] <- if (!is.null(p$plant_mw)) p$plant_mw else 50
    }

    # 4. Rasterize Results
    out_r <- terra::rast(template_raster, nlyrs = 4)
    names(out_r) <- c("Net_Value_USD", "Total_Cost_USD_Mg", "Abatement_tCO2", "Transport_Cost_USD_Mg")

    # Fill values
    # Using cell IDs to ensure alignment
    out_r[["Net_Value_USD"]][df$cell] <- net_value_vec
    out_r[["Total_Cost_USD_Mg"]][df$cell] <- total_cost_vec
    out_r[["Abatement_tCO2"]][df$cell] <- abatement_vec
    out_r[["Transport_Cost_USD_Mg"]][df$cell] <- ts_cost_vec

    return(out_r)
}
#' Calculate CO2 Transport Costs (Pipeline vs. Shipping)
#'
#' Implements the technoeconomic cost functions from the "Global Geologic Carbon Storage Assessment".
#' Uses a hybrid routing algorithm: Pipelines for short distance (<1000km), Shipping for long distance.
#'
#' @param mass_flow_mtpa Numeric. Annual CO2 mass flow in Million Tonnes Per Annum (Mtpa).
#' @param distance_km Numeric. Transport distance in kilometers.
#' @param region Character. One of "North America", "Europe", "China", "India".
#' @param is_offshore Logical. If TRUE, applies offshore multipliers to pipeline costs.
#' @param force_mode Character (optional). "pipeline" or "shipping" to override the optimization logic.
#'
#' @return A list containing Total Cost ($/tonne), CAPEX, OPEX, and selected Mode.
#' @export
calc_transport_cost <- function(mass_flow_mtpa, distance_km, region, is_offshore = FALSE, force_mode = NULL) {
    # --- 1. Constants & Physics [cite: 223, 224, 225] ---
    CO2_DENSITY_KG_M3 <- 800 # Dense phase density
    VELOCITY_M_S <- 2.0 # Economic velocity (1.5 - 3.0 m/s)
    PIPELINE_AVAIL <- 0.95 # Availability factor

    # Convert Mass Flow to kg/s
    # 1 Mtpa = 1e9 kg / (365 * 24 * 3600) seconds
    mass_flow_kgs <- (mass_flow_mtpa * 1e9) / (365 * 24 * 3600)

    # --- 2. Regional Factors [cite: 281] ---
    # US = 1.0 (Base), EU = 1.2, China/India = 0.7
    reg_factor <- dplyr::case_when(
        region == "North America" ~ 1.0,
        region == "Europe" ~ 1.2,
        region %in% c("China", "India") ~ 0.7,
        TRUE ~ 1.0
    )

    # --- 3. Mode Selection  ---
    # Recommendation: Pipeline if < 1000km, Shipping if > 1000km (or if offshore distance is vast)
    if (!is.null(force_mode)) {
        mode <- force_mode
    } else {
        if (distance_km > 1000) {
            mode <- "shipping"
        } else {
            mode <- "pipeline"
        }
    }

    # --- 4. Pipeline Cost Model [cite: 222, 228, 230] ---
    if (mode == "pipeline") {
        # A. Hydraulic Design: Calculate Internal Diameter (meters)
        # Area = Flow / (Density * Velocity) -> D = sqrt(4*Area/pi)
        area_m2 <- mass_flow_kgs / (CO2_DENSITY_KG_M3 * VELOCITY_M_S)
        diameter_m <- sqrt((4 * area_m2) / pi)

        # B. CAPEX Calculation (Euro base converted to USD approx 1.1x)
        # Formula: I_pipe (EUR) = (2157 * D_m + 18) * Length_m
        # We convert L to meters
        length_m <- distance_km * 1000

        base_capex_usd <- (2157 * diameter_m + 18) * length_m * 1.1

        # Apply Terrain/Offshore Multipliers
        # Offshore multiplier 1.4 - 1.7
        loc_factor <- if (is_offshore) 1.5 else 1.0

        total_capex <- base_capex_usd * reg_factor * loc_factor

        # C. OPEX Calculation
        # Fixed OPEX: 2.5% of CAPEX [cite: 239]
        opex_fixed <- 0.025 * total_capex

        # Variable OPEX (Compression): ~90 kWh/t for initial, ~7.5 kWh/t/100km for booster [cite: 241, 242]
        # Assuming electricity cost $0.06/kWh (US) to $0.15/kWh (EU). Simplified to $0.10 avg
        elec_price <- 0.10
        energy_per_tonne <- 90 + (7.5 * (distance_km / 100))
        opex_variable_annual <- energy_per_tonne * elec_price * (mass_flow_mtpa * 1e6)

        total_annual_cost <- (total_capex / 20) + opex_fixed + opex_variable_annual # 20yr depreciation
        unit_cost <- total_annual_cost / (mass_flow_mtpa * 1e6)
    } else {
        # --- 5. Shipping Cost Model [cite: 245, 246] ---
        if (mode == "shipping") {
            # A. Liquefaction Cost ($15-$25/t) [cite: 249]
            liq_cost <- 20.0

            # B. Terminal Handling ($10-$20/t) [cite: 252]
            term_cost <- 15.0

            # C. Voyage Cost ($0.02 - $0.05 / t / km) [cite: 255]
            voyage_rate <- 0.035
            voyage_cost <- voyage_rate * distance_km

            # Total Unit Cost
            # Note: Shipping has high OPEX/Variables, lower infrastructure CAPEX scaling
            unit_cost <- liq_cost + term_cost + voyage_cost

            # Apply regional labor discounts to Terminal/Liquefaction operations
            unit_cost <- unit_cost * reg_factor
        }
    }

    return(list( # lintr:ok
        mode = mode,
        unit_cost_usd_per_tonne = round(unit_cost, 2),
        details = paste0("Region: ", region, " | Dist: ", distance_km, "km")
    ))
}


#' Calculate CCS Pipeline Transport Cost
#'
#' Estimates the cost of transporting CO2 via pipeline based on mass flow and distance.
#' Uses a power-law scaling model for economies of scale.
#'
#' Reference: Generally derived from Zero Emissions Platform (ZEP) and similar engineering cost models.
#' CAPEX propto Distance * Capacity^0.5 (Diameter scaling).
#' But typically Cost/t propto Capacity^-0.4 to -0.6.
#'
#' @param co2_mass Annual CO2 mass to transport (Mg/year).
#' @param distance Transport distance (km).
#' @param discount_rate Discount rate (decimal). Default 0.10.
#' @param lifetime Project lifetime (years). Default 20.
#' @return Transport cost ($/Mg CO2).
#' @export
calculate_ccs_transport <- function(co2_mass, distance, is_offshore = FALSE, discount_rate = 0.10, lifetime = 20) {
    if (co2_mass <= 0) {
        return(0)
    }

    # 1. Check for Shipping override (Offshore OR Dist > 1000km)
    # [cite: 23, 258]
    if (is_offshore || distance > 1000) {
        # Shipping Logic (Simplified from calc_transport_cost)
        liq_cost <- 20.0
        term_cost <- 15.0
        voyage_cost <- 0.035 * distance
        # Shipping is mostly OPEX/Unit cost, treated here as $/t directly
        return(liq_cost + term_cost + voyage_cost)
    }

    # 2. Pipeline Hub-and-Spoke Logic (Onshore < 1000km)
    ref_mass <- 1000000
    ref_dist <- 100
    base_capex_ref <- 50000000
    scale_factor <- 0.6
    opex_factor <- 0.04

    feeder_threshold_km <- 50
    trunk_mass_flow <- max(co2_mass, 3000000)
    annuity_fac <- (1 - (1 + discount_rate)^(-lifetime)) / discount_rate

    if (distance > feeder_threshold_km) {
        # Split Distance
        dist_feeder <- feeder_threshold_km
        dist_trunk <- distance - feeder_threshold_km

        # Feeder Leg (User Scale)
        scaler_f <- (co2_mass / ref_mass)^scale_factor
        capex_f <- base_capex_ref * (dist_feeder / ref_dist) * scaler_f

        # Trunk Leg (Efficient Scale)
        scaler_t <- (trunk_mass_flow / ref_mass)^scale_factor
        capex_t_total <- base_capex_ref * (dist_trunk / ref_dist) * scaler_t

        # User Share of Trunk Capex
        capex_t_share <- capex_t_total * (co2_mass / trunk_mass_flow)

        total_capex_share <- capex_f + capex_t_share
    } else {
        # Direct Pipeline
        scaler <- (co2_mass / ref_mass)^scale_factor
        total_capex_share <- base_capex_ref * (distance / ref_dist) * scaler
    }

    annual_capex <- total_capex_share / annuity_fac
    annual_opex <- total_capex_share * opex_factor

    return((annual_capex + annual_opex) / co2_mass)
}
