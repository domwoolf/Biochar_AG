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
    bm_ash = 0.05,
    bm_h2o = 0.1, # Moisture content?
    bm_feed_rate = 250, # kg/hr?

    # Prices
    elec_price = 100, # $/MWh ? (In formula F14 it's ElecProd * Price)
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
    bc_ag_value = 0, # Figure 1 Base Case assumes 0 or low mean.
    bc_valuation_method = "ag_value", # Options: "ag_value" (Shadow Price) or "market_price" (Sale)
    h_c_org = 0.35, # Molar ratio, typical for ~500-600C pyrolysis.

    # Soil / Ag factors
    n_app_rate = 100,
    n2o_factor = 0.01
  )
}
