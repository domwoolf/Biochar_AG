# BiocharAG/data-raw/generate_parameters.R
# Generates the default_parameters dataset and exports parameters.csv

# Define the base default parameters list
default_parameters <- list(
  # Financial
  discount_rate = 0.08,
  plant_mw_th = 50,
  O_M_factor = 0.04,

  # Biomass
  bm_lhv = 18.6,
  bm_c = 0.48,
  bm_transport_fixed = 5.0,
  bm_transport_var = 0.15,
  bm_ash = 0.05,
  bm_h2o = 0.1,
  bm_feed_rate = 250,

  # Prices
  elec_price = 100,
  wholesale_discount_factor = 0.4,
  c_price = 50,
  bc_price = 100,
  price_lime = 60,
  price_n = 0.92,
  price_p = 1.10,
  price_k = 0.62,

  # BES (Modernized 2024 Basis)
  bes_life = 30,
  bes_energy_efficiency = 0.30,
  bes_capital_cost = 3000,
  bes_om_factor = 0.04,
  ff_c_intensity = 12 / 3600,
  use_flat_ci = FALSE,
  flat_ci_tCO2_GJ = 12 / 3600,
  optimize_scale = FALSE,
  plant_sizes_mw_th = c(5, 25, 50, 100, 250, 500),
  rebound = 0.0,

  # BECCS
  beccs_available = TRUE,
  beccs_eff_penalty = 0.08,
  ccs_cc = 500,
  beccs_seq_fraction = 0.9,
  beccs_efficiency = 0.28,
  capture_rate = 0.90,
  ccs_distance = 100,
  ccs_storage_cost = 15,
  beccs_capital_cost = 4000,
  beccs_om_factor = 0.05,
  early_adoption = FALSE,
  allow_eor = FALSE,

  # BEBCS (Pyrolysis)
  py_temp = 500,
  py_life = 20,
  py_cc = 500,
  lignin = 0.2,
  time_frame = 100,
  bc_stab_factor = 4.6,

  # Biochar Ag Properties (Defaults)
  bc_cce = 0.15,
  bc_n_content = 0.005,
  bc_p_content = 0.002,
  bc_k_content = 0.005,
  ag_impact_duration = 10,
  bc_ag_value = 0,
  bc_valuation_method = "advanced_mechanistic",
  h_c_org = 0.35,

  # Soil / Ag factors
  n_app_rate = 100,
  n2o_factor = 0.01
)

# Build the data frame for the CSV
df <- data.frame(
  name = names(default_parameters),
  description = character(length(default_parameters)),
  units = character(length(default_parameters)),
  default_value = character(length(default_parameters)),
  distribution = character(length(default_parameters)),
  dispersion = rep(NA_real_, length(default_parameters)),
  minimum = rep(NA_real_, length(default_parameters)),
  maximum = rep(NA_real_, length(default_parameters)),
  notes = character(length(default_parameters)),
  stringsAsFactors = FALSE
)

# Helper function to populate row
populate_row <- function(p_name, desc, unit, dist, note = "") {
  idx <- which(df$name == p_name)
  if (length(idx) == 0) stop(paste("Parameter not found:", p_name))
  
  df$description[idx] <<- desc
  df$units[idx] <<- unit
  
  val <- default_parameters[[p_name]]
  if (is.numeric(val) && length(val) > 1) {
    df$default_value[idx] <<- paste(val, collapse = ",")
  } else {
    df$default_value[idx] <<- as.character(val)
  }
  
  df$distribution[idx] <<- dist
  df$notes[idx] <<- note
}

# Populate data
populate_row("discount_rate", "Discount rate", "fraction", "uniform", "High Priority. Cost of capital varies wildly by country risk profiles.")
populate_row("plant_mw_th", "Plant thermal capacity", "MWth", "none")
populate_row("O_M_factor", "Operations & Maintenance", "% of Capex", "uniform")

populate_row("bm_lhv", "Biomass lower heating value", "GJ/Mg", "normal", "Medium Priority. Varies by regional feedstock (e.g., rice straw vs corn stover).")
populate_row("bm_c", "Carbon fraction of biomass", "fraction", "normal")
populate_row("bm_transport_fixed", "Fixed transport cost (Loading/Handling)", "$/Mg", "normal")
populate_row("bm_transport_var", "Variable transport cost (Trucking)", "$/Mg/km", "normal")
populate_row("bm_ash", "Biomass ash content", "fraction", "normal", "Medium Priority. Varies by regional feedstock.")
populate_row("bm_h2o", "Biomass moisture content", "fraction", "normal")
populate_row("bm_feed_rate", "Biomass feed rate", "kg/hr", "normal")

populate_row("elec_price", "Electricity price", "$/MWh", "normal")
populate_row("wholesale_discount_factor", "Ratio of Wholesale to Retail", "ratio", "uniform")
populate_row("c_price", "Carbon price", "$/tCO2e", "normal")
populate_row("bc_price", "Biochar price", "$/t Biochar", "normal")
populate_row("price_lime", "Bulk Ag Lime price", "$/Mg", "normal", "Low Priority. Varies by local transport distance.")
populate_row("price_n", "Nitrogen fertilizer price", "$/kg N", "normal", "High Priority. Massive regional variations due to subsidies and tariffs.")
populate_row("price_p", "Phosphorus fertilizer price", "$/kg P2O5", "normal", "High Priority. Massive regional variations due to subsidies and tariffs.")
populate_row("price_k", "Potassium fertilizer price", "$/kg K2O", "normal", "High Priority. Massive regional variations due to subsidies and tariffs.")

populate_row("bes_life", "BES project lifetime", "years", "none")
populate_row("bes_energy_efficiency", "BES energy efficiency", "fraction", "uniform")
populate_row("bes_capital_cost", "BES capital cost", "$/kW", "log-normal", "Medium Priority. Differ significantly due to local labor/material costs.")
populate_row("bes_om_factor", "BES O&M factor", "fraction", "uniform", "Medium Priority. Highly labor-dependent.")
populate_row("ff_c_intensity", "Fossil fuel carbon intensity", "tCO2eq/GJ", "uniform")
populate_row("use_flat_ci", "Use flat carbon intensity", "logical", "none")
populate_row("flat_ci_tCO2_GJ", "Flat carbon intensity value", "tCO2eq/GJ", "uniform")
populate_row("optimize_scale", "Optimize plant scale", "logical", "none")
populate_row("plant_sizes_mw_th", "Available plant sizes", "MWth", "none")
populate_row("rebound", "Rebound effect", "fraction", "uniform")

populate_row("beccs_available", "Is BECCS available", "logical", "none")
populate_row("beccs_eff_penalty", "BECCS efficiency penalty", "fraction", "uniform")
populate_row("ccs_cc", "CCS capital cost", "$/kW", "log-normal")
populate_row("beccs_seq_fraction", "BECCS sequestration fraction", "fraction", "uniform")
populate_row("beccs_efficiency", "BECCS efficiency", "fraction", "uniform")
populate_row("capture_rate", "CO2 capture rate", "fraction", "uniform", "Validation needed: Real-world data is sparse. Recommend sensitivity analysis (85-95%).")
populate_row("ccs_distance", "CCS transport distance", "km", "log-normal")
populate_row("ccs_storage_cost", "CCS injection/monitoring cost", "$/Mg CO2", "log-normal", "High Priority. Geologic storage costs vary drastically. Consider spatial raster.")
populate_row("beccs_capital_cost", "BECCS capital cost", "$/kW", "log-normal", "Medium Priority. Differ significantly due to local labor/material costs.")
populate_row("beccs_om_factor", "BECCS O&M factor", "fraction", "uniform", "Medium Priority. Highly labor-dependent.")
populate_row("early_adoption", "BECCS early adoption scenario", "logical", "none")
populate_row("allow_eor", "Allow Enhanced Oil Recovery", "logical", "none")

populate_row("py_temp", "Pyrolysis temperature", "°C", "uniform")
populate_row("py_life", "Pyrolysis project lifetime", "years", "none")
populate_row("py_cc", "Pyrolysis capital cost", "$/kW", "log-normal")
populate_row("lignin", "Lignin fraction", "fraction", "uniform")
populate_row("time_frame", "Time frame for stability", "years", "none")
populate_row("bc_stab_factor", "Biochar stability factor", "factor", "uniform", "Validation needed: Ensure aligns with IPCC or verified carbon standard methodologies.")

populate_row("bc_cce", "Calcium Carbonate Equivalent", "fraction", "uniform", "Validation needed: Does 15% hold true globally, or should it scale with bm_ash?")
populate_row("bc_n_content", "Biochar Nitrogen content", "fraction", "uniform")
populate_row("bc_p_content", "Biochar Phosphorus content", "fraction", "uniform")
populate_row("bc_k_content", "Biochar Potassium content", "fraction", "uniform")
populate_row("ag_impact_duration", "Agronomic impact duration", "years", "uniform", "Medium Priority. Duration depends heavily on local climate and soil. 10-year flat assumption poorly validated.")
populate_row("bc_ag_value", "Biochar agronomic value", "$/Mg", "normal")
populate_row("bc_valuation_method", "Biochar valuation method", "character", "none")
populate_row("h_c_org", "H:C organic molar ratio", "ratio", "uniform")

populate_row("n_app_rate", "Nitrogen application rate", "kg/ha", "normal")
populate_row("n2o_factor", "N2O emission factor", "fraction", "uniform", "Validation needed: Ensure model accounts for biochar-induced N2O suppression.")

# Write CSV
dir.create("data-raw", showWarnings = FALSE)
write.csv(df, "data-raw/parameters.csv", row.names = FALSE)

# Export dataset
usethis::use_data(default_parameters, internal = FALSE, overwrite = TRUE)
