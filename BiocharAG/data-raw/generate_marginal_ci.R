# data-raw/generate_marginal_ci.R
# This script calculates the marginal carbon intensity (CI) of new electricity generation capacity 
# by country and US state by analyzing recent growth (2019-2024) across generation sources.
# It also ingests average grid CIs as a fallback for regions without growth or missing data.
#
# Provenance (issue #5): R port of the original Python build-margin script (Ember monthly generation,
# 2018-2023 window, IPCC AR5 median life-cycle intensities: Schlomer et al. 2014). Changes since: window
# moved to 2019-2024; bioenergy set to 0 g/kWh instead of 230 (its emissions are counted explicitly in the
# TEA); the original Paris-aligned bound (displaced generation = nuclear, 12 g/kWh) is superseded by the
# NGFS-calibrated price-response curve MEF(P), whose floor is MEF_min = 20 g/kWh (Pehl et al. 2017).

library(dplyr)
library(tidyr)

message("======================================================================")
message("Starting Marginal Carbon Intensity Data Processing Pipeline...")
message("======================================================================")

# ------------------------------------------------------------------------------
# 1. Load Datasets
# ------------------------------------------------------------------------------
# Ember annual electricity data (long format): countries (yearly_full_release_long_format.csv, TWh) and
# US states (us_yearly_full_release_long_format.csv, GWh), downloaded from
# https://storage.googleapis.com/emb-prod-bkt-publicdata/public-downloads/ (June 2026 release).
# The annual series cover 2019-2024 for all US states and for countries whose monthly series start later
# (e.g. Moldova from May 2020, US states from January 2023); with the monthly file, a missing start year
# counted the whole end-year generation as growth, so those anchors were average mixes, not build margins.
gen_path <- "BiocharAG/data-raw/ember_yearly_full_release_long_format.csv"     # raw download (not tracked)
us_path <- "BiocharAG/data-raw/ember_us_yearly_full_release_long_format.csv"   # raw download (not tracked)
extract_path <- "BiocharAG/data-raw/ember_annual_generation_by_fuel.csv"       # tracked extract, 2015-2025
avg_ci_path <- "BiocharAG/data-raw/carbon-intensity-electricity.csv"
if (!file.exists(avg_ci_path)) stop("Dataset not found: ", avg_ci_path)
df_avg_ci <- read.csv(avg_ci_path, stringsAsFactors = FALSE)

# Generation by fuel, TWh; Code = ISO 3166-1 alpha-3 for countries, "US-<state>" for US states. Rebuilt
# from the raw Ember files when they are present; otherwise read from the tracked extract.
if (file.exists(gen_path) && file.exists(us_path)) {
  message("Loading Ember annual generation data (raw downloads) and writing the extract...")
  gen_c <- read.csv(gen_path, check.names = FALSE, stringsAsFactors = FALSE)
  gen_s <- read.csv(us_path, check.names = FALSE, stringsAsFactors = FALSE)
  df_yr <- rbind(
    gen_c %>%
      filter(`Area type` == "Country or economy", Category == "Electricity generation", Subcategory == "Fuel",
             Unit == "TWh") %>%
      transmute(Code = `ISO 3 code`, Clean_Name = Area, Source = Variable, Year = as.character(Year), Generation = Value),
    gen_s %>%
      filter(`State type` == "state", Category == "Electricity generation", Subcategory == "Fuel", Unit == "GWh") %>%
      transmute(Code = paste0("US-", `State code`), Clean_Name = State, Source = Variable, Year = as.character(Year),
                Generation = Value / 1000)
  ) %>%
    filter(!is.na(Generation), !is.na(Code), Code != "", as.integer(Year) >= 2015)
  write.csv(df_yr, extract_path, row.names = FALSE)
} else {
  if (!file.exists(extract_path)) stop("Neither the raw Ember files nor ", extract_path, " were found.")
  message("Loading Ember annual generation extract...")
  df_yr <- read.csv(extract_path, stringsAsFactors = FALSE, colClasses = c(Year = "character"))
}

# ------------------------------------------------------------------------------
# 2. Life-cycle intensities by fuel
# ------------------------------------------------------------------------------
# IPCC median life-cycle intensities (gCO2eq/kWh; Schlomer et al. 2014). Bioenergy is set to 0 (IPCC: 230):
# bioenergy emissions are accounted for explicitly in the TEA, and the NGFS-calibrated MEF(P) curve
# (ngfs_mef_fit.py) uses the same convention, so the anchor and the curve shape share one basis. Growing
# bioenergy generation still counts in the build-margin denominator. Ember's "Other Fossil" (mostly oil)
# takes the oil value and "Other Renewables" (mostly geothermal) the geothermal value.
ipcc_ci <- c(
  Coal = 820,
  Gas = 490,
  Bioenergy = 0,
  `Other Renewables` = 38,
  Hydro = 24,
  Nuclear = 12,
  Solar = 48,
  Wind = 11.5,
  `Other Fossil` = 700
)
df_yr <- df_yr %>% filter(Source %in% names(ipcc_ci))

# ------------------------------------------------------------------------------
# 3. Build margin over 2019-2024
# ------------------------------------------------------------------------------
start_year <- "2019"
end_year <- "2024"
message(sprintf("Calculating generation change (delta) between %s and %s...", start_year, end_year))

# Only regions with generation data in both years get a build margin (others fall back to the average CI)
has_both <- df_yr %>%
  group_by(Code) %>%
  summarize(ok = sum(Generation[Year == start_year], na.rm = TRUE) > 0 &
              sum(Generation[Year == end_year], na.rm = TRUE) > 0, .groups = "drop") %>%
  filter(ok) %>%
  pull(Code)

df_start <- df_yr %>%
  filter(Year == start_year, Code %in% has_both) %>%
  select(Code, Clean_Name, Source, Gen_Start = Generation)

df_end <- df_yr %>%
  filter(Year == end_year, Code %in% has_both) %>%
  select(Code, Clean_Name, Source, Gen_End = Generation)

df_delta <- full_join(df_start, df_end, by = c("Code", "Clean_Name", "Source")) %>%
  mutate(
    Gen_Start = replace_na(Gen_Start, 0),
    Gen_End = replace_na(Gen_End, 0),
    Delta_Gen = Gen_End - Gen_Start
  )

# Sources whose generation grew (Delta_Gen > 0) define the build margin
df_growth <- df_delta %>%
  filter(Delta_Gen > 0) %>%
  mutate(CI = ipcc_ci[Source]) %>%
  mutate(Emissions_Added = Delta_Gen * CI)

df_marginal <- df_growth %>%
  group_by(Code, Clean_Name) %>%
  summarize(
    Total_Delta_Gen = sum(Delta_Gen, na.rm = TRUE),
    Total_Emissions_Added = sum(Emissions_Added, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Marginal_CI = Total_Emissions_Added / Total_Delta_Gen)

# ------------------------------------------------------------------------------
# 4. Average grid carbon intensity (fallback)
# ------------------------------------------------------------------------------
message("Processing average grid carbon intensity fallback layer...")
df_avg_latest <- df_avg_ci %>%
  group_by(Entity) %>%
  filter(Year == max(Year)) %>%
  ungroup() %>%
  select(Entity, Avg_Code = Code, Avg_Year = Year, Average_CI = Carbon.intensity.of.electricity.per.kWh) %>%
  mutate(Clean_Name = case_when(
    Entity == "United States" ~ "United States",
    Entity == "China" ~ "China",
    Entity == "India" ~ "India",
    TRUE ~ Entity
  ))

# ------------------------------------------------------------------------------
# 5. Merge and apply the fallback
# ------------------------------------------------------------------------------
message("Merging marginal and average datasets...")

# Join on the ISO 3166-1 alpha-3 code (both Ember and OWID use it), not on names
df_merged <- full_join(
  df_marginal,
  df_avg_latest %>% filter(!is.na(Avg_Code), Avg_Code != "") %>% select(-Clean_Name),
  by = c("Code" = "Avg_Code")
) %>%
  mutate(Clean_Name = if_else(is.na(Clean_Name), Entity, Clean_Name))

# Extract national average grid CI for United States to use as fallback for US States
us_avg_ci <- df_avg_latest %>%
  filter(Clean_Name == "United States") %>%
  pull(Average_CI)

if (length(us_avg_ci) > 0) {
  us_avg_ci <- us_avg_ci[1]
} else {
  us_avg_ci <- 370.0 # Default fallback if missing
}

# Fill missing Average_CI for US states with US national average
df_merged <- df_merged %>%
  mutate(
    Average_CI = if_else(is.na(Average_CI) & grepl("^US-", Code), us_avg_ci, Average_CI)
  )

# Calculate final merged CI (Marginal CI as primary, Average CI as fallback)
df_merged <- df_merged %>%
  mutate(
    Merged_CI = case_when(
      !is.na(Marginal_CI) & Marginal_CI > 0 ~ Marginal_CI,
      !is.na(Average_CI) ~ Average_CI,
      TRUE ~ 400.0 # Global fallback baseline
    )
  )

# ------------------------------------------------------------------------------
# 6. Convert units and select output columns
# ------------------------------------------------------------------------------
message("Converting units and formatting output...")
# 1 gCO2eq/kWh = 1/3600 tCO2eq/GJ
df_final <- df_merged %>%
  mutate(
    Marginal_CI_gCO2_kWh = Marginal_CI,
    Marginal_CI_tCO2_GJ  = Marginal_CI / 3600,
    Average_CI_gCO2_kWh  = Average_CI,
    Average_CI_tCO2_GJ   = Average_CI / 3600,
    Merged_CI_gCO2_kWh   = Merged_CI,
    Merged_CI_tCO2_GJ    = Merged_CI / 3600
  ) %>%
  select(
    Code,
    Name = Clean_Name,
    Total_Delta_Gen,
    Total_Emissions_Added,
    Marginal_CI_gCO2_kWh,
    Marginal_CI_tCO2_GJ,
    Average_CI_gCO2_kWh,
    Average_CI_tCO2_GJ,
    Merged_CI_gCO2_kWh,
    Merged_CI_tCO2_GJ
  ) %>%
  filter(!is.na(Code) & Code != "") %>%
  arrange(Code)

# ------------------------------------------------------------------------------
# 7. Save output CSV
# ------------------------------------------------------------------------------
out_csv <- "BiocharAG/data-raw/marginal_ci_by_country.csv"
message("Saving results to: ", out_csv)
write.csv(df_final, out_csv, row.names = FALSE)

message("======================================================================")
message("Marginal CI data processing completed successfully!")
message("======================================================================")
