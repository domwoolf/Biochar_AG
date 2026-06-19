# data-raw/generate_marginal_ci.R
# This script calculates the marginal carbon intensity (CI) of new electricity generation capacity 
# by country and US state by analyzing recent growth (2019-2024) across generation sources.
# It also ingests average grid CIs as a fallback for regions without growth or missing data.

library(dplyr)
library(tidyr)

message("======================================================================")
message("Starting Marginal Carbon Intensity Data Processing Pipeline...")
message("======================================================================")

# ------------------------------------------------------------------------------
# 1. Load Datasets
# ------------------------------------------------------------------------------
gen_path <- "BiocharAG/data-raw/generation-including-net-imports-monthly.csv"
avg_ci_path <- "BiocharAG/data-raw/carbon-intensity-electricity.csv"

if (!file.exists(gen_path)) stop("Monthly generation dataset not found at: ", gen_path)
if (!file.exists(avg_ci_path)) stop("Average carbon intensity dataset not found at: ", avg_ci_path)

message("Loading monthly generation data...")
df_gen <- read.csv(gen_path, check.names = FALSE, stringsAsFactors = FALSE)

message("Loading average carbon intensity data...")
df_avg_ci <- read.csv(avg_ci_path, stringsAsFactors = FALSE)

# Rename the first three columns of the generation dataset for easier access
colnames(df_gen)[1] <- "Code"
colnames(df_gen)[2] <- "Name"
colnames(df_gen)[3] <- "Source"

# ------------------------------------------------------------------------------
# 2. Reshape and Melt Monthly Data
# ------------------------------------------------------------------------------
message("Melting monthly generation data into long format...")
month_cols <- grep("^20", colnames(df_gen), value = TRUE)

df_melt <- df_gen %>%
  select(Code, Name, Source, all_of(month_cols)) %>%
  pivot_longer(
    cols = all_of(month_cols),
    names_to = "Month",
    values_to = "Generation"
  ) %>%
  mutate(
    Generation = as.numeric(Generation),
    Year = substr(Month, 1, 4)
  ) %>%
  filter(!is.na(Generation))

# ------------------------------------------------------------------------------
# 3. Standardize Names and US State Mapping
# ------------------------------------------------------------------------------
message("Standardizing region and US state names...")

us_state_map <- c(
  "AL" = "Alabama", "AK" = "Alaska", "AZ" = "Arizona", "AR" = "Arkansas", "CA" = "California",
  "CO" = "Colorado", "CT" = "Connecticut", "DE" = "Delaware", "FL" = "Florida", "GA" = "Georgia",
  "HI" = "Hawaii", "ID" = "Idaho", "IL" = "Illinois", "IN" = "Indiana", "IA" = "Iowa",
  "KS" = "Kansas", "KY" = "Kentucky", "LA" = "Louisiana", "ME" = "Maine", "MD" = "Maryland",
  "MA" = "Massachusetts", "MI" = "Michigan", "MN" = "Minnesota", "MS" = "Mississippi", "MO" = "Missouri",
  "MT" = "Montana", "NE" = "Nebraska", "NV" = "Nevada", "NH" = "New Hampshire", "NJ" = "New Jersey",
  "NM" = "New Mexico", "NY" = "New York", "NC" = "North Carolina", "ND" = "North Dakota", "OH" = "Ohio",
  "OK" = "Oklahoma", "OR" = "Oregon", "PA" = "Pennsylvania", "RI" = "Rhode Island", "SC" = "South Carolina",
  "SD" = "South Dakota", "TN" = "Tennessee", "TX" = "Texas", "UT" = "Utah", "VT" = "Vermont",
  "VA" = "Virginia", "WA" = "Washington", "WV" = "West Virginia", "WI" = "Wisconsin", "WY" = "Wyoming",
  "DC" = "District of Columbia"
)

standardize_us_state <- function(code) {
  suffix <- sub("US-", "", code)
  if (suffix %in% names(us_state_map)) {
    return(us_state_map[[suffix]])
  }
  return(code)
}

df_melt <- df_melt %>%
  mutate(Clean_Name = case_when(
    Name == "People's Republic of China" ~ "China",
    Name == "Republic of China (Taiwan)" ~ "Taiwan",
    Name == "Bosnia & Herzegovina" ~ "Bosnia and Herzegovina",
    grepl("^Special region: US-", Name) ~ sapply(Code, standardize_us_state),
    TRUE ~ Name
  ))

# ------------------------------------------------------------------------------
# 4. Filter and Aggregate by Year
# ------------------------------------------------------------------------------
# IPCC default values of lifecycle carbon intensity by generation type (gCO2eq / kWh)
ipcc_ci <- c(
  coal = 820,
  gas = 490,
  biofuels = 230,
  geothermal = 38,
  hydro = 24,
  nuclear = 12,
  solar = 48,
  wind = 11.5,
  oil = 700
)

message("Filtering primary mutually-exclusive sources and aggregating by year...")
df_primary <- df_melt %>%
  filter(Source %in% names(ipcc_ci))

df_yr <- df_primary %>%
  group_by(Code, Clean_Name, Source, Year) %>%
  summarize(Generation = sum(Generation, na.rm = TRUE), .groups = "drop")

# ------------------------------------------------------------------------------
# 5. Extract Recent 5-Year Window (2019 to 2024)
# ------------------------------------------------------------------------------
start_year <- "2019"
end_year <- "2024"
message(sprintf("Calculating generation change (delta) between %s and %s...", start_year, end_year))

df_start <- df_yr %>%
  filter(Year == start_year) %>%
  select(Code, Clean_Name, Source, Gen_Start = Generation)

df_end <- df_yr %>%
  filter(Year == end_year) %>%
  select(Code, Clean_Name, Source, Gen_End = Generation)

# Calculate the change in generation (delta)
df_delta <- full_join(df_start, df_end, by = c("Code", "Clean_Name", "Source")) %>%
  mutate(
    Gen_Start = replace_na(Gen_Start, 0),
    Gen_End = replace_na(Gen_End, 0),
    Delta_Gen = Gen_End - Gen_Start
  )

# Isolate sources adding new capacity (Delta_Gen > 0)
df_growth <- df_delta %>%
  filter(Delta_Gen > 0) %>%
  mutate(CI = ipcc_ci[Source]) %>%
  mutate(Emissions_Added = Delta_Gen * CI)

# Aggregate by country/state to find the weighted marginal CI
df_marginal <- df_growth %>%
  group_by(Code, Clean_Name) %>%
  summarize(
    Total_Delta_Gen = sum(Delta_Gen, na.rm = TRUE),
    Total_Emissions_Added = sum(Emissions_Added, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Marginal_CI = Total_Emissions_Added / Total_Delta_Gen)

# ------------------------------------------------------------------------------
# 6. Process Average Grid Carbon Intensity Fallback
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
# 7. Merge and Apply Fallback Logic
# ------------------------------------------------------------------------------
message("Merging marginal and average datasets...")

df_merged <- full_join(
  df_marginal, 
  df_avg_latest, 
  by = "Clean_Name"
)

# Populate Code and Name for any rows that were only present in average dataset
df_merged <- df_merged %>%
  mutate(
    Code = if_else(is.na(Code), Avg_Code, Code),
    Code = if_else(Clean_Name == "China", "CN", Code),
    Code = if_else(Clean_Name == "India", "IN", Code),
    Code = if_else(Clean_Name == "United States", "US", Code)
  )

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
# 8. Convert Units and Select Output Columns
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
# 9. Save Output CSV
# ------------------------------------------------------------------------------
out_csv <- "BiocharAG/data-raw/marginal_ci_by_country.csv"
message("Saving results to: ", out_csv)
write.csv(df_final, out_csv, row.names = FALSE)

message("======================================================================")
message("Marginal CI data processing completed successfully!")
message("======================================================================")
