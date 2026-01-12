# # data-raw/generate_sinks.R

# Block below commented out, as deprecated.  TODO: Remove later
# library(sf)
# library(dplyr)
# # Approximate centroids of major CO2 storage basins (NETL / CO2StoP / Global)
# sinks_list <- tribble(
#     ~Region, ~Basin, ~Lat, ~Lon,
#     "North America", "Illinois Basin", 39.0, -89.0,
#     "North America", "Permian Basin", 31.5, -103.0,
#     "North America", "Gulf Coast", 29.5, -95.0,
#     "North America", "Williston Basin", 47.5, -103.5,
#     "North America", "Alberta Basin", 54.0, -114.0,
#     "Europe", "North Sea (Sleipner/Aurora)", 58.5, 1.9,
#     "Europe", "Rotterdam/Porthos", 51.9, 4.0,
#     "Europe", "Adriatic", 44.5, 13.0,
#     "Asia", "Ordos Basin (China)", 38.0, 109.0,
#     "Asia", "Songliao Basin (China)", 45.0, 125.0,
#     "Asia", "Cambay Basin (India)", 22.5, 72.5,
#     "Asia", "Bombay High (India)", 19.5, 71.3
# )
# # Convert to sf object
# co2_sinks <- st_as_sf(sinks_list, coords = c("Lon", "Lat"), crs = 4326)
# usethis::use_data(co2_sinks, overwrite = TRUE)

library(sf)
library(dplyr)
library(tibble)
library(usethis)

# ==============================================================================
# Global Carbon Sink Database
# Source: Global Geologic Carbon Storage Assessment & Technoeconomic Transport Modeling
# Table 1: Comprehensive Global Storage Basin and Sink List
# ==============================================================================

sinks_list <- tribble(
    ~Region, ~Basin_Name, ~Sub_Unit, ~Type, ~Lat, ~Lon, ~Regional_Factor, ~Is_EOR, ~Notes,

    # --- NORTH AMERICA (USA) ---
    "North America", "Gulf Coast Basin", "Frio/Miocene Sands", "Onshore", 29.5, -95.0, 1.0, FALSE, "Premier global hub; <$10/t transport",
    "North America", "Permian Basin", "San Andres/Clearfork", "Onshore", 31.5, -103.5, 1.0, TRUE, "EOR & Saline; Existing pipeline network",
    "North America", "Illinois Basin", "Mt. Simon Sandstone", "Onshore", 39.8, -89.0, 1.0, FALSE, "Proven by Decatur ADM project",
    "North America", "Williston Basin", "Madison/Broom Creek", "Onshore", 47.5, -103.0, 1.0, TRUE, "Weyburn-Midale region (EOR)",
    "North America", "Michigan Basin", "St. Peter Sandstone", "Onshore", 44.0, -85.0, 1.0, FALSE, "Saline capacity",
    "North America", "Appalachian Basin", "Oriskany/Rose Run", "Onshore", 40.0, -80.0, 1.0, FALSE, "Critical for East Coast ind. corridor",
    "North America", "Powder River Basin", "Muddy Sandstone", "Onshore", 44.5, -105.5, 1.0, TRUE, "Wyoming coal/EOR belt",
    "North America", "San Juan Basin", "Entrada Sandstone", "Onshore", 36.5, -107.5, 1.0, FALSE, "Four Corners region",
    "North America", "Anadarko Basin", "Granite Wash", "Onshore", 35.5, -99.0, 1.0, TRUE, "Oklahoma/Texas Panhandle (EOR)",

    # --- EUROPE (Offshore Focus) ---
    "Europe", "Northern North Sea (NO)", "Utsira Formation", "Offshore", 58.4, 1.9, 1.2, FALSE, "Sleipner site; Massive aquifer",
    "Europe", "Northern North Sea (NO)", "Johansen Formation", "Offshore", 60.5, 3.5, 1.2, FALSE, "Northern Lights / Aurora",
    "Europe", "Southern North Sea (NL)", "P18/P15 Fields", "Offshore", 52.0, 3.5, 1.2, FALSE, "Porthos (Rotterdam)",
    "Europe", "Southern North Sea (UK)", "Goldeneye/Viking", "Offshore", 53.5, 2.0, 1.2, FALSE, "UK Sector depleted gas",
    "Europe", "North German Basin", "Mid. Buntsandstein", "Onshore", 53.0, 10.0, 1.2, FALSE, "Onshore Germany",
    "Europe", "Paris Basin", "Keuper/Dogger", "Onshore", 48.5, 3.0, 1.2, FALSE, "France industrial hub",
    "Europe", "Pannonian Basin", "Sava/Drava Depr.", "Onshore", 46.0, 17.0, 1.2, TRUE, "Croatia/Hungary EOR",

    # --- CHINA (Source-Sink Mismatch) ---
    "China", "Ordos Basin", "Triassic Liujiagou", "Onshore", 39.33, 110.15, 0.7, TRUE, "Shenhua region; EOR Potential",
    "China", "Songliao Basin", "Cretaceous Sands", "Onshore", 45.0, 125.0, 0.7, TRUE, "Daqing Oilfield (EOR)",
    "China", "Bohai Bay Basin", "Shahejie Formation", "Offshore", 38.5, 119.5, 0.7, TRUE, "Shengli/Dagang Oilfields (EOR)",
    "China", "Tarim Basin", "Carboniferous", "Onshore", 40.0, 84.0, 0.7, TRUE, "Deep saline & EOR",
    "China", "Subei Basin", "Paleogene Sands", "Onshore", 33.0, 119.5, 0.7, FALSE, "Near Yangtze Delta",
    "China", "Junggar Basin", "Jurassic/Triassic", "Onshore", 45.0, 86.0, 0.7, TRUE, "Xinjiang Oilfield (EOR)",
    "China", "Pearl River Mouth", "Enping 15-1", "Offshore", 21.5, 114.5, 0.7, FALSE, "Greater Bay Area",

    # --- INDIA (Emerging / Data Poor) ---
    "India", "Cambay Basin", "Gandhar/Ankleshwar", "Onshore", 21.7, 72.9, 0.7, TRUE, "Gujarat industrial belt; EOR Potential",
    "India", "Krishna-Godavari", "Syn-rift sediments", "Offshore", 16.5, 82.0, 0.7, FALSE, "East Coast (Visakhapatnam)",
    "India", "Assam-Arakan", "Barail/Tipam", "Onshore", 27.5, 95.5, 0.7, TRUE, "Northeast; Older oilfields",
    "India", "Cauvery Basin", "Cretaceous Sands", "Onshore", 11.0, 79.5, 0.7, FALSE, "Tamil Nadu region",
    "India", "Rajasthan Basin", "Barmer/Jaisalmer", "Onshore", 26.0, 71.0, 0.7, TRUE, "Northwest desert (Cairn Oil)",
    "India", "Mahanadi Basin", "Mesozoic Sediments", "Onshore", 20.0, 87.0, 0.7, FALSE, "Odisha region"
)

# Convert to sf object (CRS 4326 for WGS84)
co2_sinks <- st_as_sf(sinks_list, coords = c("Lon", "Lat"), crs = 4326)

# Save to package data
usethis::use_data(co2_sinks, overwrite = TRUE)

# Print Summary
message("Sinks database updated with ", nrow(sinks_list), " entries.")
print(table(sinks_list$Region, sinks_list$Type))
