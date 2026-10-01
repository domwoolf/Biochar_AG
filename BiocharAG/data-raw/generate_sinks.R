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
# Global Carbon Sink Database (issue #63)
# Basin centroids and storage types follow the OGCI CO2 Storage Resource Catalogue, Cycle 4 (2024) and,
# for the United States, the USGS (2013) National Assessment of Geologic CO2 Storage Resources (Circular
# 1386). Basins added on 28 Sep 2026 are placed at operating or planned projects listed in the Global CCS
# Institute CO2RE facilities database (e.g. Porthos, Northern Lights, Greensand, Ravenna CCS, HyNet,
# Bayou Bend, Elk Hills).
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
    # Added 28 Sep 2026 (coverage review). The Alberta Basin (Canada) is not added: it lies outside the
    # US routing grid, so routes would be snapped to the border and understated.
    "North America", "Gulf of Mexico (offshore)", "Miocene sands (Texas/Louisiana shelf)", "Offshore", 28.8, -94.0, 1.0, FALSE, "Offshore saline; e.g. Bayou Bend CCS",
    "North America", "San Joaquin Basin", "Monterey/Stevens sands", "Onshore", 35.3, -119.4, 1.0, FALSE, "California; Elk Hills Class VI project",
    "North America", "Sacramento Basin", "Starkey/Winters sands", "Onshore", 38.8, -121.8, 1.0, FALSE, "California; saline and depleted gas",
    "North America", "Denver-Julesburg Basin", "Lyons/Dakota sandstones", "Onshore", 40.5, -104.0, 1.0, FALSE, "Colorado Front Range saline",
    "North America", "Greater Green River Basin", "Weber/Madison (Rock Springs Uplift)", "Onshore", 41.6, -108.9, 1.0, FALSE, "Wyoming saline",

    # --- EUROPE (Offshore Focus) ---
    "Europe", "Northern North Sea (NO)", "Utsira Formation", "Offshore", 58.4, 1.9, 1.2, FALSE, "Sleipner site; Massive aquifer",
    "Europe", "Northern North Sea (NO)", "Johansen Formation", "Offshore", 60.5, 3.5, 1.2, FALSE, "Northern Lights / Aurora",
    "Europe", "Southern North Sea (NL)", "P18/P15 Fields", "Offshore", 52.0, 3.5, 1.2, FALSE, "Porthos (Rotterdam)",
    "Europe", "Southern North Sea (UK)", "Goldeneye/Viking", "Offshore", 53.5, 2.0, 1.2, FALSE, "UK Sector depleted gas",
    "Europe", "North German Basin", "Mid. Buntsandstein", "Onshore", 53.0, 10.0, 1.2, FALSE, "Onshore Germany",
    "Europe", "Paris Basin", "Keuper/Dogger", "Onshore", 48.5, 3.0, 1.2, FALSE, "France industrial hub",
    "Europe", "Pannonian Basin", "Sava/Drava Depr.", "Onshore", 46.0, 17.0, 1.2, TRUE, "Croatia/Hungary EOR",
    # Added 28 Sep 2026 (coverage review)
    "Europe", "Adriatic (Ravenna)", "Depleted gas fields (Porto Corsini)", "Offshore", 44.4, 12.6, 1.2, FALSE, "Eni Ravenna CCS; injecting since 2024",
    "Europe", "East Irish Sea (HyNet)", "Hamilton depleted gas fields", "Offshore", 53.6, -3.6, 1.2, FALSE, "Liverpool Bay",
    "Europe", "Danish North Sea (Greensand)", "Nini West field", "Offshore", 56.5, 4.8, 1.2, FALSE, "Greensand; injection from 2025",
    "Europe", "Prinos (Greece)", "Prinos depleted oil field", "Offshore", 40.8, 24.5, 1.2, FALSE, "North Aegean",
    "Europe", "Duero Basin (Spain)", "Utrillas sandstone", "Onshore", 41.8, -4.5, 1.2, FALSE, "Iberian onshore saline",
    "Europe", "Polish Lowlands", "Lower Jurassic saline", "Onshore", 52.3, 18.5, 1.2, FALSE, "Polish onshore saline",

    # --- CHINA (Source-Sink Mismatch) ---
    "China", "Ordos Basin", "Triassic Liujiagou", "Onshore", 39.33, 110.15, 0.7, TRUE, "Shenhua region; EOR Potential",
    "China", "Songliao Basin", "Cretaceous Sands", "Onshore", 45.0, 125.0, 0.7, TRUE, "Daqing Oilfield (EOR)",
    "China", "Bohai Bay Basin", "Shahejie Formation", "Offshore", 38.5, 119.5, 0.7, TRUE, "Shengli/Dagang Oilfields (EOR)",
    "China", "Tarim Basin", "Carboniferous", "Onshore", 40.0, 84.0, 0.7, TRUE, "Deep saline & EOR",
    "China", "Subei Basin", "Paleogene Sands", "Onshore", 33.0, 119.5, 0.7, FALSE, "Near Yangtze Delta",
    "China", "Junggar Basin", "Jurassic/Triassic", "Onshore", 45.0, 86.0, 0.7, TRUE, "Xinjiang Oilfield (EOR)",
    "China", "Pearl River Mouth", "Enping 15-1", "Offshore", 21.5, 114.5, 0.7, FALSE, "Greater Bay Area",
    # Added 28 Sep 2026 (coverage review)
    "China", "Sichuan Basin", "Triassic/Jurassic saline and gas fields", "Onshore", 30.5, 105.5, 0.7, FALSE, "Large saline and depleted gas capacity",
    "China", "Jianghan Basin", "Qianjiang Formation", "Onshore", 30.3, 112.8, 0.7, TRUE, "Jianghan Oilfield (EOR)",
    "China", "Beibu Gulf", "Weixinan sag", "Offshore", 20.5, 108.5, 0.7, FALSE, "Offshore saline",
    "China", "East China Sea Shelf", "Xihu depression", "Offshore", 29.0, 124.0, 0.7, FALSE, "Offshore saline and gas fields",

    # --- INDIA (Emerging / Data Poor) ---
    "India", "Cambay Basin", "Gandhar/Ankleshwar", "Onshore", 21.7, 72.9, 0.7, TRUE, "Gujarat industrial belt; EOR Potential",
    "India", "Krishna-Godavari", "Syn-rift sediments", "Offshore", 16.5, 82.0, 0.7, FALSE, "East Coast (Visakhapatnam)",
    "India", "Assam-Arakan", "Barail/Tipam", "Onshore", 27.5, 95.5, 0.7, TRUE, "Northeast; Older oilfields",
    "India", "Cauvery Basin", "Cretaceous Sands", "Onshore", 11.0, 79.5, 0.7, FALSE, "Tamil Nadu region",
    "India", "Rajasthan Basin", "Barmer/Jaisalmer", "Onshore", 26.0, 71.0, 0.7, TRUE, "Northwest desert (Cairn Oil)",
    "India", "Mahanadi Basin", "Mesozoic Sediments", "Onshore", 20.0, 87.0, 0.7, FALSE, "Odisha region",
    # Added 28 Sep 2026 (coverage review)
    "India", "Mumbai Offshore", "Bombay High / Bassein", "Offshore", 19.4, 71.3, 0.7, FALSE, "West coast; saline and depleted fields"
)

# ==============================================================================
# Storage cost by sink (issue #103), 2024 USD per t CO2 injected (exploration to post-closure)
# ==============================================================================
# Storage_Class: "netl" (US onshore saline, formation-level cost), "open_saline", "closed_saline",
# "depleted" (depleted oil/gas field), or "unclassified". Storage_Cost is NA for unclassified sinks and
# for EOR-only sinks, which use the regional parameters ccs_storage_cost / cost_offshore_storage.
#
# netl: FECM/NETL CO2_S_COM v4 (2024) baseline first-year break-even price (2023 USD) of the cheapest
#   formation in the sink's state with >= 1 Gt prospective resource (else in the basin), x 1.0295
#   (US CPI-U 2024/2023). Pressure build-up and interference are modelled per formation.
# open_saline / closed_saline: Global CCS Institute (2025), midpoint of the open-boundary range
#   (onshore 2-15, offshore 5-31) or the moderate closed-boundary range (onshore 5-33, offshore 8-50).
# depleted: ZEP (2011) offshore depleted fields, mean of the cases with and without re-usable legacy
#   wells (medium EUR 6 and 10/t, 2009 EUR) x 1.387 USD/EUR (ZEP) x 1.462 (US CPI-U 2024/2009).
#   GCCSI's closed case (a saline aquifer pressurised above hydrostatic) does not apply: depleted
#   fields start below their original pressure.
netl_usd <- function(x) round(x * 313.689 / 304.702, 2)
class_cost <- list(
  open_saline = c(Onshore = 8.5, Offshore = 18),
  closed_saline = c(Onshore = 19, Offshore = 29),
  depleted = c(Onshore = NA, Offshore = round(mean(c(6, 10)) * 1.387 * 313.689 / 214.537, 2))
)
storage_tbl <- tribble(
  ~Basin_Name, ~Storage_Class, ~NETL_2023, ~Storage_Basis,
  # US onshore saline: NETL formation used
  "Gulf Coast Basin", "netl", 6.22, "NETL CO2_S_COM: Frio, TX (fluvial)",
  "Illinois Basin", "netl", 7.77, "NETL CO2_S_COM: Mount Simon, IL",
  "Michigan Basin", "netl", 7.95, "NETL CO2_S_COM: Mount Simon, MI",
  "Appalachian Basin", "netl", 24.00, "NETL CO2_S_COM: Copper Ridge, OH (no formation >= 1 Gt in PA; Rose Run PA 80)",
  "San Juan Basin", "netl", 9.46, "NETL CO2_S_COM: Morrison, NM",
  "San Joaquin Basin", "netl", 7.53, "NETL CO2_S_COM: Starkey, CA",
  "Sacramento Basin", "netl", 6.84, "NETL CO2_S_COM: Forbes, CA",
  "Denver-Julesburg Basin", "netl", 21.89, "NETL CO2_S_COM: Morrison, CO",
  "Greater Green River Basin", "netl", 13.90, "NETL CO2_S_COM: Nugget, WY",
  # EOR basins that are also saline sinks (issue #105)
  "Permian Basin", "netl", 10.79, "NETL CO2_S_COM: Canyon, TX",
  "Williston Basin", "netl", 11.08, "NETL CO2_S_COM: Inyan Kara, ND",
  "Powder River Basin", "netl", 15.47, "NETL CO2_S_COM: Minnelusa, WY",
  "Anadarko Basin", "netl", 29.74, "NETL CO2_S_COM: Simpson Sandstone, OK",
  "Ordos Basin", "closed_saline", NA, "Liujiagou: low-permeability sandstone (Shenhua saline demonstration)",
  # Open saline aquifers: regionally extensive, well-connected sands
  "Gulf of Mexico (offshore)", "open_saline", NA, "Regionally extensive Miocene shelf sands",
  "Northern North Sea (NO)", "open_saline", NA, "Utsira/Johansen: regionally extensive aquifers; no measurable pressure build-up at Sleipner",
  "Paris Basin", "open_saline", NA, "Dogger: regionally extensive carbonate aquifer (geothermal use across the basin)",
  # Closed / compartmentalised saline aquifers
  "North German Basin", "closed_saline", NA, "Buntsandstein compartmentalised by salt structures and faults",
  # Depleted oil and gas fields
  "Southern North Sea (NL)", "depleted", NA, "P18/P15 depleted gas fields (Porthos)",
  "Southern North Sea (UK)", "depleted", NA, "Goldeneye depleted gas-condensate field",
  "Adriatic (Ravenna)", "depleted", NA, "Porto Corsini depleted gas fields",
  "East Irish Sea (HyNet)", "depleted", NA, "Hamilton depleted gas fields",
  "Danish North Sea (Greensand)", "depleted", NA, "Nini West depleted oil field",
  "Prinos (Greece)", "depleted", NA, "Prinos depleted oil field"
)
# Saline and EOR are separate flags (issue #105). Has_Saline: the sink is a saline storage target; EOR
# basins with documented saline storage resource are both. Jianghan, Assam-Arakan and Rajasthan remain
# EOR-only (no saline resource assessment found).
eor_and_saline <- c("Permian Basin", "Williston Basin", "Powder River Basin", "Anadarko Basin", "Pannonian Basin",
                    "Ordos Basin", "Songliao Basin", "Bohai Bay Basin", "Tarim Basin", "Junggar Basin", "Cambay Basin")
sinks_list <- sinks_list |>
  mutate(Has_Saline = !Is_EOR | Basin_Name %in% eor_and_saline) |>
  left_join(storage_tbl, by = "Basin_Name") |>
  mutate(
    Storage_Class = dplyr::coalesce(Storage_Class, "unclassified"),
    # Storage_Cost is the saline storage cost; EOR routes use the regional parameter
    Storage_Cost = dplyr::case_when(
      !Has_Saline ~ NA_real_,
      Storage_Class == "netl" ~ netl_usd(NETL_2023),
      Storage_Class %in% names(class_cost) ~ vapply(seq_along(Type), function(i) {
        cc <- class_cost[[Storage_Class[i]]]
        if (is.null(cc)) NA_real_ else unname(cc[Type[i]])
      }, numeric(1)),
      TRUE ~ NA_real_
    )
  ) |>
  select(-NETL_2023)

# Convert to sf object (CRS 4326 for WGS84)
co2_sinks <- st_as_sf(sinks_list, coords = c("Lon", "Lat"), crs = 4326)

# Save to package data
usethis::use_data(co2_sinks, overwrite = TRUE)

# Print Summary
message("Sinks database updated with ", nrow(sinks_list), " entries.")
print(table(sinks_list$Region, sinks_list$Type))
print(as.data.frame(sinks_list[, c("Region", "Basin_Name", "Type", "Is_EOR", "Has_Saline", "Storage_Class", "Storage_Cost")]))
