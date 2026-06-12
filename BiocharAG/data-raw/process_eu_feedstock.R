# data-raw/process_eu_feedstock.R
# Processes European country-level biomass prices into a spatial raster layer
# for the TEA model.

library(terra)
library(sf)
library(dplyr)
library(giscoR) # For fetching European country geometries

# ==============================================================================
# Setup: Define Paths and Load Template
# ==============================================================================
raw_dir <- "../GIS/raw"
proc_dir <- "../GIS/processed"

if (!dir.exists(raw_dir)) dir.create(raw_dir, recursive = TRUE)
if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)

message("Loading Europe Biomass Template...")
template_path <- file.path(proc_dir, "europe_biomass.tif")
if (!file.exists(template_path)) stop("Template raster not found. Run process_china_europe.R first.")
r_template <- terra::rast(template_path)

# ==============================================================================
# 1. Prepare Country Geometries
# ==============================================================================
message("Fetching Europe Country Boundaries...")
# Fetch countries using giscoR (includes all of Europe/Eurasia)
eu_countries <- gisco_get_countries(region = c("Europe", "Asia")) %>%
  sf::st_as_sf()

# Project countries to match the raster template
eu_countries_proj <- sf::st_transform(eu_countries, terra::crs(r_template))

# ==============================================================================
# 2. Process Biomass Prices
# ==============================================================================
message("Processing Biomass Prices...")

# Read the CSV with NUTS-3 baseline prices per country
csv_path <- "Biomass_price_Europe.csv"
if (!file.exists(csv_path)) {
  stop(sprintf("Prices CSV not found at: %s", csv_path))
}

price_data <- read.csv(csv_path, stringsAsFactors = FALSE)

# Clean up column names since it has spaces
names(price_data) <- gsub("\\.", "_", make.names(names(price_data)))

# Map specific country names to match the Eurostat (giscoR) NAME_ENGL column
price_data <- price_data %>%
  dplyr::mutate(Country_Name = dplyr::case_when(
    Country_Name == "Czech Republic (Czechia)" ~ "Czechia",
    Country_Name == "Russia" ~ "Russian Federation",
    Country_Name == "Republic of Ireland" ~ "Ireland",
    TRUE ~ Country_Name
  ))

# Join the price data to the spatial geometries
counties_cost <- eu_countries_proj %>%
  dplyr::left_join(price_data, by = c("NAME_ENGL" = "Country_Name")) %>%
  # Keep only those that have a price mapping, or keep all to allow NA areas
  # We will just pass the ones we joined, and fill the rest as NA during rasterization
  dplyr::select(NAME_ENGL, eu_base_eur = Price)

# Ensure the column to be rasterized is numeric
counties_cost$eu_base_eur <- as.numeric(counties_cost$eu_base_eur)

# Rasterize the spatial polygons to the template grid
# We rasterize the 'eu_base_eur' field
r_base_cost <- terra::rasterize(
  terra::vect(counties_cost), 
  r_template, 
  field = "eu_base_eur", 
  background = NA
)

# Mask out areas outside the biomass template
r_base_cost <- terra::mask(r_base_cost, r_template)

# Name the layer explicitly
names(r_base_cost) <- "eu_base_eur"

# Write the final raster to disk
out_path <- file.path(proc_dir, "europe_eu_base_eur.tif")
terra::writeRaster(r_base_cost, out_path, overwrite = TRUE)
message("  -> Saved: ", basename(out_path))

message("=== Europe Feedstock Processing Complete ===")
