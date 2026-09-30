# data-raw/process_eu_feedstock.R
# Rasterizes European country-level crop-residue road-side costs for the TEA model (issue #52).
#
# Input: data-raw/Biomass_price_Europe.csv, S2Biom NUTS-3 road-side costs of cereal straw (Dees et al.
# 2017, D1.8 atlas) aggregated to country level as cost-class midpoints (20, 37.5, 62.5 EUR per t dry
# matter; 2012 EUR).
# Output: GIS/processed/europe_feedstock_cost.tif, layer `eu_feedstock_usd`: field-side feedstock cost
# in 2024 USD per dry Mg = EUR (2012) x 1.285 USD/EUR (2012 average) x 1.366 (US CPI-U 2024/2012).
# Storage is not included; the model adds `feedstock_storage_cost` in calculate_regional_feedstock_cost().
# Cells with no country price (e.g. Cyprus, Kosovo, Turkiye) are NA and use the model's European default.
#
# Usage (from BiocharAG/): Rscript data-raw/process_eu_feedstock.R

library(terra)
library(sf)
library(dplyr)

proc_dir <- "../GIS/processed"
eur2012_to_usd2024 <- 1.285 * 1.366

message("Loading Europe biomass template...")
template_path <- file.path(proc_dir, "europe_biomass.tif")
if (!file.exists(template_path)) stop("Template raster not found: ", template_path)
r_template <- terra::rast(template_path)

# Country boundaries: the same admin0 layer the model uses for maps
admin0 <- sf::st_read(file.path(proc_dir, "europe_admin0.gpkg"), quiet = TRUE)
admin0 <- sf::st_transform(admin0, terra::crs(r_template))

message("Processing S2Biom country prices...")
price_data <- read.csv("data-raw/Biomass_price_Europe.csv", check.names = FALSE, stringsAsFactors = FALSE)
price_data$ISO_A3 <- countrycode::countrycode(price_data[["Country Name"]], "country.name", "iso3c")
price_data$eu_feedstock_usd <- as.numeric(price_data$Price) * eur2012_to_usd2024

countries_cost <- admin0 %>%
  dplyr::left_join(price_data[, c("ISO_A3", "eu_feedstock_usd")], by = "ISO_A3") %>%
  dplyr::filter(!is.na(eu_feedstock_usd))
missing <- setdiff(admin0$NAM_0, countries_cost$NAM_0)
message("  No price (model default used): ", paste(missing, collapse = "; "))

r_cost <- terra::rasterize(terra::vect(countries_cost), r_template, field = "eu_feedstock_usd", background = NA)
r_cost <- terra::mask(r_cost, r_template)
names(r_cost) <- "eu_feedstock_usd"

out_path <- file.path(proc_dir, "europe_feedstock_cost.tif")
terra::writeRaster(r_cost, out_path, overwrite = TRUE)
message("  -> Saved: ", basename(out_path), " (range ", paste(round(range(terra::values(r_cost), na.rm = TRUE), 1), collapse = "-"), " USD/Mg)")
