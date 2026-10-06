# data-raw/process_eu_feedstock.R
# Rasterizes European country-level crop-residue road-side costs for the TEA model (issue #52).
#
# Input: data-raw/Biomass_price_Europe.csv, S2Biom NUTS-3 road-side costs of cereal straw (Dees et al.
# 2017, D1.8 atlas) aggregated to country level as cost-class midpoints (20, 37.5, 62.5 EUR per t dry
# matter; 2012 EUR).
# Output: GIS/processed/europe_feedstock_cost.tif, layer `eu_feedstock_usd`: field-side feedstock cost
# in 2024 USD per dry Mg = EUR (2012) x 1.285 USD/EUR (2012 average) x 1.366 (US CPI-U 2024/2012).
# Storage is not included; the model adds `feedstock_storage_cost` in calculate_regional_feedstock_cost().
# Rows with no country name set a default for unpriced countries in a World Bank region (e.g.
# "N. Africa and M.E." -> MENA: Algeria, Morocco, Tunisia, Syria). Values for Kosovo (as Serbia/Croatia),
# Cyprus (as Greece) and Türkiye (middle class; Turkish fodder-market straw prices of 1,500-1,750 TL/t in 2023, about 65-76 US$/Mg in 2024 USD rule out the lowest class) are author judgements; North Africa/Middle East defaults lie outside the study region (issue #102).
# Cells still without a price are NA and use the model's European default.
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
price_data$eu_feedstock_usd <- as.numeric(price_data$Price) * eur2012_to_usd2024
has_country <- !is.na(price_data[["Country Name"]]) & price_data[["Country Name"]] != "NA"
country_prices <- price_data[has_country, ]
country_prices$ISO_A3 <- countrycode::countrycode(country_prices[["Country Name"]], "country.name", "iso3c",
  custom_match = c(Kosovo = "XKX"))

admin0 <- admin0 %>% dplyr::left_join(country_prices[, c("ISO_A3", "eu_feedstock_usd")], by = "ISO_A3")

# Region-level defaults for countries without their own value
region_wb <- c("N. Africa and M.E." = "MENA")
for (k in which(!has_country)) {
  wb <- region_wb[[price_data$Region[k]]]
  fill <- is.na(admin0$eu_feedstock_usd) & admin0$WB_REGION == wb
  admin0$eu_feedstock_usd[fill] <- price_data$eu_feedstock_usd[k]
  message("  Region default ", price_data$Region[k], ": ", paste(admin0$NAM_0[fill], collapse = "; "))
}
countries_cost <- dplyr::filter(admin0, !is.na(eu_feedstock_usd))
missing <- setdiff(admin0$NAM_0, countries_cost$NAM_0)
message("  No price (model default used): ", paste(missing, collapse = "; "))

r_cost <- terra::rasterize(terra::vect(countries_cost), r_template, field = "eu_feedstock_usd", background = NA)
r_cost <- terra::mask(r_cost, r_template)
names(r_cost) <- "eu_feedstock_usd"

out_path <- file.path(proc_dir, "europe_feedstock_cost.tif")
terra::writeRaster(r_cost, out_path, overwrite = TRUE)
message("  -> Saved: ", basename(out_path), " (range ", paste(round(range(terra::values(r_cost), na.rm = TRUE), 1), collapse = "-"), " USD/Mg)")
