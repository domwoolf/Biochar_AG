# data-raw/generate_ci_layers.R
# This script rasterizes the marginal carbon intensity by country/state 
# into regional `_ff_c_intensity.tif` layers for the TEA pipeline.

library(terra)
library(sf)
library(dplyr)

message("======================================================================")
message("Generating Spatial Carbon Intensity Layers...")
message("======================================================================")

gis_path <- "GIS/processed/"
csv_path <- "BiocharAG/data-raw/marginal_ci_by_country.csv"

if (!file.exists(csv_path)) {
  stop("Input CSV not found at: ", csv_path)
}

df <- read.csv(csv_path, stringsAsFactors = FALSE)

# Check for the expected column
if (!"Merged_CI_tCO2_GJ" %in% names(df)) {
  stop("Missing 'Merged_CI_tCO2_GJ' column in the CSV data.")
}

# Cells with biomass whose centre falls just outside every polygon (coasts, borders) take the value of the
# nearest polygon, so that no biomass cell is left without a carbon intensity
fill_nearest <- function(ci, admin, field, bm) {
  miss <- which(is.na(terra::values(ci, mat = FALSE)) & !is.na(terra::values(bm, mat = FALSE)) &
                  terra::values(bm, mat = FALSE) > 0)
  if (!length(miss)) return(ci)
  adm <- admin[!is.na(admin[[field]]), ]
  pts <- sf::st_as_sf(as.data.frame(terra::xyFromCell(ci, miss)), coords = c("x", "y"), crs = terra::crs(ci))
  adm <- sf::st_transform(adm, sf::st_crs(pts))
  old_s2 <- sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(old_s2))
  nn <- sf::st_nearest_feature(pts, adm)
  ci[miss] <- adm[[field]][nn]
  message(sprintf("  Filled %d edge cells from the nearest unit", length(miss)))
  ci
}

# ------------------------------------------------------------------------------
# 1. Process US
# ------------------------------------------------------------------------------
message("Processing US carbon intensity layer...")
us_admin <- st_read(paste0(gis_path, "us_admin1.gpkg"), quiet = TRUE)
us_df <- df %>% filter(grepl("^US-", Code))

# Join by state name
us_admin <- merge(us_admin, us_df, by.x = "NAM_1", by.y = "Name", all.x = TRUE)

us_bm <- rast(paste0(gis_path, "us_biomass.tif"))
us_ci <- rasterize(us_admin, us_bm, field = "Merged_CI_tCO2_GJ")
us_ci <- fill_nearest(us_ci, us_admin, "Merged_CI_tCO2_GJ", us_bm)

writeRaster(us_ci, paste0(gis_path, "us_ff_c_intensity.tif"), overwrite = TRUE, gdal = c("COMPRESS=LZW"))
message("  -> Created us_ff_c_intensity.tif")

# ------------------------------------------------------------------------------
# 2. Process Europe
# ------------------------------------------------------------------------------
message("Processing Europe carbon intensity layer...")
eu_admin <- st_read(paste0(gis_path, "europe_admin0.gpkg"), quiet = TRUE)

# Join on the ISO 3166-1 alpha-3 code (the CSV's Code for countries). Territories without Ember data take
# the value of the grid they belong to.
parent <- c(ALA = "FIN", SJM = "NOR", GGY = "GBR", JEY = "GBR", IMN = "GBR", AND = "ESP", LIE = "CHE",
            MCO = "FRA", SMR = "ITA", VAT = "ITA")
iso <- ifelse(eu_admin$ISO_A3 %in% names(parent), parent[eu_admin$ISO_A3], eu_admin$ISO_A3)
eu_admin$Merged_CI_tCO2_GJ <- df$Merged_CI_tCO2_GJ[match(iso, df$Code)]
missing_ci <- eu_admin$NAM_0[is.na(eu_admin$Merged_CI_tCO2_GJ)]
if (length(missing_ci)) message("  No carbon intensity for: ", paste(missing_ci, collapse = "; "))

eu_bm <- rast(paste0(gis_path, "europe_biomass.tif"))
eu_ci <- rasterize(eu_admin, eu_bm, field = "Merged_CI_tCO2_GJ")
eu_ci <- fill_nearest(eu_ci, eu_admin, "Merged_CI_tCO2_GJ", eu_bm)

writeRaster(eu_ci, paste0(gis_path, "europe_ff_c_intensity.tif"), overwrite = TRUE, gdal = c("COMPRESS=LZW"))
message("  -> Created europe_ff_c_intensity.tif")

# ------------------------------------------------------------------------------
# 3. Process China
# ------------------------------------------------------------------------------
message("Processing China carbon intensity layer...")
cn_val <- df$Merged_CI_tCO2_GJ[df$Code == "CHN"]

if (length(cn_val) > 0) {
  cn_bm <- rast(paste0(gis_path, "china_biomass.tif"))
  cn_ci <- rast(cn_bm)
  values(cn_ci) <- cn_val[1]
  
  # Mask to the country extent
  cn_admin <- st_read(paste0(gis_path, "china_admin0.gpkg"), quiet = TRUE)
  cn_ci <- mask(cn_ci, cn_admin)
  
  writeRaster(cn_ci, paste0(gis_path, "china_ff_c_intensity.tif"), overwrite = TRUE, gdal = c("COMPRESS=LZW"))
  message("  -> Created china_ff_c_intensity.tif")
} else {
  warning("  -> Skipping China: No matching data in CSV.")
}

# ------------------------------------------------------------------------------
# 4. Process India
# ------------------------------------------------------------------------------
message("Processing India carbon intensity layer...")
in_val <- df$Merged_CI_tCO2_GJ[df$Code == "IND"]

if (length(in_val) > 0) {
  in_bm <- rast(paste0(gis_path, "india_biomass.tif"))
  in_ci <- rast(in_bm)
  values(in_ci) <- in_val[1]
  
  # Mask to the country extent
  in_admin <- st_read(paste0(gis_path, "india_admin0.gpkg"), quiet = TRUE)
  in_ci <- mask(in_ci, in_admin)
  
  writeRaster(in_ci, paste0(gis_path, "india_ff_c_intensity.tif"), overwrite = TRUE, gdal = c("COMPRESS=LZW"))
  message("  -> Created india_ff_c_intensity.tif")
} else {
  warning("  -> Skipping India: No matching data in CSV.")
}

message("======================================================================")
message("Carbon Intensity Layer Processing Complete!")
message("======================================================================")
