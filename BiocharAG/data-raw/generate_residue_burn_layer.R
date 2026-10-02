# data-raw/generate_residue_burn_layer.R
# Spatial share of the available residue supply that would otherwise be burned in the field (issue #31).
#
#   f_S     = burnt / production of cereal residues (Smerald et al. 2023, KIT dataset doi:10.35097/989,
#             0.5 deg, 2017-2021 mean)
#   burnt_C = f_S * cereal field residue C (SPAM 2010 residue production, as used by Karan et al. 2023)
#   f_burn  = burnt_C / available residue C (Karan et al. 2023 constrained potential, res_avail.tif)
#
# i.e. f_burn = f_S * Fraction_cereals_Karan * Total_residues_Karan / Avail_residues_Karan. Using the
# Smerald *fraction* (not its burnt mass) keeps the two datasets' different residue totals from
# entering the ratio. Burning of non-cereal residues (e.g. sugarcane trash) is not counted, and all
# burnt cereal residue is assumed to lie within the available pool (f_burn is capped at 1).
# Cereal field residues exclude maize cobs (MAIZC) and rice husks (RICEH), which arise off-field.
#
# Masses are averaged to each region's model grid before dividing, so f_burn is mass-weighted.
# Output: GIS/processed/<prefix>_residue_burn.tif (layer residue_burn_map, fraction 0-1).

library(terra)

gis_raw <- "GIS/raw"
gis_proc <- "GIS/processed"
karan_dir <- "Resources/Karan_biomass_calcs"

# 1. Smerald burnt fraction of cereal residue production, 2017-2021 mean (layers 1..25 = 1997..2021)
nc <- file.path(gis_raw, "Global_crop_residue/data/dataset/crop_residue_usage_mean.nc")
yrs <- which(1997:2021 %in% 2017:2021)
prod_s <- mean(rast(nc, subds = "residue_production")[[yrs]], na.rm = TRUE)
burnt_s <- mean(rast(nc, subds = "burnt_residues")[[yrs]], na.rm = TRUE)
f_s <- ifel(prod_s > 0, burnt_s / prod_s, NA)
crs(f_s) <- "EPSG:4326"

# 2. Cereal field residue carbon from SPAM 2010 (Mg C per 5' pixel), C contents from Karan et al.
res_tbl <- read.csv(file.path(karan_dir, "Residue_Characterization_2023.csv"), check.names = FALSE)
cereal <- setdiff(res_tbl$name[res_tbl$category == "Cereals"], c("MAIZC", "RICEH"))
spam_dir <- file.path(karan_dir, "spam2010V2r0_151122_global_residue_productionV1.geotiff")
spam_files <- list.files(spam_dir, pattern = "\\.tif$", ignore.case = TRUE, full.names = TRUE)
spam_crop <- sub(".+_R_(.+)\\.tif$", "\\1", basename(spam_files), ignore.case = TRUE)
keep <- spam_crop %in% cereal
c_dm <- res_tbl$C_dm[match(spam_crop[keep], res_tbl$name)] / 100
cereal_c <- sum(rast(spam_files[keep]) * c_dm, na.rm = TRUE)

# 3. Burnt cereal residue carbon and available residue carbon at 5'
avail_c <- rast(file.path(gis_raw, "res_avail.tif")) # Mg C / yr per pixel
cereal_c <- resample(cereal_c, avail_c, method = "near")
burnt_c <- resample(f_s, avail_c, method = "near") * cereal_c

# 4. Per region: average both masses to the model grid, divide, cap at 1
for (prefix in c("us", "europe", "china", "india")) {
  tmpl <- rast(file.path(gis_proc, paste0(prefix, "_biomass.tif")))
  e <- ext(tmpl)
  a <- resample(crop(avail_c, e, snap = "out"), tmpl, method = "average")
  b <- resample(crop(burnt_c, e, snap = "out"), tmpl, method = "average")
  f <- clamp(ifel(a > 0, b / a, NA), 0, 1)
  names(f) <- "residue_burn_map"
  out <- file.path(gis_proc, paste0(prefix, "_residue_burn.tif"))
  writeRaster(f, out, overwrite = TRUE)
  a0 <- vect(file.path(gis_proc, paste0(prefix, "_admin0.gpkg")))
  bm <- mask(tmpl * cellSize(tmpl, unit = "km"), project(a0, crs(tmpl))) # summary inside the region only
  w <- global(f * bm, "sum", na.rm = TRUE)[1, 1] / global(mask(bm, f), "sum", na.rm = TRUE)[1, 1]
  message(sprintf("%-7s biomass-weighted f_burn = %.3f; share of cells at cap = %.3f -> %s", prefix, w,
                  global(f >= 1, "mean", na.rm = TRUE)[1, 1], out))
}
