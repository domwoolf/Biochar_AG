# BiocharAG/data-raw/process_crop_layers.R
# Crop layers for the biochar field-application model (docs/biochar_agronomy_handover):
#   <prefix>_harv_frac.tif   harvested area of the crops whose residues are in the residue map, as a
#                            fraction of cell area (can exceed 1 under multiple cropping)
#   <prefix>_crop_value.tif  value of production of the major field crops per hectare harvested
#                            (2024 US$ ha-1 yr-1)
# and the price table inst/extdata/crop_prices_faostat.csv.
#
# Inputs (not in the repository; run from BiocharAG/data-raw/):
#   GIS/raw/spam2010   MapSPAM 2010 v2.0 harvested area (H) and production (P), all technologies (_A),
#                      5 arcmin; Harvard Dataverse doi:10.7910/DVN/PRFF8V
#   GIS/raw/faostat    FAOSTAT bulk downloads: Prices_E_All_Data_(Normalized).csv and
#                      Production_Crops_Livestock_E_All_Data_(Normalized).csv
#
# Crop prices are FAOSTAT producer prices (US$ per tonne), averaged over countries weighted by their
# production in the same year, escalated to 2024 USD with the US CPI-U, and averaged over PRICE_YEARS.
# A SPAM crop group with several FAOSTAT items takes the production-weighted mean of their prices.
# Major field crops are cereals, roots and tubers, pulses, oilseeds, sugar crops and fibres; tree crops,
# fruit, vegetables, coffee, tea, cocoa and tobacco are excluded.
library(terra)
library(data.table)

gis_raw <- "../../GIS/raw"
gis_proc <- "../../GIS/processed"
PRICE_YEARS <- 2019:2023
CPI_U <- c("2019" = 255.657, "2020" = 258.811, "2021" = 270.970, "2022" = 292.655, "2023" = 304.702, "2024" = 313.689)

# SPAM crops with residues in the residue map (data-raw/karan_residues; residue-only codes such as RICEH,
# MAIZC and SUGCB belong to their parent crop)
residue_crops <- c("ACOF", "BANA", "BARL", "BEAN", "CASS", "CHIC", "CNUT", "COCO", "COTT", "COWP", "GROU", "MAIZ",
                   "OCER", "OILP", "OOIL", "PIGE", "PLNT", "PMIL", "POTA", "RAPE", "RCOF", "RICE", "SESA", "SMIL",
                   "SORG", "SOYB", "SUGB", "SUGC", "SUNF", "TEAS", "TEMF", "TOBA", "TROF", "WHEA")

# Major field crops and their FAOSTAT price items
field_items <- list(
  WHEA = "Wheat", RICE = "Rice", MAIZ = "Maize (corn)", BARL = "Barley", PMIL = "Millet", SMIL = "Millet",
  SORG = "Sorghum", OCER = c("Oats", "Rye", "Triticale", "Buckwheat", "Fonio", "Quinoa", "Cereals n.e.c."),
  POTA = "Potatoes", SWPO = "Sweet potatoes", YAMS = "Yams", CASS = "Cassava, fresh",
  ORTS = c("Taro", "Edible roots and tubers with high starch or inulin content, n.e.c., fresh"),
  BEAN = "Beans, dry", CHIC = "Chick peas, dry", COWP = "Cow peas, dry", PIGE = "Pigeon peas, dry",
  LENT = "Lentils, dry", OPUL = c("Peas, dry", "Broad beans and horse beans, dry", "Lupins", "Bambara beans, dry"),
  SOYB = "Soya beans", GROU = "Groundnuts, excluding shelled", SUNF = "Sunflower seed", RAPE = "Rape or colza seed",
  SESA = "Sesame seed", OOIL = c("Linseed", "Safflower seed", "Mustard seed", "Castor oil seeds", "Hempseed"),
  SUGC = "Sugar cane", SUGB = "Sugar beet", COTT = "Seed cotton, unginned",
  OFIB = c("Jute, raw or retted", "Flax, processed but not spun", "Sisal, raw", "Kenaf, and other textile bast fibres, raw or retted")
)

# ------------------------------------------------------------------------------
# 1. Crop prices (2024 US$ per Mg)
# ------------------------------------------------------------------------------
fao <- file.path(gis_raw, "faostat")
pr <- fread(file.path(fao, "Prices_E_All_Data_(Normalized).csv"))
pr <- pr[Element == "Producer Price (USD/tonne)" & Months == "Annual value" & Year %in% PRICE_YEARS & `Area Code` < 5000,
         .(area = `Area Code`, item = Item, year = Year, price = Value)]
pq <- fread(file.path(fao, "Production_Crops_Livestock_E_All_Data_(Normalized).csv"))
pq <- pq[Element == "Production" & Unit == "t" & Year %in% PRICE_YEARS & `Area Code` < 5000,
         .(area = `Area Code`, item = Item, year = Year, prod = Value)]
d <- merge(pr, pq, by = c("area", "item", "year"))[is.finite(price) & price > 0 & is.finite(prod) & prod > 0]
d[, price24 := price * CPI_U[["2024"]] / CPI_U[as.character(year)]]
item_price <- d[, .(price = weighted.mean(price24, prod), prod = sum(prod) / length(PRICE_YEARS), n_countries = uniqueN(area)), by = item]

missing <- setdiff(unlist(field_items), item_price$item)
if (length(missing)) message("FAOSTAT items without prices (ignored): ", paste(missing, collapse = "; "))
crop_price <- rbindlist(lapply(names(field_items), function(cr) {
  ip <- item_price[item %in% field_items[[cr]]]
  if (!nrow(ip)) stop("No FAOSTAT price for ", cr)
  data.table(spam_crop = cr, fao_items = paste(ip$item, collapse = "; "),
             price_usd2024_per_Mg = weighted.mean(ip$price, ip$prod), n_countries = max(ip$n_countries))
}))
fwrite(crop_price, "../inst/extdata/crop_prices_faostat.csv")
print(crop_price[, .(spam_crop, price = round(price_usd2024_per_Mg), n_countries)])

# ------------------------------------------------------------------------------
# 2. Global 5-arcmin layers: harvested area of residue crops; value and area of field crops
# ------------------------------------------------------------------------------
spam <- file.path(gis_raw, "spam2010")
spam_file <- function(var, cr) file.path(spam, sprintf("spam2010V2r0_global_%s_%s_A.tif", var, cr))
sum_layers <- function(files, w = NULL) {
  out <- NULL
  for (i in seq_along(files)) {
    x <- terra::classify(terra::rast(files[i]), cbind(NA, 0))
    if (!is.null(w)) x <- x * w[i]
    out <- if (is.null(out)) x else out + x
  }
  out
}
h_res <- sum_layers(spam_file("H", residue_crops)) # ha per pixel
h_field <- sum_layers(spam_file("H", names(field_items)))
v_field <- sum_layers(spam_file("P", names(field_items)), crop_price$price_usd2024_per_Mg) # US$ per pixel
pix_ha <- terra::cellSize(h_res, unit = "ha")

# ------------------------------------------------------------------------------
# 3. Region layers on the region templates (densities averaged, then ratios)
# ------------------------------------------------------------------------------
for (prefix in c("us", "china", "europe", "india")) {
  tmpl <- terra::rast(file.path(gis_proc, paste0(prefix, "_biomass.tif")))
  e <- terra::ext(tmpl) + 1
  avg <- function(x) terra::resample(terra::crop(x / pix_ha, e), tmpl, method = "average")
  harv_frac <- terra::mask(avg(h_res), tmpl)
  names(harv_frac) <- "harv_frac"
  vf <- avg(v_field)
  hf <- avg(h_field)
  crop_value <- terra::mask(terra::ifel(hf > 0, vf / hf, NA), tmpl)
  names(crop_value) <- "crop_value"
  terra::writeRaster(harv_frac, file.path(gis_proc, paste0(prefix, "_harv_frac.tif")), overwrite = TRUE, gdal = c("COMPRESS=ZSTD", "PREDICTOR=3"))
  terra::writeRaster(crop_value, file.path(gis_proc, paste0(prefix, "_crop_value.tif")), overwrite = TRUE, gdal = c("COMPRESS=ZSTD", "PREDICTOR=3"))
  w <- terra::values(hf * terra::cellSize(tmpl, unit = "ha"), mat = FALSE)
  cv <- terra::values(crop_value, mat = FALSE)
  ok <- is.finite(cv) & is.finite(w) & w > 0
  message(sprintf("%-7s harvested-area-weighted crop value %.0f US$/ha (median %.0f); harvested area %.1f Mha",
                  prefix, weighted.mean(cv[ok], w[ok]), median(cv[ok]),
                  terra::global(harv_frac * terra::cellSize(tmpl, unit = "ha"), "sum", na.rm = TRUE)[[1]] / 1e6))
}
