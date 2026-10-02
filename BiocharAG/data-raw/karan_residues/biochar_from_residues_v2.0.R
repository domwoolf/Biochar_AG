# Karan et al. (2023) residue supply, v2.0: fixes duplicated country records in the Herrero-region
# aggregation (gadm_table.csv repeats country names for disputed regions), which made regional residue
# production, and hence the livestock fraction, wrong. Writes res_prod/res_harv/res_avail.tif (Mg C
# per pixel per year) to GIS/processed/. Run from BiocharAG/data-raw/karan_residues/; needs
# res_with_biochar.csv from biochar_from_residues_v1.04.R. The SPAM, boundaries and soil temperature
# folders are symlinks to local GIS data.
library(sf)
library(dplyr)
library(data.table)
library(terra)

# ------------ total residue production ----------------
res_files = list.files('spam2010V2r0_151122_global_residue_productionV1.geotiff', full.names = TRUE) # symlink
res_files = grep("tif", res_files, ignore.case=TRUE, value=TRUE)
res = rast(res_files) # units are Mg DM per pixel 
names(res) = sub('.+_R_(.+)', '\\1', names(res))
res_tot = global(res, fun = 'sum', na.rm=TRUE) # Mg
sum(res_tot)/1e9 # world total residues (Pg DM) ---> returns 5.481676


# ------------ Add residue production to countries ------------
sf_use_s2(FALSE)
countries = st_read('boundaries/gadm_410-levels.gpkg') # symlink
countries = st_simplify(countries, dTolerance = 0.1)
countries = countries[countries$COUNTRY != "Antarctica", ]
country_res = extract(res, countries, fun = sum, na.rm=TRUE)
countries = cbind(countries, country_res[,-1])
countries$res.prod = rowSums(country_res[,-1], na.rm = TRUE)

# Add Herrero.region field to countries
w.regions = unique(fread('gadm_table.csv'))
stopifnot(!anyDuplicated(w.regions$COUNTRY))
n0 = nrow(countries)
countries.r = merge(countries, w.regions, all.x = TRUE, by = 'COUNTRY')
stopifnot(nrow(countries.r) == n0)

Herrero.res = countries.r %>%
    st_make_valid() %>%
    group_by(Herrero.region) %>%
    summarise(res.prod = sum(res.prod, na.rm = TRUE)) %>%
    st_drop_geometry() %>%
    setDT()
fwrite(Herrero.res, 'Herrero.res.csv')

Herrero.res[, sum(res.prod)]/1e9 # -----> returns 5.449

#  table of harvestable fraction by crop
res_tbl = fread('res_with_biochar.csv')
res_tbl[, .(name, harvestable_res_fraction)]

#  create table of livestock fraction by country
residue_consumption = fread("Herrero_stover_consumption_by_region.csv")
Herrero.res = Herrero.res[residue_consumption, on='Herrero.region']
Herrero.res[, res.livestock := stover_feed_mt * 1e6]
Herrero.res[, res.livestock.frac := res.livestock / res.prod]
countries.r = merge(countries, Herrero.res[, .(Herrero.region, res.livestock.frac)], all.x = TRUE, by = 'Herrero.region')
stopifnot(nrow(countries.r) == n0)

#  create table of available fraction by crop x region
crop.region = expand.grid(Herrero.region = Herrero.res$Herrero.region, crop = res_tbl$name)
setDT(crop.region)
crop.region = crop.region[res_tbl[, .(crop = name, harv.frac = harvestable_res_fraction)], on = 'crop']
crop.region = crop.region[Herrero.res[,.(Herrero.region, livestock.frac = res.livestock.frac)], on='Herrero.region']
crop.region[, avail.frac := pmax(0, harv.frac-livestock.frac)]

res.harv.frac = dcast(crop.region, Herrero.region~crop, value.var = 'harv.frac')
res.live.frac = dcast(crop.region, Herrero.region~crop, value.var = 'livestock.frac')
res.avai.frac = dcast(crop.region, Herrero.region~crop, value.var = 'avail.frac')

setnames(res.harv.frac, -1, \(x) paste0(x, '.harv'), skip_absent=T)
setnames(res.live.frac, -1, \(x) paste0(x, '.live'), skip_absent=T)
setnames(res.avai.frac, -1, \(x) paste0(x, '.avai'), skip_absent=T)
countries.r = merge(countries.r, res.harv.frac, all.x = TRUE, by = 'Herrero.region')
countries.r = merge(countries.r, res.live.frac, all.x = TRUE, by = 'Herrero.region')
countries.r = merge(countries.r, res.avai.frac, all.x = TRUE, by = 'Herrero.region')

# ------------ Calculate available residues per crop ----------------
harv                = list()
avai                = list()
res.prod            = list()
res.harv            = list()
res.avai            = list()
i=0
for (.crop in names(res)) {
  i = i+1
  cat(round(100 * i / nlyr(res)), "% ", .crop, '\n', sep='')
  harv     [[.crop]] = rasterize(countries.r, res, paste0(.crop, '.harv')) # harvestable fraction by crop x country
  avai     [[.crop]] = rasterize(countries.r, res, paste0(.crop, '.avai')) # availability of residues by crop x country
  res.prod [[.crop]] = res[[.crop]] * res_tbl[name == .crop, C_dm] # Mg C /pixel/year
  res.harv [[.crop]] = res.prod[[.crop]] * harv[[.crop]]           # Mg C /pixel/year
  res.avai [[.crop]] = res.prod[[.crop]] * avai[[.crop]]           # Mg C /pixel/year
}

# all in Mg C /pixel/year
sum.res.prod            = sum(rast(res.prod), na.rm = TRUE)
sum.res.harv            = sum(rast(res.harv), na.rm=TRUE)
sum.res.avai            = sum(rast(res.avai), na.rm=TRUE)
writeRaster(sum.res.prod, '../../../GIS/processed/res_prod.tif', gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"), overwrite = TRUE)
writeRaster(sum.res.harv, '../../../GIS/processed/res_harv.tif', gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"), overwrite = TRUE)
writeRaster(sum.res.avai, '../../../GIS/processed/res_avail.tif', gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"), overwrite = TRUE)
