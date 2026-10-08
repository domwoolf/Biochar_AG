# Histograms of potential biochar production per hectare of harvested area or cropland, for developing the biochar
# yield-response method. Run from the repository root.
#
# Cells are the model's active cells for each region (load_region_data(): cells with residue inside the
# region's countries). Biochar production of a cell is the biochar made from all of its available residue
# (as if every cell supplied PyCCS), returned to the same cell: residue (Mg dry ash-free yr-1) x biochar
# yield (Mg biochar, including ash, per Mg dry ash-free feed) at the regional default parameters.
# The processed biomass layers hold as-received mass, Mg C / bm_c / (1 - bm_h2o - bm_ash)
# (data-raw/process_regions.R; issue #114), so the dry ash-free residue is that mass x (1 - bm_h2o - bm_ash).
# Biochar carbon is the organic carbon in that biochar, net of any biochar combusted to close the energy
# balance.
#
# The area denominator (DENOMINATOR) is either
#   "harvested": MapSPAM 2010 v2.0 harvested area (all production systems) summed over the 34 crops whose
#     residues are in the residue map (GIS/processed/spam2010_harv_area_residue_crops.tif, built from
#     GIS/raw/spam2010; Harvard Dataverse doi:10.7910/DVN/PRFF8V). Multiple cropping counts each harvest,
#     so this gives production per harvested hectare. Consistent with the SPAM-based residue map.
#   "cropland": cropland fraction (GIS/raw/cropland/cropland.tif, 5 arcmin), which is not consistent with
#     the residue map.
# Either is converted to a fraction of pixel area, averaged onto the region grid and multiplied by cell
# area. Cells with no area, or with more available residue per hectare than is physically plausible
# (MAX_RESIDUE_PER_HA), are excluded and reported separately.
#
# Outputs (results/, suffixed with the denominator):
#   biochar_production_histograms_<denominator>.csv  N_BINS rows per region, binned on biochar per hectare
#   biochar_production_excluded_<denominator>.csv    cells excluded per region and reason
#   biochar_production_cells_<denominator>.csv.gz    per-cell values (all active cells, `excluded` flag)
suppressMessages(devtools::load_all("BiocharAG", quiet = TRUE))
library(data.table)

DENOMINATOR <- "harvested" # or "cropland"
N_BINS <- 20
# Upper bound on available residue per hectare (Mg dry ash-free ha-1 yr-1). Total residue production of
# double- or triple-cropped high-yield cereals is about 15-20 Mg DM per hectare of cropland per year (less
# per harvested hectare), and the available fraction is well below one, so 20 is a generous bound.
MAX_RESIDUE_PER_HA <- 20

area_frac <- if (DENOMINATOR == "harvested") {
  h <- terra::rast("GIS/processed/spam2010_harv_area_residue_crops.tif") # ha per pixel
  h / terra::cellSize(h, unit = "ha")
} else {
  terra::rast("GIS/raw/cropland/cropland.tif")
}
p0 <- get_default_parameters()

cells <- rbindlist(lapply(c("US", "Europe", "China", "India"), function(r) {
  q <- apply_regional_overrides(p0, r)
  ph <- calculate_pyrolysis_physics(q$py_temp, q$lignin, q$bm_lhv, q$bm_h2o, q$bm_ash, q$bm_c, q$bm_h,
    heater_eff = q$py_heater_eff, parasitic_power = q$py_parasitic_power, power_eff = q$bebcs_power_efficiency,
    exhaust_temp = q$py_exhaust_temp)
  dat <- load_region_data(r)
  idx <- dat$vec$active_indices
  crop <- terra::resample(area_frac, dat$template, method = "average") # area as a fraction of the cell
  d <- data.table(region = r, x = dat$vec$xy[, 1], y = dat$vec$xy[, 2],
                  area_km2 = dat$vec$cell_area,
                  residue_daf_Mg = dat$vec$layers$biomass_density * dat$vec$cell_area * (1 - q$bm_h2o - q$bm_ash),
                  area_frac = terra::values(crop, mat = FALSE)[idx])
  d[is.na(area_frac), area_frac := 0]
  d[, area_ha := area_frac * area_km2 * 100]
  d[, `:=`(biochar_Mg = residue_daf_Mg * ph$yield_bc, biochar_C_Mg = residue_daf_Mg * ph$bc_c_yield)]
  d[, `:=`(residue_daf_Mg_per_ha = fifelse(area_ha > 0, residue_daf_Mg / area_ha, NA_real_),
           biochar_Mg_per_ha = fifelse(area_ha > 0, biochar_Mg / area_ha, NA_real_),
           biochar_C_Mg_per_ha = fifelse(area_ha > 0, biochar_C_Mg / area_ha, NA_real_))]
  d[, excluded := fcase(area_ha <= 0, "no area",
                        residue_daf_Mg_per_ha > MAX_RESIDUE_PER_HA, "implausible residue per ha",
                        default = "")]
  message(sprintf("%s: %d active cells; biochar yield %.3f Mg / Mg daf, C yield %.3f",
                  r, nrow(d), ph$yield_bc, ph$bc_c_yield))
  d
}))

# Equal-width bins of biochar per hectare: N_BINS - 1 bins from 0 to the 99th percentile
# of the retained cells, and one open bin above it
hist <- rbindlist(lapply(split(cells[excluded == ""], by = "region"), function(d) {
  br <- c(seq(0, quantile(d$biochar_Mg_per_ha, 0.99), length.out = N_BINS), Inf)
  d[, bin := cut(biochar_Mg_per_ha, br, include.lowest = TRUE, right = FALSE, labels = FALSE)]
  out <- d[, .(n_cells = .N, mean_cell_area_km2 = mean(area_km2), area_ha = sum(area_ha),
               residue_daf_Mg = sum(residue_daf_Mg), biochar_Mg = sum(biochar_Mg),
               biochar_C_Mg = sum(biochar_C_Mg)), by = bin]
  out <- merge(data.table(bin = seq_len(N_BINS), lower_Mg_ha = head(br, -1), upper_Mg_ha = br[-1]), out,
               by = "bin", all.x = TRUE)
  for (v in setdiff(names(out), c("bin", "lower_Mg_ha", "upper_Mg_ha"))) set(out, which(is.na(out[[v]])), v, 0)
  out[, `:=`(mean_biochar_Mg_per_ha = fifelse(area_ha > 0, biochar_Mg / area_ha, NA_real_),
             mean_biochar_C_Mg_per_ha = fifelse(area_ha > 0, biochar_C_Mg / area_ha, NA_real_),
             share_cells = n_cells / sum(n_cells), share_area = area_ha / sum(area_ha),
             share_biochar = biochar_Mg / sum(biochar_Mg))]
  out[, cum_share_biochar := cumsum(share_biochar)]
  cbind(data.table(region = d$region[1]), out)
}))

excl <- cells[excluded != "", .(n_cells = .N, area_ha = sum(area_ha), biochar_Mg = sum(biochar_Mg)),
              by = .(region, excluded)]
excl <- merge(excl, cells[, .(total_cells = .N, total_biochar_Mg = sum(biochar_Mg)), by = region], by = "region")
excl[, `:=`(share_cells = n_cells / total_cells, share_biochar = biochar_Mg / total_biochar_Mg)]

dir.create("results", showWarnings = FALSE)
fwrite(hist, paste0("results/biochar_production_histograms_", DENOMINATOR, ".csv"))
fwrite(excl, paste0("results/biochar_production_excluded_", DENOMINATOR, ".csv"))
fwrite(cells, paste0("results/biochar_production_cells_", DENOMINATOR, ".csv.gz"))

print(excl[, .(region, excluded, n_cells, share_cells = round(share_cells, 4), share_biochar = round(share_biochar, 4))])
print(cells[excluded == "", .(cells = .N, mean_cell_km2 = mean(area_km2), residue_daf_Tg = sum(residue_daf_Mg) / 1e6,
                              biochar_Tg = sum(biochar_Mg) / 1e6, area_Mha = sum(area_ha) / 1e6,
                              p50_Mg_ha = median(biochar_Mg_per_ha), p90_Mg_ha = quantile(biochar_Mg_per_ha, 0.9),
                              mean_Mg_ha = sum(biochar_Mg) / sum(area_ha)), by = region], digits = 3)
