# Mineral composition of cereal residues from the Phyllis2 database (TNO), issue #110.
# Writes cereal_composition_phyllis2.csv: median contents (kg per Mg dry matter) of Ca, Mg, K, Na, P,
# N, Cl and S, the ash content (% dry) and the SiO2 share of ash, by SPAM residue code.
#
# Input: Resources/Phyllis2/phyllis.sqlite (a local scrape of https://phyllis.nl, not in the
# repository). Run from BiocharAG/data-raw/karan_residues/.
#
# Elements from the ash analysis (oxides, wt% of ash) are converted with the record's own ash content;
# records without an ash content use the median ash of their crop, but only when fewer than three
# records have both. N, Cl and S come from the fuel analysis (dry basis), not from lab ash, which
# loses Cl and S. Washed, soaked, sprayed and leached samples are excluded.
#
# Gaps filled by assumption: millets (PMIL, SMIL) take the sorghum values; maize cob (MAIZC) takes
# the maize stover ash composition scaled to the cob ash content, because the single cob ash analysis
# in Phyllis2 has an implausibly low K content.
library(DBI)
library(data.table)

db <- dbConnect(RSQLite::SQLite(), "../../../Resources/Phyllis2/phyllis.sqlite", flags = RSQLite::SQLITE_RO)
recs <- setDT(dbGetQuery(db, "SELECT id, material, classification_ecn AS cls FROM records"))
props <- setDT(dbGetQuery(db, "SELECT record_id AS id, property_name AS prop, value_ar, value_dry FROM properties"))
dbDisconnect(db)

crop_group <- function(cls, mat) {
  m <- tolower(mat); k <- tolower(cls)
  out <- rep(NA_character_, length(m))
  straw <- grepl("straw (stalk/cob/ear)", k, fixed = TRUE)
  k2 <- sub(".*straw \\(stalk/cob/ear\\)", "", k)
  out[straw & grepl("\u25b8 rice", k2)] <- "RICE"
  out[straw & grepl("\u25b8 wheat", k2) & !grepl("rye", k2)] <- "WHEA"
  maize <- straw & grepl("maize|corn", k2)
  out[maize] <- ifelse(grepl("cob", m[maize]) | grepl("cob", k2[maize]), "MAIZC", "MAIZ")
  out[straw & grepl("barley", k2)] <- "BARL"
  out[straw & grepl("sorghum", k2)] <- "SORG"
  out[straw & is.na(out) & (grepl("rye", k2) | grepl("oat", m) | grepl("oat", k2))] <- "OCER"
  out[grepl("husk/shell/pit", k, fixed = TRUE) & grepl("\u25b8 rice", k) & grepl("hull|husk", m)] <- "RICEH"
  out[grepl("wash|soak|leach|spray", m)] <- NA
  out
}
recs[, crop := crop_group(cls, material)]
recs <- recs[!is.na(crop)]

val <- function(p, col) {
  x <- props[prop == p & id %in% recs$id & !is.na(get(col)), .(id, v = get(col))]
  x[!duplicated(id)]
}
d <- recs[, .(id, crop)]
d <- merge(d, setnames(val("Ash content", "value_dry"), "v", "ash"), by = "id", all.x = TRUE)
oxides <- c(CaO = "Ca", MgO = "Mg", K2O = "K", Na2O = "Na", P2O5 = "P", SiO2 = "SiO2")
for (o in names(oxides)) d <- merge(d, setnames(val(o, "value_ar"), "v", o), by = "id", all.x = TRUE) # wt% of ash
d <- merge(d, setnames(val("Nitrogen", "value_dry"), "v", "N_pct"), by = "id", all.x = TRUE)
d <- merge(d, setnames(val("Chlorine (Cl)", "value_dry"), "v", "Cl_mgkg"), by = "id", all.x = TRUE)
d <- merge(d, setnames(val("Sulphur", "value_dry"), "v", "S_pct"), by = "id", all.x = TRUE)

# Element mass fraction of each oxide
ox_frac <- c(CaO = 40.078 / 56.077, MgO = 24.305 / 40.304, K2O = 78.196 / 94.196, Na2O = 45.979 / 61.979, P2O5 = 61.948 / 141.945)
ash_med <- d[!is.na(ash), .(ash_med = median(ash)), by = crop]
d <- merge(d, ash_med, by = "crop", all.x = TRUE)
summ <- d[, {
  out <- list(n_records = .N, ash_pct = median(ash, na.rm = TRUE), SiO2_pct_ash = median(SiO2, na.rm = TRUE))
  for (o in names(ox_frac)) {
    meas <- !is.na(get(o)) & !is.na(ash)
    v <- get(o)[meas] / 100 * ash[meas] * 10 * ox_frac[[o]]
    if (length(v) < 3) {
      imp <- !is.na(get(o)) & is.na(ash)
      v <- c(v, get(o)[imp] / 100 * ash_med[imp] * 10 * ox_frac[[o]])
    }
    out[[oxides[[o]]]] <- if (length(v)) median(v) else NA_real_
    out[[paste0("n_", oxides[[o]])]] <- length(v)
  }
  out$N <- median(N_pct * 10, na.rm = TRUE); out$n_N <- sum(!is.na(N_pct))
  out$Cl <- median(Cl_mgkg / 1000, na.rm = TRUE); out$n_Cl <- sum(!is.na(Cl_mgkg))
  out$S <- median(S_pct * 10, na.rm = TRUE); out$n_S <- sum(!is.na(S_pct))
  out
}, by = crop]

# Gap filling
cation_cols <- c("Ca", "Mg", "K", "Na", "P")
cob <- summ[crop == "MAIZC"]; stover <- summ[crop == "MAIZ"]
for (e in cation_cols) summ[crop == "MAIZC", (e) := stover[[e]] * cob$ash_pct / stover$ash_pct]
summ[crop == "MAIZC", note := "Ca, Mg, K, Na, P: maize stover ash composition scaled to cob ash content"]
for (cn in c("PMIL", "SMIL")) summ <- rbind(summ, copy(summ[crop == "SORG"])[, `:=`(crop = cn, note = "sorghum values")], fill = TRUE)
summ[is.na(note), note := ""]

setcolorder(summ, c("crop", "n_records", "ash_pct", "SiO2_pct_ash", "Ca", "Mg", "K", "Na", "P", "N", "Cl", "S"))
fwrite(summ[order(crop)], "cereal_composition_phyllis2.csv")
print(summ[order(crop), .(crop, n_records, ash_pct, Ca, Mg, K, Na, P, N, Cl, S, n_K, n_Cl)], digits = 3)
