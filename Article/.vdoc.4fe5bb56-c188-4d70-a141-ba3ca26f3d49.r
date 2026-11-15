# Numbers quoted in the Abstract and Results, computed from the model outputs (results/ai_summaries, results/mc_*)
suppressPackageStartupMessages(library(data.table))
tl <- c(BES = "BE", BECCS = "BECCS", BEBCS = "PyCCS")
ev <- fread("results/ai_summaries/evaporation_grid_summary_default.csv")[, tech := tl[technology]]
sh <- function(r, cp_, tech_, dr_ = "market") 100 * ev[region == r & dr == dr_ & cp == cp_ & tech == tech_ & viable == TRUE, sum(biomass_share)]
bio_total <- ev[dr == "market" & cp == 0, sum(biomass_Mg)] / 1e6
mc <- fread("results/ai_summaries/macc_data_default.csv")[, tech := tl[Technology]]
ab <- function(r, price, tech_ = c("BE", "BECCS", "PyCCS")) mc[Region %in% r & Price == price & Metric == "Abatement" & tech %in% tech_, sum(Value)]
cp <- fread("results/ai_summaries/per_tonne_components.csv")
comp <- function(r, tech, q) cp[region %in% r & technology == tech & quantity == q, value]
pyshare <- function(q) 100 * comp(R4, "PyCCS", q) / comp(R4, "PyCCS", "abatement") # % of PyCCS abatement, by region
mcr <- fread("results/mc_analysis_results.csv")
mcs <- mcr[, .(
  p_py = mean(be_PyCCS < be_BECCS), d50 = median(be_BECCS - be_PyCCS), n0pos = mean(n0_BE > 0),
  to05 = quantile(takeover, 0.05), to50 = median(takeover), to95 = quantile(takeover, 0.95),
  bepy50 = median(be_PyCCS), bebc50 = median(be_BECCS),
  bepy05 = quantile(be_PyCCS, 0.05), bepy95 = quantile(be_PyCCS, 0.95),
  a100_05 = quantile(abate_100, 0.05), a100_95 = quantile(abate_100, 0.95), n = .N
), by = region]
m <- function(r, v) mcs[region == r][[v]]
va <- fread("results/ai_summaries/variant_summary.csv")[, tech := tl[Technology]]
vv <- function(r, price, variant, tech_) va[Region == r & Price == price & startsWith(Variant, variant) & tech == tech_, Abatement]
sp <- fread("results/ai_summaries/spatial_shap_importance.csv")
imp <- function(r, tg, f) sp[region == r & target == tg & feature == f, share_pct]
ms <- fread("results/mc_shap/mc_shap_importance.csv")
cells <- fread("results/spatial_shap/shap_targets_by_cell.csv.gz")[, w := biomass_density * cell_area_km2]
# Technology allocated at the top carbon price (terminal pathway), by region (% of residue); the text states
# that no cell leaves BECCS once allocated to it
tp <- fread("results/ai_summaries/terminal_pathway_summary.csv")
stopifnot("A grid cell leaves BECCS at a higher carbon price; revise the takeover-price text" = sum(tp$cells_leaving_BECCS) == 0)
# Lock-in along the 2 °C carbon-price trajectory: paths for residue on which PyCCS is built first and BECCS later
# takes over (abatement 2030-2069, Gt CO2e; net present value, billion US$; years of PyCCS and BE operation)
lk_all <- fread("results/ai_summaries/lockin_cohorts.csv")
lk <- lk_all[co2_network == "shared" & trajectory == "below_2C"]
# Variants: dedicated pipelines (no shared trunklines) and the slower NGFS NDC carbon-price trajectory
lk_s <- unique(lk_all[, .(trajectory, co2_network, region, exposed_residue_Tg, pyccs_residue_Tg, median_takeover_year)])
lX <- function(tr, nw, v) { x <- lk_s[trajectory == tr & co2_network == nw]; if (v == "year") x[[ "median_takeover_year" ]] else sum(x[[v]]) }
ld <- lk_all[trajectory == "below_2C" & co2_network == "dedicated"]
ldt <- ld[, .(A = sum(abatement_Gt), V = sum(npv_bn_usd), py = weighted.mean(pyccs_years, exposed_residue_Tg)), by = path]
ldA <- function(pth) ldt[path == pth, A]
ldV <- function(pth) ldt[path == pth, V]
ld_npv <- dcast(ld, region ~ path, value.var = "npv_bn_usd")[, d := 100 * (beccs_first / sunk - 1)]
ndc_p <- 313.689 / 218.056 * c(52.0, 68.3) # NGFS NDC world carbon price 2030 and 2050, US$2010 to 2024 (as in the lock-in step)
lt <- lk[, .(A = sum(abatement_Gt), V = sum(npv_bn_usd), py = weighted.mean(pyccs_years, exposed_residue_Tg),
             be = weighted.mean(be_years, exposed_residue_Tg)), by = path]
lA <- function(pth) lt[path == pth, A]
lV <- function(pth) lt[path == pth, V]
lk_reg <- unique(lk[, .(region, exposed_residue_Tg, median_cohort_year, median_takeover_year)])
lk_npv <- dcast(lk, region ~ path, value.var = "npv_bn_usd")[, d := 100 * (beccs_first / sunk - 1)]
reg_name <- c(US = "the United States", Europe = "Europe", China = "China", India = "India")
rngY <- function(x) if (min(x) == max(x)) as.character(min(x)) else paste0(min(x), "–", max(x))
pct <- function(x) sub("-", "−", sprintf("%+.0f", x), fixed = TRUE)
py_life <- fread("../BiocharAG/inst/extdata/parameters.csv")[name == "py_life", as.numeric(default_value)]
py_first <- cells[, .(s = 100 * sum(w[which(be_PyCCS < be_BECCS | (is.finite(be_PyCCS) & !is.finite(be_BECCS)))]) / sum(w)), by = region]$s
to_spread <- cells[, .(d = quantile(takeover_BECCS, 0.9, na.rm = TRUE) - quantile(takeover_BECCS, 0.1, na.rm = TRUE)), by = region]$d
c_in <- 0.48 * 44 / 12 # CO2 equivalent of the feedstock carbon (Mg CO2 Mg-1 dry, ash-free)
msh <- function(r, q, p) 100 * ms[region == r & quantity == q & parameter == p, share]
f0 <- function(x) format(round(x), big.mark = ",")
f1 <- function(x) format(round(x, 1), nsmall = 1)
f2 <- function(x) format(round(x, 2), nsmall = 2)
rng0 <- function(x) paste0(f0(min(x)), "–", f0(max(x)))
rng2 <- function(x) paste0(f2(min(x)), "–", f2(max(x)))
R4 <- c("US", "Europe", "China", "India")
# Ratio of carbon stored by BECCS to that stored by PyCCS, per megagram of residue
st_ratio <- comp(R4, "BECCS", "carbon_stored") / comp(R4, "PyCCS", "carbon_stored")
# Fall in GHG abatement per megagram of residue between carbon prices of 0 and 200 US$ Mg-1 CO2e, by region (%)
ab_drop <- function(tech) 100 * (1 - comp(R4, tech, "abatement_cp200") / comp(R4, tech, "abatement_cp0"))
# Land area of the cells allocated to a technology at a carbon price, as % of the area of all cells that supply residue
ar <- function(r, price, tech_ = c("BE", "BECCS", "PyCCS")) {
  100 * mc[Region %in% r & Price == price & Metric == "Area" & tech %in% tech_, sum(Value)] /
    mc[Region %in% r & Metric == "Area", sum(Value), by = Price][, max(V1)]
}
# AR6 WGIII marginal abatement costs of CO2 (US$2015 per Mg CO2), escalated to 2024 with CPI-U (313.689 / 237.017)
cpi <- 313.689 / 237.017
