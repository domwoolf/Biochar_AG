# scripts/mc_analysis.R
# Monte Carlo uncertainty analysis. Each draw samples the uncertain parameters of parameters.csv
# (PERT, uniform or normal marginals; correlated pairs jointly through a Gaussian copula,
# parameter_correlations.csv) and runs the same carbon-price sweep as the main results
# (run_price_sweep) for every grid cell of the region, at the regional discount rate and the reference
# plant size. Structural choices (plant size, pipeline architecture, EOR sinks, social discount rate) are
# tested separately as variants (variant_sweeps.R, manuscript_figures.R), not sampled here.
#
# For each region and draw it records the regional quantities that summarize the competition between
# technologies, matching the targets of the spatial SHAP analysis (spatial_shap.R):
#   be_PyCCS, be_BECCS   biomass-weighted median break-even carbon price ($/tCO2e) over all cells;
#                        cells that never break even count as above any price; censored at the top of
#                        the price grid (MC_PRICES)
#   takeover             biomass-weighted median price at which BECCS becomes the technology with the
#                        highest non-negative net value (censored likewise)
#   n0_BE                biomass-weighted mean net value of BE without carbon revenue ($/Mg)
# and, at each carbon price in REPORT_PRICES, the share of biomass allocated to each technology (highest
# non-negative net value) and the abatement of the allocated cells (Tg CO2e/yr).
#
# Output: results/mc_analysis_results.csv (one row per region and draw: sampled parameters + metrics).
# MC_shap.R fits the SHAP models and writes the summaries.

library(parallel)

source("scripts/manuscript_figures.R") # load_all and helpers

# Configuration. Run size (mc_size, set by the manuscript render or here):
#   quick     100 draws per region on a 25% sample of cells (smoke test)
#   revision  2,000 draws per region on a 25% sample of cells (during revisions)
#   full      5,000 draws per region on all cells (production)
# The cell sample is fixed per region and stratified by biomass (deciles); cells are weighted by stratum
# size / sample size, so biomass-weighted medians, shares and abatement totals are unbiased estimates of
# the all-cell values (at 25%, errors of 1-3 US$/t in the median prices and <2 percentage points in the shares,
# estimated at the central parameters).
if (!exists("mc_size")) mc_size <- if (exists("test_mode") && isTRUE(test_mode)) "quick" else "full"
MC_SIZES <- list(quick = c(n_runs = 100, cell_fraction = 0.25), revision = c(n_runs = 2000, cell_fraction = 0.25),
                 full = c(n_runs = 5000, cell_fraction = 1))
if (!mc_size %in% names(MC_SIZES)) stop("mc_size must be one of: ", paste(names(MC_SIZES), collapse = ", "))
n_runs <- MC_SIZES[[mc_size]][["n_runs"]]
cell_fraction <- MC_SIZES[[mc_size]][["cell_fraction"]]
# Throughput plateaus at about 8-12 workers on a 12-core machine: the vectorized R model is limited by
# memory bandwidth (fresh vectors for every operation), so more workers only slow each draw
n_cores <- min(12, max(1, parallel::detectCores() - 2))
regions <- c("US", "Europe", "China", "India")
MC_PRICES <- c(0, 25, 50, 75, 100, 125, 150, 175, 200, 250, 300, 400) # sweep grid ($/tCO2e)
REPORT_PRICES <- c(0, 50, 100, 150, 200, 250)

message(sprintf("Monte Carlo run size '%s': %d draws per region, %.0f%% of cells.", mc_size, n_runs, 100 * cell_fraction))
message("Using ", n_cores, " cores.")

params_df <- read.csv("BiocharAG/inst/extdata/parameters.csv", stringsAsFactors = FALSE)
correlations_df <- read.csv("BiocharAG/inst/extdata/parameter_correlations.csv", stringsAsFactors = FALSE)

# Scenario dimensions are fixed at their regional values, not sampled
fixed_params <- c("c_price", "discount_rate", "plant_mw_th")
# Spatial layers perturbed by a scalar multiplier (their bounds must be relative)
spatial_multiplier_params <- c(elec_price = "elec_price_multiplier", ff_c_intensity = "ff_ci_multiplier")

# Biomass-weighted median; NA (never) counts as +Inf
wmedian <- function(x, w) {
  x[is.na(x)] <- Inf
  o <- order(x)
  cw <- cumsum(w[o]) / sum(w)
  x[o][which(cw >= 0.5)[1]]
}

draw_metrics <- function(sw, bm) {
  top <- max(MC_PRICES)
  be <- sweep_breakeven(sw)
  out <- list(
    be_PyCCS = min(wmedian(be[, 3], bm), top),
    be_BECCS = min(wmedian(be[, 2], bm), top),
    takeover = min(wmedian(sweep_takeover(sw, k = 2, max_price = top), bm), top),
    n0_BE = sum((sw$n0[, 1] * bm)[is.finite(sw$n0[, 1])]) / sum(bm[is.finite(sw$n0[, 1])])
  )
  for (cp in REPORT_PRICES) {
    net <- sweep_net(sw, cp)
    net[is.na(net)] <- -Inf
    b <- max.col(net, ties.method = "first")
    nb <- net[cbind(seq_along(b), b)]
    viable <- is.finite(nb) & nb >= 0
    ab <- sweep_abate(sw, cp)[cbind(seq_along(b), b)]
    for (k in 1:3) out[[sprintf("share_%s_%d", c("BE", "BECCS", "PyCCS")[k], cp)]] <- sum(bm[viable & b == k]) / sum(bm)
    out[[sprintf("abate_%d", cp)]] <- sum((bm * ab)[viable], na.rm = TRUE) / 1e6
  }
  out
}

# Fixed, biomass-stratified sample of cells (own seed; does not affect the parameter draws). Returns
# the sampled indices and their weights (stratum size / sample size).
sample_cells <- function(bm, fraction, seed) {
  if (fraction >= 1) return(list(idx = seq_along(bm), w = rep(1, length(bm))))
  old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()))
  set.seed(seed)
  strata <- cut(rank(bm, ties.method = "first"), 10, labels = FALSE)
  idx <- integer(0); w <- numeric(0)
  for (s in unique(strata)) {
    cells <- which(strata == s)
    k <- max(1, round(fraction * length(cells)))
    pick <- if (length(cells) > 1) sample(cells, k) else cells
    idx <- c(idx, pick); w <- c(w, rep(length(cells) / k, k))
  }
  o <- order(idx)
  list(idx = idx[o], w = w[o])
}

# Subset the region's cell vectors
subset_vec <- function(vec, idx) {
  vec$active_indices <- vec$active_indices[idx]
  vec$xy <- vec$xy[idx, , drop = FALSE]
  vec$cell_area <- vec$cell_area[idx]
  vec$layers <- lapply(vec$layers, function(x) if (length(x) > 1) x[idx] else x)
  vec
}

set.seed(42)
all_results <- list()
for (r in regions) {
  dat <- load_region_data(r)
  vec_all <- dat$vec
  bm_all <- vec_all$layers$biomass_density * vec_all$cell_area
  smp <- sample_cells(bm_all, cell_fraction, seed = 1000 + match(r, regions))
  vec0 <- subset_vec(vec_all, smp$idx)
  bm <- bm_all[smp$idx] * smp$w # sampling-weighted biomass (Mg/yr represented by each sampled cell)

  p_local <- BiocharAG::set_scenario(region = r)
  for (sp in names(spatial_multiplier_params)) {
    if (tolower(params_df$dist_bounds[params_df$name == sp]) != "relative") stop(sp, " must use relative bounds.")
    p_local[[sp]] <- 1
  }
  dist_table <- BiocharAG::mc_distribution_table(params_df, central = p_local)
  dist_table <- dist_table[!dist_table$name %in% fixed_params, ]
  draws <- BiocharAG::sample_mc_parameters(dist_table, n_runs, correlations = correlations_df)
  for (sp in names(spatial_multiplier_params)) names(draws)[names(draws) == sp] <- spatial_multiplier_params[[sp]]

  message(sprintf("%s: %d draws, %d of %d cells", r, n_runs, length(bm), length(bm_all)))
  rows <- parallel::mclapply(seq_len(n_runs), function(m) {
    d <- draws[m, , drop = FALSE]
    p <- BiocharAG::set_scenario(region = r)
    p$region <- r
    for (nm in setdiff(names(d), spatial_multiplier_params)) p[[nm]] <- d[[nm]]
    vec <- vec0
    if (!is.null(vec$layers$elec_price)) vec$layers$elec_price <- vec$layers$elec_price * d$elec_price_multiplier
    if (!is.null(vec$layers$ff_c_intensity)) vec$layers$ff_c_intensity <- vec$layers$ff_c_intensity * d$ff_ci_multiplier
    sw <- run_price_sweep(dat$template, dat$layers, p, vec = vec, prices = MC_PRICES)
    c(list(mc_run_id = m, region = r, cell_fraction = cell_fraction), as.list(d), draw_metrics(sw, bm))
  }, mc.cores = n_cores, mc.preschedule = TRUE)
  err <- vapply(rows, inherits, logical(1), "try-error")
  if (any(err)) stop("Monte Carlo draw failed in ", r, ":\n", rows[[which(err)[1]]])
  all_results[[r]] <- do.call(rbind, lapply(rows, as.data.frame))
  message(sprintf("Finished %s", r))
}

results_df <- do.call(rbind, all_results)
dir.create("results", showWarnings = FALSE)
write.csv(results_df, "results/mc_analysis_results.csv", row.names = FALSE)
message("Monte Carlo analysis complete: results/mc_analysis_results.csv")
