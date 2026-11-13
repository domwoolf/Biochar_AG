# scripts/spatial_shap.R
# Spatial drivers of technology costs and of the takeover of biochar by geological storage (issue #111).
#
# For each region, the main model path (run_price_sweep(), default regional parameters) gives four
# targets per grid cell, in the order in which they are usually reached as the carbon price rises:
#   be_PyCCS        break-even carbon price of PyCCS (US$ Mg-1 CO2e)
#   beccs_over_be   lowest carbon price at which BECCS has a higher net value than BE
#   be_BECCS        break-even carbon price of BECCS
#   takeover_BECCS  lowest carbon price at which BECCS has the highest non-negative net value (displayed as
#                   the price at which BECCS outcompetes PyCCS; the crossover price in the manuscript)
# BE has no break-even target: it abates too little for its break-even price to fall within the price range.
# A gradient-boosted regression (XGBoost) predicts each target from the cell's spatial inputs, and SHAP
# values attribute each prediction to the inputs. Models are fitted per region, so that inputs constant
# within a region (e.g. feedstock cost outside Europe) cannot stand in for the region itself.
#
# Outputs (results/spatial_shap/): shap_importance.csv, shap_targets_by_cell.csv.gz, importance figure
# (input groups per target and region, anchored on the pooled median net value of each technology against
# the carbon price), dependence figures, maps of the takeover price and its dominant driver; and
# results/ai_summaries/spatial_shap_admin1_summary.csv, spatial_shap_importance.csv and
# terminal_pathway_summary.csv (technology allocated at the top carbon price, by region).

library(data.table)
library(xgboost)
library(shapviz)
library(ggplot2)
library(patchwork)
library(terra)
library(sf)

sf::sf_use_s2(FALSE)
source("scripts/manuscript_figures.R") # load_all, display_admin(), tech_label(), TECH_COLORS
source("scripts/results_cache.R") # cached(), cache_key(), fp_model()

OUT_DIR <- "results/spatial_shap"
REGIONS <- c("US", "Europe", "China", "India")
MAP_HEIGHT <- 3.4 # height of each map panel (inches); widths follow each region's aspect ratio
N_PLOT <- 20000 # cells sampled for dependence plots (SHAP is computed on all cells)
MAX_PRICE <- 500 # top of the carbon-price grid (US$ Mg-1 CO2e)
XGB_THREADS <- 1L # threads per XGBoost model: one model per worker, all models at once
N_WORKERS <- max(1L, parallel::detectCores() - 2L) # parallel workers (capped by the number of models in par_map)

# Apply FUN to each element of X in separate R processes (PSOCK, not fork: forked children can hang in
# OpenMP once the parent has used it, e.g. through data.table). Objects named in `export` are copied from
# the global environment; worker messages go to the log.
par_map <- function(X, FUN, export = character()) {
  if (N_WORKERS <= 1L || length(X) <= 1L) return(lapply(X, FUN))
  cl <- parallel::makePSOCKcluster(min(N_WORKERS, length(X)), outfile = "")
  on.exit(parallel::stopCluster(cl), add = TRUE)
  wd <- getwd()
  parallel::clusterCall(cl, function(wd) {
    setwd(wd)
    suppressPackageStartupMessages({ library(data.table); library(xgboost); library(shapviz) })
    data.table::setDTthreads(1L)
    source("scripts/results_cache.R")
    NULL
  }, wd)
  if (length(export)) parallel::clusterExport(cl, export, envir = globalenv())
  parallel::parLapplyLB(cl, X, FUN)
}
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

TARGETS <- c(be_PyCCS = "Break-even price, PyCCS", beccs_over_be = "Price at which BECCS outcompetes BE",
             be_BECCS = "Break-even price, BECCS", takeover_BECCS = "Price at which BECCS outcompetes PyCCS")
TARGET_UNITS <- setNames(rep("US$ Mg⁻¹ CO₂e", length(TARGETS)), names(TARGETS))
ANCHOR_PRICES <- seq(0, 300, by = 5) # carbon prices of the net-value curves in the importance figure

feature_labels <- c(
  biomass_density = "Residue density", dist_collect = "Collection distance", haul_mult = "Haulage cost multiplier",
  burn_frac = "Share of residue burned", feedstock_cost = "Feedstock cost", elec_price = "Electricity price",
  ff_c_intensity = "Grid carbon intensity", soil_temp = "Soil temperature", soil_ph = "Soil pH", soil_cec = "Soil CEC",
  bc_supply = "Biochar supply per hectare", crop_value = "Crop value per hectare",
  co2_route_km = "CO₂ route length", co2_offshore = "Offshore CO₂ sink", co2_storage_cost = "CO₂ storage cost"
)
feature_units <- c(
  biomass_density = "Mg km\u207b\u00b2 yr\u207b\u00b9", dist_collect = "km",
  haul_mult = "ratio", burn_frac = "fraction", feedstock_cost = "US$ Mg\u207b\u00b9", elec_price = "US$ MWh\u207b\u00b9",
  ff_c_intensity = "Mg CO\u2082 GJ\u207b\u00b9", soil_temp = "\u00b0C", soil_ph = "pH", soil_cec = "cmolc kg\u207b\u00b9",
  bc_supply = "Mg ha\u207b\u00b9 yr\u207b\u00b9", crop_value = "US$ ha\u207b\u00b9 yr\u207b\u00b9", co2_route_km = "km",
  co2_offshore = "0/1", co2_storage_cost = "US$ Mg\u207b\u00b9 CO\u2082"
)
feature_palette <- c(
  biomass_density = "#2ca02c", dist_collect = "#98df8a", haul_mult = "#c5b0d5", burn_frac = "#ff9896",
  feedstock_cost = "#17becf", elec_price = "#1f77b4", ff_c_intensity = "#ff7f0e", soil_temp = "#d62728",
  soil_ph = "#7f7f7f", soil_cec = "#bcbd22", bc_supply = "#8c6d31", crop_value = "#e7ba52",
  co2_route_km = "#9467bd", co2_offshore = "#e377c2", co2_storage_cost = "#8c564b"
)

# Groups of inputs in the importance figure, in stacking order: inputs specific to PyCCS, inputs common to
# the technologies, inputs specific to BECCS
INPUT_GROUP <- c(soil_temp = "Soil and crop", soil_cec = "Soil and crop", soil_ph = "Soil and crop",
                 bc_supply = "Soil and crop", crop_value = "Soil and crop",
                 biomass_density = "Residue supply and management", dist_collect = "Residue supply and management",
                 haul_mult = "Residue supply and management", feedstock_cost = "Residue supply and management",
                 burn_frac = "Residue supply and management",
                 elec_price = "Electricity grid", ff_c_intensity = "Electricity grid",
                 co2_route_km = "CO₂ transport and storage", co2_offshore = "CO₂ transport and storage",
                 co2_storage_cost = "CO₂ transport and storage")
GROUP_COLS <- c("Soil and crop" = "#e0a100", "Residue supply and management" = "#a8642a",
                "Electricity grid" = "#1f9e9e", "CO₂ transport and storage" = "#9b5fc9") # validated palette (light)

# Lowest carbon price at which technology k has a higher net value than technology j, exact on the
# piecewise-quadratic net value of the sweep (as sweep_takeover(), for one pair); NA if not below max_price
pair_price <- function(sw, k, j, max_price = MAX_PRICE) {
  n_cell <- dim(sw$abate)[1]
  out <- rep(NA_real_, n_cell)
  todo <- rep(TRUE, n_cell)
  for (i in seq_along(sw$prices)) {
    if (!any(todo) || sw$prices[i] > max_price) break
    sg <- BiocharAG:::sweep_segment(sw, i)
    wmax <- min(sg$wmax, (max_price - sg$p) / sg$d)
    idx <- which(todo)
    c0 <- sg$c0[idx, k] - sg$c0[idx, j]; b <- sg$b[idx, k] - sg$b[idx, j]; a <- sg$a[idx, k] - sg$a[idx, j]
    w <- ifelse(!is.na(c0) & c0 >= 0, 0, NA_real_)
    r <- BiocharAG:::quad_roots01(c0, b, a, wmax)
    eps <- 1e-9 * if (is.finite(sg$wmax)) 1 else max(1, sg$p)
    for (m in 1:2) {
      wt <- pmin(r[, m] + eps, wmax)
      ok <- is.na(w) & !is.na(r[, m]) & (c0 + b * wt + a * wt^2) >= 0
      w[ok] <- r[ok, m]
    }
    hit <- !is.na(w)
    out[idx[hit]] <- sg$p + sg$d * w[hit]
    todo[idx[hit]] <- FALSE
  }
  out
}

# ==============================================================================
# Targets and spatial inputs per region
# ==============================================================================

region_data <- function(r) {
  message("Region ", r, ": price sweep and spatial inputs...")
  dat <- load_region_data(r)
  p0 <- set_scenario(region = r)
  p0$region <- r
  sw <- run_price_sweep(dat$template, dat$layers, p0, vec = dat$vec, prices = c(seq(0, 250, by = 5), seq(275, MAX_PRICE, by = 25)))
  be_all <- sweep_breakeven(sw)
  be <- function(k) be_all[, k]
  # Allocation at each sweep price (0 none, 1 BE, 2 BECCS, 3 PyCCS): the technology with the highest
  # non-negative net value. The terminal pathway is the allocation at the top price; leaves_BECCS flags
  # cells allocated to BECCS at some price but to another technology (or none) at a higher one.
  alloc <- sapply(sw$prices, function(cp) {
    nv <- sweep_net(sw, cp)
    nv[!is.finite(nv)] <- -Inf
    a <- max.col(nv, ties.method = "first")
    a[apply(nv, 1, max) < 0] <- 0L
    a
  })
  first_beccs <- apply(alloc == 2, 1, function(v) if (any(v)) which(v)[1] else NA_integer_)
  leaves_beccs <- vapply(seq_len(nrow(alloc)), function(i) {
    f <- first_beccs[i]
    !is.na(f) && any(alloc[i, f:ncol(alloc)] != 2)
  }, logical(1))
  # Inputs at the default plant size, as in run_scenario()
  p <- cell_params(p0, dat$vec)
  p$c_price <- 0
  beccs <- calculate_beccs(p)
  pyccs <- calculate_bebcs(p)
  sl <- dat$vec$layers
  sclass <- beccs$co2_sink_class
  sfac <- if (!is.null(p$storage_cost_factor)) p$storage_cost_factor else 1
  stor_of <- function(layer, base) {
    v <- sl[[layer]]
    if (is.null(v)) rep(base, length(sclass)) else ifelse(is.na(v), base, v * sfac)
  }
  co2_storage_cost <- ifelse(sclass == 1, stor_of("onsal_storage_cost", p$ccs_storage_cost),
    ifelse(sclass == 2, p$ccs_storage_cost,
      ifelse(sclass == 3, stor_of("offship_storage_cost", p$cost_offshore_storage), stor_of("offpipe_storage_cost", p$cost_offshore_storage))))
  fac <- function(x) if (is.null(x)) 1 else ifelse(is.na(x), 1, x)
  s_t <- p$haul_time_share
  s_f <- p$haul_fuel_share
  haul_mult <- s_t * fac(p$haul_kt) + s_f * fac(p$haul_kd) * fac(p$haul_g) + max(0, 1 - s_t - s_f) * fac(p$haul_kd)
  n <- length(dat$vec$active_indices)
  rep_n <- function(x) if (length(x) == n) x else rep_len(x, n)
  # Net value of each technology (cells x 3: BE, BECCS, PyCCS) at the anchor prices
  nv <- lapply(ANCHOR_PRICES, function(cp) sweep_net(sw, cp))
  cells <- data.table(
    region = r, x = dat$vec$xy[, 1], y = dat$vec$xy[, 2], cell_area_km2 = dat$vec$cell_area,
    be_PyCCS = be(3), beccs_over_be = pair_price(sw, 2, 1), be_BECCS = be(2),
    takeover_BECCS = sweep_takeover(sw, k = 2, max_price = MAX_PRICE),
    biomass_density = sl$biomass_density, dist_collect = rep_n(p$avg_dist_BES_BASE), haul_mult = rep_n(haul_mult),
    burn_frac = rep_n(residue_burn_share(p)), feedstock_cost = rep_n(p$feedstock_cost),
    elec_price = rep_n(p$elec_price), ff_c_intensity = rep_n(p$ff_c_intensity), soil_temp = rep_n(p$soil_temp),
    soil_ph = rep_n(p$soil_ph), soil_cec = rep_n(p$soil_cec), bc_supply = rep_n(pyccs$bc_supply_ha),
    crop_value = rep_n(if (is.null(p$crop_value)) NA_real_ else p$crop_value),
    co2_route_km = rep_n(beccs$co2_transport_distance_km), co2_offshore = as.numeric(sclass %in% c(3, 4)),
    co2_storage_cost = co2_storage_cost,
    biomass = sl$biomass_density * dat$vec$cell_area, terminal = alloc[, ncol(alloc)], leaves_BECCS = leaves_beccs,
    beccs_no_route = is.na(sclass)
  )
  list(cells = cells, nv = nv, w = sl$biomass_density * dat$vec$cell_area)
}

# Pooled median net value of each technology at each anchor price (weighted by residue, all regions)
anchor_curves <- function(rd) {
  w <- unlist(lapply(rd, `[[`, "w"))
  wmed <- function(x) { ok <- is.finite(x) & is.finite(w); o <- order(x[ok]); x[ok][o][which(cumsum(w[ok][o]) >= sum(w[ok]) / 2)[1]] }
  rbindlist(lapply(seq_along(ANCHOR_PRICES), function(k) {
    m <- do.call(rbind, lapply(rd, function(x) x$nv[[k]]))
    data.table(price = ANCHOR_PRICES[k], tech = c("BE", "BECCS", "PyCCS"), net_value = apply(m, 2, wmed))
  }))
}

# Results cache (scripts/results_cache.R): the cell table, the fitted models and the SHAP interaction values
# are each reused while the model and the code of their own stage are unchanged, so that edits to figures
# or summaries do not repeat the price sweeps, fits or interaction values
key_cells <- cache_key(fp_model(), region_data, pair_price, anchor_curves, REGIONS, MAX_PRICE, ANCHOR_PRICES)
cc <- cached("cells", key_cells, {
  rd <- lapply(REGIONS, region_data)
  list(cells = rbindlist(lapply(rd, `[[`, "cells")), curves = anchor_curves(rd))
}, sub = "spatial_shap")
cells <- cc$cells
curves <- cc$curves
fwrite(cells, file.path(OUT_DIR, "shap_targets_by_cell.csv.gz"))
# Terminal pathway by region (biomass shares, %): technology allocated at the top of the price sweep; cells
# that leave BECCS at a higher price after being allocated to it; and, of the biomass not terminal in BECCS,
# the part with no CO2 route to a storage basin (BECCS undefined) and the part with a route, where BECCS
# would take over only above the top price.
term <- cells[, {
  w <- biomass / sum(biomass, na.rm = TRUE) * 100
  nb <- terminal != 2
  .(cells = .N, top_price = MAX_PRICE,
    terminal_BECCS = sum(w[terminal == 2], na.rm = TRUE), terminal_PyCCS = sum(w[terminal == 3], na.rm = TRUE),
    terminal_BE = sum(w[terminal == 1], na.rm = TRUE), terminal_none = sum(w[terminal == 0], na.rm = TRUE),
    cells_leaving_BECCS = sum(leaves_BECCS), leaving_BECCS = sum(w[leaves_BECCS], na.rm = TRUE),
    not_terminal_no_route = sum(w[nb & beccs_no_route], na.rm = TRUE),
    not_terminal_with_route = sum(w[nb & !beccs_no_route], na.rm = TRUE))
}, by = region]
fwrite(term, "results/ai_summaries/terminal_pathway_summary.csv")
FEATURES <- names(feature_labels)
# Each target sees only the inputs that enter the economics of the technologies it depends on, so that
# SHAP cannot attribute a cost to a correlated input the technology does not use (e.g. soil temperature
# for BE, which correlates with the haulage multiplier in India). Residue density enters through the
# collection distance, which is computed from it. One collection distance (a plant of the reference size at
# base load) stands for all technologies: each technology's distance scales with it through its capacity
# factor (rank correlation 0.993-1.000 between technologies in every region).
BASE_INPUTS <- c("dist_collect", "haul_mult", "feedstock_cost", "elec_price", "soil_ph") # soil pH: lime credit of ash and biochar, removal charge
TARGET_FEATURES <- list(
  be_PyCCS = c(BASE_INPUTS, "ff_c_intensity", "burn_frac", "soil_temp", "soil_cec", "bc_supply", "crop_value"),
  be_BECCS = c(BASE_INPUTS, "ff_c_intensity", "burn_frac", "co2_route_km", "co2_offshore", "co2_storage_cost")
)
TARGET_FEATURES$beccs_over_be <- TARGET_FEATURES$be_BECCS # BE uses a subset of the inputs of BECCS
TARGET_FEATURES$takeover_BECCS <- union(TARGET_FEATURES$be_BECCS, TARGET_FEATURES$be_PyCCS)
TARGET_FEATURES <- TARGET_FEATURES[names(TARGETS)]

# ==============================================================================
# Regression models and SHAP values (one per target and region)
# ==============================================================================

fit_shap <- function(d, target) {
  y <- d[[target]]
  ok <- is.finite(y) & abs(y) <= MAX_PRICE * 4
  d <- d[ok]
  y <- y[ok]
  cand <- intersect(FEATURES, TARGET_FEATURES[[target]])
  v <- vapply(cand, function(f) stats::var(d[[f]], na.rm = TRUE), numeric(1))
  f <- cand[!is.na(v) & v > 1e-10]
  X <- as.matrix(d[, ..f])
  set.seed(42)
  test <- sample.int(nrow(X), floor(0.2 * nrow(X)))
  pars <- list(objective = "reg:squarederror", max_depth = 6, eta = 0.05, subsample = 0.8, nthread = XGB_THREADS)
  m_cv <- xgb.train(params = pars, data = xgb.DMatrix(X[-test, , drop = FALSE], label = y[-test]), nrounds = 400, verbose = 0)
  pred <- predict(m_cv, X[test, , drop = FALSE])
  r2 <- 1 - sum((y[test] - pred)^2) / sum((y[test] - mean(y[test]))^2)
  model <- xgb.train(params = pars, data = xgb.DMatrix(X, label = y), nrounds = 400, verbose = 0)
  shp <- shapviz(model, X_pred = X)
  message(sprintf("  %-14s %-7s %6d cells (%4.1f%% undefined), %2d inputs, holdout R2 %.3f",
                  target, d$region[1], nrow(X), 100 * (1 - mean(ok)), length(f), r2))
  list(target = target, region = d$region[1], features = f, shp = shp, r2 = r2, data = d, model = model, X = X,
       undefined = 1 - mean(ok))
}

fit_all <- function(cells) {
  message("Fitting regression models (", N_WORKERS, " in parallel)...")
  jobs <- list()
  for (tg in names(TARGETS)) for (r in REGIONS) {
    d <- cells[region == r]
    if (sum(is.finite(d[[tg]])) < 200) {
      message("  ", tg, " ", r, ": too few defined cells; skipped")
      next
    }
    jobs[[paste(tg, r)]] <- list(d = d, tg = tg)
  }
  par_map(jobs, function(j) fit_shap(j$d, j$tg), export = c("fit_shap", "FEATURES", "TARGET_FEATURES", "MAX_PRICE", "XGB_THREADS"))
}
key_models <- cache_key(key_cells, fit_all, fit_shap, FEATURES, names(TARGETS), TARGET_FEATURES) # names, not labels
models <- cached("models", key_models, fit_all(cells), sub = "spatial_shap")
for (m in models) message(sprintf("  %-14s %-7s holdout R2 %.3f", m$target, m$region, m$r2))

imp <- rbindlist(lapply(models, function(m) {
  s <- colMeans(abs(m$shp$S))
  data.table(target = m$target, region = m$region, feature = m$features, mean_abs_shap = s,
             share_pct = 100 * s / sum(s), holdout_r2 = m$r2, share_undefined = m$undefined)
}))
fwrite(imp, file.path(OUT_DIR, "shap_importance.csv"))

# ==============================================================================
# Importance figure. Panels a-d: share of mean |SHAP| by group of inputs, per target (in carbon-price order)
# and region, with the region's residue-weighted median of the target. Panel e: pooled median net value of
# each technology against the carbon price, marked where the curves cross zero or each other (a-d)
# ==============================================================================
INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID_COL <- "#e6e5e1"
REGION_LAB <- c(US = "United States", Europe = "Europe", China = "China", India = "India")
theme_imp <- function() {
  theme_minimal(base_size = 9) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = GRID_COL, linewidth = 0.3),
          axis.text = element_text(colour = INK2), axis.title = element_text(colour = INK2),
          plot.title = element_text(face = "bold", size = 9, colour = INK), legend.position = "bottom",
          plot.background = element_rect(fill = "white", colour = NA))
}
LETTERS_T <- setNames(letters[seq_along(TARGETS)], names(TARGETS))
wmed_cells <- function(x, w) { ok <- is.finite(x) & is.finite(w); o <- order(x[ok]); x[ok][o][which(cumsum(w[ok][o]) >= sum(w[ok]) / 2)[1]] }
reg_med <- rbindlist(lapply(names(TARGETS), function(tg) cells[, .(target = tg, med = wmed_cells(get(tg), biomass)), by = region]))
grp <- imp[, .(share = sum(share_pct)), by = .(target, region, group = INPUT_GROUP[feature])]
grp <- grp[CJ(target = names(TARGETS), region = REGIONS, group = names(GROUP_COLS)), on = .(target, region, group)][is.na(share), share := 0]
grp[, group := factor(group, levels = names(GROUP_COLS))]
grp[, region := factor(region, levels = rev(REGIONS))]
reg_med[, region := factor(region, levels = rev(REGIONS))]
bar_panel <- function(tg) {
  ggplot(grp[target == tg], aes(x = share, y = region, fill = group)) +
    geom_col(width = 0.72, colour = "white", linewidth = 0.5, position = position_stack(reverse = TRUE)) +
    geom_text(aes(label = ifelse(share >= 10, sprintf("%.0f", share), "")),
              position = position_stack(vjust = 0.5, reverse = TRUE), size = 2.6, colour = INK) +
    geom_text(data = reg_med[target == tg], aes(x = 101.5, y = region, label = sprintf("$%.0f", med)), inherit.aes = FALSE,
              hjust = 0, size = 2.6, colour = INK2) +
    scale_fill_manual(values = GROUP_COLS, name = NULL, drop = FALSE) +
    scale_y_discrete(labels = REGION_LAB) +
    scale_x_continuous(limits = c(0, 110), expand = c(0, 0)) +
    labs(title = sprintf("%s  %s", LETTERS_T[[tg]], TARGETS[[tg]]), x = NULL, y = NULL) + theme_imp() +
    theme(panel.grid.major.x = element_blank(), axis.text.x = element_blank())
}
cw <- dcast(curves, price ~ tech, value.var = "net_value")
first_root <- function(y) { i <- which(diff(sign(y)) != 0)[1]; cw$price[i] - y[i] * diff(cw$price)[i] / diff(y)[i] }
anc <- data.table(target = names(TARGETS),
                  x = c(first_root(cw$PyCCS), first_root(cw$BECCS - cw$BE), first_root(cw$BECCS), first_root(cw$BECCS - cw$PyCCS)))
anc[, y := c(0, approx(cw$price, cw$BE, x[2])$y, 0, approx(cw$price, cw$PyCCS, x[4])$y)]
anc[, lab := LETTERS_T[target]]
message("Importance figure anchors (US$ Mg-1 CO2e): ", paste(sprintf("%s %.0f", anc$lab, anc$x), collapse = ", "))
TECH_COL3 <- c(BE = TECH_COLORS[["BES"]], BECCS = TECH_COLORS[["BECCS"]], PyCCS = TECH_COLORS[["BEBCS"]])
ytop <- max(curves$net_value) * 1.1
p_curve <- ggplot(curves, aes(price, net_value, colour = tech)) +
  geom_hline(yintercept = 0, colour = "#c9c8c2", linewidth = 0.4) +
  geom_segment(data = anc, aes(x = x, xend = x, y = y, yend = ytop * 0.94), inherit.aes = FALSE,
               colour = INK2, linewidth = 0.35, linetype = "22") +
  geom_line(linewidth = 0.9) +
  geom_point(data = anc, aes(x, y), inherit.aes = FALSE, shape = 21, size = 2.4, fill = "white", colour = INK, stroke = 0.7) +
  annotate("label", x = anc$x, y = ytop, label = anc$lab, size = 3, fontface = "bold", colour = INK,
           fill = "white", label.padding = unit(0.12, "lines")) +
  geom_text(data = curves[price == max(price)], aes(label = tech), hjust = -0.15, size = 3, fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = TECH_COL3, guide = "none") +
  scale_x_continuous(limits = c(0, max(ANCHOR_PRICES) * 1.1), breaks = seq(0, max(ANCHOR_PRICES), 50), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = sprintf("%s  Net value of each technology", letters[length(TARGETS) + 1]),
       x = "Carbon price (US$ Mg⁻¹ CO₂e)", y = "Net value (US$ Mg⁻¹ residue)") +
  theme_imp() + theme(panel.grid.major.y = element_line(colour = GRID_COL, linewidth = 0.3),
                      axis.title.y = element_text(margin = margin(r = 3)))
p_imp <- wrap_plots(c(lapply(names(TARGETS), bar_panel), list(free(p_curve, type = "label", side = "l"))), ncol = 1) +
  plot_layout(heights = c(rep(1, length(TARGETS)), 1.6), guides = "collect") &
  theme(legend.position = "bottom") & guides(fill = guide_legend(nrow = 2, byrow = TRUE))
ggsave(file.path(OUT_DIR, "shap_importance.png"), p_imp, width = 7.2, height = 8.8, dpi = 300, bg = "white")
fwrite(curves, file.path(OUT_DIR, "net_value_curves.csv"))

# ==============================================================================
# Dependence plots (SI): one figure per target and region, inputs with >= 2% of mean |SHAP|. Points are
# colored by the input with the largest mean |SHAP interaction value| with the plotted input (exact TreeSHAP
# interactions on the plotted sample); the color scale is clipped at the 2nd and 98th percentiles of that input
# ==============================================================================
# SHAP interaction values on the plotted sample of each model, computed in parallel and cached per model
dep_jobs <- lapply(models, function(m) {
  idx <- if (nrow(m$data) <= N_PLOT) seq_len(nrow(m$data)) else { set.seed(1); sort(sample.int(nrow(m$data), N_PLOT)) }
  list(name = sprintf("interactions_%s_%s", m$target, m$region), key = cache_key(key_models, idx),
       model = m$model, X = m$X[idx, , drop = FALSE])
})
message("SHAP interaction values (", N_WORKERS, " in parallel)...")
shp_inter <- par_map(dep_jobs, function(j) cached(j$name, j$key, shapviz(j$model, X_pred = j$X, interactions = TRUE), sub = "spatial_shap"))
for (k in seq_along(models)) {
  m <- models[[k]]
  shares <- imp[target == m$target & region == m$region]
  v_show <- shares[share_pct >= 2][order(-share_pct), feature]
  if (!length(v_show)) next
  shp_i <- shp_inter[[k]]
  deps <- lapply(v_show, function(v) {
    iv <- abs(shp_i$S_inter[, v, , drop = FALSE])[, 1, ]
    iv[, v] <- 0
    cv <- names(which.max(colMeans(iv)))
    lim <- stats::quantile(shp_i$X[[cv]], c(0.02, 0.98), na.rm = TRUE, names = FALSE)
    if (diff(lim) <= 0) lim <- range(shp_i$X[[cv]], na.rm = TRUE)
    if (diff(lim) <= 0) lim <- lim + c(-0.5, 0.5)
    sv_dependence(shp_i, v = v, color_var = cv, interactions = FALSE, alpha = 0.4, size = 0.5) +
      scale_colour_viridis_c(option = "plasma", end = 0.9, limits = lim, oob = scales::squish, breaks = scales::breaks_pretty(n = 4),
                             labels = scales::label_comma(),
                             name = paste0(feature_labels[[cv]], "\n(", feature_units[[cv]], ")")) +
      scale_x_continuous(labels = scales::label_comma()) +
      guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5)) +
      theme_bw(base_size = 9) +
      theme(legend.position = "bottom", legend.key.height = unit(0.2, "cm"), legend.key.width = unit(1.1, "cm"),
            legend.title = element_text(size = 7), legend.text = element_text(size = 7)) +
      labs(title = NULL, x = paste0(feature_labels[[v]], " (", feature_units[[v]], ")"),
           y = paste0("SHAP value (", TARGET_UNITS[[m$target]], ")"))
  })
  ggsave(file.path(OUT_DIR, sprintf("dependence_%s_%s.png", m$target, m$region)), wrap_plots(deps, ncol = 3),
         width = 10, height = 3.6 * ceiling(length(deps) / 3), dpi = 250)
}

# ==============================================================================
# Maps: takeover price and its dominant driver
# ==============================================================================
dom <- rbindlist(lapply(models[grepl("^takeover_BECCS", names(models))], function(m) {
  S <- m$shp$S
  j <- max.col(abs(S), ties.method = "first")
  data.table(region = m$region, x = m$data$x, y = m$data$y, dominant = m$features[j],
             dominant_shap = S[cbind(seq_len(nrow(S)), j)])
}))
map_cells <- merge(cells[, .(region, x, y, takeover_BECCS)], dom, by = c("region", "x", "y"), all.x = TRUE)
fwrite(map_cells, file.path(OUT_DIR, "takeover_dominant_driver_by_cell.csv.gz"))

map_data <- lapply(stats::setNames(REGIONS, REGIONS), function(r) {
  disp <- display_admin(load_region_data(r))
  d <- map_cells[region == r]
  pts <- sf::st_as_sf(d, coords = c("x", "y"), crs = 4326, remove = FALSE)
  list(df = d[lengths(sf::st_intersects(pts, sf::st_transform(disp$land, 4326))) > 0], admin0 = disp$admin0)
})
map_aspect <- function(r) {
  b <- sf::st_bbox(map_data[[r]]$admin0)
  unname((b$xmax - b$xmin) * cos(mean(c(b$ymin, b$ymax)) * pi / 180) / (b$ymax - b$ymin))
}
map_panel <- function(r, fill_layer, scale_layer) {
  m <- map_data[[r]]
  ggplot() + fill_layer(m$df) +
    geom_sf(data = m$admin0, fill = NA, color = "black", linewidth = 0.3) +
    coord_sf(crs = 4326, expand = FALSE) + scale_layer + theme_void(base_size = 11) + labs(subtitle = r) +
    theme(plot.subtitle = element_text(face = "bold", hjust = 0.5, margin = margin(b = 4)),
          legend.position = "none", plot.margin = margin(4, 8, 4, 8))
}
# Two rows (US, Europe; China, India); every map has the same height and a width set by its aspect ratio
save_map <- function(fill_layer, scale_layer, guide, out_path, legend_height = 0.9) {
  rows <- list(c("US", "Europe"), c("China", "India"))
  asp <- lapply(rows, function(rr) vapply(rr, map_aspect, numeric(1)))
  row_plots <- lapply(seq_along(rows), function(i) {
    wrap_plots(lapply(rows[[i]], map_panel, fill_layer, scale_layer), nrow = 1, widths = asp[[i]])
  })
  leg <- cowplot::get_legend(map_panel("US", fill_layer, scale_layer) + guide +
    theme(legend.position = "bottom", legend.title.position = "top", legend.title = element_text(hjust = 0.5)))
  row_h <- MAP_HEIGHT + 0.35
  combined <- wrap_elements(row_plots[[1]]) / wrap_elements(row_plots[[2]]) / wrap_elements(leg) +
    plot_layout(heights = c(row_h, row_h, legend_height))
  ggsave(out_path, plot = combined, width = MAP_HEIGHT * max(vapply(asp, sum, numeric(1))) + 0.6,
         height = 2 * row_h + legend_height, dpi = 300, bg = "white")
  message("Saved map: ", out_path)
}

save_map(
  function(d) geom_tile(data = d, aes(x = x, y = y, fill = takeover_BECCS)),
  scale_fill_viridis_c(name = paste0("Crossover price of BECCS (", TARGET_UNITS[["takeover_BECCS"]], ")"),
                       option = "viridis", limits = c(0, MAX_PRICE), na.value = "grey80"),
  guides(fill = guide_colourbar(direction = "horizontal", barwidth = unit(9, "cm"), barheight = unit(0.35, "cm"))),
  file.path(OUT_DIR, "map_takeover_price.png")
)
# Legend: drivers that dominate at least 0.5% of cells
dom_share <- prop.table(table(map_cells$dominant))
used <- intersect(names(feature_palette), names(dom_share)[dom_share >= 0.005])
save_map(
  function(d) geom_tile(data = d, aes(x = x, y = y, fill = dominant)),
  scale_fill_manual(name = "Dominant driver of the crossover price of BECCS", values = feature_palette, limits = used,
                    labels = feature_labels[used], drop = FALSE, na.value = "grey80"),
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)),
  file.path(OUT_DIR, "map_takeover_driver.png")
)

# ==============================================================================
# AI summary: takeover price and dominant driver by admin-1 unit; importance by target and region
# ==============================================================================
ai_dir <- "results/ai_summaries/"
dir.create(ai_dir, showWarnings = FALSE, recursive = TRUE)
adm <- rbindlist(lapply(REGIONS, function(r) {
  a1 <- load_region_data(r)$admin1
  if (is.null(a1)) return(NULL)
  d <- merge(cells[region == r], dom, by = c("region", "x", "y"), all.x = TRUE)
  pts <- sf::st_as_sf(d, coords = c("x", "y"), crs = 4326)
  j <- setDT(sf::st_drop_geometry(sf::st_join(pts, sf::st_as_sf(a1))))
  j[, .(region = r, cells = .N,
        median_takeover_BECCS = median(takeover_BECCS, na.rm = TRUE), share_no_takeover = mean(is.na(takeover_BECCS)),
        median_be_PyCCS = median(be_PyCCS, na.rm = TRUE), median_be_BECCS = median(be_BECCS, na.rm = TRUE),
        median_beccs_over_be = median(beccs_over_be, na.rm = TRUE),
        dominant_takeover_driver = if (all(is.na(dominant))) NA_character_ else names(which.max(table(dominant)))),
    by = .(NAM_0, NAM_1)]
}))
fwrite(adm, file.path(ai_dir, "spatial_shap_admin1_summary.csv"))
fwrite(imp[, .(target, region, feature, share_pct = round(share_pct, 1), holdout_r2 = round(holdout_r2, 3),
               share_undefined = round(share_undefined, 3))][order(target, region, -share_pct)],
       file.path(ai_dir, "spatial_shap_importance.csv"))
message("Spatial SHAP regression analysis complete.")
