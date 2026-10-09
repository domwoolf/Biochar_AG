# scripts/spatial_shap.R
# Spatial drivers of technology costs and of the takeover of biochar by geological storage (issue #111).
#
# For each region, the main model path (run_price_sweep(), default regional parameters) gives four
# targets per grid cell:
#   be_PyCCS        break-even carbon price of PyCCS (US$ Mg-1 CO2e)
#   be_BECCS        break-even carbon price of BECCS
#   n0_BE           net value of BE without a carbon price (US$ Mg-1 feedstock; BE abates too little for a
#                   meaningful break-even price)
#   takeover_BECCS  lowest carbon price at which BECCS has the highest non-negative net value
# A gradient-boosted regression (XGBoost) predicts each target from the cell's spatial inputs, and SHAP
# values attribute each prediction to the inputs. Models are fitted per region, so that inputs constant
# within a region (e.g. feedstock cost outside Europe) cannot stand in for the region itself.
#
# Outputs (results/spatial_shap/): shap_importance.csv, shap_targets_by_cell.csv.gz, importance and
# dependence figures, maps of the takeover price and its dominant driver; and
# results/ai_summaries/spatial_shap_admin1_summary.csv and spatial_shap_importance.csv.

library(data.table)
library(xgboost)
library(shapviz)
library(ggplot2)
library(patchwork)
library(terra)
library(sf)

sf::sf_use_s2(FALSE)
source("scripts/manuscript_figures.R") # load_all, display_admin(), tech_label(), TECH_COLORS

OUT_DIR <- "results/spatial_shap"
REGIONS <- c("US", "Europe", "China", "India")
MAP_HEIGHT <- 3.4 # height of each map panel (inches); widths follow each region's aspect ratio
N_PLOT <- 20000 # cells sampled for dependence plots (SHAP is computed on all cells)
MAX_PRICE <- 500 # top of the carbon-price grid (US$ Mg-1 CO2e)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

TARGETS <- c(be_PyCCS = "Break-even carbon price, PyCCS", be_BECCS = "Break-even carbon price, BECCS",
             n0_BE = "Net value of BE without a carbon price", takeover_BECCS = "Carbon price at which BECCS takes over")
TARGET_UNITS <- c(be_PyCCS = "US$ Mg⁻¹ CO₂e", be_BECCS = "US$ Mg⁻¹ CO₂e",
                  n0_BE = "US$ Mg⁻¹ feedstock", takeover_BECCS = "US$ Mg⁻¹ CO₂e")

feature_labels <- c(
  biomass_density = "Residue density", dist_BE_base = "Collection distance, BE (base load)",
  dist_BE_flex = "Collection distance, BE (flexible)", dist_BECCS = "Collection distance, BECCS",
  dist_PyCCS = "Collection distance, PyCCS", haul_mult = "Haulage cost multiplier",
  burn_frac = "Share of residue burned", feedstock_cost = "Feedstock cost", elec_price = "Electricity price",
  ff_c_intensity = "Grid carbon intensity", soil_temp = "Soil temperature", soil_ph = "Soil pH", soil_cec = "Soil CEC",
  bc_supply = "Biochar supply per hectare", crop_value = "Crop value per hectare",
  co2_route_km = "CO₂ route length", co2_offshore = "Offshore CO₂ sink", co2_storage_cost = "CO₂ storage cost"
)
feature_units <- c(
  biomass_density = "Mg km\u207b\u00b2 yr\u207b\u00b9", dist_BE_base = "km", dist_BE_flex = "km", dist_BECCS = "km", dist_PyCCS = "km",
  haul_mult = "ratio", burn_frac = "fraction", feedstock_cost = "US$ Mg\u207b\u00b9", elec_price = "US$ MWh\u207b\u00b9",
  ff_c_intensity = "Mg CO\u2082 GJ\u207b\u00b9", soil_temp = "\u00b0C", soil_ph = "pH", soil_cec = "cmolc kg\u207b\u00b9",
  bc_supply = "Mg ha\u207b\u00b9 yr\u207b\u00b9", crop_value = "US$ ha\u207b\u00b9 yr\u207b\u00b9", co2_route_km = "km",
  co2_offshore = "0/1", co2_storage_cost = "US$ Mg\u207b\u00b9 CO\u2082"
)
feature_palette <- c(
  biomass_density = "#2ca02c", dist_BE_base = "#98df8a", dist_BE_flex = "#5ab55a", dist_BECCS = "#98df8a",
  dist_PyCCS = "#98df8a", haul_mult = "#c5b0d5", burn_frac = "#ff9896",
  feedstock_cost = "#17becf", elec_price = "#1f77b4", ff_c_intensity = "#ff7f0e", soil_temp = "#d62728",
  soil_ph = "#7f7f7f", soil_cec = "#bcbd22", bc_supply = "#8c6d31", crop_value = "#e7ba52",
  co2_route_km = "#9467bd", co2_offshore = "#e377c2", co2_storage_cost = "#8c564b"
)

# ==============================================================================
# Targets and spatial inputs per region
# ==============================================================================

#' Lowest carbon price at which BECCS has the highest non-negative net value (NA if never <= MAX_PRICE)
takeover_price <- function(sweep, k = 2, step = 1) {
  out <- rep(NA_real_, nrow(sweep$n0))
  for (cp in seq(0, MAX_PRICE, by = step)) {
    net <- sweep_net(sweep, cp)
    net[is.na(net)] <- -Inf
    best <- max.col(net, ties.method = "first")
    hit <- is.na(out) & best == k & net[cbind(seq_along(best), best)] >= 0
    out[hit] <- cp
  }
  out
}

region_data <- function(r) {
  message("Region ", r, ": price sweep and spatial inputs...")
  dat <- load_region_data(r)
  p0 <- set_scenario(region = r)
  p0$region <- r
  sw <- run_price_sweep(dat$template, dat$layers, p0, vec = dat$vec, prices = c(seq(0, 250, by = 5), seq(275, MAX_PRICE, by = 25)))
  be <- function(k) price_root(function(cp) sweep_n0(sw, cp)[, k], function(cp) sweep_abate(sw, cp)[, k], sw$prices)
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
  data.table(
    region = r, x = dat$vec$xy[, 1], y = dat$vec$xy[, 2], cell_area_km2 = dat$vec$cell_area,
    be_PyCCS = be(3), be_BECCS = be(2), n0_BE = sw$n0[, 1], takeover_BECCS = takeover_price(sw),
    biomass_density = sl$biomass_density, dist_BE_base = rep_n(p$avg_dist_BES_BASE), dist_BE_flex = rep_n(p$avg_dist_BES_FLEX),
    dist_BECCS = rep_n(p$avg_dist_BECCS), dist_PyCCS = rep_n(p$avg_dist_BEBCS), haul_mult = rep_n(haul_mult),
    burn_frac = rep_n(residue_burn_share(p)), feedstock_cost = rep_n(p$feedstock_cost),
    elec_price = rep_n(p$elec_price), ff_c_intensity = rep_n(p$ff_c_intensity), soil_temp = rep_n(p$soil_temp),
    soil_ph = rep_n(p$soil_ph), soil_cec = rep_n(p$soil_cec), bc_supply = rep_n(pyccs$bc_supply_ha),
    crop_value = rep_n(if (is.null(p$crop_value)) NA_real_ else p$crop_value),
    co2_route_km = rep_n(beccs$co2_transport_distance_km), co2_offshore = as.numeric(sclass %in% c(3, 4)),
    co2_storage_cost = co2_storage_cost
  )
}

cells <- rbindlist(lapply(REGIONS, region_data))
fwrite(cells, file.path(OUT_DIR, "shap_targets_by_cell.csv.gz"))
FEATURES <- names(feature_labels)
# Each target sees only the inputs that enter the economics of the technologies it depends on, so that
# SHAP cannot attribute a cost to a correlated input the technology does not use (e.g. soil temperature
# for BE, which correlates with the haulage multiplier in India). Residue density enters through the
# collection distances, which are computed from it. Each technology hauls over its own collection
# distance, set by its capacity factor.
BASE_INPUTS <- c("haul_mult", "feedstock_cost", "elec_price", "soil_ph") # soil pH: lime credit of ash and biochar, removal charge
TARGET_FEATURES <- list(
  n0_BE = c(BASE_INPUTS, "dist_BE_base", "dist_BE_flex"),
  be_BECCS = c(BASE_INPUTS, "dist_BECCS", "ff_c_intensity", "burn_frac", "co2_route_km", "co2_offshore", "co2_storage_cost"),
  be_PyCCS = c(BASE_INPUTS, "dist_PyCCS", "ff_c_intensity", "burn_frac", "soil_temp", "soil_cec", "bc_supply", "crop_value")
)
TARGET_FEATURES$takeover_BECCS <- union(TARGET_FEATURES$be_BECCS, TARGET_FEATURES$be_PyCCS)

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
  pars <- list(objective = "reg:squarederror", max_depth = 6, eta = 0.05, subsample = 0.8, nthread = 4)
  m_cv <- xgb.train(params = pars, data = xgb.DMatrix(X[-test, , drop = FALSE], label = y[-test]), nrounds = 400, verbose = 0)
  pred <- predict(m_cv, X[test, , drop = FALSE])
  r2 <- 1 - sum((y[test] - pred)^2) / sum((y[test] - mean(y[test]))^2)
  model <- xgb.train(params = pars, data = xgb.DMatrix(X, label = y), nrounds = 400, verbose = 0)
  shp <- shapviz(model, X_pred = X)
  message(sprintf("  %-14s %-7s %6d cells (%4.1f%% undefined), %2d inputs, holdout R2 %.3f",
                  target, d$region[1], nrow(X), 100 * (1 - mean(ok)), length(f), r2))
  list(target = target, region = d$region[1], features = f, shp = shp, r2 = r2, data = d,
       undefined = 1 - mean(ok))
}

message("Fitting regression models...")
models <- list()
for (tg in names(TARGETS)) for (r in REGIONS) {
  d <- cells[region == r]
  if (sum(is.finite(d[[tg]])) < 200) {
    message("  ", tg, " ", r, ": too few defined cells; skipped")
    next
  }
  models[[paste(tg, r)]] <- fit_shap(d, tg)
}

imp <- rbindlist(lapply(models, function(m) {
  s <- colMeans(abs(m$shp$S))
  data.table(target = m$target, region = m$region, feature = m$features, mean_abs_shap = s,
             share_pct = 100 * s / sum(s), holdout_r2 = m$r2, share_undefined = m$undefined)
}))
fwrite(imp, file.path(OUT_DIR, "shap_importance.csv"))

# ==============================================================================
# Importance figure: share of mean |SHAP| by input, target and region
# ==============================================================================
imp_plot <- copy(imp)
imp_plot[, feature_lab := factor(feature_labels[feature], levels = rev(feature_labels))]
imp_plot[, target_lab := factor(TARGETS[target], levels = TARGETS)]
imp_plot[, region := factor(region, levels = REGIONS)]
p_imp <- ggplot(imp_plot, aes(x = region, y = feature_lab, fill = share_pct)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = ifelse(share_pct >= 5, sprintf("%.0f", share_pct), "")), size = 2.8) +
  facet_wrap(~target_lab, nrow = 1) +
  scale_fill_gradient(name = "Share of mean |SHAP| (%)", low = "#f7fbff", high = "#08519c", limits = c(0, 100)) +
  labs(x = NULL, y = NULL) + theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(), legend.position = "bottom", strip.text = element_text(face = "bold"))
ggsave(file.path(OUT_DIR, "shap_importance.png"), p_imp, width = 13, height = 5.2, dpi = 300, bg = "white")

# ==============================================================================
# Dependence plots (SI): one figure per target and region, inputs with >= 2% of mean |SHAP|
# ==============================================================================
for (m in models) {
  shares <- imp[target == m$target & region == m$region]
  v_show <- shares[share_pct >= 2][order(-share_pct), feature]
  if (!length(v_show)) next
  idx <- if (nrow(m$data) <= N_PLOT) seq_len(nrow(m$data)) else { set.seed(1); sort(sample.int(nrow(m$data), N_PLOT)) }
  deps <- lapply(v_show, function(v) {
    sv_dependence(m$shp[idx, ], v = v, color_var = NULL, alpha = 0.3, size = 0.5, color = "#08519c") +
      theme_bw(base_size = 9) +
      labs(title = feature_labels[[v]], x = paste0(feature_labels[[v]], " (", feature_units[[v]], ")"),
           y = paste0("SHAP value (", TARGET_UNITS[[m$target]], ")"))
  })
  ggsave(file.path(OUT_DIR, sprintf("dependence_%s_%s.png", m$target, m$region)), wrap_plots(deps, ncol = 3),
         width = 10, height = 3 * ceiling(length(deps) / 3), dpi = 250)
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
  scale_fill_viridis_c(name = paste0("Carbon price at which BECCS takes over (", TARGET_UNITS[["takeover_BECCS"]], ")"),
                       option = "viridis", limits = c(0, MAX_PRICE), na.value = "grey80"),
  guides(fill = guide_colourbar(direction = "horizontal", barwidth = unit(9, "cm"), barheight = unit(0.35, "cm"))),
  file.path(OUT_DIR, "map_takeover_price.png")
)
# Legend: drivers that dominate at least 0.5% of cells
dom_share <- prop.table(table(map_cells$dominant))
used <- intersect(names(feature_palette), names(dom_share)[dom_share >= 0.005])
save_map(
  function(d) geom_tile(data = d, aes(x = x, y = y, fill = dominant)),
  scale_fill_manual(name = "Dominant driver of the takeover price", values = feature_palette, limits = used,
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
        median_n0_BE = median(n0_BE, na.rm = TRUE),
        dominant_takeover_driver = if (all(is.na(dominant))) NA_character_ else names(which.max(table(dominant)))),
    by = .(NAM_0, NAM_1)]
}))
fwrite(adm, file.path(ai_dir, "spatial_shap_admin1_summary.csv"))
fwrite(imp[, .(target, region, feature, share_pct = round(share_pct, 1), holdout_r2 = round(holdout_r2, 3),
               share_undefined = round(share_undefined, 3))][order(target, region, -share_pct)],
       file.path(ai_dir, "spatial_shap_importance.csv"))
message("Spatial SHAP regression analysis complete.")
