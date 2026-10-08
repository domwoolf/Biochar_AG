# scripts/spatial_shap.R
# XGBoost classification and SHAP attribution of the optimal technology in the spatial sensitivity
# results. A surrogate classifier predicts the optimal technology in each grid cell from that cell's
# spatial inputs; SHAP values attribute each prediction to the inputs.
#
# Two kinds of model are fitted:
#   - one global model on all regions, for global beeswarm and dependence plots;
#   - one model per region, for regional beeswarm and dependence plots and for the maps of the dominant
#     driver. Within a region, inputs that are constant there (e.g. feedstock cost outside Europe) drop
#     out, so they cannot act as region labels in the maps (issue #37).
#
# Outputs: results/spatial_shap/ (figures, location table, importance table) and
# results/ai_summaries/spatial_shap_admin1_summary.csv. The SI reads the figures via Article/results.

library(data.table)
library(dplyr)
library(xgboost)
library(shapviz)
library(ggplot2)
library(patchwork)
library(terra)
library(sf)

sf::sf_use_s2(FALSE)

source("scripts/manuscript_figures.R")

INPUT_FILE <- "results/spatial_sensitivity_results.csv"
OUT_DIR <- "results/spatial_shap"
N_PLOT <- 20000 # cells sampled for beeswarm and dependence plots (SHAP is computed on all cells)
REGIONS <- c("US", "Europe", "China", "India")
MAP_HEIGHT <- 3.4 # height of each map panel (inches); widths follow each region's aspect ratio

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

message("Loading spatial sensitivity results from: ", INPUT_FILE)
df_valid <- fread(INPUT_FILE, data.table = FALSE) %>% filter(!is.na(best_technology))
if (nrow(df_valid) == 0) stop("No valid cells found in spatial sensitivity results.")
message("Total viable cells across all regions: ", nrow(df_valid))

# Spatial inputs used as predictors. CO2 transport is described by the route BECCS chooses: its length,
# whether the sink is offshore, and the storage cost at that sink.
features_all <- intersect(c(
  "biomass_density", "soil_temp", "elec_price", "feedstock_cost",
  "co2_route_km", "co2_offshore", "co2_storage_cost", "soil_ph", "soil_cec", "ff_c_intensity"
), names(df_valid))

feature_labels <- c(
  biomass_density = "Biomass density", soil_temp = "Soil temperature", elec_price = "Electricity price",
  feedstock_cost = "Feedstock cost", co2_route_km = "CO\u2082 route length", co2_offshore = "Offshore CO\u2082 sink",
  co2_storage_cost = "CO\u2082 storage cost", soil_ph = "Soil pH", soil_cec = "Soil CEC",
  ff_c_intensity = "Grid carbon intensity"
)

feature_palette <- c(
  biomass_density = "#2ca02c", soil_temp = "#d62728", elec_price = "#1f77b4", feedstock_cost = "#17becf",
  co2_route_km = "#9467bd", co2_offshore = "#e377c2", co2_storage_cost = "#8c564b", soil_ph = "#7f7f7f",
  soil_cec = "#bcbd22", ff_c_intensity = "#ff7f0e"
)

# ==============================================================================
# Model fitting
# ==============================================================================

#' Fit the surrogate classifier and compute SHAP values for one subset of cells.
#' Always uses multi:softprob (also for two classes) so that every class has its own SHAP matrix.
fit_shap <- function(dat, label) {
  v <- vapply(dat[, features_all, drop = FALSE], stats::var, numeric(1), na.rm = TRUE)
  f <- features_all[!is.na(v) & v > 1e-8]
  lv <- sort(unique(dat$best_technology))
  if (length(lv) < 2) {
    message("  ", label, ": only one optimal technology (", lv, "); no model fitted.")
    return(NULL)
  }
  X <- as.matrix(dat[, f, drop = FALSE])
  colnames(X) <- feature_labels[f] # readable names in all plots; column order still follows f
  y <- as.numeric(factor(dat$best_technology, levels = lv)) - 1
  set.seed(42)
  model <- xgb.train(
    params = list(objective = "multi:softprob", num_class = length(lv), max_depth = 6, eta = 0.05, nthread = 4),
    data = xgb.DMatrix(X, label = y), nrounds = 120, verbose = 0
  )
  prob <- predict(model, X) # n x k matrix (xgboost >= 2); flat row-major vector in older versions
  if (is.null(dim(prob))) prob <- matrix(prob, ncol = length(lv), byrow = TRUE)
  acc <- mean(lv[max.col(prob, ties.method = "first")] == dat$best_technology)
  message(sprintf("  %s: %d cells, classes %s, %d features, training accuracy %.1f%%",
                  label, nrow(X), paste(lv, collapse = "/"), length(f), 100 * acc))
  shp <- shapviz(model, X_pred = X)
  names(shp) <- lv
  list(label = label, features = f, classes = lv, shp = shp, acc = acc, data = dat)
}

message("Fitting global and regional models...")
models <- list(Global = fit_shap(df_valid, "Global"))
for (r in REGIONS) models[[r]] <- fit_shap(df_valid[df_valid$region == r, ], r)
models <- models[!vapply(models, is.null, logical(1))]

# Share of mean |SHAP| (summed over classes) by feature and model
imp_tbl <- do.call(rbind, lapply(models, function(m) {
  s <- Reduce(`+`, lapply(m$classes, function(cl) colMeans(abs(m$shp[[cl]]$S))))
  data.frame(model = m$label, feature = m$features, share_pct = round(100 * s / sum(s), 1),
             accuracy_pct = round(100 * m$acc, 1))
}))
write.csv(imp_tbl, file.path(OUT_DIR, "shap_importance_by_model.csv"), row.names = FALSE)

# ==============================================================================
# Beeswarm and dependence plots (one figure per model; dependence one per model and class)
# ==============================================================================

plot_sample <- function(m) {
  n <- nrow(m$data)
  if (n <= N_PLOT) return(seq_len(n))
  set.seed(1)
  sort(sample.int(n, N_PLOT))
}

for (m in models) {
  idx <- plot_sample(m)
  # Beeswarm: one panel per class
  bees <- lapply(m$classes, function(cl) {
    sv_importance(m$shp[[cl]][idx, ], kind = "beeswarm", max_display = length(m$features)) +
      theme_bw(base_size = 10) + labs(title = tech_label(cl), x = "SHAP value (log-odds)")
  })
  p_bee <- wrap_plots(bees, nrow = 1) +
    plot_layout(guides = "collect") # the panels share one feature-value colour bar; no title (see caption)
  ggsave(file.path(OUT_DIR, sprintf("beeswarm_%s.png", m$label)), p_bee,
         width = 4.5 * length(m$classes), height = 0.35 * length(m$features) + 2, dpi = 300)

  # Dependence: one panel per feature, coloured by the feature with the strongest interaction, three
  # panels per row with the colour bar under each panel so that all panels have the same size. Features
  # whose SHAP values are all zero for this class are omitted.
  for (cl in m$classes) {
    S <- m$shp[[cl]]$S
    v_show <- colnames(S)[apply(abs(S), 2, max) > 1e-6]
    deps <- lapply(v_show, function(v) {
      sv_dependence(m$shp[[cl]][idx, ], v = v, color_var = "auto", alpha = 0.4, size = 0.6) +
        theme_bw(base_size = 9) + labs(title = v, y = "SHAP value") +
        guides(colour = guide_colourbar(direction = "horizontal", barwidth = unit(3.5, "cm"), barheight = unit(0.25, "cm"))) +
        theme(legend.position = "bottom", legend.title.position = "top", legend.title = element_text(size = 8),
              legend.text = element_text(size = 7), legend.margin = margin(0, 0, 0, 0))
    })
    p_dep <- wrap_plots(deps, ncol = 3)
    ggsave(file.path(OUT_DIR, sprintf("dependence_%s_%s.png", m$label, cl)), p_dep,
           width = 10, height = 3.4 * ceiling(length(deps) / 3), dpi = 250)
  }
}

# ==============================================================================
# Location-level dominant driver from the regional models
# ==============================================================================
message("Extracting location-level dominant SHAP features (regional models)...")
df_shap_loc <- do.call(rbind, lapply(intersect(REGIONS, names(models)), function(r) {
  m <- models[[r]]
  d <- m$data
  S <- lapply(m$classes, function(cl) m$shp[[cl]]$S)
  names(S) <- m$classes
  win <- match(d$best_technology, m$classes)
  n <- nrow(d)
  dom_w <- character(n); val_w <- numeric(n); dom_o <- character(n); val_o <- numeric(n)
  for (k in seq_along(m$classes)) {
    rows <- which(win == k)
    if (!length(rows)) next
    Sk <- S[[k]][rows, , drop = FALSE]
    j <- max.col(abs(Sk), ties.method = "first")
    dom_w[rows] <- m$features[j]
    val_w[rows] <- Sk[cbind(seq_along(rows), j)]
  }
  A <- Reduce(pmax, lapply(S, abs))
  j <- max.col(A, ties.method = "first")
  dom_o <- m$features[j]
  val_o <- A[cbind(seq_len(n), j)]
  data.frame(x = d$x, y = d$y, region = r, best_technology = d$best_technology,
             dominant_feature_winning_class = dom_w, max_shap_value_winning_class = val_w,
             dominant_feature_overall = dom_o, max_shap_magnitude_overall = val_o)
}))
write.csv(df_shap_loc, file.path(OUT_DIR, "spatial_shap_values_by_location.csv"), row.names = FALSE)

# --- AI Summary Export: Zonal Stats ---
ai_dir <- "results/ai_summaries/"
dir.create(ai_dir, showWarnings = FALSE, recursive = TRUE)
df_admin_all <- data.frame()
for (r in REGIONS) {
  dat <- BiocharAG:::load_region_data(r)
  pts_r <- df_shap_loc[df_shap_loc$region == r, ]
  if (!is.null(dat$admin1) && nrow(pts_r) > 0) {
    pts <- sf::st_as_sf(pts_r, coords = c("x", "y"), crs = 4326)
    joined <- sf::st_join(pts, sf::st_as_sf(dat$admin1))
    admin_sum <- joined %>%
      sf::st_drop_geometry() %>%
      group_by(NAM_0, NAM_1) %>%
      summarize(
        region = first(region),
        dominant_feature = names(which.max(table(dominant_feature_winning_class))),
        avg_shap_magnitude = mean(max_shap_value_winning_class, na.rm = TRUE),
        .groups = "drop"
      )
    df_admin_all <- bind_rows(df_admin_all, admin_sum)
  }
}
write.csv(df_admin_all, paste0(ai_dir, "spatial_shap_admin1_summary.csv"), row.names = FALSE)

# ==============================================================================
# Maps
# ==============================================================================
message("Generating multi-region spatial maps...")
used_features <- intersect(names(feature_palette), unique(df_shap_loc$dominant_feature_winning_class))

# Cells and outlines as displayed (small islands dropped; display_admin() in manuscript_figures.R)
map_data <- lapply(stats::setNames(REGIONS, REGIONS), function(r) {
  disp <- display_admin(load_region_data(r))
  d <- df_shap_loc[df_shap_loc$region == r, ]
  pts <- sf::st_as_sf(d, coords = c("x", "y"), crs = 4326)
  list(df = d[lengths(sf::st_intersects(pts, sf::st_transform(disp$land, 4326))) > 0, ], admin0 = disp$admin0)
})
# Width/height ratio of a region's map in longitude-latitude coordinates (as coord_sf draws them)
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

# Two rows (US, Europe; China, India). Every map has the same height and a width set by its aspect
# ratio, so the regions are drawn at comparable scales; one legend below.
save_map <- function(fill_layer, scale_layer, guide, out_path, legend_height = 0.9) {
  rows <- list(c("US", "Europe"), c("China", "India"))
  asp <- lapply(rows, function(rr) vapply(rr, map_aspect, numeric(1)))
  row_plots <- lapply(seq_along(rows), function(i) {
    wrap_plots(lapply(rows[[i]], map_panel, fill_layer, scale_layer), nrow = 1, widths = asp[[i]])
  })
  leg <- cowplot::get_legend(map_panel("US", fill_layer, scale_layer) + guide +
    theme(legend.position = "bottom", legend.title.position = "top", legend.title = element_text(hjust = 0.5)))
  row_h <- MAP_HEIGHT + 0.35 # map plus panel title
  combined <- wrap_elements(row_plots[[1]]) / wrap_elements(row_plots[[2]]) / wrap_elements(leg) +
    plot_layout(heights = c(row_h, row_h, legend_height))
  width <- MAP_HEIGHT * max(vapply(asp, sum, numeric(1))) + 0.6
  ggsave(out_path, plot = combined, width = width, height = 2 * row_h + legend_height, dpi = 300, bg = "white")
  message("Saved map: ", out_path)
}

save_map(
  function(d) geom_tile(data = d, aes(x = x, y = y, fill = dominant_feature_winning_class), show.legend = TRUE),
  scale_fill_manual(name = "Dominant SHAP predictor", values = feature_palette, limits = used_features,
                    labels = feature_labels[used_features], drop = FALSE, na.value = "grey80"),
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)),
  file.path(OUT_DIR, "map_dominant_shap_feature.png")
)

save_map(
  function(d) geom_tile(data = d, aes(x = x, y = y, fill = abs(max_shap_value_winning_class))),
  scale_fill_viridis_c(name = "|SHAP value| (log-odds)", option = "inferno", direction = 1,
                       limits = c(0, max(abs(df_shap_loc$max_shap_value_winning_class), na.rm = TRUE))), # one shared scale
  guides(fill = guide_colourbar(direction = "horizontal", barwidth = unit(8, "cm"), barheight = unit(0.35, "cm"))),
  file.path(OUT_DIR, "map_max_shap_magnitude.png")
)

message("XGBoost + SHAP spatial analysis and mapping complete!")
