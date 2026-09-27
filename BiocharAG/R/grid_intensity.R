#' Price-Dependent Marginal Emission Factor of Displaced Electricity
#'
#' The carbon price P implies a position along the decarbonisation pathway, so the grid electricity a
#' bioenergy plant displaces gets cleaner as P rises. Each grid cell's empirical build margin
#' (`ff_c_intensity`, the anchor at today's effective price `grid_p_now`) is scaled by a regional Hill
#' curve H(P) fitted to NGFS Phase 5 scenarios (`data-raw/ngfs_mef_fit.py`):
#'
#' MEF(P) = floor + (MEF_anchor - floor) * H(P) / H(P_now)
#'
#' with H(P) = 1 / (1 + (P / P50)^k) (single stage) or a two-stage mixture
#' s * H(P; P50_2, k2) + (1 - s) * H(P; P50_1, k1). The floor (`mef_floor`, life-cycle tCO2/MWh) is
#' the residual intensity of a decarbonised grid; anchors already below it are left unchanged. The
#' result is capped at the life-cycle intensity of coal (0.82 tCO2/MWh).
#'
#' Returns `ff_c_intensity` unchanged when `use_flat_ci` is TRUE, when `mef_price_dependent` is FALSE,
#' or when the region is unknown.
#'
#' @param params Parameter list with `ff_c_intensity` (tCO2/GJ electricity), `c_price` ($/tCO2),
#'   `region`, `grid_p_now`, `mef_floor` and optionally `mef_draw_u` (0 = central fit; in (0, 1] picks
#'   a bootstrap draw, for Monte Carlo runs) and `mef_include_remind`.
#' @return Displaced grid carbon intensity (tCO2/GJ electricity), same shape as `ff_c_intensity`.
#' @export
displaced_grid_ci <- function(params) {
  ci <- if (!is.null(params$ff_c_intensity)) params$ff_c_intensity else 12 / 3600
  if (isTRUE(as.logical(params$use_flat_ci)) || isFALSE(as.logical(params$mef_price_dependent))) return(ci)
  mef_region <- mef_region_key(params$region)
  if (is.null(mef_region)) return(ci)

  th <- mef_curve_params(mef_region, draw_u = params$mef_draw_u, include_remind = params$mef_include_remind)
  p_now <- if (!is.null(params$grid_p_now)) params$grid_p_now else 0
  price <- if (!is.null(params$c_price)) params$c_price else 0
  ratio <- mef_hill(price, th) / mef_hill(p_now, th)

  floor_gj <- (if (!is.null(params$mef_floor)) params$mef_floor else 0.02) / 3.6
  coal_gj <- 0.82 / 3.6
  mef <- pmin_raster(ci, floor_gj) + pmax_raster(ci - floor_gj, 0) * ratio
  pmin_raster(mef, coal_gj)
}

#' Regional Hill Curve H(P)
#'
#' @param price Carbon price ($/tCO2, US$2024).
#' @param th One row of the curve parameter table (`spec`, `P50`, `k`, or the two-stage parameters).
#' @return Fraction of the gap to the floor remaining at `price`.
#' @keywords internal
mef_hill <- function(price, th) {
  h1 <- function(p, p50, k) 1 / (1 + (pmax(p, 0) / p50)^k)
  if (identical(th$spec, "two")) {
    th$s * h1(price, th$P50_2, th$k2) + (1 - th$s) * h1(price, th$P50_1, th$k1)
  } else {
    h1(price, th$P50, th$k)
  }
}

#' Curve Parameters for a Region
#'
#' Reads the precomputed NGFS fit (draw 0 = central, draws 1..N = bootstrap) from
#' `inst/extdata/mef_shape_draws.csv` (GCAM and MESSAGE; REMIND excluded because its decline is
#' driven by time rather than price) or `mef_shape_draws_all_models.csv`.
#'
#' @param mef_region One of "USA", "EU", "China", "India".
#' @param draw_u NULL or 0 for the central fit; a value in (0, 1] selects bootstrap draw ceiling(u * N).
#' @param include_remind Use the fit that includes REMIND-MAgPIE.
#' @return A one-row data frame of curve parameters.
#' @keywords internal
mef_curve_params <- function(mef_region, draw_u = NULL, include_remind = FALSE) {
  tab <- mef_shape_table(isTRUE(as.logical(include_remind)))
  reg <- tab[tab$region == mef_region, ]
  if (!nrow(reg)) stop("No MEF curve for region ", mef_region)
  n <- max(reg$draw)
  u <- if (is.null(draw_u) || is.na(draw_u)) 0 else draw_u
  d <- if (u <= 0) 0 else min(n, max(1, ceiling(u * n)))
  reg[reg$draw == d, ][1, ]
}

.mef_cache <- new.env(parent = emptyenv())

mef_shape_table <- function(include_remind = FALSE) {
  key <- if (include_remind) "all" else "default"
  if (is.null(.mef_cache[[key]])) {
    fn <- if (include_remind) "mef_shape_draws_all_models.csv" else "mef_shape_draws.csv"
    path <- system.file("extdata", fn, package = "BiocharAG")
    if (!nzchar(path)) stop("MEF curve table not found: ", fn)
    .mef_cache[[key]] <- utils::read.csv(path, stringsAsFactors = FALSE)
  }
  .mef_cache[[key]]
}

mef_region_key <- function(region) {
  if (is.null(region)) return(NULL)
  r <- tryCatch(normalize_region_name(region), error = function(e) region)
  switch(as.character(r), "US" = "USA", "North America" = "USA", "USA" = "USA",
         "Europe" = "EU", "EU" = "EU", "China" = "China", "India" = "India", NULL)
}
