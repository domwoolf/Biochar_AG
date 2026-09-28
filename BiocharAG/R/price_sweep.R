#' Carbon-Price Sweep
#'
#' Net value is exactly N0 + C * A(C), where N0 (the net value without carbon revenue) does not depend
#' on the carbon price C, and the abatement A(C) depends on C only through the displaced grid intensity
#' MEF(P) (`displaced_grid_ci`). A linear extrapolation from C = 0, N0 + C * A(0), therefore overstates
#' the grid-displacement credit at high prices. This function runs the model on a grid of prices and
#' stores A at each; `sweep_abate` and `sweep_net` interpolate between them, and `sweep_breakeven`
#' finds the price at which net value turns positive.
#'
#' The price grid must start at 0: MEF(P) is flat below zero, so A(C) = A(0) for C < 0 and the
#' extrapolation below the grid is exact. Above the grid, A is held at its value at the top price.
#'
#' @param template Reference SpatRaster template.
#' @param layers List of spatial layers.
#' @param params Scenario parameter list (`c_price` is overwritten).
#' @param vec Pre-extracted vectors from `load_region_data()$vec` (required).
#' @param prices Carbon-price grid ($/tCO2), increasing and starting at 0.
#' @return A list with `prices`, `n0` (cells x 3 net value without carbon revenue), `abate`
#'   (cells x 3 x prices), `active_indices` and `template`.
#' @export
run_price_sweep <- function(template, layers, params, vec, prices = c(seq(0, 250, by = 5), seq(275, 500, by = 25))) {
  if (is.null(vec) || is.null(vec[["active_indices", exact = TRUE]])) stop("run_price_sweep() needs `vec`.")
  prices <- sort(unique(prices))
  if (prices[1] != 0) stop("The price grid must start at 0.")
  n_cell <- length(vec[["active_indices", exact = TRUE]])
  abate <- array(NA_real_, dim = c(n_cell, 3, length(prices)), dimnames = list(NULL, c("BES", "BECCS", "BEBCS"), NULL))
  n0 <- NULL
  for (i in seq_along(prices)) {
    params[["c_price"]] <- prices[i]
    res <- run_scenario(template, layers, params, vec = vec)[["vec_res", exact = TRUE]]
    abate[, , i] <- res[["abate", exact = TRUE]]
    if (i == 1) n0 <- res[["net", exact = TRUE]]
  }
  colnames(n0) <- c("BES", "BECCS", "BEBCS")
  list(prices = prices, n0 = n0, abate = abate, active_indices = vec[["active_indices", exact = TRUE]], template = template)
}

#' Abatement at a Carbon Price, Interpolated from a Sweep
#'
#' @param sweep Output of `run_price_sweep`.
#' @param cp Carbon price ($/tCO2), a scalar.
#' @return Cells x 3 matrix of abatement (tCO2e/Mg).
#' @export
sweep_abate <- function(sweep, cp) {
  p <- sweep$prices
  n <- length(p)
  if (cp <= p[1]) return(sweep$abate[, , 1])
  if (cp >= p[n]) return(sweep$abate[, , n])
  i <- findInterval(cp, p)
  w <- (cp - p[i]) / (p[i + 1] - p[i])
  (1 - w) * sweep$abate[, , i] + w * sweep$abate[, , i + 1]
}

#' Net Value at a Carbon Price, Interpolated from a Sweep
#'
#' @inheritParams sweep_abate
#' @return Cells x 3 matrix of net value ($/Mg).
#' @export
sweep_net <- function(sweep, cp) {
  sweep$n0 + cp * sweep_abate(sweep, cp)
}

#' Lowest Carbon Price at which N0 + C * A(C) Turns Non-Negative
#'
#' Scans the price range of `prices` in steps of `step`, interpolating linearly between steps. Below 0
#' and above the top of the grid, A is constant and the root is solved exactly. Cells that are already
#' non-negative at C = 0 get the (zero or negative) root -N0 / A(0).
#'
#' @param n0 Value at C = 0 (vector over cells).
#' @param a_at Function of a scalar price returning A (vector over cells).
#' @param prices Price grid of the sweep (starts at 0).
#' @param step Scan step ($/tCO2).
#' @return Break-even price per cell; NA where the value never turns positive with rising price.
#' @export
price_root <- function(n0, a_at, prices, step = 1) {
  a0 <- a_at(0)
  out <- ifelse(n0 >= 0 & a0 > 0, -n0 / a0, NA_real_)
  todo <- which(is.finite(n0) & n0 < 0)
  c_prev <- 0
  f_prev <- n0
  for (cp in unique(c(seq(step, max(prices), by = step), max(prices)))) {
    if (!length(todo)) break
    f <- n0 + cp * a_at(cp)
    hit <- todo[is.finite(f[todo]) & f[todo] >= 0]
    if (length(hit)) {
      out[hit] <- c_prev + (cp - c_prev) * (-f_prev[hit]) / (f[hit] - f_prev[hit])
      todo <- setdiff(todo, hit)
    }
    c_prev <- cp
    f_prev <- f
  }
  a_top <- a_at(max(prices))
  beyond <- todo[is.finite(a_top[todo]) & a_top[todo] > 0]
  out[beyond] <- -n0[beyond] / a_top[beyond]
  out
}

#' Break-Even Carbon Price per Technology from a Sweep
#'
#' @param sweep Output of `run_price_sweep`.
#' @param step Scan step ($/tCO2).
#' @return Cells x 3 matrix of break-even prices ($/tCO2); NA where a technology never breaks even.
#' @export
sweep_breakeven <- function(sweep, step = 1) {
  out <- sapply(1:3, function(j) price_root(sweep$n0[, j], function(cp) sweep_abate(sweep, cp)[, j], sweep$prices, step))
  colnames(out) <- c("BES", "BECCS", "BEBCS")
  out
}

#' Map Cell Vectors from a Sweep onto a Raster
#'
#' @param sweep Output of `run_price_sweep`.
#' @param m Vector or cells x k matrix.
#' @param names Layer names.
#' @return SpatRaster on the sweep's template.
#' @export
sweep_to_raster <- function(sweep, m, names = colnames(m)) {
  m <- as.matrix(m)
  r <- terra::rast(sweep$template, nlyrs = ncol(m), vals = NA)
  r[sweep$active_indices] <- m
  if (!is.null(names)) names(r) <- names
  r
}
