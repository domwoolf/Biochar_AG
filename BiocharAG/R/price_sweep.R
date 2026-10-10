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
#' @param method `"linear"` (default): each technology is evaluated once, at two fixed displaced grid
#'   intensities, and abatement at each price follows by linearity in MEF(P); for BEBCS, every dose option
#'   and both energy modes are compared at each price (see [price_sweep_linear()]). `"full"`: the model is
#'   run at every price. Both give the same results (up to floating-point rounding); `"linear"` falls back
#'   to `"full"` where linearity does not hold (BEBCS heat mode).
#' @return A list with `prices`, `n0` (cells x 3 net value without carbon revenue at C = 0), `n0_grid`
#'   (the same at each price; see `sweep_n0`), `abate` (cells x 3 x prices), `active_indices` and
#'   `template`.
#' @export
run_price_sweep <- function(template, layers, params, vec, prices = c(seq(0, 250, by = 5), seq(275, 500, by = 25)),
                            method = c("linear", "full")) {
  method <- match.arg(method)
  if (is.null(vec) || is.null(vec[["active_indices", exact = TRUE]])) stop("run_price_sweep() needs `vec`.")
  prices <- sort(unique(prices))
  if (prices[1] != 0) stop("The price grid must start at 0.")
  bebcs_mode <- if (!is.null(params$bebcs_energy_mode)) params$bebcs_energy_mode else "flex"
  if (method == "linear" && bebcs_mode != "heat") {
    out <- price_sweep_linear(params, vec, prices)
    out$active_indices <- vec[["active_indices", exact = TRUE]]
    out$template <- template
    return(out)
  }
  n_cell <- length(vec[["active_indices", exact = TRUE]])
  abate <- array(NA_real_, dim = c(n_cell, 3, length(prices)), dimnames = list(NULL, c("BES", "BECCS", "BEBCS"), NULL))
  n0_grid <- abate
  for (i in seq_along(prices)) {
    params[["c_price"]] <- prices[i]
    res <- run_scenario(template, layers, params, vec = vec, raster_out = FALSE)[["vec_res", exact = TRUE]]
    abate[, , i] <- res[["abate", exact = TRUE]]
    # Net value without carbon revenue, from the configuration chosen at this price. Constant across
    # prices unless a technology switches configuration with the carbon price (BEBCS "flex" mode).
    n0_grid[, , i] <- res[["net", exact = TRUE]] - prices[i] * res[["abate", exact = TRUE]]
  }
  n0 <- n0_grid[, , 1]
  colnames(n0) <- c("BES", "BECCS", "BEBCS")
  list(prices = prices, n0 = n0, n0_grid = n0_grid, abate = abate,
       active_indices = vec[["active_indices", exact = TRUE]], template = template)
}

#' Carbon-Price Sweep by Linearity in the Displaced Grid Intensity
#'
#' The carbon price enters the model in two ways only: through the displaced grid intensity MEF(P)
#' (`displaced_grid_ci`), on which abatement depends linearly for every technology (grid credit, and for
#' BECCS the grid electricity of CO2 lift pumping), and through the BEBCS choice of dose option and energy
#' mode, whose terms enter net value and abatement additively. Each technology is therefore evaluated once
#' with the grid intensity fixed at 0 and at 1 Mg CO2/GJ (BES and BECCS; BEBCS "power" at one dose option,
#' "none" once), at zero carbon price; at each price, abatement is A(0) + MEF(P) (A(1) - A(0)), and for
#' BEBCS the dose and mode with the highest net value are chosen exactly as in [calculate_bebcs()].
#'
#' @param params Scenario parameter list.
#' @param vec Pre-extracted vectors from `load_region_data()$vec`.
#' @param prices Carbon-price grid.
#' @return A list with `prices`, `n0`, `n0_grid` and `abate`, as [run_price_sweep()].
#' @keywords internal
price_sweep_linear <- function(params, vec, prices) {
  p <- cell_params(params, vec)
  n_cell <- length(vec[["active_indices", exact = TRUE]])
  np <- length(prices)
  # Displaced grid intensity at each price (cells x prices)
  mef <- vapply(prices, function(cp) {
    q <- p
    q$c_price <- cp
    rep_len(displaced_grid_ci(q), n_cell)
  }, numeric(n_cell))
  mef <- matrix(mef, n_cell, np)
  fixed <- function(ci, extra = list()) {
    q <- p
    q$ff_c_intensity <- ci
    q$mef_price_dependent <- FALSE
    q$c_price <- 0
    utils::modifyList(q, extra)
  }
  vecn <- function(x) rep_len(as.numeric(x), n_cell)
  abate <- array(NA_real_, dim = c(n_cell, 3, np), dimnames = list(NULL, c("BES", "BECCS", "BEBCS"), NULL))
  n0_grid <- abate
  # BES and BECCS: net value without carbon revenue does not depend on the price; abatement is affine in MEF
  for (k in 1:2) {
    fun <- if (k == 1) calculate_bes else calculate_beccs
    r0 <- fun(fixed(0))
    r1 <- fun(fixed(1))
    a0 <- vecn(r0$tot_c_abatement)
    g <- vecn(r1$tot_c_abatement) - a0
    n0_grid[, k, ] <- vecn(r0$net_value)
    abate[, k, ] <- a0 + mef * g
  }
  # BEBCS: base values of each energy mode at dose option 1, field terms of every dose option
  dose1 <- list(bc_dose_index = 1L, bc_return_field_table = TRUE)
  mode_base <- function(m, with_grid) {
    r0 <- calculate_bebcs_mode(fixed(0, c(dose1, list(bebcs_energy_mode = m))))
    ft <- r0$field_table
    d1n <- ft$v_yield[, 1] - ft$v_spread[, 1]
    d1a <- ft$a_n2o[, 1] - ft$e_diesel[, 1]
    g <- if (with_grid) vecn(calculate_bebcs_mode(fixed(1, c(dose1, list(bebcs_energy_mode = m))))$tot_c_abatement) -
      vecn(r0$tot_c_abatement) else 0
    list(n0 = vecn(r0$net_value) - d1n, a0 = vecn(r0$tot_c_abatement) - d1a, g = g, ft = ft)
  }
  bebcs_mode <- if (!is.null(params$bebcs_energy_mode)) params$bebcs_energy_mode else "flex"
  modes <- if (bebcs_mode == "flex") c("power", "none") else bebcs_mode
  base <- lapply(stats::setNames(modes, modes), function(m) mode_base(m, m != "none"))
  for (i in seq_len(np)) {
    cp <- prices[i]
    val <- lapply(base, function(b) {
      di <- bc_dose_index(b$ft, cp)
      f <- pick_bc_dose(b$ft, di)
      n0 <- b$n0 + f$v_yield - f$v_spread
      a <- b$a0 + f$a_n2o - f$e_diesel + mef[, i] * b$g
      list(n0 = n0, a = a, net = n0 + cp * a)
    })
    if (length(val) == 2) {
      use_none <- val$none$net > val$power$net
      n0_grid[, 3, i] <- fast_ifelse(use_none, val$none$n0, val$power$n0)
      abate[, 3, i] <- fast_ifelse(use_none, val$none$a, val$power$a)
    } else {
      n0_grid[, 3, i] <- val[[1]]$n0
      abate[, 3, i] <- val[[1]]$a
    }
  }
  # As in the full sweep (N0 = net - C A), N0 is undefined where abatement is (e.g. missing grid intensity)
  n0_grid[is.na(abate)] <- NA
  n0 <- n0_grid[, , 1]
  colnames(n0) <- c("BES", "BECCS", "BEBCS")
  list(prices = prices, n0 = n0, n0_grid = n0_grid, abate = abate)
}

#' Net Value without Carbon Revenue at a Carbon Price, Interpolated from a Sweep
#'
#' Equal to `sweep$n0` for every cell whose configuration does not change with the carbon price; for
#' BEBCS in "flex" mode it follows the energy mode chosen at each price.
#'
#' @inheritParams sweep_abate
#' @return Cells x 3 matrix ($/Mg).
#' @export
sweep_n0 <- function(sweep, cp) {
  if (is.null(sweep$n0_grid)) return(sweep$n0)
  p <- sweep$prices
  n <- length(p)
  if (cp <= p[1]) return(sweep$n0_grid[, , 1])
  if (cp >= p[n]) return(sweep$n0_grid[, , n])
  i <- findInterval(cp, p)
  if (cp == p[i]) return(sweep$n0_grid[, , i]) # avoids 0 * -Inf where a technology has no route
  w <- (cp - p[i]) / (p[i + 1] - p[i])
  lo <- sweep$n0_grid[, , i]
  hi <- sweep$n0_grid[, , i + 1]
  out <- (1 - w) * lo + w * hi
  out[is.infinite(lo) & lo == hi] <- lo[is.infinite(lo) & lo == hi]
  out
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
  sweep_n0(sweep, cp) + cp * sweep_abate(sweep, cp)
}

#' Lowest Carbon Price at which N0 + C * A(C) Turns Non-Negative
#'
#' Scans the price range of `prices` in steps of `step`, interpolating linearly between steps. Below 0
#' and above the top of the grid, A is constant and the root is solved exactly. Cells that are already
#' non-negative at C = 0 get the (zero or negative) root -N0 / A(0).
#'
#' @param n0 Value without carbon revenue: a vector over cells, or a function of a scalar price returning one.
#' @param a_at Function of a scalar price returning A (vector over cells).
#' @param prices Price grid of the sweep (starts at 0).
#' @param step Scan step ($/tCO2).
#' @return Break-even price per cell; NA where the value never turns positive with rising price.
#' @export
price_root <- function(n0, a_at, prices, step = 1) {
  n0_at <- if (is.function(n0)) n0 else function(cp) n0
  n00 <- n0_at(0)
  a0 <- a_at(0)
  out <- fast_ifelse(n00 >= 0 & a0 > 0, -n00 / a0, NA_real_)
  todo <- which(is.finite(n00) & n00 < 0)
  c_prev <- 0
  f_prev <- n00
  for (cp in unique(c(seq(step, max(prices), by = step), max(prices)))) {
    if (!length(todo)) break
    f <- n0_at(cp) + cp * a_at(cp)
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
  out[beyond] <- -n0_at(max(prices))[beyond] / a_top[beyond]
  out
}

#' Piecewise-Quadratic Net Value on a Sweep Segment
#'
#' Between grid prices p_i and p_i+1, both N0 and A are linear in the carbon price C, so the net value
#' f(C) = N0(C) + C A(C) is quadratic in w = (C - p_i) / (p_i+1 - p_i): f = c0 + b w + a w^2. Above the
#' top of the grid, N0 and A are constant and f is linear in w = C - p_top (unbounded). Non-finite values
#' (no CO2 route, masked cells) give c0 = -Inf, b = a = 0.
#'
#' @param sweep Output of `run_price_sweep`.
#' @param i Segment index (1 .. length(prices)); i = length(prices) is the open segment above the grid.
#' @return A list with matrices `c0`, `b`, `a` (cells x 3), the segment start `p`, width `d` (1 for the
#'   open segment) and upper bound of w, `wmax`.
#' @keywords internal
sweep_segment <- function(sweep, i) {
  p <- sweep$prices
  n <- length(p)
  n0 <- if (is.null(sweep$n0_grid)) array(sweep$n0, dim(sweep$abate)) else sweep$n0_grid
  if (i < n) {
    d <- p[i + 1] - p[i]
    n_lo <- n0[, , i]; n_hi <- n0[, , i + 1]
    a_lo <- sweep$abate[, , i]; a_hi <- sweep$abate[, , i + 1]
    dn <- n_hi - n_lo; da <- a_hi - a_lo
    out <- list(c0 = n_lo + p[i] * a_lo, b = dn + p[i] * da + d * a_lo, a = d * da, p = p[i], d = d, wmax = 1)
  } else {
    n_lo <- n0[, , n]; a_lo <- sweep$abate[, , n]
    out <- list(c0 = n_lo + p[n] * a_lo, b = a_lo, a = 0 * a_lo, p = p[n], d = 1, wmax = Inf)
    n_hi <- n_lo; a_hi <- a_lo
  }
  bad <- !is.finite(n_lo) | !is.finite(n_hi) | !is.finite(a_lo) | !is.finite(a_hi)
  out$c0[bad] <- -Inf; out$b[bad] <- 0; out$a[bad] <- 0
  out
}

#' Roots of c0 + b w + a w^2 in [0, wmax]
#'
#' @param c0,b,a Numeric vectors of equal length.
#' @param wmax Upper bound of w (scalar, may be Inf).
#' @return A two-column matrix of the roots in [0, wmax] (smaller first; NA where absent).
#' @keywords internal
quad_roots01 <- function(c0, b, a, wmax) {
  n <- length(c0)
  r <- matrix(NA_real_, n, 2)
  lin <- abs(a) < 1e-12 * pmax(abs(b), abs(c0), 1e-300)
  ok <- is.finite(c0)
  i <- which(lin & ok & b != 0)
  r[i, 1] <- -c0[i] / b[i]
  q <- which(!lin & ok)
  if (length(q)) {
    disc <- b[q]^2 - 4 * a[q] * c0[q]
    real <- disc >= 0
    s <- sqrt(pmax(disc, 0))
    # Numerically stable pair of roots
    t <- -0.5 * (b[q] + fast_ifelse(b[q] >= 0, s, -s))
    r1 <- fast_ifelse(t != 0, t / a[q], 0)
    r2 <- fast_ifelse(t != 0, c0[q] / t, 0)
    r[q, 1] <- fast_ifelse(real, pmin(r1, r2), NA)
    r[q, 2] <- fast_ifelse(real, pmax(r1, r2), NA)
  }
  r[!is.na(r) & (r < 0 | r > wmax)] <- NA
  # Keep the smaller valid root in column 1
  sw <- is.na(r[, 1]) & !is.na(r[, 2])
  r[sw, 1] <- r[sw, 2]; r[sw, 2] <- NA
  r
}

#' Break-Even Carbon Price per Technology from a Sweep
#'
#' Exact on the piecewise-linear interpolation of N0 and A between the grid prices (see
#' [sweep_segment()]): the lowest carbon price at which the net value N0(C) + C A(C) turns non-negative.
#' Cells whose net value is already non-negative at C = 0 get the (zero or negative) root -N0(0) / A(0),
#' where A(0) > 0; above the grid, N0 and A are held at their top values.
#'
#' @param sweep Output of `run_price_sweep`.
#' @param step Ignored (kept for compatibility; the roots are exact).
#' @return Cells x 3 matrix of break-even prices ($/tCO2); NA where a technology never breaks even.
#' @export
sweep_breakeven <- function(sweep, step = NULL) {
  n0 <- if (is.null(sweep$n0_grid)) sweep$n0 else sweep$n0_grid[, , 1]
  a0 <- sweep$abate[, , 1]
  out <- fast_ifelse(is.finite(n0) & is.finite(a0) & n0 >= 0 & a0 > 0, -n0 / a0, NA_real_)
  todo <- is.finite(n0) & is.finite(a0) & n0 < 0
  for (i in seq_along(sweep$prices)) {
    if (!any(todo)) break
    sg <- sweep_segment(sweep, i)
    idx <- which(todo)
    w <- quad_roots01(sg$c0[idx], sg$b[idx], sg$a[idx], sg$wmax)[, 1]
    w[is.na(w) & sg$c0[idx] >= 0] <- 0 # crossing exactly at the grid price
    hit <- !is.na(w)
    out[idx[hit]] <- sg$p + sg$d * w[hit]
    todo[idx[hit]] <- FALSE
  }
  colnames(out) <- c("BES", "BECCS", "BEBCS")
  out
}

#' Takeover Carbon Price from a Sweep
#'
#' Exact lowest carbon price at which technology `k` has the highest net value of the three and that
#' value is non-negative, on the piecewise-linear interpolation of N0 and A between the grid prices.
#' Candidate prices in each segment are its start and the roots of f_k and of f_k - f_j (j != k); the
#' condition is tested just above each candidate.
#'
#' @param sweep Output of `run_price_sweep`.
#' @param k Technology column (2 = BECCS).
#' @param max_price Highest price considered (Inf: also the open segment above the grid).
#' @return Vector of takeover prices ($/tCO2); NA where `k` never takes over up to `max_price`.
#' @export
sweep_takeover <- function(sweep, k = 2, max_price = Inf) {
  n_cell <- dim(sweep$abate)[1]
  out <- rep(NA_real_, n_cell)
  todo <- rep(TRUE, n_cell)
  others <- setdiff(1:3, k)
  for (i in seq_along(sweep$prices)) {
    if (!any(todo) || sweep$prices[i] > max_price) break
    sg <- sweep_segment(sweep, i)
    wmax <- min(sg$wmax, (max_price - sg$p) / sg$d)
    idx <- which(todo)
    f <- function(j, w) sg$c0[idx, j] + sg$b[idx, j] * w + sg$a[idx, j] * w^2
    cand <- cbind(0, quad_roots01(sg$c0[idx, k], sg$b[idx, k], sg$a[idx, k], wmax))
    for (j in others) {
      cand <- cbind(cand, quad_roots01(sg$c0[idx, k] - sg$c0[idx, j], sg$b[idx, k] - sg$b[idx, j],
                                       sg$a[idx, k] - sg$a[idx, j], wmax))
    }
    best <- rep(NA_real_, length(idx))
    eps <- 1e-9 * if (is.finite(sg$wmax)) 1 else max(1, sg$p)
    for (m in seq_len(ncol(cand))) {
      w <- cand[, m]
      wt <- pmin(w + eps, wmax)
      fk <- f(k, wt)
      okk <- !is.na(w) & is.finite(fk) & fk >= 0
      for (j in others) okk <- okk & (fk >= f(j, wt) | !is.finite(f(j, wt)))
      better <- okk & (is.na(best) | w < best)
      best[better] <- w[better]
    }
    hit <- !is.na(best)
    out[idx[hit]] <- sg$p + sg$d * best[hit]
    todo[idx[hit]] <- FALSE
  }
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
