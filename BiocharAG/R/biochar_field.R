#' Biochar Field Application: Yield, Soil N2O and Spreading for Each Dose Strategy
#'
#' Field-application model of docs/biochar_agronomy_handover/AGRONOMY_SPEC.md (section 4). The biochar
#' made from a cell's residue is applied to the cell's own cropland. With `b` Mg biochar per physical
#' hectare per year and a dose `D` per application, the cropland is split into `n = max(1, ceiling(D/b))`
#' cohorts, each treated with `D_eff = n b` every `n` years during the plant life `T`. The effective stock
#' of a cohort decays at `k` (biochar carbon decay plus any extra decline of the agronomic effect):
#' `B = sum_i D_eff exp(-k (t - t_i))`. Relative yield gain is `exp(a10 g(B)) - 1`, with
#' `g(B) = (B / B_r)^p` above `B_min` and linear through the origin below it. Soil N2O reduction `R`
#' (scaled down linearly below the lowest tested dose) decays with half-life `t_half` after each
#' application and is restored by the next. Each cohort pass costs `c_pass`.
#'
#' Levelised economic values per Mg feedstock are `CRF(r, T) sum_{t=1}^{H} x_t (1 + r)^-t / m` with
#' `m = b / Y_bc` the feedstock per physical hectare per year; avoided N2O is summed over the horizon
#' without discounting and attributed to the feedstock of the plant life (`/(m T)`), as biochar carbon is.
#'
#' Every cohort has either `q + 1` or `q` applications (`q = floor(T / n)`), and cohort `j` starts in
#' year `j + 1`, so cohort sums are evaluated exactly from the two application histories, shifted in time
#' (one pass over the horizon, vectorised over cells), rather than cohort by cohort.
#'
#' @param b Biochar supplied per physical hectare per year (Mg ha-1 yr-1; vector over cells).
#' @param y_bc Biochar yield (Mg biochar per Mg dry ash-free feedstock).
#' @param k Decay rate of the effective biochar stock (yr-1): carbon decay plus the decline of the yield
#'   effect expressed as a stock decay (response decline rate / dose exponent).
#' @param a10 Log response ratio of crop yield at the reference stock `B_r` (>= 0).
#' @param val_ha Value of crop production per physical hectare (US$ ha-1 yr-1): cropping intensity x
#'   value per harvested hectare.
#' @param n_dir Direct fertiliser-induced N2O emission per physical hectare (kg N2O-N ha-1 yr-1).
#' @param doses Dose options (Mg biochar ha-1); 0 means annual application of `b` to all cropland.
#' @param r Discount rate; `T` plant life (yr); `H` horizon (yr, >= T).
#' @param p Dose exponent; `B_r` reference stock; `B_min` smallest stock on the power law (Mg ha-1).
#' @param R N2O reduction (fraction); `D_n2o` lowest tested dose (Mg ha-1); `t_half` half-life (yr).
#' @param c_pass Cost per hectare and pass (US$ ha-1); `L_pass` diesel per pass (L ha-1).
#' @param gwp_n2o 100-year GWP of N2O; `ef_diesel` Mg CO2 per L diesel.
#' @return List of cells x doses matrices: `v_yield`, `v_spread` (US$ Mg-1 feedstock), `a_n2o`,
#'   `e_diesel` (Mg CO2e Mg-1 feedstock), `cohorts` and `d_eff` (Mg ha-1).
#' @export
biochar_field_effects <- function(b, y_bc, k, a10, val_ha, n_dir, doses = c(0, 2.5, 5, 10, 20),
                                  r = 0.08, T = 20, H = 100, p = 0.30, B_r = 10, B_min = 2.5,
                                  R = 0.20, D_n2o = 2.2, t_half = 3, c_pass = 87, L_pass = 13.1,
                                  gwp_n2o = 273, ef_diesel = 2.68e-3) {
  if (H < T) stop("The horizon H must be at least the plant life T.")
  nc <- max(length(b), length(k), length(a10), length(val_ha), length(n_dir))
  rep_n <- function(x) if (length(x) == nc) x else rep_len(x, nc)
  b <- rep_n(b); k <- rep_n(k); a10 <- rep_n(a10); val_ha <- rep_n(val_ha); n_dir <- rep_n(n_dir)
  y_bc <- rep_n(y_bc)
  # Cells with missing inputs are evaluated with placeholder values and returned as NA
  ok <- is.finite(b) & b > 0 & is.finite(y_bc) & y_bc > 0 & is.finite(k) & is.finite(a10) & is.finite(val_ha) &
    is.finite(n_dir)
  m <- ifelse(ok, b / y_bc, NA_real_)
  b[!ok] <- 1; k[!ok] <- 0; a10[!ok] <- 0; val_ha[!ok] <- 0; n_dir[!ok] <- 0
  crf <- if (r > 0) r / (1 - (1 + r)^-T) else 1 / T
  lam <- log(2) / t_half
  disc <- (1 + r)^-(seq_len(H))
  s_lin <- (B_min / B_r)^p / B_min # slope of g below B_min

  out <- lapply(c("v_yield", "v_spread", "a_n2o", "e_diesel", "cohorts", "d_eff"),
                function(x) matrix(NA_real_, nc, length(doses), dimnames = list(NULL, as.character(doses))))
  names(out) <- c("v_yield", "v_spread", "a_n2o", "e_diesel", "cohorts", "d_eff")

  for (di in seq_along(doses)) {
    D <- doses[di]
    n <- if (D <= 0) rep(1, nc) else pmax(1, ceiling(D / b - 1e-9))
    n[!ok] <- 1
    d_eff <- n * b
    q <- T %/% n                     # applications of the "lo" cohorts
    rem <- T - q * n                 # cohorts 0 .. rem-1 get q + 1 applications
    nt <- pmin(n, T)                 # cohorts treated at least once
    ek <- exp(-k)
    ekp <- exp(-k * p)
    elam <- exp(-lam)
    g_full <- function(B) {
      gB <- (B / B_r)^p
      lin <- B < B_min
      gB[lin] <- s_lin * B[lin]
      gB
    }
    # Two cohort histories (starting in relative year 1, with c_hi = q + 1 and q applications every n
    # years), stepped forward one year at a time: the stock decays between applications, so g(B) follows by
    # multiplication (power-law regime) or proportionally to B (linear regime), and is recomputed only in
    # application years, which all fall within the first T years
    c_hi <- q + 1
    w_lo <- as.numeric(q > 0)        # "lo" cohorts with no application contribute nothing
    any_lo <- any(rem < nt)          # some cells have treated "lo" cohorts
    B1 <- g1 <- a1 <- B0 <- g0 <- a0 <- numeric(nc)
    nx1 <- nx0 <- rep(1, nc)
    d1 <- d0 <- numeric(nc)
    P_hi <- P_lo <- N_hi <- N_lo <- S_y <- S_n <- numeric(nc)
    for (u in seq_len(H)) {
      B1 <- B1 * ek; g1 <- g1 * ekp; a1 <- a1 * elam
      lin <- which(B1 < B_min)
      g1[lin] <- s_lin * B1[lin]
      if (u <= T) {
        app <- which(nx1 == u & d1 < c_hi)
        nx1[app] <- nx1[app] + n[app]; d1[app] <- d1[app] + 1
        B1[app] <- B1[app] + d_eff[app]; g1[app] <- g_full(B1[app]); a1[app] <- 1
      }
      P_hi <- P_hi + disc[u] * expm1(a10 * g1)
      N_hi <- N_hi + a1
      if (any_lo) {
        B0 <- B0 * ek; g0 <- g0 * ekp; a0 <- a0 * elam
        lin <- which(B0 < B_min)
        g0[lin] <- s_lin * B0[lin]
        if (u <= T) {
          app <- which(nx0 == u & d0 < q)
          nx0[app] <- nx0[app] + n[app]; d0[app] <- d0[app] + 1
          B0[app] <- B0[app] + d_eff[app]; g0[app] <- g_full(B0[app]); a0[app] <- 1
        }
        P_lo <- P_lo + disc[u] * w_lo * expm1(a10 * g0)
        N_lo <- N_lo + w_lo * a0
      }
      j <- H - u                                      # cohort whose horizon ends at relative year u
      if (j <= T - 1) {
        use <- as.numeric(j < nt)
        is_hi <- j < rem
        S_y <- S_y + use * (1 + r)^-j * (if (any_lo) ifelse(is_hi, P_hi, P_lo) else P_hi)
        S_n <- S_n + use * (if (any_lo) ifelse(is_hi, N_hi, N_lo) else N_hi)
      }
    }
    out$v_yield[, di] <- crf * val_ha * S_y / n / m
    out$v_spread[, di] <- c_pass / (n * m)
    out$a_n2o[, di] <- n_dir * R * pmin(1, d_eff / D_n2o) * S_n / n * 44 / 28 * 1e-3 * gwp_n2o / (m * T)
    out$e_diesel[, di] <- L_pass * ef_diesel / (n * m)
    out$cohorts[, di] <- ifelse(ok, n, NA)
    out$d_eff[, di] <- d_eff
  }
  out
}

#' Biochar Supply and Field Effects of a Grid Cell, by Dose Strategy
#'
#' Assembles the inputs of [biochar_field_effects()] from the parameter list and spatial layers, with
#' results cached across calls that differ only in the carbon price (the price sweep).
#'
#' Supply per physical hectare is `b = CI q_h Y_bc`, with `q_h` the available residue per harvested
#' hectare (`biomass_density` / `harv_frac`, Mg dry ash-free ha-1 yr-1, capped at `max_residue_per_ha`
#' where the residue and harvested-area maps disagree) and `CI` the cropping intensity. The yield
#' amplitude is `a10 = max(0, bc_yield_b0 + bc_yield_bcec ln CEC)`; the crop value per physical hectare
#' is `CI crop_value`. Direct N2O is `n_app_rate n2o_ef` (N use per hectare of cropland). The stock decays
#' at the biochar carbon decay rate `-ln(F_perm)/100` plus `bc_yield_decay / bc_dose_exponent`, so that a
#' single application reproduces a yield response declining at `bc_yield_decay` (above `bc_yield_bmin`;
#' below it the conversion over-states the decline).
#'
#' @param params Parameter list (with spatial layers as vectors or rasters).
#' @param y_bc Biochar yield (Mg per Mg dry ash-free feedstock).
#' @param bc_stability 100-year permanence factor `F_perm` (sets the stock decay rate).
#' @return Output of [biochar_field_effects()] plus `doses`, `b` and `m`.
#' @export
biochar_field_table <- function(params, y_bc, bc_stability) {
  pv <- function(n, d) if (!is.null(params[[n, exact = TRUE]])) params[[n, exact = TRUE]] else d
  vals <- function(x) if (inherits(x, "SpatRaster")) terra::values(x, mat = FALSE) else x
  fill <- function(x, d) {
    x <- vals(x)
    if (is.null(x)) return(d)
    if (length(x) > 1 && any(is.na(x))) x[is.na(x)] <- if (all(is.na(x))) d else stats::median(x, na.rm = TRUE)
    if (length(x) == 1 && is.na(x)) x <- d
    x
  }
  ci <- pv("cropping_intensity", 1)
  q_max <- pv("max_residue_per_ha", 20)
  dens <- vals(params[["biomass_density", exact = TRUE]])
  hf <- vals(params[["harv_frac", exact = TRUE]])
  q_h <- if (is.null(dens) || is.null(hf)) pv("residue_per_ha_default", 3) else
    ifelse(is.finite(hf) & hf > 0, pmin(dens / (hf * 100), q_max), q_max)
  b <- ci * q_h * vals(y_bc)
  cec <- fill(params[["soil_cec", exact = TRUE]], 20)
  a10 <- pmax(0, pv("bc_yield_b0", 0.3607) + pv("bc_yield_bcec", -0.1008) * log(pmax(cec, 0.1)))
  # bc_yield_decay is the decline rate of the yield RESPONSE (as estimated from repeated measurements); on
  # the power-law dose response the effective stock must decay at that rate / dose exponent to give it
  k <- -log(pmax(vals(bc_stability), 1e-6)) / 100 + pv("bc_yield_decay", 0.05) / pv("bc_dose_exponent", 0.30)
  val_ha <- ci * fill(params[["crop_value", exact = TRUE]], pv("crop_value_default", 1500))
  n_dir <- pv("n_app_rate", 68) * pv("n2o_ef", 0.01)
  doses <- bc_dose_options(params)
  args <- list(
    b = b, y_bc = vals(y_bc), k = k, a10 = a10, val_ha = val_ha, n_dir = n_dir, doses = doses,
    r = pv("discount_rate", 0.08), T = pv("py_life", 20), H = pv("bc_horizon", 100),
    p = pv("bc_dose_exponent", 0.30), B_r = pv("bc_yield_bref", 10), B_min = pv("bc_yield_bmin", 2.5),
    R = 1 - exp(pv("n2o_lnrr", -0.224)), D_n2o = pv("n2o_min_dose", 2.2), t_half = pv("n2o_half_life", 3),
    c_pass = bc_pass_cost(params), L_pass = bc_pass_diesel(params), gwp_n2o = pv("gwp_n2o", 273)
  )
  key <- rlang::hash(args)
  hit <- .field_cache[[key]]
  if (!is.null(hit)) return(hit)
  res <- do.call(biochar_field_effects, args)
  res$doses <- doses
  res$b <- b
  res$m <- b / vals(y_bc)
  cache_keys <- ls(.field_cache)
  if (length(cache_keys) >= 8) rm(list = cache_keys, envir = .field_cache)
  assign(key, res, envir = .field_cache)
  res
}

.field_cache <- new.env(parent = emptyenv())

#' Dose Options for Biochar Application
#'
#' @param params Parameter list; `bc_dose_options` is a number vector or a semicolon-separated string
#'   (0 = annual application to all cropland).
#' @return Numeric vector of doses (Mg biochar ha-1).
#' @keywords internal
bc_dose_options <- function(params) {
  x <- params[["bc_dose_options", exact = TRUE]]
  if (is.null(x)) return(c(0, 2.5, 5, 10, 20))
  if (is.character(x)) x <- as.numeric(strsplit(x, ";", fixed = TRUE)[[1]])
  x
}

#' Cost and Diesel per Hectare and Pass of Spreading Biochar or Ash
#'
#' Spreading only: application is timed to precede tillage, which incorporates the biochar or ash at no
#' extra cost. Cost scaled by the haulage location factor.
#'
#' @param params Parameter list.
#' @return US$ ha-1 (`bc_pass_cost`) or L ha-1 (`bc_pass_diesel`).
#' @keywords internal
bc_pass_cost <- function(params) {
  pv <- function(n, d) if (!is.null(params[[n, exact = TRUE]])) params[[n, exact = TRUE]] else d
  pv("bc_spread_cost", 81) * location_factor(params, "haulage")
}

#' @rdname bc_pass_cost
#' @keywords internal
bc_pass_diesel <- function(params) {
  pv <- function(n, d) if (!is.null(params[[n, exact = TRUE]])) params[[n, exact = TRUE]] else d
  pv("bc_spread_diesel", 12.25)
}

#' Choose the Dose Strategy with the Highest Net Value at a Carbon Price
#'
#' @param ft Output of [biochar_field_table()].
#' @param c_price Carbon price (US$ Mg-1 CO2e).
#' @return List of per-cell vectors for the chosen dose: `v_yield`, `v_spread`, `a_n2o`, `e_diesel`,
#'   `dose` (option value; 0 = annual), `d_eff`, `cohorts`.
#' @export
choose_bc_dose <- function(ft, c_price) {
  val <- ft$v_yield - ft$v_spread + c_price * (ft$a_n2o - ft$e_diesel)
  val[is.na(val)] <- -Inf
  i <- max.col(val, ties.method = "first")
  pick <- function(mx) mx[cbind(seq_len(nrow(mx)), i)]
  out <- lapply(ft[c("v_yield", "v_spread", "a_n2o", "e_diesel", "d_eff", "cohorts")], pick)
  out$dose <- ft$doses[i]
  bad <- !is.finite(ft$b) | ft$b <= 0
  for (nm in names(out)) out[[nm]][bad] <- NA
  out
}
