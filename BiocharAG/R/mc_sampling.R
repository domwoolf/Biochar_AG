#' Quantile Function of the (Modified) PERT Distribution
#'
#' @param p Vector of probabilities.
#' @param min,mode,max Lower bound, most likely value and upper bound.
#' @param lambda Shape parameter (4 = classic PERT).
#' @return Vector of quantiles.
#' @export
qpert <- function(p, min, mode, max, lambda = 4) {
  if (max <= min) {
    return(rep(mode, length(p)))
  }
  alpha <- 1 + lambda * (mode - min) / (max - min)
  beta <- 1 + lambda * (max - mode) / (max - min)
  min + (max - min) * stats::qbeta(p, alpha, beta)
}

#' Build Monte Carlo Marginal Distributions
#'
#' Resolves the sampling bounds in `parameters.csv` against central (typically regional)
#' values. `dist_bounds = "relative"` bounds are multipliers on the central value, so each
#' region gets its own distribution; `"absolute"` bounds are used as given.
#'
#' @param params_df Parameter table (as read from `parameters.csv`).
#' @param central Named list of central values (e.g. `set_scenario(region = r)`); parameters
#'   missing from it fall back to `default_value`.
#' @param names Optional subset of parameter names to include.
#' @return A data frame with columns `name`, `distribution`, `min`, `mode`, `max`.
#' @export
mc_distribution_table <- function(params_df, central = list(), names = NULL) {
  rows <- params_df[!is.na(params_df$distribution) & !tolower(params_df$distribution) %in% c("", "none"), ]
  if (!is.null(names)) rows <- rows[rows$name %in% names, ]

  out <- lapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, ]
    mode <- central[[row$name]]
    if (is.null(mode) || length(mode) != 1 || !is.numeric(mode)) mode <- suppressWarnings(as.numeric(row$default_value))
    lo <- as.numeric(row$dist_min)
    hi <- as.numeric(row$dist_max)
    bounds <- tolower(trimws(row$dist_bounds))
    if (bounds == "relative") {
      lo <- lo * mode
      hi <- hi * mode
    } else if (bounds != "absolute") {
      stop("Parameter ", row$name, ": dist_bounds must be 'relative' or 'absolute'.")
    }
    if (any(is.na(c(lo, mode, hi))) || lo > mode || mode > hi) {
      stop(sprintf("Parameter %s: invalid bounds (min=%s, mode=%s, max=%s).", row$name, lo, mode, hi))
    }
    data.frame(name = row$name, distribution = tolower(row$distribution), min = lo, mode = mode, max = hi)
  })
  do.call(rbind, out)
}

#' Sample Monte Carlo Parameters with Rank Correlation
#'
#' Draws correlated uniforms via a Gaussian copula and maps them through each parameter's
#' marginal (PERT or uniform), so marginals are exact and bounded.
#'
#' @param dist_table Output of [mc_distribution_table()].
#' @param n Number of draws.
#' @param correlations Optional data frame with columns `param_a`, `param_b`, `rho`
#'   (Gaussian copula correlations). Pairs involving parameters not in `dist_table` are ignored.
#' @return A data frame with one column per parameter and `n` rows.
#' @export
sample_mc_parameters <- function(dist_table, n, correlations = NULL) {
  k <- nrow(dist_table)
  corr <- diag(k)
  dimnames(corr) <- list(dist_table$name, dist_table$name)
  if (!is.null(correlations)) {
    for (i in seq_len(nrow(correlations))) {
      a <- correlations$param_a[i]
      b <- correlations$param_b[i]
      if (a %in% dist_table$name && b %in% dist_table$name) {
        corr[a, b] <- corr[b, a] <- correlations$rho[i]
      }
    }
  }
  if (min(eigen(corr, symmetric = TRUE, only.values = TRUE)$values) <= 0) {
    stop("Parameter correlation matrix is not positive definite; check parameter_correlations.csv.")
  }

  z <- matrix(stats::rnorm(n * k), nrow = n) %*% chol(corr)
  u <- stats::pnorm(z)

  draws <- lapply(seq_len(k), function(j) {
    d <- dist_table[j, ]
    switch(d$distribution,
      pert = qpert(u[, j], d$min, d$mode, d$max),
      uniform = d$min + (d$max - d$min) * u[, j],
      stop("Unsupported distribution '", d$distribution, "' for ", d$name)
    )
  })
  names(draws) <- dist_table$name
  as.data.frame(draws)
}
