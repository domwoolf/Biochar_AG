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
#' region gets its own distribution; `"absolute"` bounds are used as given. A `dist_min` or
#' `dist_max` cell may add regional overrides after the default, separated by semicolons
#' (e.g. `0.85;India=0.5`); the region is taken from `central$region`.
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
    region <- normalize_region_name(central[["region", exact = TRUE]])
    lo <- mc_bound_value(row$dist_min, region)
    hi <- mc_bound_value(row$dist_max, region)
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

#' Resolve a Sampling Bound with Optional Regional Overrides
#'
#' @param x A `dist_min` or `dist_max` cell: a number, or a default followed by `Region=value`
#'   overrides separated by semicolons (e.g. `"0.85;India=0.5"`).
#' @param region Normalised region name, or NULL.
#' @return The numeric bound for `region`.
#' @keywords internal
mc_bound_value <- function(x, region = NULL) {
  parts <- trimws(strsplit(as.character(x), ";", fixed = TRUE)[[1]])
  if (!length(parts)) return(NA_real_)
  val <- suppressWarnings(as.numeric(parts[1]))
  for (p in parts[-1]) {
    kv <- trimws(strsplit(p, "=", fixed = TRUE)[[1]])
    if (length(kv) != 2) stop("Invalid regional bound '", p, "' in '", x, "'.")
    if (!is.null(region) && identical(normalize_region_name(kv[1]), region)) val <- as.numeric(kv[2])
  }
  val
}

#' Sample Monte Carlo Parameters with Rank Correlation
#'
#' Draws correlated uniforms via a Gaussian copula and maps them through each parameter's
#' marginal (PERT, uniform or normal), so marginals are exact. PERT and uniform marginals are bounded;
#' for a normal marginal, `min` and `max` are its 5th and 95th percentiles.
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
      # Normal: min and max are the 5th and 95th percentiles; with normal marginals the Gaussian copula
      # gives exactly a multivariate normal (e.g. correlated regression coefficients)
      normal = stats::qnorm(u[, j], mean = d$mode, sd = (d$max - d$min) / (2 * stats::qnorm(0.95))),
      stop("Unsupported distribution '", d$distribution, "' for ", d$name)
    )
  })
  names(draws) <- dist_table$name
  as.data.frame(draws)
}
