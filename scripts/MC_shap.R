# scripts/MC_shap.R
# Attribution of Monte Carlo output uncertainty to the sampled parameters. For each region and each
# regional quantity recorded by mc_analysis.R (median break-even prices of PyCCS and BECCS, median BECCS
# takeover price, mean BE net value without carbon revenue), fits gradient-boosted regression trees
# (XGBoost) to the sampled parameters and computes SHAP values (shapviz). Importance is the mean absolute
# SHAP value, in the units of the quantity, and its share of the total.
#
# Outputs:
#   results/mc_shap/mc_shap_importance.csv          all parameters (and groups, group = TRUE; share relative
#                                                   to the sum over individual parameters), held-out R2
#   results/mc_shap/beeswarm_<target>_<region>.png  top parameters
#   results/ai_summaries/mc_shap_importance.csv     top 10 parameters per region and quantity
#   results/ai_summaries/mc_quantiles_summary.csv   5th/50th/95th percentiles of every recorded quantity
# Entry point: run_mc_shap().

library(data.table)
library(xgboost)
library(shapviz)
library(ggplot2)

MC_TARGETS <- c(be_PyCCS = "Median PyCCS break-even price (US$/Mg CO2e)",
                be_BECCS = "Median BECCS break-even price (US$/Mg CO2e)",
                takeover = "Median BECCS takeover price (US$/Mg CO2e)",
                n0_BE = "Mean BE net value without carbon revenue (US$/Mg)")
MC_META <- c("mc_run_id", "region")
MC_TOP_PRICE <- 400 # top of the Monte Carlo sweep grid (censoring level)
# Parameters sampled jointly whose SHAP values are also reported as a group (SHAP divides the effect of
# correlated inputs between them): importance of the group = mean |sum of their SHAP values|
MC_GROUPS <- list(bc_yield_amplitude = c("bc_yield_b0", "bc_yield_bcec"))

mc_metric_names <- function(df) {
  grep("^(be_|takeover$|n0_|share_|abate_)", names(df), value = TRUE)
}

fit_mc_shap <- function(X, y, seed = 1) {
  set.seed(seed)
  test <- sample(nrow(X), round(0.2 * nrow(X)))
  prm <- list(max_depth = 4, eta = 0.05, subsample = 0.8, colsample_bytree = 0.8,
              objective = "reg:squarederror", nthread = 4)
  m_test <- xgb.train(prm, xgb.DMatrix(X[-test, , drop = FALSE], label = y[-test]), nrounds = 400, verbose = 0)
  pred <- predict(m_test, xgb.DMatrix(X[test, , drop = FALSE]))
  r2 <- 1 - sum((y[test] - pred)^2) / sum((y[test] - mean(y[test]))^2)
  model <- xgb.train(prm, xgb.DMatrix(X, label = y), nrounds = 400, verbose = 0)
  list(shp = shapviz(model, X_pred = X), r2 = r2)
}

run_mc_shap <- function(data_path = "results/mc_analysis_results.csv", min_runs = 200) {
  df <- fread(data_path, data.table = FALSE)
  metrics <- mc_metric_names(df)
  params <- setdiff(names(df), c(MC_META, metrics))
  pdesc <- read.csv("BiocharAG/inst/extdata/parameters.csv", stringsAsFactors = FALSE)[, c("name", "description")]
  ai_dir <- "results/ai_summaries"
  out_dir <- "results/mc_shap"
  dir.create(ai_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # Percentiles of every recorded quantity
  q <- rbindlist(lapply(split(df, df$region), function(d) {
    rbindlist(lapply(metrics, function(m) data.table(
      region = d$region[1], quantity = m, p05 = quantile(d[[m]], 0.05, na.rm = TRUE),
      p50 = median(d[[m]], na.rm = TRUE), p95 = quantile(d[[m]], 0.95, na.rm = TRUE),
      share_censored = if (m %in% c("be_PyCCS", "be_BECCS", "takeover")) mean(d[[m]] >= MC_TOP_PRICE) else NA_real_,
      n = sum(!is.na(d[[m]])))))
  }))
  fwrite(q, file.path(ai_dir, "mc_quantiles_summary.csv"))

  n_runs <- max(table(df$region))
  if (n_runs < min_runs) {
    message(sprintf("Skipping Monte Carlo SHAP: %d draws per region (< %d).", n_runs, min_runs))
    return(invisible(NULL))
  }

  imp <- list()
  for (r in unique(df$region)) {
    d <- df[df$region == r, ]
    X <- as.matrix(d[, params, drop = FALSE])
    keep <- apply(X, 2, function(v) var(v, na.rm = TRUE) > 1e-12)
    X <- X[, keep, drop = FALSE]
    for (tg in names(MC_TARGETS)) {
      y <- d[[tg]]
      ok <- is.finite(y)
      if (sum(ok) < min_runs || var(y[ok]) == 0) next
      fit <- fit_mc_shap(X[ok, , drop = FALSE], y[ok])
      mas <- colMeans(abs(fit$shp$S))
      grp <- vapply(MC_GROUPS, function(g) {
        g <- intersect(g, colnames(fit$shp$S))
        if (length(g)) mean(abs(rowSums(fit$shp$S[, g, drop = FALSE]))) else NA_real_
      }, numeric(1))
      imp[[paste(r, tg)]] <- rbind(
        data.table(region = r, quantity = tg, parameter = names(mas), mean_abs_shap = mas,
                   share = mas / sum(mas), r2_test = fit$r2, group = FALSE),
        data.table(region = r, quantity = tg, parameter = names(grp), mean_abs_shap = grp,
                   share = grp / sum(mas), r2_test = fit$r2, group = TRUE)[is.finite(mean_abs_shap)])
      top <- names(sort(mas, decreasing = TRUE))[seq_len(min(12, length(mas)))]
      p <- sv_importance(fit$shp[, top], kind = "beeswarm", max_display = length(top)) +
        labs(x = paste0("SHAP value: ", MC_TARGETS[[tg]])) + theme_bw(base_size = 11)
      ggsave(file.path(out_dir, sprintf("beeswarm_%s_%s.png", tg, r)), p, width = 8, height = 5.5, dpi = 300)
      message(sprintf("%s %s: R2 (held out) = %.2f", r, tg, fit$r2))
    }
  }
  imp <- rbindlist(imp)
  imp <- merge(imp, as.data.table(pdesc), by.x = "parameter", by.y = "name", all.x = TRUE)
  setorder(imp, region, quantity, -mean_abs_shap)
  fwrite(imp, file.path(out_dir, "mc_shap_importance.csv"))
  fwrite(imp[, .SD[seq_len(min(10, .N))], by = .(region, quantity)], file.path(ai_dir, "mc_shap_importance.csv"))
  invisible(imp)
}
