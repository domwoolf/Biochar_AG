# scripts/lockin_cohorts.R
# Lock-in of residues to PyCCS as the carbon price rises: cohorts of plants along a carbon-price trajectory.
#
# Carbon price: the median marginal abatement cost of CO2 in AR6 pathways likely to limit warming to 2 C
# (Riahi et al. 2022): 90 US$2015 Mg-1 in 2030 and 210 in 2050, interpolated linearly, escalated to 2024 with
# CPI-U, and held at the 2050 value afterwards. Years 2030-2069 cover the life of every cohort.
#
# Each grid cell (central regional parameters, regional discount rate) builds a PyCCS plant in the first year
# from 2030 in which PyCCS has the highest non-negative net value (its cohort year b). Cells in which BECCS
# later becomes the technology with the highest non-negative net value (year T) are exposed to lock-in. For
# these cells, six paths are compared from year b on:
#   timely     PyCCS until T, then BECCS; the PyCCS plant is retired at T and its annualized capital is
#              still paid until the end of its life (stranded)
#   end_of_life PyCCS for its full life (py_life), then BECCS
#   sunk       PyCCS until BECCS's net value exceeds PyCCS's net value plus PyCCS's annualized capital cost
#              (the switch an owner makes once the PyCCS capital is sunk), or until the end of life; stranded
#              capital as in "timely"
#   wait       no plant until T, then BECCS
#   beccs_first BECCS from b, built at a carbon price at which PyCCS has the higher net value
#   retrofit   a BE plant from b, retrofitted with CO2 capture once BECCS's net value exceeds BE's; the
#              retrofit is assumed to cost the capture premium of a new BECCS plant, with no retrofit penalty
# Each comparison is made along each carbon-price trajectory, with shared CO2 trunklines (the main results) and with a dedicated pipeline from each
# BECCS plant to its sink over the whole period (no shared network; early adoption of BECCS).
# Per path, region and CO2 network: cumulative abatement over 2030-2069 (Gt CO2e, undiscounted) and the net present value
# in 2030 at the regional discount rate of the net value, which includes abatement valued at the carbon price
# (billion US$).
#
# Output: results/ai_summaries/lockin_cohorts.csv

suppressPackageStartupMessages(library(data.table))
source("scripts/manuscript_figures.R") # load_all and helpers

CPI <- 313.689 / 237.017 # CPI-U 2024 / 2015
YEARS <- 2030:2069
# Carbon-price trajectories (2024 US$ Mg-1 CO2e), each held at its 2050 value afterwards:
#   below_2C  median marginal abatement cost of CO2 in AR6 pathways likely to limit warming to 2 C (Riahi et al. 2022)
#   ndc       NGFS Phase 5 "Nationally Determined Contributions" scenario (peak warming 2.0-2.5 C), world
#             Price|Carbon, mean of GCAM 6.0, MESSAGEix-GLOBIOM 2.0 and REMIND-MAgPIE 3.3-4.8 at 2030-2050
#             (US$2010: 52.0, 54.7, 59.7, 63.7, 68.3), escalated to 2024 with CPI-U
CPI10 <- 313.689 / 218.056 # CPI-U 2024 / 2010
hold <- function(y, x, v) approx(x, v, xout = pmin(y, 2050), rule = 2)$y
TRAJ <- list(
  below_2C = CPI * hold(YEARS, c(2030, 2050), c(90, 210)),
  ndc = CPI10 * hold(YEARS, seq(2030, 2050, 5), c(52.0, 54.7, 59.7, 63.7, 68.3))
)
REGIONS <- c("US", "Europe", "China", "India")

region_paths <- function(r, network = c("shared", "dedicated")) {
  network <- match.arg(network)
  message("Region ", r, ", CO2 network: ", network)
  dat <- load_region_data(r)
  p0 <- set_scenario(region = r)
  p0$region <- r
  p0$early_adoption <- network == "dedicated"
  sw <- run_price_sweep(dat$template, dat$layers, p0, vec = dat$vec, prices = c(seq(0, 250, by = 5), seq(275, 500, by = 25)))
  rbindlist(lapply(names(TRAJ), function(tr) trajectory_paths(r, network, tr, TRAJ[[tr]], dat, p0, sw)))
}

trajectory_paths <- function(r, network, tr, P_T, dat, p0, sw) {
  bm <- dat$vec$layers$biomass_density * dat$vec$cell_area
  n <- length(bm)
  ny <- length(YEARS)
  nv <- ab <- cap <- array(NA_real_, c(n, 3, ny))
  for (k in seq_len(ny)) {
    if (k > 1 && P_T[k] == P_T[k - 1]) {
      nv[, , k] <- nv[, , k - 1]; ab[, , k] <- ab[, , k - 1]; cap[, , k] <- cap[, , k - 1]
      next
    }
    nv[, , k] <- sweep_net(sw, P_T[k])
    ab[, , k] <- sweep_abate(sw, P_T[k])
    q <- p0
    q$c_price <- P_T[k]
    qc <- cell_params(q, dat$vec)
    cap[, 1, k] <- rep_len(calculate_bes(qc)$capital_cost_mg, n)
    cap[, 2, k] <- rep_len(calculate_beccs(qc)$capital_cost_mg, n)
    cap[, 3, k] <- rep_len(calculate_bebcs(qc)$capital_cost_mg, n)
  }
  best <- sapply(seq_len(ny), function(k) {
    x <- nv[, , k]
    x[!is.finite(x)] <- -Inf
    b <- max.col(x, ties.method = "first")
    b[apply(x, 1, max) < 0] <- 0L
    b
  })
  first_year <- function(m) apply(m, 1, function(v) if (any(v)) which(v)[1] else NA_integer_)
  b_yr <- first_year(best == 3)
  after_b <- !is.na(b_yr) & col(best) >= b_yr # b_yr recycles down the columns: row i is compared with b_yr[i]
  after_b[is.na(after_b)] <- FALSE
  t_yr <- first_year(best == 2 & after_b)
  life <- p0$py_life
  dr <- p0$discount_rate
  disc <- (1 + dr)^-(YEARS - YEARS[1])
  exposed <- which(!is.na(b_yr) & !is.na(t_yr) & t_yr > b_yr)

  # Technology in each year (0 none, 1 BE, 2 BECCS, 3 PyCCS) and stranded PyCCS capital, per path
  path_tech <- function(i, path) {
    b <- b_yr[i]; Tt <- t_yr[i]; k <- seq_len(ny)
    tech <- rep(0L, ny); strand <- rep(FALSE, ny)
    eol <- b + life # first year after the end of the PyCCS plant's life
    if (path == "timely") {
      tech[k >= b & k < Tt] <- 3L; tech[k >= Tt] <- 2L; strand <- k >= Tt & k < eol
    } else if (path == "end_of_life") {
      s <- max(Tt, eol); tech[k >= b & k < s] <- 3L; tech[k >= s] <- 2L
    } else if (path == "sunk") {
      ok <- which(k >= b & is.finite(nv[i, 2, ]) & nv[i, 2, ] >= nv[i, 3, ] + cap[i, 3, ] & nv[i, 2, ] >= 0)
      s <- min(c(ok, eol, Inf)); s <- max(s, Tt)
      tech[k >= b & k < s] <- 3L; tech[k >= s] <- 2L; strand <- k >= s & k < eol
    } else if (path == "beccs_first") {
      tech[k >= b] <- 2L
    } else if (path == "wait") {
      tech[k >= Tt] <- 2L
    } else if (path == "retrofit") {
      ok <- which(k >= b & is.finite(nv[i, 2, ]) & nv[i, 2, ] >= nv[i, 1, ])
      s <- min(c(ok, Tt)); tech[k >= b & k < s] <- 1L; tech[k >= s] <- 2L
    }
    list(tech = tech, strand = strand)
  }
  paths <- c("timely", "end_of_life", "sunk", "wait", "beccs_first", "retrofit")
  res <- rbindlist(lapply(paths, function(pth) {
    A <- 0; V <- 0; yrs_py <- 0; yrs_be <- 0
    for (i in exposed) {
      pt <- path_tech(i, pth)
      on <- pt$tech > 0
      a <- numeric(ny); v <- numeric(ny)
      a[on] <- ab[cbind(i, pt$tech[on], which(on))]
      v[on] <- nv[cbind(i, pt$tech[on], which(on))]
      v[pt$strand] <- v[pt$strand] - cap[i, 3, pt$strand]
      A <- A + bm[i] * sum(a)
      V <- V + bm[i] * sum(v * disc)
      yrs_py <- yrs_py + bm[i] * sum(pt$tech == 3L)
      yrs_be <- yrs_be + bm[i] * sum(pt$tech == 1L)
    }
    data.table(region = r, co2_network = network, trajectory = tr, path = pth, abatement_Gt = A / 1e9, npv_bn_usd = V / 1e9,
               pyccs_years = yrs_py / sum(bm[exposed]), be_years = yrs_be / sum(bm[exposed]))
  }))
  res[, `:=`(exposed_residue_Tg = sum(bm[exposed]) / 1e6, pyccs_residue_Tg = sum(bm[!is.na(b_yr)]) / 1e6,
             median_cohort_year = YEARS[median(b_yr[exposed])], median_takeover_year = YEARS[median(t_yr[exposed])],
             discount_rate = dr)]
  res
}

runs <- CJ(region = REGIONS, network = c("shared", "dedicated"), sorted = FALSE)
out <- rbindlist(Map(region_paths, runs$region, runs$network))
dir.create("results/ai_summaries", showWarnings = FALSE, recursive = TRUE)
fwrite(out, "results/ai_summaries/lockin_cohorts.csv")
print(out, digits = 3)
message("Lock-in cohorts written: results/ai_summaries/lockin_cohorts.csv")
