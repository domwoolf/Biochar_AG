library(testthat)
library(BiocharAG)

test_that("MEF(P) passes through the anchor at today's price and declines to the floor", {
    p <- set_scenario(list(), region = "China")
    p$ff_c_intensity <- c(0.01, 0.1, 0.4) / 3.6 # first cell below the floor
    p$c_price <- p$grid_p_now
    expect_equal(displaced_grid_ci(p), p$ff_c_intensity)
    ci <- sapply(c(50, 100, 200, 1000), function(cp) { p$c_price <- cp; displaced_grid_ci(p)[3] })
    expect_true(all(diff(ci) < 0))
    expect_gt(min(ci), p$mef_floor / 3.6)
    p$c_price <- 500
    expect_equal(displaced_grid_ci(p)[1], 0.01 / 3.6) # below-floor anchors are unchanged
})

test_that("MEF(P) respects the control flags and caps at coal", {
    p <- set_scenario(list(), region = "Europe")
    p$ff_c_intensity <- 0.7 / 3.6
    p$c_price <- 0 # below the EU's current price: dirtier than the anchor, capped at coal
    expect_equal(displaced_grid_ci(p), 0.82 / 3.6)
    p$c_price <- 150
    p$mef_price_dependent <- FALSE
    expect_equal(displaced_grid_ci(p), 0.7 / 3.6)
    p$mef_price_dependent <- TRUE
    p$use_flat_ci <- TRUE
    expect_equal(displaced_grid_ci(p), 0.7 / 3.6)
})

test_that("Bootstrap draws vary the curve; draw 0 is the central fit", {
    p <- set_scenario(list(), region = "USA")
    p$ff_c_intensity <- 0.34 / 3.6
    p$c_price <- 100
    central <- displaced_grid_ci(p)
    draws <- sapply(c(0.1, 0.5, 0.9), function(u) { p$mef_draw_u <- u; displaced_grid_ci(p) })
    expect_gt(length(unique(round(draws, 8))), 1)
    p$mef_draw_u <- 0
    expect_equal(displaced_grid_ci(p), central)
})

test_that("Technologies use the price-dependent intensity, including BEBCS heat mode", {
    p <- set_scenario(list(), region = "India")
    p$ff_c_intensity <- 0.57 / 3.6
    p$c_price <- 0
    a0 <- calculate_bes(p)$tot_c_abatement
    p$c_price <- 150
    a150 <- calculate_bes(p)$tot_c_abatement
    expect_lt(a150, a0)
    p$bebcs_energy_mode <- "heat"
    h <- calculate_bebcs(p)
    p$mef_price_dependent <- FALSE
    expect_lt(h$tot_c_abatement, calculate_bebcs(p)$tot_c_abatement)
})
