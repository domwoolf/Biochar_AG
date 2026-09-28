library(testthat)
library(BiocharAG)

# Synthetic sweep: 2 cells x 3 technologies, abatement falling with price like MEF(P)
fake_sweep <- function() {
    prices <- c(0, 50, 100, 200)
    a_fun <- function(cp) cbind(c(1, 1), c(1, 1), c(1, 1)) * (0.5 + 0.5 / (1 + cp / 50))
    abate <- array(NA_real_, c(2, 3, length(prices)))
    for (i in seq_along(prices)) abate[, , i] <- a_fun(prices[i])
    list(prices = prices, n0 = cbind(c(-30, 10), c(-80, -500), c(5, -10)), abate = abate)
}

test_that("sweep_abate interpolates within the grid and is flat outside it", {
    sw <- fake_sweep()
    expect_equal(sweep_abate(sw, -20), sw$abate[, , 1])
    expect_equal(sweep_abate(sw, 75), 0.5 * (sw$abate[, , 2] + sw$abate[, , 3]))
    expect_equal(sweep_abate(sw, 400), sw$abate[, , 4])
    expect_equal(sweep_net(sw, 75), sw$n0 + 75 * sweep_abate(sw, 75))
})

test_that("sweep_breakeven finds where net value turns positive", {
    sw <- fake_sweep()
    be <- sweep_breakeven(sw, step = 0.5)
    # Inside the grid, net value at the break-even price is ~0
    for (k in which(is.finite(be) & be > 0 & be < 200)) {
        i <- (k - 1) %% 2 + 1
        j <- (k - 1) %/% 2 + 1
        expect_lt(abs(sweep_net(sw, be[i, j])[i, j]), 1e-2)
    }
    # Profitable at C = 0: exact root below zero using A(0)
    expect_equal(unname(be[2, 1]), -10 / sw$abate[2, 1, 1])
    # Beyond the grid: exact root with A held at the top price
    expect_equal(unname(be[2, 2]), 500 / sw$abate[2, 2, 4])
    # Break-even is later than the linear extrapolation from C = 0, since A falls with price
    expect_gt(be[1, 2], 80 / sw$abate[1, 2, 1])
})

test_that("price_root returns NA when value never turns positive", {
    expect_true(is.na(price_root(-10, function(cp) 0, c(0, 100))))
    expect_true(is.na(price_root(5, function(cp) -1, c(0, 100))))
})
