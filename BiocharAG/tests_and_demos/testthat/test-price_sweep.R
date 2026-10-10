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

test_that("Exact break-even and takeover prices on a synthetic sweep", {
    prices <- c(0, 50, 100, 200)
    n <- 4
    n0 <- matrix(c(-50, -50, 10, -Inf,   # BES
                   -100, -100, -100, -100, # BECCS
                   -20, 5, -300, -20), n, 3) # BEBCS
    a_const <- matrix(c(0.1, 0.5, 0.2, 0.1,
                        1.0, 0.5, 2.0, 1.0,
                        0.4, 0.4, 0.4, 0.4), n, 3)
    abate <- array(rep(a_const, length(prices)), c(n, 3, length(prices)))
    abate[1, 1, ] <- c(0.1, 0.3, 0.5, 0.5) # piecewise-linear abatement for cell 1, BES
    sw <- list(prices = prices, n0 = n0, n0_grid = array(rep(n0, length(prices)), c(n, 3, length(prices))), abate = abate)
    be <- sweep_breakeven(sw)
    # Constant abatement: root = -N0 / A
    expect_equal(unname(be[, 2]), c(100, 200, 50, 100))
    expect_equal(unname(be[2, 3]), -5 / 0.4) # already positive at zero price: negative root
    expect_true(is.na(be[4, 1])) # no route
    # Piecewise abatement, cell 1 BES: f(C) = -50 + C A(C); on [100, 200] A = 0.5, so the root is C = 100
    expect_equal(unname(be[1, 1]), 100)
    # Agreement with the scanning solver
    old <- price_root(function(cp) sweep_n0(sw, cp)[, 2], function(cp) sweep_abate(sw, cp)[, 2], prices)
    expect_equal(unname(be[, 2]), old, tolerance = 1e-6)
    # Takeover: cell 3, BECCS f = -100 + 2C overtakes BES (10 + 0.2C) at C = 61.1 and PyCCS stays negative
    to <- sweep_takeover(sw, k = 2)
    expect_equal(to[3], 110 / 1.8, tolerance = 1e-6)
    # Cell 4: BES has no route, PyCCS f = -20 + 0.4C; BECCS f = -100 + C exceeds it from C = 133.3
    expect_equal(to[4], 80 / 0.6, tolerance = 1e-6)
    expect_true(is.na(sweep_takeover(sw, k = 2, max_price = 60)[3]))
})
