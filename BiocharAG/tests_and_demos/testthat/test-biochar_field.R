library(testthat)
library(BiocharAG)

# Reference values: docs/biochar_agronomy_handover/agro_ref.py and agro_ref_testvalues.txt
# (r = 0.062, T = 20, H = 100, p = 0.30, k = 0.002, R = 0.20, N2O half-life 3 yr, B_min = 2.5,
# c_pass = 25, 17.5 L per pass). The reference applies N use per harvested hectare (x CI); the model
# uses N use per hectare of cropland, so the test passes CI x N_app x EF1 as the direct emission.
ref_case <- function(b_h, CI, Y_bc, CEC, V_crop, N_app, EF1, k = 0.002, ...) {
  biochar_field_effects(b = b_h * CI, y_bc = Y_bc, k = k, a10 = max(0, 0.3607 - 0.1008 * log(CEC)),
    val_ha = CI * V_crop, n_dir = CI * N_app * EF1, r = 0.062, T = 20, H = 100, R = 0.20, t_half = 3,
    c_pass = 25, L_pass = 17.5, ...)
}
expect_ref <- function(o, col, ref) expect_equal(unname(o[[col]][1, ]), ref, tolerance = 2e-3, ignore_attr = TRUE)

test_that("field effects match the reference implementation (case A, S China-like)", {
  o <- ref_case(1.62, 1.5, 0.30, 8, 1500, 200, 0.016)
  expect_equal(unname(o$cohorts[1, ]), c(1, 2, 3, 5, 9))
  expect_ref(o, "v_yield", c(86.04, 85.11, 84.01, 81.68, 76.71))
  expect_ref(o, "v_spread", c(3.0864, 1.5432, 1.0288, 0.6173, 0.3429))
  expect_ref(o, "a_n2o", c(0.0606, 0.0534, 0.0472, 0.0376, 0.0255))
  expect_ref(o, "e_diesel", c(0.0058, 0.0029, 0.0019, 0.0012, 0.0006))
})

test_that("field effects match the reference implementation (case B, Corn Belt-like)", {
  o <- ref_case(1.18, 1.0, 0.30, 20, 1300, 150, 0.010)
  expect_equal(unname(o$cohorts[1, ]), c(1, 3, 5, 9, 17))
  expect_ref(o, "v_yield", c(29.41, 29.06, 28.22, 26.43, 23.08))
  expect_ref(o, "a_n2o", c(0.0209, 0.0304, 0.0242, 0.0164, 0.0093))
})

test_that("field effects match the reference implementation (case C, India-like)", {
  o <- ref_case(0.30, 1.4, 0.28, 10, 1600, 120, 0.004)
  expect_equal(unname(o$cohorts[1, ]), c(1, 6, 12, 24, 48))
  expect_ref(o, "v_yield", c(206.01, 205.65, 185.91, 144.58, 90.36))
  expect_ref(o, "v_spread", c(16.67, 2.78, 1.39, 0.69, 0.35))
  expect_ref(o, "a_n2o", c(0.0087, 0.0256, 0.0151, 0.0078, 0.0039))
})

test_that("persistence and dose-exponent sensitivities match the reference", {
  a <- list(1.62, 1.5, 0.30, 8, 1500, 200, 0.016)
  expect_equal(unname(do.call(ref_case, c(a, list(k = 0.002 + log(2) / 3)))$v_yield[1, "5"]), 46.98, tolerance = 2e-3, ignore_attr = TRUE)
  expect_equal(unname(do.call(ref_case, c(a, list(p = 0.50)))$v_yield[1, "5"]), 107.59, tolerance = 2e-3, ignore_attr = TRUE)
})

test_that("cells are evaluated independently when vectorised", {
  one <- ref_case(0.30, 1.4, 0.28, 10, 1600, 120, 0.004)
  two <- biochar_field_effects(b = c(1.18, 0.42), y_bc = c(0.30, 0.28), k = 0.002,
    a10 = c(max(0, 0.3607 - 0.1008 * log(20)), max(0, 0.3607 - 0.1008 * log(10))),
    val_ha = c(1300, 1.4 * 1600), n_dir = c(1.5, 1.4 * 120 * 0.004), r = 0.062, R = 0.20, t_half = 3,
    c_pass = 25, L_pass = 17.5)
  expect_equal(two$v_yield[2, ], one$v_yield[1, ], ignore_attr = TRUE)
  expect_equal(two$a_n2o[2, ], one$a_n2o[1, ], ignore_attr = TRUE)
})
