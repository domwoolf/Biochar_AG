library(testthat)
library(BiocharAG)

# Reference values: cached results from the 'bebcs' sheet of Resources/nets1.xlsm at the inputs below.
sheet_inputs <- list(
  py_temp = 608.90250205993652, lignin = 0.25, bm_lhv = 18.76367157894737,
  moisture = 0, ash = 0.1015709114074707, feed_c = 0.5, feed_h = 0.06, feed_o = 0.44,
  feed_rate_kg_hr = 224.607, heater_eff = 0.8, parasitic_power = 0.07, exhaust_temp = 170
)
phys <- do.call(calculate_pyrolysis_physics, sheet_inputs)

test_that("pyrolysis yields match nets1.xlsm", {
  expect_equal(phys$yield_bc_daf, 0.24162, tolerance = 1e-4)
  expect_equal(phys$bc_c_yield, 0.224597, tolerance = 1e-4)
  expect_equal(phys$yield_bo, 0.305409, tolerance = 1e-4)
  expect_equal(phys$yield_gas, 0.222311, tolerance = 1e-4)
  expect_equal(phys$yield_h2o, 0.230659, tolerance = 1e-4)
  expect_equal(phys$energy_char, 8.25591, tolerance = 1e-4)
})

test_that("pyrolysis heat losses match nets1.xlsm", {
  expect_equal(unname(phys$heat_losses["wall"]), 0.117712, tolerance = 1e-3)
  expect_equal(unname(phys$heat_losses["biochar"]), 0.100789, tolerance = 1e-4)
  expect_equal(unname(phys$heat_losses["biooil"]), 0.464222, tolerance = 1e-4)
  expect_equal(unname(phys$heat_losses["h2o"]), 0.651152, tolerance = 1e-4)
  # Gas cp uses the mixture molar mass instead of the sheet's fixed 15 g/mol
  expect_lt(unname(phys$heat_losses["gas"]), 0.0666933)
})

test_that("heat supply differs from nets1.xlsm only by the HHV basis and gas molar mass", {
  feed_hhv_minus_lhv <- 2.442 * (18.015 / 2.016) * sheet_inputs$feed_h
  gas_loss_change <- unname(phys$heat_losses["gas"]) - 0.0666933
  expect_equal(phys$heat_supply, 1.61771 - feed_hhv_minus_lhv + gas_loss_change, tolerance = 1e-3)
})

test_that("H:C and derived permanence proxy behave sensibly", {
  hc <- sapply(c(350, 500, 700), function(t) calculate_pyrolysis_physics(t, 0.2, 18.6)$bc_h_c_molar)
  expect_true(all(diff(hc) < 0))
  expect_equal(hc[2], 0.347, tolerance = 1e-2)
})
