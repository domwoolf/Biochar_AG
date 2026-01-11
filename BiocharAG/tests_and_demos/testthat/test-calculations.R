library(testthat)
library(BiocharAG)

test_that("Default parameters load correctly", {
    params <- default_parameters()
    expect_type(params, "list")
    expect_true(is.numeric(params$discount_rate))
    expect_equal(params$bc_ag_value, 0)
})

test_that("BES returns valid metrics", {
    params <- default_parameters()
    res <- calculate_bes(params)

    expect_equal(res$technology, "BES")
    expect_true(res$total_revenue > 0)
    expect_true(res$total_cost > 0)
    expect_equal(res$net_value, res$total_revenue - res$total_cost)
    expect_true(res$elec_prod > 0)
})

test_that("BEBCS returns valid metrics and soil benefits", {
    params <- default_parameters()
    res <- calculate_bebcs(params)

    expect_equal(res$technology, "BEBCS")
    expect_true(res$nbcf >= 0) # Soil benefit can be 0 or positive
    expect_true(res$bc_yield > 0 && res$bc_yield < 1)
    expect_equal(res$net_value, res$total_revenue - res$total_cost)
})

test_that("BEBCS energy output logic (volatile fraction)", {
    params <- default_parameters()
    res <- calculate_bebcs(params)

    # Check that energy output is roughly consistent with mass balance
    # If yield is ~0.25, energy should be from ~0.75 mass * efficiency
    expect_true(res$energy_output < (params$bm_lhv * (1 - res$bc_yield)))
})

test_that("RPV comparison works", {
    params <- default_parameters()
    bes <- calculate_bes(params)
    beccs <- calculate_beccs(params)
    bebcs <- calculate_bebcs(params)

    res <- calculate_rpv(list(bes, beccs, bebcs))
    expect_equal(nrow(res), 3)
    expect_true("RPV" %in% names(res))
    # Sum of RPVs is not necessarily 0, but max RPV should be > 0 ?
    # Actually RPV = NPV - Max(Other).
    # The "Winner" will have positive RPV.
    winner <- res[which.max(res$NPV), ]
    expect_true(winner$RPV >= 0)
})
