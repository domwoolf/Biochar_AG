library(testthat)
library(BiocharAG)

test_that("Fperm calculates correctly with default data", {
    # Default case (~14.9 C), H:C 0.35
    # Formula should correspond roughly to 1.04 - 0.63*0.35 = ~0.82
    val <- calculate_fperm(0.35, method = "HC", soil_temp = 14.9)
    expect_true(val > 0.7 && val < 0.9)
})

test_that("Fperm responds to temperature adjustments", {
    # Higher soil temperature -> Faster decay -> Lower Fperm
    fperm_cool <- calculate_fperm(0.35, soil_temp = 10)
    fperm_warm <- calculate_fperm(0.35, soil_temp = 25)

    expect_true(fperm_cool > fperm_warm)
})

test_that("Pyrolysis Temperature method works", {
    # High Temp Biochar -> Stable -> High Fperm
    # Low Temp Biochar -> Labile -> Low Fperm
    fperm_highT <- calculate_fperm(600, method = "Temp")
    fperm_lowT <- calculate_fperm(400, method = "Temp")

    expect_true(fperm_highT > fperm_lowT)
})

test_that("BEBCS integration uses soil_temp", {
    params <- default_parameters()
    params$soil_temp <- 25 # Warm soil

    res_warm <- calculate_bebcs(params)

    params$soil_temp <- 5 # Cold soil
    res_cold <- calculate_bebcs(params)

    # Cold soil -> higher stability -> higher sequestration
    expect_true(res_cold$c_sequestered > res_warm$c_sequestered)
})

test_that("Approximate Fperm matches exact calculation", {
    # Random sampling for comparison
    # Tolerance of 0.01 is reasonable for interpolation

    vals <- seq(0.1, 0.6, by = 0.1)
    temps <- seq(5, 25, by = 5)

    for (v in vals) {
        for (t in temps) {
            exact <- calculate_fperm(v, method = "HC", soil_temp = t)
            approx <- calculate_fperm_approx(v, method = "HC", soil_temp = t)

            expect_equal(approx, exact, tolerance = 0.01)
        }
    }
})

test_that("Vectorized 1D Fperm matches 2D approx", {
    # Strategy: Pre-calc vector for fixed H:C, then run across temps
    v <- 0.35
    temps <- seq(-10, 30, by = 0.5)

    # 2D Approx
    res_2d <- calculate_fperm_approx(v, method = "HC", soil_temp = temps)

    # 1D Optimization
    prep <- prep_fperm_vector(v, method = "HC")
    res_1d <- calculate_fperm_vectorized(prep, temps)

    # Should be effectively identical (floating point diffs only)
    expect_equal(res_1d, res_2d)
})
