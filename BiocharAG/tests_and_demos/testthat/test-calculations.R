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
    expect_true(res$biochar_value >= 0) # Soil benefit can be 0 or positive
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

test_that("plant_mw_th accepts named vector and resolves correctly", {
    params <- default_parameters()
    
    # 1. Individual runs
    p_bes <- params; p_bes$plant_mw_th <- 10
    res_bes_indiv <- calculate_bes(p_bes)
    
    p_beccs <- params; p_beccs$plant_mw_th <- 20
    res_beccs_indiv <- calculate_beccs(p_beccs)
    
    p_bebcs <- params; p_bebcs$plant_mw_th <- 30
    res_bebcs_indiv <- calculate_bebcs(p_bebcs)
    
    # 2. Vectorized run
    p_vector <- params
    p_vector$plant_mw_th <- c(BES = 10, BECCS = 20, BEBCS = 30)
    
    res_bes_vector <- calculate_bes(p_vector)
    res_beccs_vector <- calculate_beccs(p_vector)
    res_bebcs_vector <- calculate_bebcs(p_vector)
    
    # Compare
    expect_equal(res_bes_vector$total_cost, res_bes_indiv$total_cost)
    expect_equal(res_beccs_vector$total_cost, res_beccs_indiv$total_cost)
    expect_equal(res_bebcs_vector$total_cost, res_bebcs_indiv$total_cost)
})

test_that("BECCS early adoption transport cost logic", {
    # Test calculate_ccs_transport directly
    # For a distance > 50km (where standard sharing fraction would apply)
    dist <- 200
    co2_mass <- 100000 # 0.1 Mtpa
    
    cost_std <- calculate_ccs_transport(co2_mass = co2_mass, distance = dist, early_adoption = FALSE)
    cost_ea <- calculate_ccs_transport(co2_mass = co2_mass, distance = dist, early_adoption = TRUE)
    
    # Early adoption should have much higher cost because it does not benefit from shared network
    expect_true(cost_ea > cost_std)
    
    # Test calculate_beccs with early adoption parameter
    params_std <- default_parameters()
    params_std$early_adoption <- FALSE
    res_std <- calculate_beccs(params_std)
    
    params_ea <- default_parameters()
    params_ea$early_adoption <- TRUE
    res_ea <- calculate_beccs(params_ea)
    
    expect_true(res_ea$total_cost > res_std$total_cost)
    expect_true(res_ea$ts_cost > res_std$ts_cost)
})
