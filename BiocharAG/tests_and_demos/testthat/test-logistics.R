library(testthat)
library(BiocharAG)

test_that("Haulage without terrain factors reproduces the flat-rate formula", {
    p <- set_scenario()
    p$avg_dist <- c(20, 40)
    lg <- BiocharAG:::biomass_logistics(p)
    # Haulage costs are per Mg dry matter: dm_per_daf() Mg hauled per Mg dry, ash-free feed (issue #114)
    expect_equal(lg$cost, dm_per_daf(p) * (p$bm_transport_fixed + p$bm_transport_var * p$avg_dist * p$tortuosity) *
      p$haulage_location_factor)
    # NA factors are treated as 1
    p$haul_kt <- c(NA, 1); p$haul_kd <- c(1, NA); p$haul_g <- c(NA, NA)
    expect_equal(BiocharAG:::biomass_logistics(p)$cost, lg$cost)
})

test_that("Terrain factors scale the time, fuel and distance components", {
    p <- set_scenario()
    p$avg_dist <- 30
    base <- BiocharAG:::biomass_logistics(p)
    var0 <- dm_per_daf(p) * p$bm_transport_var * 30 * p$tortuosity * p$haulage_location_factor
    p$haul_kt <- 2
    expect_equal(BiocharAG:::biomass_logistics(p)$cost - base$cost, var0 * p$haul_time_share)
    expect_equal(BiocharAG:::biomass_logistics(p)$emissions, base$emissions) # time does not burn extra fuel here
    p$haul_kt <- 1; p$haul_g <- 1.5
    expect_equal(BiocharAG:::biomass_logistics(p)$cost - base$cost, var0 * p$haul_fuel_share * 0.5)
    expect_equal(BiocharAG:::biomass_logistics(p)$emissions, 1.5 * base$emissions)
})

test_that("BEBCS pays handling for returning biochar to the fields as a backhaul", {
    p <- set_scenario()
    p$avg_dist <- 30
    with_haul <- calculate_bebcs(p)
    p$bc_return_haul <- FALSE
    without <- calculate_bebcs(p)
    expect_gt(with_haul$biochar_haul_cost_mg, 0)
    expect_equal(without$biochar_haul_cost_mg, 0)
    expect_equal(with_haul$total_cost - without$total_cost, with_haul$biochar_haul_cost_mg)
    # Backhaul: handling only, no distance cost or extra emissions
    expect_equal(with_haul$biochar_haul_cost_mg, with_haul$bc_yield * p$bm_transport_fixed * p$haulage_location_factor)
    expect_equal(with_haul$tot_c_abatement, without$tot_c_abatement)
})
