library(testthat)
library(BiocharAG)

test_that("Pipeline terrain multiplier scales transport CAPEX", {
    base <- calculate_ccs_transport(co2_mass = 5e5, distance = 300)
    expect_equal(calculate_ccs_transport(co2_mass = 5e5, distance = 300, terrain_mult = 1.3), 1.3 * base)
    expect_equal(calculate_ccs_transport(co2_mass = 5e5, distance = 300, terrain_mult = 1.3, terrain_share = 0.5), 1.15 * base)
    # NA and < 1 are treated as flat terrain
    expect_equal(calculate_ccs_transport(co2_mass = 5e5, distance = c(300, 300), terrain_mult = c(NA, 0.9)), c(base, base))
})

test_that("Elevation lift adds booster pumping cost only above the inlet margin", {
    base <- calculate_ccs_transport(co2_mass = 5e5, distance = 300)
    # 100 m of lift (~0.9 MPa) is within the 1 MPa design margin
    expect_equal(calculate_ccs_transport(co2_mass = 5e5, distance = 300, hrel_max_m = 100, elec_price = 60), base)
    lift <- calculate_ccs_transport(co2_mass = 5e5, distance = 300, hrel_max_m = 1000, elec_price = 60) - base
    # Pumping energy for 7.8 MPa is ~3.2 kWh/t (~$0.19/t at $60/MWh); pump CAPEX adds a little more
    expect_gt(lift, 0.19)
    expect_lt(lift, 1)
})

test_that("BECCS chooses the sink class with the lowest transport + storage cost (v2 layers)", {
    p <- set_scenario()
    p$allow_eor <- FALSE
    # cell 1: short onshore route; cell 2: long mountainous onshore route, long sea leg (ship);
    # cell 3: only EOR reachable (excluded without EOR); cell 4: nothing reachable;
    # cell 5: coastal source, sink 30 km offshore (subsea pipeline beats liquefaction + ship)
    p$onsal_len_km <- c(50, 1500, NA, NA, 900)
    p$onsal_terrain_mult <- c(1, 1.4, NA, NA, 1)
    p$onsal_hrel_max_m <- c(0, 1500, NA, NA, 0)
    p$oneor_len_km <- c(NA, NA, 40, NA, NA)
    p$oneor_terrain_mult <- c(NA, NA, 1, NA, NA)
    p$oneor_hrel_max_m <- c(NA, NA, 0, NA, NA)
    for (cls in c("offship", "offpipe")) {
        p[[paste0(cls, "_len_km")]] <- c(200, 20, NA, NA, 5)
        p[[paste0(cls, "_terrain_mult")]] <- c(1, 1, NA, NA, 1)
        p[[paste0(cls, "_hrel_max_m")]] <- c(0, 0, NA, NA, 0)
    }
    p$offship_sea_km <- c(300, 1500, NA, NA, 30)
    p$offpipe_sea_km <- c(300, 1500, NA, NA, 30)

    res <- calculate_beccs(p)
    expect_equal(res$co2_sink_class, c(1, 3, NA, NA, 4))
    expect_equal(res$co2_transport_distance_km, c(50, 1520, NA, NA, 35))
    expect_true(all(is.finite(res$ts_cost[c(1, 2, 5)])))
    expect_true(all(is.infinite(res$ts_cost[3:4])))

    # Subsea factor raises the offshore-pipeline cost
    p2 <- p
    p2$co2_subsea_capex_factor <- 3
    expect_gt(calculate_beccs(p2)$ts_cost[5], res$ts_cost[5])

    p$allow_eor <- TRUE
    res_eor <- calculate_beccs(p)
    expect_equal(res_eor$co2_sink_class, c(1, 3, 2, NA, 4))
    expect_true(is.finite(res_eor$ts_cost[3]))
})

test_that("sink_storage_costs maps routed sinks to site-specific storage costs", {
    lk <- tempfile(fileext = ".csv")
    utils::write.csv(data.frame(
        Basin_Name = c("Illinois Basin", "Permian Basin", "Cauvery Basin"),
        Sub_Unit = c("Mt. Simon Sandstone", "San Andres/Clearfork", "Cretaceous Sands"),
        sink = c(2, 1, 3)
    ), lk, row.names = FALSE)
    cost <- sink_storage_costs(lk)
    expect_equal(cost[2], 8.00) # NETL Mount Simon, IL (2023 USD 7.77 x CPI)
    expect_true(is.na(cost[1])) # EOR sink: regional parameter applies
    expect_true(is.na(cost[3])) # unclassified sink
})
