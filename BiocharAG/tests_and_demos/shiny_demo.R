# shiny_demo.R
library(shiny)
library(shinyjs)
library(terra)
library(dplyr)
library(sf)
library(BiocharAG)

# Ensure data dictionary / parameters are available
if (!exists("default_parameters")) {
    message("Package loaded but default_parameters not found on search path.")
}

# 2. UI Definition
ui <- fluidPage(
    useShinyjs(),
    titlePanel("BiocharAG Spatial TEA explorer"),
    sidebarLayout(
        sidebarPanel(
            selectInput("region", "Region:",
                choices = c("USA (Midwest)" = "USA", "India" = "India"),
                selected = "USA"
            ),
            numericInput("plant_mw", "Plant Capacity (MW) [Leave Empty for Auto]:",
                value = NA
            ),
            sliderInput("c_price", "Carbon Price ($/Mg CO2e):",
                min = 0, max = 500, value = 100, step = 10
            ),
            sliderInput("discount_rate", "Discount Rate (%):",
                min = 0, max = 20, value = 8, step = 0.5
            ),
            selectInput("bc_valuation_method", "Biochar Value Source:",
                choices = c(
                    "Agronomic Value" = "ag_value",
                    "Mechanistic Substitution" = "advanced_mechanistic"
                ),
                selected = "advanced_mechanistic"
            ),
            div(
                id = "ag_val_wrapper",
                numericInput("bc_ag_value", "Biochar Ag Value ($/Mg):",
                    value = 50, min = 0, step = 10
                )
            ),
            div(
                id = "food_price_wrapper",
                sliderInput("food_price_scalar", "Food Price Scalar:",
                    min = 0.5, max = 2.0, value = 1.0, step = 0.1
                )
            ),
            sliderInput("elec_price_scalar", "Electricity Price Scalar:",
                min = 0.5, max = 2.0, value = 1.0, step = 0.1
            ),
            checkboxInput("allow_eor", "Allow EOR Sinks?", value = TRUE),
            sliderInput("capture_eff", "BECCS Capture Efficiency:",
                min = 0.5, max = 1.0, value = 0.90, step = 0.05
            ),
            sliderInput("eff_penalty", "BECCS Efficiency Penalty:",
                min = 0.0, max = 0.20, value = 0.08, step = 0.01
            ),
            hr(),
            actionButton("run_btn", "Run Analysis", class = "btn-primary", width = "100%"),
            p(
                class = "text-muted", style = "margin-top: 10px;",
                "Note: Analysis may take 10-20 seconds to run across the full grid."
            )
        ),
        mainPanel(
            plotOutput("map_plot", height = "800px")
        )
    )
)

# --- DEBUG: OVERRIDE PACKAGE FUNCTIONS ---

calculate_ccs_transport <- function(co2_mass, distance, discount_rate = 0.10, lifetime = 20) {
    if (co2_mass <= 0) {
        return(0)
    }

    # Reference Project (Medium/Large Scale)
    ref_mass <- 1000000
    ref_dist <- 100

    base_capex_ref <- 50000000
    scale_factor <- 0.6 # Economies of scale exponent
    opex_factor <- 0.04 # Annual O&M as % of CAPEX

    # Heuristic Parameters
    feeder_threshold_km <- 50
    trunk_mass_flow <- max(co2_mass, 3000000) # Assume Trunkline is at least 3 Mtpa

    # Calculate Annuity Factor
    annuity_fac <- (1 - (1 + discount_rate)^(-lifetime)) / discount_rate

    if (distance > feeder_threshold_km) {
        # Split Distance
        dist_feeder <- feeder_threshold_km
        dist_trunk <- distance - feeder_threshold_km

        # 1. Feeder Leg
        scaler_f <- (co2_mass / ref_mass)^scale_factor
        capex_f <- base_capex_ref * (dist_feeder / ref_dist) * scaler_f

        # 2. Trunk Leg
        scaler_t <- (trunk_mass_flow / ref_mass)^scale_factor
        capex_t_total <- base_capex_ref * (dist_trunk / ref_dist) * scaler_t
        ann_capex_t_total <- capex_t_total / annuity_fac
        annual_capex_t_share <- ann_capex_t_total * (co2_mass / trunk_mass_flow)

        # Calculate Share of TRUNK Total Capex (not annual)
        capex_t_share <- capex_t_total * (co2_mass / trunk_mass_flow)

        annual_capex <- (capex_f / annuity_fac) + annual_capex_t_share

        # FIX: OPEX is % of Total Capex Share, not Annual Payment
        total_capex_share <- capex_f + capex_t_share
        annual_opex <- total_capex_share * opex_factor
    } else {
        # Distance < Feeder Threshold (Direct Pipeline)
        scaler <- (co2_mass / ref_mass)^scale_factor
        capex <- base_capex_ref * (distance / ref_dist) * scaler

        annual_capex <- capex / annuity_fac
        annual_opex <- capex * opex_factor # FIX: % of Total Capex
    }

    total_annual_cost <- annual_capex + annual_opex
    cost_per_ton <- total_annual_cost / co2_mass

    # DEBUG LOGGING (Sampled)
    if (runif(1) < 0.001) {
        message(sprintf("DEBUG BECCS TRANSPORT: Dist=%.1f km, Mass=%.1f t/y, Cost=%.2f $/t", distance, co2_mass, cost_per_ton))
    }

    return(cost_per_ton)
}

calculate_beccs <- function(params) {
    # Default to modern params if not present
    if (is.null(params$beccs_efficiency)) params$beccs_efficiency <- 0.28
    if (is.null(params$capture_rate)) params$capture_rate <- 0.90

    allow_eor <- if (!is.null(params$allow_eor)) as.logical(params$allow_eor) else TRUE

    # Check for Spatial Inputs
    dist_spatial <- NULL
    if (allow_eor) {
        if (!is.null(params$dist_sink_km)) dist_spatial <- params$dist_sink_km
    } else {
        if (!is.null(params$dist_sink_saline_km)) dist_spatial <- params$dist_sink_saline_km
    }

    if (!is.null(dist_spatial)) {
        params$ccs_distance <- dist_spatial
    } else if (is.null(params$ccs_distance)) {
        if (!is.null(params$lat) && !is.null(params$lon)) {
            # Cannot call pkg private function easily, assume 100 if missing
            params$ccs_distance <- 100
        } else {
            params$ccs_distance <- 100
        }
    }

    if (is.null(params$ccs_storage_cost)) params$ccs_storage_cost <- 15
    if (is.null(params$beccs_capital_cost)) params$beccs_capital_cost <- 4000

    # Apply Fuel Quality Penalties (High Ash -> Higher Cost)
    # params <- adjust_costs_for_fuel(params) # Skipping for simplified debug

    with(params, {
        # 1. Energy Output
        energy_output <- bm_lhv * beccs_efficiency
        elec_prod <- energy_output * 0.277778 # MWh / Mg biomass

        # 2. Carbon Capture
        co2_produced <- bm_c * (44 / 12)
        co2_captured <- co2_produced * capture_rate

        # 3. Transport & Storage Costs
        plant_mw <- if (!is.null(params$plant_mw)) params$plant_mw else 50
        capacity_factor <- 0.85
        annual_biomass <- (plant_mw * 8760 * capacity_factor) / elec_prod
        annual_co2_total <- annual_biomass * co2_captured

        # Explicit Transport Cost ($/Mg CO2)
        transport_cost_per_ton <- calculate_ccs_transport(
            co2_mass = annual_co2_total,
            distance = ccs_distance,
            discount_rate = discount_rate,
            lifetime = bes_life
        )

        # Total T&S Cost ($/Mg Biomass)
        ts_cost <- (transport_cost_per_ton + ccs_storage_cost) * co2_captured

        # 4. Plant Costs (CAPEX/OPEX)
        scaling_factor <- 0.7
        base_cost_beccs <- beccs_capital_cost * 50 * 1000
        total_capex <- base_cost_beccs * ((plant_mw / 50)^scaling_factor)

        # Simple Annuity Factor inline
        annuity_fac <- (1 - (1 + discount_rate)^(-bes_life)) / discount_rate
        annual_capex_payment <- total_capex / annuity_fac
        capex_per_mg <- annual_capex_payment / annual_biomass

        opex_per_mg <- capex_per_mg * 0.05

        # Logistics Cost (Biomass Transport)
        radius <- if (!is.null(params$collection_radius)) params$collection_radius else 50
        avg_dist <- (2 / 3) * radius
        tf <- if (!is.null(params$bm_transport_fixed)) params$bm_transport_fixed else 5.0
        tv <- if (!is.null(params$bm_transport_var)) params$bm_transport_var else 0.15
        logistics_cost <- tf + (tv * avg_dist)

        total_cost <- capex_per_mg + opex_per_mg + ts_cost + logistics_cost

        # 5. Revenue & Value
        elec_revenue <- elec_prod * elec_price

        # Carbon Abatement
        c_sequestered <- bm_c * capture_rate # C equivalent
        c_displaced <- energy_output * ff_c_intensity
        tot_c_abatement <- c_sequestered + c_displaced
        abatement_value <- tot_c_abatement * c_price

        total_revenue <- elec_revenue + abatement_value
        net_value <- total_revenue - total_cost

        if (runif(1) < 0.001) {
            message(sprintf(
                "DEBUG BECCS BREAKDOWN: Rev=%.1f (Elec=%.1f, Abate=%.1f), Capex=%.1f, Opex=%.1f, T&S=%.1f (Dist=%.1f, Unit=$%.1f/tCO2), Logist=%.1f | TOT Cost=%.1f -> Net=%.1f",
                total_revenue, elec_revenue, abatement_value, capex_per_mg, opex_per_mg, ts_cost, ccs_distance, transport_cost_per_ton, logistics_cost, total_cost, net_value
            ))
        }

        list(
            technology = "BECCS",
            energy_output = energy_output,
            elec_prod = elec_prod,
            c_sequestered = c_sequestered,
            tot_c_abatement = tot_c_abatement,
            total_cost = total_cost,
            ts_cost = ts_cost,
            total_revenue = total_revenue,
            net_value = net_value
        )
    })
}

# 3. Server Logic
server <- function(input, output, session) {
    # 0. UI Logic (shinyjs)
    observe({
        if (input$bc_valuation_method == "ag_value") {
            shinyjs::show("ag_val_wrapper")
            shinyjs::hide("food_price_wrapper")
        } else {
            shinyjs::hide("ag_val_wrapper")
            shinyjs::show("food_price_wrapper")
        }
    })

    # Reactive Values to store results
    rv <- reactiveValues(map_data = NULL)

    # Load Data Reactive
    data_r <- reactive({
        req(input$region)

        # Robust Path Logic
        possible_paths <- c(
            "../GIS/processed/",
            "GIS/processed/",
            "/media/dominic/Data/git/Biochar_AG/BiocharAG/GIS/processed/"
        )
        gis_path <- NULL
        for (p in possible_paths) {
            if (dir.exists(p)) {
                gis_path <- p
                break
            }
        }
        if (is.null(gis_path)) stop("Spatial data directory not found.")


        if (input$region == "India") {
            bm <- terra::rast(paste0(gis_path, "india_biomass.tif"))
            st <- terra::rast(paste0(gis_path, "india_soil_temp.tif"))
            ep <- terra::rast(paste0(gis_path, "india_elec_price.tif"))
            ph <- terra::rast(paste0(gis_path, "india_soil_ph.tif"))
            cec <- terra::rast(paste0(gis_path, "india_soil_cec.tif"))

            # Transport Layers
            ds <- terra::rast(paste0(gis_path, "india_dist_sink.tif"))
            ds_saline <- terra::rast(paste0(gis_path, "india_dist_sink_saline.tif"))
            stype <- terra::rast(paste0(gis_path, "india_sink_type.tif"))

            processed_layers <- list(
                biomass_density = bm, soil_temp = st, elec_price = ep,
                soil_ph = ph, soil_cec = cec,
                dist_sink_km = ds, dist_sink_saline_km = ds_saline, sink_is_offshore = stype
            )
            template <- bm
        } else {
            # USA / Demo Logic
            bm <- terra::rast(paste0(gis_path, "demo_biomass.tif"))
            st <- terra::rast(paste0(gis_path, "demo_soil_temp.tif"))
            ep <- terra::rast(paste0(gis_path, "demo_elec_price.tif"))

            # Transport (US Demo)
            if (file.exists(paste0(gis_path, "us_dist_sink.tif"))) {
                ds <- terra::rast(paste0(gis_path, "us_dist_sink.tif"))
                ds_saline <- terra::rast(paste0(gis_path, "us_dist_sink_saline.tif"))
                stype <- terra::rast(paste0(gis_path, "us_sink_type.tif"))
            } else {
                # Fallback if US Transport layers missing (use demo defaults)
                ds <- terra::rast(bm)
                values(ds) <- 100
                ds_saline <- ds
                stype <- terra::rast(bm)
                values(stype) <- 0
            }
            ph <- NULL
            cec <- NULL
            # Prefer Real Soil Data if available
            if (file.exists(paste0(gis_path, "soil_ph.tif"))) {
                ph <- terra::rast(paste0(gis_path, "soil_ph.tif"))
            } else {
                if (file.exists(paste0(gis_path, "demo_soil_ph.tif"))) ph <- terra::rast(paste0(gis_path, "demo_soil_ph.tif"))
            }

            if (file.exists(paste0(gis_path, "soil_cec.tif"))) {
                cec <- terra::rast(paste0(gis_path, "soil_cec.tif"))
            } else {
                if (file.exists(paste0(gis_path, "demo_soil_cec.tif"))) cec <- terra::rast(paste0(gis_path, "demo_soil_cec.tif"))
            }

            processed_layers <- list(
                biomass_density = bm, soil_temp = st, elec_price = ep,
                dist_sink_km = ds, dist_sink_saline_km = ds_saline, sink_is_offshore = stype
            )

            # DEBUG: Print Check
            r_min <- minmax(ds)[1]
            r_max <- minmax(ds)[2]

            if (!is.null(ph)) processed_layers$soil_ph <- ph
            if (!is.null(cec)) processed_layers$soil_cec <- cec
            template <- bm
        }

        list(layers = processed_layers, template = template)
    })
    # Reactive values to modify params based on inputs
    params_r <- reactive({
        if (!exists("default_parameters")) stop("CRITICAL: default_parameters is missing inside server scope!")

        if (input$region == "India") {
            tryCatch(p <- parameters_india(), error = function(e) {
                message("Warning: parameters_india not found, falling back to default.")
                p <- default_parameters()
            })
        } else {
            p <- default_parameters()
        }
        p$c_price <- input$c_price
        p$discount_rate <- input$discount_rate / 100
        p$bc_ag_value <- input$bc_ag_value
        p$bc_valuation_method <- input$bc_valuation_method

        # Pass Region for Transport Cost Factors
        p$region <- if (input$region == "USA") "North America" else input$region
        p$allow_eor <- input$allow_eor

        if (!is.na(input$plant_mw)) {
            p$plant_mw <- input$plant_mw
        }

        # --- SCALARS ---
        # 1. Electricity Price
        if (!is.null(p$elec_price)) p$elec_price <- p$elec_price * input$elec_price_scalar

        # 2. Food Price / Biochar Value
        if (!is.null(p$bc_ag_value)) p$bc_ag_value <- p$bc_ag_value * input$food_price_scalar
        keys <- c("price_lime", "price_n", "price_p", "price_k")
        for (k in keys) if (!is.null(p[[k]])) p[[k]] <- p[[k]] * input$food_price_scalar

        # 3. BECCS Params
        p$capture_rate <- input$capture_eff
        p$beccs_efficiency <- if (!is.null(p$bes_energy_efficiency)) (p$bes_energy_efficiency - input$eff_penalty) else (0.30 - input$eff_penalty)

        p
    })

    # Event: Run Button Clicked
    observeEvent(input$run_btn, {
        # Progress Bar
        withProgress(message = "Running Spatial Analysis...", value = 0, {
            # Get current params
            curr_params <- params_r()

            # Get Data
            dat <- data_r()
            template <- dat$template
            processed_layers <- dat$layers

            # 1. BES (Standard Radius: 50km)
            message(Sys.time(), " - Starting BES...")
            incProgress(0.1, detail = "Calculating BES...")
            bes_res <- run_spatial_tea(template, curr_params, processed_layers, fun = calculate_bes, collection_radius_km = 50)

            # 2. BECCS (Large Radius: 100km to leverage economies of scale against CCS cost)
            message(Sys.time(), " - Starting BECCS...")
            incProgress(0.4, detail = "Calculating BECCS...")
            beccs_res <- run_spatial_tea(template, curr_params, processed_layers, fun = calculate_beccs, collection_radius_km = 50)

            # 3. BEBCS (Distributed Radius: 40km)
            message(Sys.time(), " - Starting BEBCS...")
            incProgress(0.7, detail = "Calculating BEBCS...")
            bebcs_res <- run_spatial_tea(template, curr_params, processed_layers, fun = calculate_bebcs, collection_radius_km = 40)

            incProgress(0.9, detail = "Rendering Maps...")

            # 3. BECCS Cost Layer extraction
            # beccs_res has a layer named "Transport_Cost_USD_Mg" (see run_spatial_tea return value)
            ts_cost_layer <- beccs_res[["Transport_Cost_USD_Mg"]]

            # Stack Net Values
            net_stack <- c(
                bes_res[["Net_Value_USD"]],
                beccs_res[["Net_Value_USD"]],
                bebcs_res[["Net_Value_USD"]]
            )
            names(net_stack) <- c("BES", "BECCS", "BEBCS")

            # Determine Optimal
            opt_idx <- terra::app(net_stack, which.max)

            # Excess NPV (Saturation) calculation
            # Calculate difference between best and second best
            excess_r <- terra::app(net_stack, function(x) {
                s <- sort(x, decreasing = TRUE)
                if (length(s) < 2) 0 else s[1] - s[2]
            })

            # Create RGB Raster (White -> Color) based on Excess
            # Saturation Cap: $50 margin = Full Color
            sat_cap <- 50 # max(excess_r, na.rm = TRUE)
            sat <- excess_r / sat_cap
            sat[sat > 1] <- 1
            sat[sat < 0] <- 0

            # Initialize RGB channels as White (1,1,1) * (1-S) + Color * S
            # Simplified: Tinting White.
            # R, G, B channels
            r <- terra::rast(opt_idx)
            values(r) <- 1
            g <- terra::rast(opt_idx)
            values(g) <- 1
            b <- terra::rast(opt_idx)
            values(b) <- 1

            # Logic:
            # If Opt=1 (BES, Blue): R=1-S, G=1-S, B=1
            # If Opt=2 (BECCS, Red): R=1, G=1-S, B=1-S
            # If Opt=3 (BEBCS, Green): R=1-S, G=1, B=1-S  (Using Green (0,1,0))

            # Vectorized assignment using masks
            # 1: BES (Blue)
            mask1 <- opt_idx == 1
            r[mask1] <- 1 - sat[mask1]
            g[mask1] <- 1 - sat[mask1]
            b[mask1] <- 1

            # 2: BECCS (Red)
            mask2 <- opt_idx == 2
            r[mask2] <- 1
            g[mask2] <- 1 - sat[mask2]
            b[mask2] <- 1 - sat[mask2]

            # 3: BEBCS (Green)
            mask3 <- opt_idx == 3
            r[mask3] <- 1 - sat[mask3]
            g[mask3] <- 1
            b[mask3] <- 1 - sat[mask3]

            # Stack RGB
            opt_rgb <- c(r, g, b)
            names(opt_rgb) <- c("red", "green", "blue")

            # Enforce Legend Consistency (Still needed for checking)
            levels(opt_idx) <- data.frame(id = 1:3, technology = c("BES", "BECCS", "BEBCS"))
            ct <- data.frame(value = 1:3, col = c("blue", "red", "green"))
            terra::coltab(opt_idx) <- ct

            rv$map_data <- list(opt_idx = opt_idx, opt_rgb = opt_rgb, net_stack = net_stack, ts_cost = ts_cost_layer)
        }) # End Progress
    })

    output$map_plot <- renderPlot({
        req(rv$map_data)

        # Setup 3x2 layout
        par(mfrow = c(3, 2))

        # Calculate common range for Net Value maps
        common_range <- range(minmax(rv$map_data$net_stack))

        # Row 1: BES & BECCS
        terra::plot(rv$map_data$net_stack[["BES"]], main = "BES Net Value ($)", range = common_range)
        terra::plot(rv$map_data$net_stack[["BECCS"]], main = "BECCS Net Value ($)", range = common_range)

        # Row 2: BEBCS & Optimal
        terra::plot(rv$map_data$net_stack[["BEBCS"]], main = "BEBCS Net Value ($)", range = common_range)

        # Optimal Technology (RGB)
        terra::plotRGB(rv$map_data$opt_rgb, scale = 1, main = "Optimal Tech (Sat = Excess Value)")
        legend("topright", legend = c("BES", "BECCS", "BEBCS"), fill = c("blue", "red", "green"), bg = "white")

        # Row 3: Transport Cost & Spare
        # Using a distinct color palette
        terra::plot(rv$map_data$ts_cost, main = "BECCS Transport Cost ($/Mg)", col = terra::map.pal("viridis", 100))
        # Empty plot for the spare slot (optional, or just leave blank)
        # plot.new()
    })
}

# Run App
options(shiny.host = "0.0.0.0")
options(shiny.port = 8100)
shinyApp(ui = ui, server = server)
