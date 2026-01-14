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
            width = 3,
            selectInput("region", "Region:",
                choices = c("USA" = "USA", "India" = "India"),
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
                id = "input_price_wrapper",
                sliderInput("input_price_scalar", "Ag Input Price Scalar:",
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


# 3. Server Logic
server <- function(input, output, session) {
    # 0. UI Logic (shinyjs)
    observe({
        if (input$bc_valuation_method == "ag_value") {
            shinyjs::show("ag_val_wrapper")
            shinyjs::hide("input_price_wrapper")
        } else {
            shinyjs::hide("ag_val_wrapper")
            shinyjs::show("input_price_wrapper")
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
        # 2. Ag Input Price Scalar
        # Multiplies the prices of substituted inputs (Fertilizer, Lime)
        keys <- c("price_lime", "price_n", "price_p", "price_k")
        for (k in keys) if (!is.null(p[[k]])) p[[k]] <- p[[k]] * input$input_price_scalar

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

            # colorized version of opt_rgb
            opt_colorize <- opt_rgb * 255
            RGB(opt_colorize) <- 1:3
            opt_colorize <- colorize(opt_colorize, "col", NAzero = TRUE)

            # Enforce Legend Consistency (Still needed for checking)
            levels(opt_idx) <- data.frame(id = 1:3, technology = c("BES", "BECCS", "BEBCS"))
            ct <- data.frame(value = 1:3, col = c("blue", "red", "green"))
            terra::coltab(opt_idx) <- ct

            rv$map_data <- list(
                opt_idx = opt_idx, opt_rgb = opt_rgb,
                opt_colorize = opt_colorize, net_stack = net_stack,
                ts_cost = ts_cost_layer
            )
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
        # terra::plotRGB(rv$map_data$opt_rgb, scale = 1, main = "Optimal Tech (Sat = Excess Value)")
        terra::plot(rv$map_data$opt_colorize, main = "Optimal Tech (Sat = Excess Value)", legend = FALSE)
        legend("topright",
            legend = c("BES", "BECCS", "BEBCS"),
            fill = c("blue", "red", "green"), bg = "white",
            xpd = TRUE, inset = 0.01
        )

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
