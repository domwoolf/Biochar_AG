# shiny_demo.R
library(shiny)
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
    titlePanel("BiocharAG Spatial TEA explorer"),
    sidebarLayout(
        sidebarPanel(
            h4("Economic Parameters"),
            sliderInput("c_price", "Carbon Price ($/Mg CO2e):",
                min = 0, max = 500, value = 100, step = 10
            ),
            sliderInput("discount_rate", "Discount Rate (%):",
                min = 0, max = 20, value = 8, step = 0.5
            ),
            numericInput("bc_ag_value", "Biochar Ag Value ($/Mg):",
                value = 50, min = 0, step = 10
            ),
            selectInput("region", "Region:",
                choices = c("USA (Midwest)" = "USA", "India" = "India"),
                selected = "USA"
            ),
            checkboxInput("allow_eor", "Allow EOR Sinks?", value = TRUE),
            selectInput("bc_valuation_method", "Biochar Value Source:",
                choices = c(
                    "Agronomic Value (Simplified)" = "ag_value",
                    "Market Price (Sales)" = "market_price",
                    "Mechanistic Substitution (Advanced)" = "advanced_mechanistic"
                ),
                selected = "advanced_mechanistic"
            ),
            hr(),
            h4("Technology Parameters"),
            numericInput("plant_mw", "Plant Capacity (MW) [Leave Empty for Auto]:",
                value = NA
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
    # 1. Load Data (Session Scope)
    # Reactive Data Loader
    data_r <- reactive({
        req(input$region)

        # Robust Path Detection
        possible_paths <- c(
            "../GIS/processed/",
            "GIS/processed/",
            "/media/dominic/Data/git/Biochar_AG/GIS/processed/"
        )
        gis_path <- NULL
        for (p in possible_paths) {
            # Check for generic indicator file
            if (file.exists(paste0(p, "demo_biomass.tif"))) {
                gis_path <- p
                break
            }
        }
        if (is.null(gis_path)) stop("Spatial data directory not found.")

        if (input$region == "India") {
            prefix <- "india_"
            if (!file.exists(paste0(gis_path, "india_biomass.tif"))) stop("India data not found.")
        } else {
            prefix <- "" # DEMO uses 'demo_' or 'soil_'
        }

        message("Loading Data for Region: ", input$region)

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
            message(sprintf("DEBUG: Loaded US dist_sink_km. Range: %.2f - %.2f", r_min, r_max))

            if (!is.null(ph)) processed_layers$soil_ph <- ph
            if (!is.null(cec)) processed_layers$soil_cec <- cec
            template <- bm
        }

        list(layers = processed_layers, template = template)
    })
    # Reactive values to modify params based on inputs
    params_r <- reactive({
        message("Debug: Inside reactive. Default params exists? ", exists("default_parameters"))
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

            # 1. BES
            message(Sys.time(), " - Starting BES...")
            incProgress(0.1, detail = "Calculating BES...")
            bes_res <- run_spatial_tea(template, curr_params, processed_layers, fun = calculate_bes)

            # 2. BECCS
            message(Sys.time(), " - Starting BECCS...")
            incProgress(0.4, detail = "Calculating BECCS...")
            beccs_res <- run_spatial_tea(template, curr_params, processed_layers, fun = calculate_beccs)

            # 3. BEBCS
            message(Sys.time(), " - Starting BEBCS...")
            incProgress(0.7, detail = "Calculating BEBCS...")
            bebcs_res <- run_spatial_tea(template, curr_params, processed_layers, fun = calculate_bebcs)

            incProgress(0.9, detail = "Rendering Maps...")

            # Render Plot
            output$map_plot <- renderPlot({
                # Stack Net Values
                net_stack <- c(
                    bes_res[["Net_Value_USD"]],
                    beccs_res[["Net_Value_USD"]],
                    bebcs_res[["Net_Value_USD"]]
                )
                names(net_stack) <- c("BES", "BECCS", "BEBCS")

                # Determine Optimal
                opt_idx <- terra::app(net_stack, which.max)

                # Enforce Legend Consistency
                # Set categories 1,2,3
                levels(opt_idx) <- data.frame(id = 1:3, technology = c("BES", "BECCS", "BEBCS"))

                # Define Fixed Color Table: 1=Blue, 2=Red, 3=Green
                # terra::plot will automatically use this table
                ct <- data.frame(value = 1:3, col = c("blue", "red", "green"))
                terra::coltab(opt_idx) <- ct

                # Plot Layout
                par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))

                # Individual Scaling for Visibility
                terra::plot(net_stack[["BES"]], main = "BES Net Value ($/Mg)", col = map.pal("viridis"))
                terra::plot(net_stack[["BECCS"]], main = "BECCS Net Value ($/Mg)", col = map.pal("viridis"))
                terra::plot(net_stack[["BEBCS"]], main = "BEBCS Net Value ($/Mg)", col = map.pal("viridis"))

                # BECCS Transport Cost (Validation)
                # Check if layer exists (it should with updated package)
                if ("Transport_Cost_USD_Mg" %in% names(beccs_res)) {
                    terra::plot(beccs_res[["Transport_Cost_USD_Mg"]],
                        main = "BECCS Transport Cost ($/Mg)",
                        col = rev(map.pal("viridis")) # Invert so High Cost is Purple/Dark? Or Red?
                    )
                } else {
                    # Fallback to Optimal Map
                    terra::plot(opt_idx,
                        main = paste0("Optimal Tech (", input$region, " | C Price: $", input$c_price, ")")
                    )
                }

                mtext(paste0("Spatial Analysis Results (Discount: ", input$discount_rate, "%)"),
                    outer = TRUE, cex = 1.5
                )
            })
        }) # End Progress
    })
}

# Run App
options(shiny.host = "0.0.0.0")
options(shiny.port = 8100)
shinyApp(ui = ui, server = server)
