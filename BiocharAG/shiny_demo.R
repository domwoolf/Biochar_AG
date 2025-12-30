# shiny_demo.R
library(shiny)
library(terra)

# Load Package (Development Mode)
devtools::load_all(".")

# 1. Load Data (Global Scope)
# Load from GIS folder (Root/GIS/processed/)
gis_path <- "../GIS/processed/"
bm_file <- paste0(gis_path, "demo_biomass.tif")

message("Loading Spatial Data from ", gis_path)

if (file.exists(bm_file)) {
    bm <- terra::rast(bm_file)
    st <- terra::rast(paste0(gis_path, "demo_soil_temp.tif"))
    ep <- terra::rast(paste0(gis_path, "demo_elec_price.tif"))
    processed_layers <- list(biomass_density = bm, soil_temp = st, elec_price = ep)
    template <- bm
} else {
    stop("Spatial data not found in ../GIS/processed/. Please run data processing scripts.")
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
            selectInput("bc_valuation_method", "Biochar Value Source:",
                choices = c("Agronomic Value" = "ag_value", "Market Price" = "market_price"),
                selected = "ag_value"
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
    # Reactive values to modify params based on inputs
    params_r <- reactive({
        p <- default_parameters()
        p$c_price <- input$c_price
        p$discount_rate <- input$discount_rate / 100
        p$bc_ag_value <- input$bc_ag_value
        p$bc_valuation_method <- input$bc_valuation_method

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

            # 1. BES
            incProgress(0.1, detail = "Calculating BES...")
            bes_res <- run_spatial_tea(template, curr_params, processed_layers, fun = calculate_bes)

            # 2. BECCS
            incProgress(0.4, detail = "Calculating BECCS...")
            beccs_res <- run_spatial_tea(template, curr_params, processed_layers, fun = calculate_beccs)

            # 3. BEBCS
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
                levels(opt_idx) <- data.frame(id = 1:3, technology = c("BES", "BECCS", "BEBCS"))
                cols <- c("blue", "red", "green")

                # Plot Layout
                par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))

                # Individual Scaling for Visibility
                terra::plot(net_stack[["BES"]], main = "BES Net Value ($/Mg)", col = map.pal("viridis"))
                terra::plot(net_stack[["BECCS"]], main = "BECCS Net Value ($/Mg)", col = map.pal("viridis"))
                terra::plot(net_stack[["BEBCS"]], main = "BEBCS Net Value ($/Mg)", col = map.pal("viridis"))

                # Optimal Map
                terra::plot(opt_idx,
                    main = paste0("Optimal Tech (C Price: $", input$c_price, ")"),
                    col = cols
                )

                mtext(paste0("Spatial Analysis Results (Discount: ", input$discount_rate, "%)"),
                    outer = TRUE, cex = 1.5
                )
            })
        }) # End Progress
    })
}

# Run App
shinyApp(ui = ui, server = server)
