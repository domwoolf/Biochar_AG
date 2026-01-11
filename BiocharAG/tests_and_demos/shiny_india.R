# shiny_india.R (India Regional Comparison)
library(shiny)
library(terra)
library(dplyr)
library(sf)

# Load Package (Robust)
tryCatch(
    {
        library(BiocharAG)
    },
    error = function(e) {
        message("Library load failed, attempting devtools::load_all()")
        devtools::load_all(".")
    }
)


# 1. Load Data (Session Scope)
server <- function(input, output, session) {
    # Path Logic
    possible_paths <- c(
        "../GIS/processed/",
        "GIS/processed/",
        "/media/dominic/Data/git/Biochar_AG/GIS/processed/"
    )
    gis_path <- NULL
    for (p in possible_paths) {
        if (file.exists(paste0(p, "india_biomass.tif"))) {
            gis_path <- p
            break
        }
    }
    if (is.null(gis_path)) stop("India spatial data not found.")

    bm <- terra::rast(paste0(gis_path, "india_biomass.tif"))
    st <- terra::rast(paste0(gis_path, "india_soil_temp.tif"))
    ep <- terra::rast(paste0(gis_path, "india_elec_price.tif"))
    ph <- terra::rast(paste0(gis_path, "india_soil_ph.tif"))
    cec <- terra::rast(paste0(gis_path, "india_soil_cec.tif"))

    processed_layers <- list(
        biomass_density = bm, soil_temp = st, elec_price = ep,
        soil_ph = ph, soil_cec = cec
    )
    template <- bm

    message("India Data Loaded. Grid: ", paste(dim(template), collapse = "x"))

    # Reactive Parameters (Using parameters_india default)
    params_r <- reactive({
        p <- parameters_india() # <--- Key Change

        p$c_price <- input$c_price
        p$discount_rate <- input$discount_rate / 100
        p$bc_valuation_method <- input$bc_valuation_method

        # Override Feedstock Cost if User Desires (Simulate Negative Cost)
        # Note: 'bc_ag_value' in UI is added to parameter list
        p$bc_ag_value <- input$bc_ag_value

        if (!is.na(input$plant_mw)) p$plant_mw <- input$plant_mw
        p
    })

    observeEvent(input$run_btn, {
        withProgress(message = "Running India Analysis...", value = 0, {
            curr_params <- params_r()

            # 1. BES
            message("Running BES")
            bes_res <- run_spatial_tea(template, curr_params, processed_layers, fun = calculate_bes)
            incProgress(0.3)

            # 2. BECCS
            message("Running BECCS")
            beccs_res <- run_spatial_tea(template, curr_params, processed_layers, fun = calculate_beccs)
            incProgress(0.6)

            # 3. BEBCS
            message("Running BEBCS")
            bebcs_res <- run_spatial_tea(template, curr_params, processed_layers, fun = calculate_bebcs)
            incProgress(0.9)

            output$map_plot <- renderPlot({
                net_stack <- c(bes_res[["Net_Value_USD"]], beccs_res[["Net_Value_USD"]], bebcs_res[["Net_Value_USD"]])
                names(net_stack) <- c("BES", "BECCS", "BEBCS")
                opt_idx <- terra::app(net_stack, which.max)
                levels(opt_idx) <- data.frame(id = 1:3, technology = c("BES", "BECCS", "BEBCS"))

                par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
                plot(net_stack[["BES"]], main = "BES Net Value ($/Mg)", col = map.pal("viridis"))
                plot(net_stack[["BECCS"]], main = "BECCS Net Value ($/Mg)", col = map.pal("viridis"))
                plot(net_stack[["BEBCS"]], main = "BEBCS Net Value ($/Mg)", col = map.pal("viridis"))

                cols <- c("blue", "red", "green")
                plot(opt_idx, main = "Optimal Tech (India)", col = cols)
                mtext(paste0("India (North-West) Scenario | C Price: $", input$c_price), outer = TRUE, cex = 1.5)
            })
        })
    })
}

ui <- fluidPage(
    titlePanel("BiocharAG: India Regional Comparison"),
    sidebarLayout(
        sidebarPanel(
            h4("India Scenario Parameters"),
            p("Assumes: Low Labor, Low Construction Cost, High Discount (12%), Null/Negative Feedstock Cost Context."),
            sliderInput("c_price", "Carbon Price ($/Mg CO2e):", min = 0, max = 200, value = 50, step = 10),
            sliderInput("discount_rate", "Discount Rate (%):", min = 0, max = 20, value = 12, step = 0.5), # Default 12 for India
            numericInput("bc_ag_value", "Biochar Ag Value ($/Mg):", value = 0), # Let Mechanistic handle it
            selectInput("bc_valuation_method", "Biochar Val Method:",
                choices = c("Mechanistic Substitution (Advanced)" = "advanced_mechanistic", "Agronomic Value" = "ag_value"),
                selected = "advanced_mechanistic"
            ),
            hr(),
            numericInput("plant_mw", "Plant Capacity (MW):", value = NA),
            actionButton("run_btn", "Run Analysis", class = "btn-primary", width = "100%")
        ),
        mainPanel(plotOutput("map_plot", height = "800px"))
    )
)

shinyApp(ui, server)
