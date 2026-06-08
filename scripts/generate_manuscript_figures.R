# generate_manuscript_figures.R
# Script to generate publication-quality display items for the
# BiocharAG manuscript.

library(terra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)

# Silence linter warnings for NSE (Non-Standard Evaluation) variables
.data <- rlang::.data

# Always load from source to ensure we use the latest code modifications
if (dir.exists("BiocharAG")) {
    devtools::load_all("BiocharAG")
} else if (dir.exists("../BiocharAG")) {
    devtools::load_all("../BiocharAG")
} else {
    stop("Could not locate BiocharAG package directory.")
}

# --- GLOBAL CONFIGURATION ---
GLOBAL_OPTIMIZE_SCALE <- FALSE

# Figure Output Directory
OUT_DIR <- if (dir.exists("figures")) "figures/" else if (dir.exists("../figures")) "../figures/" else "figures/"

# --- HELPER FUNCTIONS ---

load_region_data <- function(region_name) {
    gis_path <- "../GIS/processed/"
    if (!dir.exists(gis_path)) {
        gis_path <- "GIS/processed/"
        if (!dir.exists(gis_path)) {
            stop("Could not locate GIS/processed/ directory.")
        }
    }

    prefix_map <- list(
        "US" = list(base = "us", dist = "us"),
        "China" = list(base = "china", dist = "china"),
        "Europe" = list(base = "europe", dist = "europe"),
        "India" = list(base = "india", dist = "india")
    )

    if (!(region_name %in% names(prefix_map))) {
        stop("Unknown region: ", region_name)
    }

    p_base <- prefix_map[[region_name]]$base
    p_dist <- prefix_map[[region_name]]$dist

    bm <- terra::rast(file.path(gis_path, paste0(p_base, "_biomass.tif")))
    st <- terra::rast(file.path(gis_path, paste0(p_base, "_soil_temp.tif")))
    ep <- terra::rast(file.path(gis_path, paste0(p_base, "_elec_price.tif")))
    if (region_name == "US" &&
        file.exists(file.path(gis_path, "us_elec_price.tif"))) {
        ep <- terra::rast(file.path(gis_path, "us_elec_price.tif"))
    }
    dist_sink <- terra::rast(
        file.path(gis_path, paste0(p_dist, "_dist_sink.tif"))
    )
    dist_sink_saline <- terra::rast(
        file.path(gis_path, paste0(p_dist, "_dist_sink_saline.tif"))
    )
    sink_type <- terra::rast(
        file.path(gis_path, paste0(p_dist, "_sink_type.tif"))
    )
    ph <- terra::rast(file.path(gis_path, paste0(p_base, "_soil_ph.tif")))
    if (region_name == "US" &&
        file.exists(file.path(gis_path, "soil_ph.tif"))) {
        ph <- terra::rast(file.path(gis_path, "soil_ph.tif"))
    }
    cec <- terra::rast(file.path(gis_path, paste0(p_base, "_soil_cec.tif")))
    if (region_name == "US" &&
        file.exists(file.path(gis_path, "soil_cec.tif"))) {
        cec <- terra::rast(file.path(gis_path, "soil_cec.tif"))
    }

    a0_path <- file.path(gis_path, paste0(p_dist, "_admin0.gpkg"))
    a1_path <- file.path(gis_path, paste0(p_dist, "_admin1.gpkg"))
    admin0 <- if (file.exists(a0_path)) {
        sf::st_read(a0_path, quiet = TRUE)
    } else {
        NULL
    }
    admin1 <- if (file.exists(a1_path)) {
        sf::st_read(a1_path, quiet = TRUE)
    } else {
        NULL
    }

    layers <- list(
        biomass_density = bm,
        soil_temp = st,
        elec_price = ep,
        dist_sink_km = dist_sink,
        dist_sink_saline_km = dist_sink_saline,
        sink_is_offshore = sink_type,
        soil_ph = ph,
        soil_cec = cec
    )

    for (sz in c(5, 25, 50, 100, 250, 500, 1000)) {
        dist_name <- paste0("dist_", sz, "MWth")
        dist_file <- file.path(gis_path, paste0(p_dist, "_", dist_name, ".tif"))
        if (file.exists(dist_file)) {
            layers[[dist_name]] <- terra::rast(dist_file)
        }
    }

    list(template = bm, layers = layers, admin0 = admin0, admin1 = admin1)
}

run_scenario <- function(template, layers, params) {
    # Run the base TEA (which takes ~10-20 seconds per tech)
    bes <- BiocharAG::run_spatial_tea(
        template, params, layers,
        fun = BiocharAG::calculate_bes,
        plant_mw_th = 250,
        optimize_scale = GLOBAL_OPTIMIZE_SCALE
    )
    beccs <- BiocharAG::run_spatial_tea(
        template, params, layers,
        fun = BiocharAG::calculate_beccs,
        plant_mw_th = 250,
        optimize_scale = GLOBAL_OPTIMIZE_SCALE
    )
    bebcs <- BiocharAG::run_spatial_tea(
        template, params, layers,
        fun = BiocharAG::calculate_bebcs,
        plant_mw_th = 250,
        optimize_scale = GLOBAL_OPTIMIZE_SCALE
    )

    net_stack <- c(
        bes[["Net_Value_USD"]],
        beccs[["Net_Value_USD"]],
        bebcs[["Net_Value_USD"]]
    )
    names(net_stack) <- c("BES", "BECCS", "BEBCS")

    abate_stack <- c(
        bes[["Abatement_tCO2"]],
        beccs[["Abatement_tCO2"]],
        bebcs[["Abatement_tCO2"]]
    )
    names(abate_stack) <- c("BES", "BECCS", "BEBCS")

    opt_idx <- terra::which.max(net_stack)

    list(net = net_stack, abate = abate_stack, opt = opt_idx)
}

# Linear interpolation for fast sweeps
# Net_Value(C) = Net_Value(0) + C * Abatement
get_linear_baseline <- function(template, layers, base_params) {
    p0 <- base_params
    p0$c_price <- 0
    res0 <- run_scenario(template, layers, p0)
    res0 # Returns net at C=0, and abatement
}

# --- FIGURE GENERATORS ---

# Figure 1: Scale vs. Sink Bivariate Map
generate_fig1_phys_boundary <- function(dat, region_name, save_map = FALSE) {
    message("Generating Figure 1: Physical Boundary for ", region_name, "...")

    params <- BiocharAG::default_parameters()
    params$c_price <- 150
    params$region <- region_name
    params$bc_valuation_method <- "advanced_mechanistic"

    res <- run_scenario(dat$template, dat$layers, params)

    stack_df <- terra::as.data.frame(
        c(dat$layers$biomass_density, dat$layers$dist_sink_km, res$opt),
        xy = TRUE,
        na.rm = TRUE
    )
    names(stack_df)[3:5] <- c("biomass", "dist", "opt_tech")

    tech_levels <- c("1" = "BES", "2" = "BECCS", "3" = "BEBCS")
    stack_df$tech <- tech_levels[as.character(stack_df$opt_tech)]

    p <- ggplot(stack_df, aes(x = .data$dist, y = .data$biomass)) +
        geom_point(aes(color = .data$tech), alpha = 0.5, size = 1) +
        scale_color_manual(
            values = c("BES" = "#1f77b4", "BECCS" = "#d62728", "BEBCS" = "#2ca02c")
        ) +
        theme_minimal(base_size = 14) +
        labs(
            #      title = paste0("Scale vs. Sink (Optimal Tech at $150/t CO2) - ", region_name),
            x = "Distance to Sink (km)",
            y = expression("Biomass Density (Mg/km"^2 * ")"),
            color = "Optimal Technology"
        )

    # Contour for BECCS
    if (any(stack_df$tech == "BECCS", na.rm = TRUE)) {
        p <- p + geom_density_2d(
            data = stack_df[
                !is.na(stack_df$tech) & stack_df$tech == "BECCS",
            ],
            color = "black",
            alpha = 0.7
        )
    }

    if (save_map) {
        ggsave(
            paste0(OUT_DIR, region_name, "_Fig1_Physical_Boundary.png"),
            p,
            width = 8,
            height = 6,
            bg = "white",
            dpi = 300
        )
    } else {
        print(p)
    }
    p
}

# Figure 2: Booster Penalty CDF
generate_fig2_booster_penalty <- function(dat, region_name, save_map = FALSE) {
    message("Generating Figure 2: Booster Penalty CDF for ", region_name, "...")

    params <- BiocharAG::default_parameters()
    params$c_price <- 150
    params$region <- region_name
    params$bc_valuation_method <- "advanced_mechanistic"

    res <- run_scenario(dat$template, dat$layers, params)
    cell_area <- terra::cellSize(dat$template, unit = "km")

    stack_df <- terra::as.data.frame(
        c(dat$layers$biomass_density, dat$layers$dist_sink_km, res$opt, cell_area),
        na.rm = TRUE
    )
    names(stack_df) <- c("biomass_density", "dist", "opt_tech", "area_km2")

    tech_levels <- c("1" = "BES", "2" = "BECCS", "3" = "BEBCS")
    stack_df$tech <- tech_levels[as.character(stack_df$opt_tech)]
    stack_df$cell_biomass <- stack_df$biomass_density * stack_df$area_km2

    beccs_df <- stack_df |>
        filter(.data$tech == "BECCS") |>
        arrange(.data$dist) |>
        mutate(cumulative_biomass = cumsum(.data$cell_biomass))

    if (nrow(beccs_df) == 0) {
        message("  No BECCS optimal cells found for Figure 2. Skipping plot.")
        return(NULL)
    }

    total_biomass <- sum(stack_df$cell_biomass, na.rm = TRUE)
    beccs_df$percent_national <-
        (beccs_df$cumulative_biomass / total_biomass) * 100

    p <- ggplot(beccs_df, aes(x = .data$dist, y = .data$percent_national)) +
        geom_line(color = "#d62728", linewidth = 1.5) +
        geom_vline(xintercept = 700, linetype = "dashed", color = "black") +
        annotate(
            "text",
            x = 750,
            y = max(beccs_df$percent_national, na.rm = TRUE) * 0.5,
            label = "700km Booster Threshold",
            angle = 90
        ) +
        theme_minimal(base_size = 14) +
        labs(
            #      title = paste0("BECCS Addressable Biomass vs Distance to Sink - ",
            #        region_name
            #      ),
            x = "Distance to Sink (km)",
            y = "% of Total Available Biomass"
        )

    if (save_map) {
        ggsave(
            paste0(OUT_DIR, region_name, "_Fig2_Booster_Penalty_CDF.png"),
            p,
            width = 8,
            height = 6,
            bg = "white",
            dpi = 300
        )
    } else {
        print(p)
    }
    p
}

# Figure 3: Evaporation Maps
generate_fig3_evaporation <- function(dat, region_name, save_map = FALSE) {
    message("Generating Figure 3: Evaporation Maps for ", region_name, "...")

    d_rates <- c(0.02, 0.08, 0.15)
    c_prices <- c(100, 150)

    all_df <- data.frame()

    for (cp in c_prices) {
        for (dr in d_rates) {
            message("  Running DR: ", dr * 100, "%, C Price: $", cp)
            p <- BiocharAG::default_parameters()
            p$c_price <- cp
            p$discount_rate <- dr
            p$region <- region_name
            p$bc_valuation_method <- "advanced_mechanistic"

            res <- run_scenario(dat$template, dat$layers, p)

            df <- terra::as.data.frame(res$opt, xy = TRUE, na.rm = TRUE)
            names(df)[3] <- "opt_tech"
            tech_levels <- c("1" = "BES", "2" = "BECCS", "3" = "BEBCS")
            df$tech <- tech_levels[as.character(df$opt_tech)]
            df$dr_label <- paste0("Discount Rate: ", dr * 100, "%")
            df$cp_label <- paste0("Carbon Price: $", cp, "/t")
            all_df <- bind_rows(all_df, df)
        }
    }

    all_df$dr_label <- factor(
        all_df$dr_label,
        levels = c("Discount Rate: 2%", "Discount Rate: 8%", "Discount Rate: 15%")
    )

    plt <- ggplot() +
        geom_tile(data = all_df, aes(x = .data$x, y = .data$y, fill = .data$tech))
    if (!is.null(dat$admin0)) {
        plt <- plt + geom_sf(
            data = dat$admin0,
            fill = NA,
            color = "black",
            linewidth = 0.5
        )
    }
    if (!is.null(dat$admin1)) {
        plt <- plt + geom_sf(
            data = dat$admin1,
            fill = NA,
            color = "black",
            linetype = "dotted",
            linewidth = 0.2
        )
    }
    plt <- plt +
        coord_sf(crs = 4326) +
        scale_fill_manual(
            values = c("BES" = "#1f77b4", "BECCS" = "#d62728", "BEBCS" = "#2ca02c")
        ) +
        facet_grid(cp_label ~ dr_label) +
        theme_void(base_size = 14) +
        theme(
            strip.text = element_text(
                face = "bold",
                margin = margin(b = 5, t = 5)
            ),
            legend.position = "bottom"
        ) +
        labs(
            fill = "Optimal Technology"
            #      title = paste0("Financial Gravity: Evaporation of BECCS - ", region_name)
        )

    if (save_map) {
        ggsave(
            paste0(OUT_DIR, region_name, "_Fig3_Evaporation_Maps.png"),
            plt,
            width = 10,
            height = 7,
            bg = "white",
            dpi = 300
        )
    } else {
        print(plt)
    }
    plt
}

# Figure 4: Capital Lock-Out Wedge
generate_fig4_capital_wedge <- function(dat, region_name, save_map = FALSE) {
    message("Generating Figure 4: Capital Lock-Out Wedge for ", region_name, "...")
    cell_area <- terra::cellSize(dat$template, unit = "km")

    # We loop over discount rates. C price fixed at $150.
    dr_seq <- seq(0, 0.20, by = 0.02)
    results <- list()

    for (dr in dr_seq) {
        message("  Calculating DR: ", dr * 100, "%")
        p <- BiocharAG::default_parameters()
        p$c_price <- 150
        p$discount_rate <- dr
        p$region <- region_name
        p$bc_valuation_method <- "advanced_mechanistic"

        res <- run_scenario(dat$template, dat$layers, p)

        stack_df <- terra::as.data.frame(
            c(dat$layers$biomass_density, res$opt, cell_area),
            na.rm = TRUE
        )
        names(stack_df) <- c("biomass_density", "opt_tech", "area_km2")
        tech_levels <- c("1" = "BES", "2" = "BECCS", "3" = "BEBCS")
        stack_df$tech <- tech_levels[as.character(stack_df$opt_tech)]
        stack_df$cell_biomass <- stack_df$biomass_density * stack_df$area_km2

        agg <- stack_df |>
            group_by(.data$tech) |>
            summarize(
                total_biomass = sum(.data$cell_biomass, na.rm = TRUE),
                .groups = "drop"
            )
        agg$dr <- dr * 100
        results[[length(results) + 1]] <- agg
    }

    df_plot <- bind_rows(results)

    p <- ggplot(
        df_plot,
        aes(
            x = .data$dr,
            y = .data$total_biomass / 1e6,
            fill = .data$tech
        )
    ) +
        geom_area(alpha = 0.8) +
        scale_fill_manual(
            values = c("BES" = "#1f77b4", "BECCS" = "#d62728", "BEBCS" = "#2ca02c")
        ) +
        theme_minimal(base_size = 14) +
        labs(
            #      title = paste0("Capital Lock-Out Wedge at $150/t CO2 - ", region_name),
            x = "Discount Rate (%)",
            y = "Addressable Biomass (Million Mg)",
            fill = "Winning Technology"
        )

    if (save_map) {
        ggsave(
            paste0(OUT_DIR, region_name, "_Fig4_Capital_Wedge.png"),
            p,
            width = 8,
            height = 6,
            bg = "white",
            dpi = 300
        )
    } else {
        print(p)
    }
    p
}

# Figure 5: Carbon Price Threshold Map
generate_fig5_cprice_threshold <- function(dat, region_name, save_map = FALSE) {
    message(
        "Generating Figure 5: Carbon Price Threshold Map for ",
        region_name, "..."
    )

    # Get base NPV (at C=0) and Abatement using linear baseline
    p0 <- BiocharAG::default_parameters()
    p0$region <- region_name
    p0$bc_valuation_method <- "advanced_mechanistic"
    base_res <- get_linear_baseline(dat$template, dat$layers, p0)

    npv0 <- base_res$net
    abate <- base_res$abate

    # Calculate break-even prices
    # P = (NPV0_Base - NPV0_Target) / (Abate_Target - Abate_Base)
    # Threshold to leave BES: minimum C price where BECCS or BEBCS beats BES.

    # To BEBCS
    num_bebcs <- npv0[["BES"]] - npv0[["BEBCS"]]
    den_bebcs <- abate[["BEBCS"]] - abate[["BES"]]
    p_bebcs <- num_bebcs / den_bebcs
    p_bebcs[den_bebcs <= 0] <- Inf # If abatement isn't higher, it won't win
    # If it's negative, it already wins at $0 (unlikely for CDR vs BES)
    p_bebcs[p_bebcs < 0] <- Inf

    # To BECCS
    num_beccs <- npv0[["BES"]] - npv0[["BECCS"]]
    den_beccs <- abate[["BECCS"]] - abate[["BES"]]
    p_beccs <- num_beccs / den_beccs
    p_beccs[den_beccs <= 0] <- Inf
    p_beccs[p_beccs < 0] <- Inf

    # Min Threshold to leave BES
    min_p <- min(c(p_bebcs, p_beccs), na.rm = TRUE)
    min_p[min_p > 500] <- NA # Cap for plotting

    df_map <- terra::as.data.frame(min_p, xy = TRUE, na.rm = TRUE)
    names(df_map)[3] <- "threshold"

    p <- ggplot() +
        geom_tile(
            data = df_map,
            aes(x = .data$x, y = .data$y, fill = .data$threshold)
        )
    if (!is.null(dat$admin0)) {
        p <- p + geom_sf(
            data = dat$admin0,
            fill = NA,
            color = "black",
            linewidth = 0.5
        )
    }
    if (!is.null(dat$admin1)) {
        p <- p + geom_sf(
            data = dat$admin1,
            fill = NA,
            color = "black",
            linetype = "dotted",
            linewidth = 0.2
        )
    }
    p <- p +
        coord_sf(crs = 4326) +
        scale_fill_viridis_c(
            option = "magma",
            direction = -1,
            limits = c(0, 300),
            oob = scales::squish
        ) +
        theme_void(base_size = 14) +
        theme(legend.position = "bottom") +
        labs(
            #      title = paste0("Activation Threshold Map - ", region_name),
            subtitle = "Minimum Carbon Price ($/t) to transition from BES to CDR",
            fill = "$/t CO2"
        )

    if (save_map) {
        ggsave(
            paste0(OUT_DIR, region_name, "_Fig5_Threshold_Map.png"),
            p,
            width = 8,
            height = 6,
            bg = "white",
            dpi = 300
        )
    } else {
        print(p)
    }
    p
}

# Figure 6: Fractured Regional MACC
generate_fig6_macc <- function(dat, region_name, save_map = FALSE) {
    message("Generating Figure 6: Fractured Regional MACC for ", region_name, "...")
    cell_area <- terra::cellSize(dat$template, unit = "km")

    p0 <- BiocharAG::default_parameters()
    p0$region <- region_name
    p0$bc_valuation_method <- "advanced_mechanistic"
    base_res <- get_linear_baseline(dat$template, dat$layers, p0)

    npv0 <- base_res$net
    abate <- base_res$abate

    stack_df <- terra::as.data.frame(
        c(dat$layers$biomass_density, cell_area, npv0, abate),
        xy = TRUE,
        na.rm = TRUE
    )
    # Names: biomass, area, BES_n, BECCS_n, BEBCS_n, BES_a, BECCS_a, BEBCS_a
    names(stack_df)[3:10] <- c(
        "biomass", "area", "NPV0_BES", "NPV0_BECCS", "NPV0_BEBCS",
        "A_BES", "A_BECCS", "A_BEBCS"
    )

    stack_df$cell_bm <- stack_df$biomass * stack_df$area

    # Sweep C price from 0 to 250
    c_prices <- seq(0, 250, by = 2)
    macc_points <- list()

    # We will compute the winning tech for every cell at every C price step
    # and then identify transitions to calculate exact marginal steps.
    # A vectorized approach over prices:

    message("  Sweeping C Prices for MACC...")
    # Initialize state at C=0
    val0_bes <- stack_df$NPV0_BES
    val0_beccs <- stack_df$NPV0_BECCS
    val0_bebcs <- stack_df$NPV0_BEBCS

    vals0 <- cbind(val0_bes, val0_beccs, val0_bebcs)
    best_idx0 <- max.col(vals0, ties.method = "first")
    current_tech <- c("BES", "BECCS", "BEBCS")[best_idx0]

    for (cp in c_prices) {
        # Calculate NPVs at this C price
        val_bes <- stack_df$NPV0_BES + cp * stack_df$A_BES
        val_beccs <- stack_df$NPV0_BECCS + cp * stack_df$A_BECCS
        val_bebcs <- stack_df$NPV0_BEBCS + cp * stack_df$A_BEBCS

        # Matrix of values
        vals <- cbind(val_bes, val_beccs, val_bebcs)
        best_idx <- max.col(vals, ties.method = "first")
        techs <- c("BES", "BECCS", "BEBCS")
        new_tech <- techs[best_idx]

        # Find cells that transitioned
        switched <- which(new_tech != current_tech)

        for (idx in switched) {
            old_t <- current_tech[idx]
            new_t <- new_tech[idx]

            # Marginal abatement = A_new - A_old
            a_old <- stack_df[[paste0("A_", old_t)]][idx]
            a_new <- stack_df[[paste0("A_", new_t)]][idx]
            marg_abate_rate <- a_new - a_old

            # Only record if it actually increases abatement
            if (marg_abate_rate > 0) {
                marg_abate_total <- marg_abate_rate * stack_df$cell_bm[idx]

                macc_points[[length(macc_points) + 1]] <- data.frame(
                    price = cp,
                    abatement = marg_abate_total,
                    transition = paste0(old_t, "->", new_t)
                )
            }
        }

        current_tech <- new_tech
    }

    if (length(macc_points) > 0) {
        macc_df <- bind_rows(macc_points)

        # Aggregate by price
        agg_macc <- macc_df |>
            group_by(.data$price) |>
            summarize(
                marginal_abatement = sum(.data$abatement, na.rm = TRUE),
                .groups = "drop"
            ) |>
            arrange(.data$price) |>
            mutate(cumulative_abatement = cumsum(.data$marginal_abatement))

        p <- ggplot(
            agg_macc,
            aes(x = .data$cumulative_abatement / 1e6, y = .data$price)
        ) +
            geom_step(direction = "vh", color = "darkblue", linewidth = 1) +
            theme_minimal(base_size = 14) +
            labs(
                # title = paste0("Marginal Abatement Cost Curve - ", region_name),
                x = "Cumulative Marginal Abatement (Million tCO2e)",
                y = "Carbon Price ($/t)"
            )

        if (save_map) {
            ggsave(
                paste0(OUT_DIR, region_name, "_Fig6_MACC.png"),
                p,
                width = 8,
                height = 6,
                bg = "white",
                dpi = 300
            )
        } else {
            print(p)
        }
        p
    } else {
        message("No positive abatement transitions found!")
        NULL
    }
}

# Figure 7: Agronomic Bridge
generate_fig7_agronomic_bridge <- function(dat, region_name, save_map = FALSE) {
    message("Generating Figure 7: Agronomic Bridge for ", region_name, "...")

    # 1. With Ag Value
    p_ag <- BiocharAG::default_parameters()
    p_ag$c_price <- 30
    p_ag$region <- region_name
    p_ag$bc_valuation_method <- "advanced_mechanistic"

    res_ag <- run_scenario(dat$template, dat$layers, p_ag)

    # 2. Without Ag Value
    p_no <- BiocharAG::default_parameters()
    p_no$c_price <- 30
    p_no$region <- region_name
    p_no$bc_valuation_method <- "ag_value"
    p_no$bc_ag_value <- 0

    res_no <- run_scenario(dat$template, dat$layers, p_no)

    stack_df <- terra::as.data.frame(
        c(res_no$opt, res_ag$opt),
        xy = TRUE,
        na.rm = TRUE
    )
    names(stack_df)[3:4] <- c("opt_no", "opt_ag")

    tech_levels <- c("1" = "BES", "2" = "BECCS", "3" = "BEBCS")
    stack_df$tech_no <- tech_levels[as.character(stack_df$opt_no)]
    stack_df$tech_ag <- tech_levels[as.character(stack_df$opt_ag)]

    # Classify changes
    stack_df$status <- paste0(stack_df$tech_no, " (Baseline)")
    switched_mask <- stack_df$tech_no != stack_df$tech_ag
    stack_df$status[switched_mask] <- paste0(
        "Switched to ",
        stack_df$tech_ag[switched_mask]
    )

    color_map <- c(
        "BES (Baseline)" = "#aec7e8", # Faded blue
        "BECCS (Baseline)" = "#ff9896", # Faded red
        "BEBCS (Baseline)" = "#98df8a", # Faded green
        "Switched to BEBCS" = "#2ca02c", # Bold green
        "Switched to BECCS" = "#d62728", # Bold red
        "Switched to BES" = "#1f77b4" # Bold blue
    )

    p <- ggplot() +
        geom_tile(
            data = stack_df,
            aes(x = .data$x, y = .data$y, fill = .data$status)
        )
    if (!is.null(dat$admin0)) {
        p <- p + geom_sf(
            data = dat$admin0,
            fill = NA,
            color = "black",
            linewidth = 0.5
        )
    }
    if (!is.null(dat$admin1)) {
        p <- p + geom_sf(
            data = dat$admin1,
            fill = NA,
            color = "black",
            linetype = "dotted",
            linewidth = 0.2
        )
    }
    p <- p +
        coord_sf(crs = 4326) +
        scale_fill_manual(values = color_map) +
        theme_void(base_size = 14) +
        labs(
            #      title = paste0("The Agronomic Bridge at $30/t CO2 - ", region_name),
            #      subtitle = paste0(
            #        "Difference in optimal tech with vs without ",
            #        "Mechanistic Biochar Ag Value"
            #      ),
            fill = "Impact"
        )

    if (save_map) {
        ggsave(
            paste0(OUT_DIR, region_name, "_Fig7_Agronomic_Bridge.png"),
            p,
            width = 8,
            height = 6,
            bg = "white",
            dpi = 300
        )
    } else {
        print(p)
    }
    p
}

# --- Execution block ---
if (sys.nframe() == 0) {
    dir.create(OUT_DIR, showWarnings = FALSE)
    regions <- c("US", "China", "Europe", "India")

    for (r in regions) {
        message("\n==========================================")
        message("Processing Region: ", r)
        message("==========================================\n")

        dat <- load_region_data(r)

        save_map <- TRUE

        #        generate_fig1_phys_boundary(dat, r, save_map)
        #        generate_fig2_booster_penalty(dat, r, save_map)
        generate_fig3_evaporation(dat, r, save_map)
        #        generate_fig4_capital_wedge(dat, r, save_map)
        #        generate_fig5_cprice_threshold(dat, r, save_map)
        #        generate_fig6_macc(dat, r, save_map)
        #        generate_fig7_agronomic_bridge(dat, r, save_map)
    }
    message("All figures generated successfully for all regions.")
}
