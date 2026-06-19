# nolint start: indentation_linter, line_length_linter, object_usage_linter, commented_code_linter
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
# Global Tech Colors
TECH_COLORS <- c(
    "BES" = "#1f77b4", # Blue
    "BECCS" = "#d62728", # Red
    "BEBCS" = "#2ca02c" # Green
)

# Figure Output Directory
out_dir <- if (dir.exists("figures")) "figures/" else if (dir.exists("../figures")) "../figures/" else "figures/"

# --- HELPER FUNCTIONS ---

ggsave_with_params <- function(filename, plot, width, height, bg = "white", dpi = 300, params = NULL) {
    if (!is.null(params)) {
        defaults <- BiocharAG::default_parameters()
        diffs <- character()

        for (nm in names(params)) {
            if (nm %in% c("region")) next # Already part of base filename

            val_def <- defaults[[nm]]
            val_p <- params[[nm]]

            if (!is.null(val_p) && (is.null(val_def) || !identical(val_p, val_def))) {
                parts <- strsplit(nm, "_")[[1]]
                abbr <- paste0(substr(parts, 1, 1), collapse = "")

                val_str <- as.character(val_p)
                if (is.character(val_p)) {
                    v_parts <- strsplit(val_str, "_")[[1]]
                    if (length(v_parts) > 1) {
                        val_str <- paste0(substr(v_parts, 1, 1), collapse = "")
                    }
                }
                diffs <- c(diffs, paste0(abbr, val_str))
            }
        }

        if (length(diffs) > 0) {
            ext_idx <- regexpr("\\.[^\\.]*$", filename)
            if (ext_idx > 0) {
                base_name <- substr(filename, 1, ext_idx - 1)
                ext <- substr(filename, ext_idx, nchar(filename))
                filename <- paste0(base_name, "_", paste(diffs, collapse = "_"), ext)
            } else {
                filename <- paste0(filename, "_", paste(diffs, collapse = "_"))
            }
        }
    }

    ggplot2::ggsave(filename = filename, plot = plot, width = width, height = height, bg = bg, dpi = dpi)
}

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
    ds <- terra::rast(file.path(gis_path, paste0(p_dist, "_dist_sink.tif")))
    dss <- terra::rast(file.path(gis_path, paste0(p_dist, "_dist_sink_saline.tif")))
    stype <- terra::rast(file.path(gis_path, paste0(p_dist, "_sink_type.tif")))
    ph <- terra::rast(file.path(gis_path, paste0(p_base, "_soil_ph.tif")))
    cec <- terra::rast(file.path(gis_path, paste0(p_base, "_soil_cec.tif")))

    ci_path <- file.path(gis_path, paste0(p_base, "_ff_c_intensity.tif"))
    ci <- if (file.exists(ci_path)) terra::rast(ci_path) else NULL

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
        dist_sink_km = ds,
        dist_sink_saline_km = dss,
        sink_is_offshore = stype,
        soil_ph = ph,
        soil_cec = cec
    )

    if (!is.null(ci)) {
        layers$ff_c_intensity <- ci
    }

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
        fun = BiocharAG::calculate_bes
    )
    beccs <- BiocharAG::run_spatial_tea(
        template, params, layers,
        fun = BiocharAG::calculate_beccs
    )
    bebcs <- BiocharAG::run_spatial_tea(
        template, params, layers,
        fun = BiocharAG::calculate_bebcs
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
generate_fig1_phys_boundary <- function(dat, region_name, save_map = FALSE,
                                        params = BiocharAG::default_parameters()) {
    message("Generating Figure 1: Physical Boundary for ", region_name, "...")
    params$region <- region_name
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
        ggsave_with_params(
            paste0(out_dir, region_name, "_Fig1_Physical_Boundary.png"),
            p,
            params = params,
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
generate_fig2_booster_penalty <- function(dat, region_name, save_map = FALSE,
                                          params = BiocharAG::default_parameters()) {
    message("Generating Figure 2: Booster Penalty CDF for ", region_name, "...")
    params$region <- region_name

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
        ggsave_with_params(
            paste0(out_dir, region_name, "_Fig2_Booster_Penalty_CDF.png"),
            p,
            params = params,
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
generate_fig3_evaporation <- function(
  dat, region_name, save_map = FALSE,
  d_rates = c(0.02, 0.08, 0.15), c_prices = c(30, 100, 150),
  params = BiocharAG::default_parameters()
) {
    message("Generating Figure 3: Evaporation Maps for ", region_name, "...")
    params$region <- region_name
    all_df <- data.frame()
    for (cp in c_prices) {
        for (dr in d_rates) {
            message("  Running DR: ", dr * 100, "%, C Price: $", cp)
            params$c_price <- cp
            params$discount_rate <- dr
            params$bc_valuation_method <- "advanced_mechanistic"

            res <- run_scenario(dat$template, dat$layers, params)

            opt_raster <- res$opt
            if (!is.null(dat$admin0)) {
                opt_raster <- terra::mask(opt_raster, terra::vect(dat$admin0))
            }
            df <- terra::as.data.frame(opt_raster, xy = TRUE, na.rm = TRUE)
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
    all_df$cp_label <- factor(
        all_df$cp_label,
        levels = paste0("Carbon Price: $", sort(unique(c_prices)), "/t")
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
        ggsave_with_params(
            paste0(out_dir, region_name, "_Fig3_Evaporation_Maps.png"),
            plt,
            params = params,
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
generate_fig4_capital_wedge <- function(dat, region_name, save_map = FALSE,
                                        params = BiocharAG::default_parameters()) {
    message("Generating Figure 4: Capital Lock-Out Wedge for ", region_name, "...")
    cell_area <- terra::cellSize(dat$template, unit = "km")

    # We loop over discount rates. C price fixed.
    dr_seq <- seq(0, 0.20, by = 0.02)
    results <- list()
    params$region <- region_name
    for (dr in dr_seq) {
        message("  Calculating DR: ", dr * 100, "%")
        params$discount_rate <- dr
        res <- run_scenario(dat$template, dat$layers, params)
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
        ggsave_with_params(
            paste0(out_dir, region_name, "_Fig4_Capital_Wedge.png"),
            p,
            params = params,
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
generate_fig5_cprice_threshold <- function(dat, region_name, save_map = FALSE,
                                           params = BiocharAG::default_parameters()) {
    message(
        "Generating Figure 5: Carbon Price Threshold Map for ",
        region_name, "..."
    )

    # Get base NPV (at C=0) and Abatement using linear baseline
    params$region <- region_name
    base_res <- get_linear_baseline(dat$template, dat$layers, params)

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

    if (!is.null(dat$admin0)) {
        min_p <- terra::mask(min_p, terra::vect(dat$admin0))
    }
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
        ggsave_with_params(
            paste0(out_dir, region_name, "_Fig5_Threshold_Map.png"),
            p,
            params = params,
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
generate_fig6_macc <- function(dat, region_name, save_map = FALSE,
                               params = BiocharAG::default_parameters()) {
    message("Generating Figure 6: Fractured Regional MACC for ", region_name, "...")
    cell_area <- terra::cellSize(dat$template, unit = "km")

    params$region <- region_name
    base_res <- get_linear_baseline(dat$template, dat$layers, params)

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

    # Sweep C price from -50 to 250
    c_prices <- seq(-50, 250, by = 1)
    results <- list()

    message("  Sweeping C Prices for MACC...")

    # Extract columns to vectors for faster vectorized operations
    npv0_bes <- stack_df$NPV0_BES
    npv0_beccs <- stack_df$NPV0_BECCS
    npv0_bebcs <- stack_df$NPV0_BEBCS

    a_bes <- stack_df$A_BES
    a_beccs <- stack_df$A_BECCS
    a_bebcs <- stack_df$A_BEBCS

    # Abatement amounts pre-multiplied by cell biomass
    total_a_bes <- a_bes * stack_df$cell_bm
    total_a_beccs <- a_beccs * stack_df$cell_bm
    total_a_bebcs <- a_bebcs * stack_df$cell_bm

    for (cp in c_prices) {
        val_bes <- npv0_bes + cp * a_bes
        val_beccs <- npv0_beccs + cp * a_beccs
        val_bebcs <- npv0_bebcs + cp * a_bebcs

        # Max NPV across the 3 techs
        max_val <- pmax(val_bes, val_beccs, val_bebcs, na.rm = TRUE)

        # A pixel is adopted if max_val >= 0
        adopted <- !is.na(max_val) & (max_val >= 0)

        # Which tech is the max? Ties handled sequentially.
        is_bes <- adopted & (max_val == val_bes)
        is_beccs <- adopted & (!is_bes) & (max_val == val_beccs)
        is_bebcs <- adopted & (!is_bes) & (!is_beccs) & (max_val == val_bebcs)

        # Sum total abatement for the adopted pixels of each tech
        sum_bes <- sum(total_a_bes[is_bes], na.rm = TRUE)
        sum_beccs <- sum(total_a_beccs[is_beccs], na.rm = TRUE)
        sum_bebcs <- sum(total_a_bebcs[is_bebcs], na.rm = TRUE)

        results[[length(results) + 1]] <- data.frame(
            Price = cp,
            BES = sum_bes,
            BECCS = sum_beccs,
            BEBCS = sum_bebcs
        )
    }

    macc_df <- dplyr::bind_rows(results)

    # Pivot to long format for stacked area plot
    macc_long <- tidyr::pivot_longer(macc_df,
        cols = c("BES", "BECCS", "BEBCS"),
        names_to = "Technology", values_to = "Abatement"
    )

    # Convert Abatement to Million tCO2e
    macc_long$Abatement <- macc_long$Abatement / 1e6

    # Factor levels to control stacking order
    macc_long$Technology <- factor(macc_long$Technology, levels = c("BECCS", "BEBCS", "BES"))

    if (sum(macc_long$Abatement, na.rm = TRUE) > 0) {
        p <- ggplot(macc_long, aes(x = Price, y = Abatement, fill = Technology)) +
            geom_area(alpha = 0.9, color = "black", linewidth = 0.2) +
            scale_fill_manual(values = TECH_COLORS) +
            theme_minimal(base_size = 14) +
            labs(
                x = "Carbon Price ($/t)",
                y = "Total Annual Abatement Potential (Million tCO2e/yr)"
            ) +
            theme(
                legend.position = "bottom",
                legend.title = element_blank()
            )

        if (save_map) {
            ggsave_with_params(
                paste0(out_dir, region_name, "_Fig6_MACC.png"),
                p,
                params = params,
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
generate_fig7_agronomic_bridge <- function(dat, region_name, save_map = FALSE,
                                           params = BiocharAG::default_parameters(),
                                           c_price = 30) {
    message("Generating Figure 7: Agronomic Bridge for ", region_name, "...")

    # 1. With Ag Value
    params$c_price <- c_price
    params$region <- region_name
    res_ag <- run_scenario(dat$template, dat$layers, params)

    # 2. Without Ag Value
    params$bc_valuation_method <- "ag_value"
    params$bc_ag_value <- 0
    res_no <- run_scenario(dat$template, dat$layers, params)

    opt_stack <- c(res_no$opt, res_ag$opt)
    if (!is.null(dat$admin0)) {
        opt_stack <- terra::mask(opt_stack, terra::vect(dat$admin0))
    }
    stack_df <- terra::as.data.frame(
        opt_stack,
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
        "Switched to BEBCS" = unname(TECH_COLORS["BEBCS"]),
        "Switched to BECCS" = unname(TECH_COLORS["BECCS"]),
        "Switched to BES" = unname(TECH_COLORS["BES"])
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
        ggsave_with_params(
            paste0(out_dir, region_name, "_Fig7_Agronomic_Bridge.png"),
            p,
            params = params,
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

# Figure 9: Optimal Scale per Tech Map
generate_fig9_optimal_scale_map <- function(dat, region_name, save_map = FALSE,
                                            params = BiocharAG::default_parameters()) {
    message("Generating Figure 9: Optimal Scale Map for ", region_name, "...")
    params$region <- region_name

    # Run for each tech with optimize_scale = TRUE
    params$optimize_scale <- TRUE

    res_bes <- BiocharAG::run_spatial_tea(
        dat$template, params, dat$layers,
        fun = BiocharAG::calculate_bes
    )
    res_beccs <- BiocharAG::run_spatial_tea(
        dat$template, params, dat$layers,
        fun = BiocharAG::calculate_beccs
    )
    res_bebcs <- BiocharAG::run_spatial_tea(
        dat$template, params, dat$layers,
        fun = BiocharAG::calculate_bebcs
    )

    # Extract Optimal_Plant_MW_th layer
    sz_bes <- res_bes[["Optimal_Plant_MW_th"]]
    sz_beccs <- res_beccs[["Optimal_Plant_MW_th"]]
    sz_bebcs <- res_bebcs[["Optimal_Plant_MW_th"]]

    # Combine into a stack
    stack_r <- c(sz_bes, sz_beccs, sz_bebcs)
    names(stack_r) <- c("BES", "BECCS", "BEBCS")

    # Apply admin0 mask if available
    if (!is.null(dat$admin0)) {
        stack_r <- terra::mask(stack_r, terra::vect(dat$admin0))
    }

    # Convert to dataframe
    df <- terra::as.data.frame(stack_r, xy = TRUE, na.rm = TRUE)
    df_long <- tidyr::pivot_longer(df, cols = c("BES", "BECCS", "BEBCS"), names_to = "Technology", values_to = "Optimal_Size_MWth")

    # Ensure Optimal_Size_MWth is treated as a factor for discrete colors
    df_long$Optimal_Size_MWth <- factor(df_long$Optimal_Size_MWth, levels = c(5, 25, 50, 100, 250, 500))

    # Plot
    p <- ggplot(df_long, aes(x = x, y = y, fill = Optimal_Size_MWth)) +
        geom_tile() +
        facet_wrap(~Technology, ncol = 3) +
        scale_fill_viridis_d(option = "plasma", drop = FALSE) +
        theme_minimal(base_size = 14) +
        coord_fixed() +
        labs(
            x = "", y = "",
            fill = "Optimal Size (MWth)"
        ) +
        theme(
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            strip.text = element_text(face = "bold", size = 16)
        )

    if (save_map) {
        ggsave_with_params(
            paste0(out_dir, region_name, "_Fig9_Optimal_Scale_Map.png"),
            p,
            params = params,
            width = 12,
            height = 5,
            bg = "white",
            dpi = 300
        )
    } else {
        print(p)
    }
    p
}

# Figure 8: Global Break-Even Carbon Price Grid
generate_fig8_breakeven_cprice <- function(save_map = FALSE,
                                           params = BiocharAG::default_parameters()) {
    message("Generating Figure 8: Break-Even Carbon Price Grid...")

    # Ordered regions for columns
    regions_ordered <- c("India", "China", "US", "Europe")

    # Rows definitions
    techs <- c("BES", "BECCS", "BEBCS", "Best_Tech", "Best_C")
    row_labels <- c(
        "BES" = "Bioenergy", "BECCS" = "BECCS", "BEBCS" = "Biochar",
        "Best_Tech" = "Best Tech.", "Best_C" = "Best C Price"
    )

    df_list <- list()
    admin_list <- list()

    for (r in regions_ordered) {
        message("  Processing Region for Fig 8: ", r)
        dat <- load_region_data(r)

        # Prepare parameters
        params$region <- r

        # Get baseline NPV(0) and Abatement
        base_res <- get_linear_baseline(dat$template, dat$layers, params)

        bes_npv <- base_res$net[["BES"]]
        beccs_npv <- base_res$net[["BECCS"]]
        bebcs_npv <- base_res$net[["BEBCS"]]

        bes_abt <- base_res$abate[["BES"]]
        beccs_abt <- base_res$abate[["BECCS"]]
        bebcs_abt <- base_res$abate[["BEBCS"]]

        calc_breakeven <- function(npv, abt) {
            c_req <- -npv / abt
            # Pixels physically impossible or strictly unprofitable
            c_req <- terra::ifel(abt <= 0, NA, c_req)
            return(c_req)
        }

        bes_c <- calc_breakeven(bes_npv, bes_abt)
        beccs_c <- calc_breakeven(beccs_npv, beccs_abt)
        bebcs_c <- calc_breakeven(bebcs_npv, bebcs_abt)

        c_stack <- c(bes_c, beccs_c, bebcs_c)
        names(c_stack) <- c("BES", "BECCS", "BEBCS")

        # Find minimum break-even price across the 3 techs
        best_c <- min(c_stack, na.rm = TRUE)
        names(best_c) <- "Best_C"

        # Find which tech has that minimum
        best_idx <- terra::which.min(c_stack)
        names(best_idx) <- "Best_Tech"

        full_stack <- c(c_stack, best_c, best_idx)

        if (!is.null(dat$admin0)) {
            full_stack <- terra::mask(full_stack, terra::vect(dat$admin0))
            # Save admin boundaries for plotting
            admin_r <- dat$admin0
            admin_r$Region <- r
            admin_list[[r]] <- admin_r
        }

        # Convert to dataframe (keep NAs initially to allow independent NA patterns per tech)
        df_r <- terra::as.data.frame(full_stack, xy = TRUE, na.rm = FALSE)
        df_r <- df_r[!is.na(df_r$BES) | !is.na(df_r$BECCS) | !is.na(df_r$BEBCS), ]

        # Map integer best_tech back to strings
        tech_names <- c("BES", "BECCS", "BEBCS")
        if ("Best_Tech" %in% names(df_r)) {
            df_r$Best_Tech <- factor(tech_names[df_r$Best_Tech], levels = tech_names)
        }

        # Pivot numeric columns
        df_num <- tidyr::pivot_longer(df_r,
            cols = c("BES", "BECCS", "BEBCS", "Best_C"),
            names_to = "Technology", values_to = "Breakeven_C",
            values_drop_na = TRUE
        )
        df_num$Tech_Factor <- factor(NA, levels = tech_names)

        # Format categorical column
        if ("Best_Tech" %in% names(df_r)) {
            df_cat <- df_r[!is.na(df_r$Best_Tech), c("x", "y", "Best_Tech")]
            df_cat$Technology <- "Best_Tech"
            names(df_cat)[names(df_cat) == "Best_Tech"] <- "Tech_Factor"
            df_cat$Breakeven_C <- NA_real_

            df_long <- rbind(
                as.data.frame(df_num[, c("x", "y", "Technology", "Breakeven_C", "Tech_Factor")]),
                as.data.frame(df_cat[, c("x", "y", "Technology", "Breakeven_C", "Tech_Factor")])
            )
        } else {
            df_long <- as.data.frame(df_num[, c("x", "y", "Technology", "Breakeven_C", "Tech_Factor")])
        }

        df_long$Region <- r
        df_list[[r]] <- df_long
    }

    message("  Combining data and rendering plot...")

    # Combine all regions
    df_all <- dplyr::bind_rows(df_list)

    # Fix factor levels for desired ordering
    df_all$Region <- factor(df_all$Region, levels = regions_ordered)
    df_all$Technology <- factor(df_all$Technology, levels = techs)

    if (length(admin_list) > 0) {
        admin_all <- do.call(rbind, lapply(admin_list, function(x) x[, "Region", drop = FALSE]))
        admin_all$Region <- factor(admin_all$Region, levels = regions_ordered)
    } else {
        admin_all <- NULL
    }

    # Plotting using patchwork to avoid coord_sf() free scaling issues
    library(patchwork)
    plot_list <- list()

    # Define fixed limits for the color scale
    scale_limits <- c(-50, 200)

    for (t in techs) {
        for (r in regions_ordered) {
            sub_df <- df_all[df_all$Technology == t & df_all$Region == r, ]
            sub_admin <- if (!is.null(admin_all)) admin_all[admin_all$Region == r, ] else NULL

            if (r == regions_ordered[length(regions_ordered)]) {
                sub_df$RowLabel <- row_labels[t]
            }

            p <- ggplot()

            # Map fills depending on row type
            if (t == "Best_Tech") {
                p <- p + geom_tile(data = sub_df[!is.na(sub_df$Tech_Factor), ], aes(x = x, y = y, fill = Tech_Factor))
            } else {
                p <- p + geom_tile(data = sub_df, aes(x = x, y = y, fill = Breakeven_C))
            }

            if (!is.null(sub_admin)) {
                p <- p + geom_sf(data = sub_admin, fill = NA, color = "black", linewidth = 0.2)
            }

            p <- p + coord_sf(crs = 4326) + theme_void(base_size = 10) +
                theme(legend.position = "none")

            # Scales
            if (t == "Best_Tech") {
                p <- p + scale_fill_manual(
                    values = TECH_COLORS,
                    limits = c("BES", "BECCS", "BEBCS"),
                    na.translate = FALSE,
                    drop = FALSE
                )
            } else {
                p <- p + scale_fill_gradientn(
                    colors = c("#00008B", "#006400", "#FFD700", "#FF8C00", "#8B0000"),
                    na.value = "transparent",
                    limits = scale_limits,
                    oob = scales::squish
                )
            }

            # --- Layout Adjustments ---
            theme_adj <- theme()

            # Top Headers (Region Names)
            if (t == techs[1]) {
                p <- p + ggtitle(r)
                theme_adj <- theme_adj + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
            }

            # Right Headers (Technology Names)
            if (r == regions_ordered[length(regions_ordered)]) {
                # Use a facet strip to place the label on the right side, as theme_void drops axis titles
                p <- p + facet_grid(RowLabel ~ .)
                theme_adj <- theme_adj + theme(
                    strip.text.y = element_text(angle = -90, face = "bold", size = 12, margin = margin(l = 10)),
                    strip.background = element_blank()
                )
            }

            p <- p + theme_adj
            plot_list[[paste(t, r, sep = "_")]] <- p
        }
    }

    # Render with patchwork
    n_regions <- length(regions_ordered)
    main_plot <- patchwork::wrap_plots(plot_list, ncol = n_regions)

    # Generate isolated legends using cowplot
    p_leg_cat <- ggplot(data.frame(x = 1, y = 1, Tech = factor(c("BES", "BECCS", "BEBCS"), levels = c("BES", "BECCS", "BEBCS"))), aes(x, y, fill = Tech)) +
        geom_tile() +
        scale_fill_manual(values = TECH_COLORS, name = "Optimal\nTechnology") +
        theme_void() +
        theme(legend.position = "bottom", legend.title = element_text(vjust = 0.8), legend.margin = margin(t = 0, b = 0))

    p_leg_cont <- ggplot(data.frame(x = 1, y = 1, z = c(-50, 200)), aes(x, y, fill = z)) +
        geom_tile() +
        scale_fill_gradientn(
            colors = c("#00008B", "#006400", "#FFD700", "#FF8C00", "#8B0000"),
            limits = scale_limits,
            oob = scales::squish,
            breaks = c(-50, 0, 50, 100, 150, 200),
            labels = c("\u2264 -50", "0", "50", "100", "150", "\u2265 200"),
            name = "Break-Even C-Price\n($/tCO2e)"
        ) +
        theme_void() +
        theme(legend.position = "bottom", legend.key.width = unit(1, "cm"), legend.title = element_text(vjust = 0.8), legend.margin = margin(t = 0, b = 0))

    leg_cat <- cowplot::get_legend(p_leg_cat)
    leg_cont <- cowplot::get_legend(p_leg_cont)

    combined_legends <- cowplot::plot_grid(leg_cont, leg_cat, nrow = 1, rel_widths = c(1.5, 1)) + theme(margin = margin(t = -1))

    combined_plot <- patchwork::wrap_elements(main_plot) / patchwork::wrap_elements(combined_legends) +
        patchwork::plot_layout(heights = c(1, 0.04))

    if (save_map) {
        ggsave_with_params(
            paste0(out_dir, "Global_Fig8_Breakeven_CPrice.png"),
            combined_plot,
            params = params,
            width = 8,
            height = 9,
            bg = "white",
            dpi = 300
        )
        message("Saved: Global_Fig8_Breakeven_CPrice.png")
    } else {
        print(combined_plot)
    }
    return(combined_plot)
}

# --- Execution block ---
if (sys.nframe() == 0) {
    params <- BiocharAG::default_parameters()
    params$c_price <- 100
    params$bc_valuation_method <- "advanced_mechanistic"
    params$plant_mw_th <- c(BES = 250, BECCS = 250, BEBCS = 250)
    dir.create(out_dir, showWarnings = FALSE)
    regions <- c("US", "China", "Europe", "India")

    for (r in regions) {
        message("\n==========================================")
        message("Processing Region: ", r)
        message("==========================================\n")
        dat <- load_region_data(r)
        save_map <- TRUE
        generate_fig1_phys_boundary(dat, r, save_map, params = params)
        generate_fig2_booster_penalty(dat, r, save_map, params = params)
        generate_fig3_evaporation(dat, r, save_map, params = params)
        generate_fig4_capital_wedge(dat, r, save_map, params = params)
        generate_fig5_cprice_threshold(dat, r, save_map, params = params)
        generate_fig6_macc(dat, r, save_map, params = params)
        generate_fig7_agronomic_bridge(dat, r, save_map, params = params)
        generate_fig9_optimal_scale_map(dat, r, save_map, params = params)
    }
    generate_fig8_breakeven_cprice(save_map = TRUE, params = params)
    message("All figures generated successfully for all regions.")
}
