### Content of file scripts/diagnostics.R ###
# Diagnostic Script: Diagnose MACC vs. Spatial Map Allocation
source("scripts/manuscript_figures.R")

diagnose_region_allocation <- function(region_name = "China", c_price_test = 100, dr_test = 0.08) {
  dat <- load_region_data(region_name)
  cell_area <- terra::cellSize(dat$template, unit = "km")

  # 1. Parameter Check: ensure mechanistic ag valuation is active
  params <- BiocharAG::set_scenario(BiocharAG::scenarios[["default"]])
  params$region <- region_name
  params$discount_rate <- dr_test
  params$bc_valuation_method <- "advanced_mechanistic"

  base_res <- get_linear_baseline(dat$template, dat$layers, params, vec = dat$vec)

  stack_df <- terra::as.data.frame(
    c(dat$layers$biomass_density, cell_area, base_res$net, base_res$abate),
    na.rm = TRUE
  )
  names(stack_df)[1:8] <- c("bm_density", "area_km2", "NPV0_BES", "NPV0_BECCS", "NPV0_BEBCS", "A_BES", "A_BECCS", "A_BEBCS")
  stack_df$cell_bm <- stack_df$bm_density * stack_df$area_km2

  # Calculate NPV at target test price
  stack_df$val_bes <- stack_df$NPV0_BES + c_price_test * stack_df$A_BES
  stack_df$val_beccs <- stack_df$NPV0_BECCS + c_price_test * stack_df$A_BECCS
  stack_df$val_bebcs <- stack_df$NPV0_BEBCS + c_price_test * stack_df$A_BEBCS

  # Unconstrained Winner (as in Fig 3)
  stack_df$winner_unconstrained <- apply(stack_df[, c("val_bes", "val_beccs", "val_bebcs")], 1, function(x) c("BES", "BECCS", "BEBCS")[which.max(x)])

  # Constrained Winner with NPV >= 0 (as in Fig 6 MACC)
  stack_df$max_val <- pmax(stack_df$val_bes, stack_df$val_beccs, stack_df$val_bebcs)
  stack_df$winner_macc <- ifelse(stack_df$max_val >= 0, stack_df$winner_unconstrained, "None")

  # 2. Decompose allocations
  cat(sprintf("\n=== DIAGNOSTIC REPORT: %s at $%d/t CO2 (DR = %.1f%%) ===\n", region_name, c_price_test, dr_test * 100))
  cat(sprintf("Cells with NPV >= 0: %.2f%% of total active cells\n\n", 100 * mean(stack_df$winner_macc != "None")))

  summary_table <- data.frame(Technology = c("BES", "BECCS", "BEBCS"))

  # Metric A: Unconstrained Area Share (%)
  summary_table$Unconstrained_Area_Pct <- sapply(summary_table$Technology, function(t) {
    100 * sum(stack_df$area_km2[stack_df$winner_unconstrained == t]) / sum(stack_df$area_km2)
  })

  # Metric B: MACC Viable Area Share (%)
  viable_area <- sum(stack_df$area_km2[stack_df$winner_macc != "None"])
  summary_table$Viable_Area_Pct <- sapply(summary_table$Technology, function(t) {
    if (viable_area == 0) return(0)
    100 * sum(stack_df$area_km2[stack_df$winner_macc == t]) / viable_area
  })

  # Metric C: Biomass Processed Share (%)
  viable_bm <- sum(stack_df$cell_bm[stack_df$winner_macc != "None"])
  summary_table$Biomass_Share_Pct <- sapply(summary_table$Technology, function(t) {
    if (viable_bm == 0) return(0)
    100 * sum(stack_df$cell_bm[stack_df$winner_macc == t]) / viable_bm
  })

  # Metric D: Total Abatement Share (%) as shown on MACC
  stack_df$abatement_delivered <- 0
  stack_df$abatement_delivered[stack_df$winner_macc == "BES"] <-
    stack_df$cell_bm[stack_df$winner_macc == "BES"] * stack_df$A_BES[stack_df$winner_macc == "BES"]
  stack_df$abatement_delivered[stack_df$winner_macc == "BECCS"] <-
    stack_df$cell_bm[stack_df$winner_macc == "BECCS"] * stack_df$A_BECCS[stack_df$winner_macc == "BECCS"]
  stack_df$abatement_delivered[stack_df$winner_macc == "BEBCS"] <-
    stack_df$cell_bm[stack_df$winner_macc == "BEBCS"] * stack_df$A_BEBCS[stack_df$winner_macc == "BEBCS"]

  tot_abatement <- sum(stack_df$abatement_delivered)
  summary_table$MACC_Abatement_Share_Pct <- sapply(summary_table$Technology, function(t) {
    if (tot_abatement == 0) return(0)
    100 * sum(stack_df$abatement_delivered[stack_df$winner_macc == t]) / tot_abatement
  })

  summary_table[, -1] <- round(summary_table[, -1], 2)
  print(summary_table)
}

# Run diagnostics for China and US
diagnose_region_allocation("China", c_price_test = 100, dr_test = 0.08)
diagnose_region_allocation("US", c_price_test = 100, dr_test = 0.08)

### Content of file scripts/factorial_analysis.R ###
# scripts/run_factorial_analysis.R
# Executes a full factorial spatial TEA across 720 scenarios, evaluating technologies competitively.

source("scripts/manuscript_figures.R")

factorial_grid <- expand.grid(
  region = c("US", "China", "Europe", "India"),
  c_price = c(0, 50, 100, 150, 200),
  discount_rate = c(0.02, 0.08, 0.15),
  allow_eor = c(TRUE, FALSE),
  early_adoption = c(TRUE, FALSE),
  plant_mw_th = c(50, 150, 250),
  stringsAsFactors = FALSE # TODO this looks like a mistake.  Why is this here?
)

# For testing, truncate this to head(factorial_grid, 2)
# factorial_grid <- head(factorial_grid, 2)

results_df <- data.frame()
region_cache <- list()

extract_masked_mean <- function(layer, opt_idx, target_i) {
  if (is.null(layer)) {
    return(NA)
  }
  if (!inherits(layer, "SpatRaster")) {
    return(layer)
  }
  masked <- terra::ifel(opt_idx == target_i, layer, NA)
  return(terra::global(masked, fun = "mean", na.rm = TRUE)[[1]])
}

extract_masked_min <- function(layer, opt_idx, target_i) {
  if (is.null(layer)) {
    return(NA)
  }
  if (!inherits(layer, "SpatRaster")) {
    return(layer)
  }
  masked <- terra::ifel(opt_idx == target_i, layer, NA)
  return(terra::global(masked, fun = "min", na.rm = TRUE)[[1]])
}

extract_masked_max <- function(layer, opt_idx, target_i) {
  if (is.null(layer)) {
    return(NA)
  }
  if (!inherits(layer, "SpatRaster")) {
    return(layer)
  }
  masked <- terra::ifel(opt_idx == target_i, layer, NA)
  return(terra::global(masked, fun = "max", na.rm = TRUE)[[1]])
}

message("Starting Factorial Analysis: ", nrow(factorial_grid), " total runs (each evaluating 3 technologies competitively).")

for (i in 1:nrow(factorial_grid)) {
  if (i %% 50 == 0) message(sprintf("Processing run %d of %d...", i, nrow(factorial_grid)))
  row <- factorial_grid[i, ]

  if (is.null(region_cache[[row$region]])) {
    region_cache[[row$region]] <- load_region_data(row$region)
  }
  dat <- region_cache[[row$region]]
  spatial_layers <- dat$layers

  # Base Setup
  p <- BiocharAG::set_scenario()
  p$c_price <- row$c_price
  p$discount_rate <- row$discount_rate
  p$allow_eor <- row$allow_eor
  p$early_adoption <- row$early_adoption
  p$plant_mw_th <- row$plant_mw_th

  if ("soil_temp" %in% names(spatial_layers)) p$soil_temp <- spatial_layers$soil_temp
  if ("elec_price" %in% names(spatial_layers)) {
    factor <- if (!is.null(p$wholesale_discount_factor)) p$wholesale_discount_factor else 0.4
    p$elec_price <- spatial_layers$elec_price * factor
  }
  if ("soil_ph" %in% names(spatial_layers)) p$soil_ph <- spatial_layers$soil_ph
  if ("soil_cec" %in% names(spatial_layers)) p$soil_cec <- spatial_layers$soil_cec
  if ("dist_sink_km" %in% names(spatial_layers)) p$dist_sink_km <- spatial_layers$dist_sink_km
  if ("dist_sink_saline_km" %in% names(spatial_layers)) p$dist_sink_saline_km <- spatial_layers$dist_sink_saline_km
  if ("sink_is_offshore" %in% names(spatial_layers)) p$sink_is_offshore <- spatial_layers$sink_is_offshore
  if ("ff_c_intensity" %in% names(spatial_layers)) p$ff_c_intensity <- spatial_layers$ff_c_intensity

  for (layer_name in c("cn_weather_risk", "cn_expansion_risk", "eu_base_eur", "us_base_cost")) {
    if (layer_name %in% names(spatial_layers)) p[[layer_name]] <- spatial_layers[[layer_name]]
  }

  dist_layer_name <- paste0("dist_", row$plant_mw_th, "MWth")
  if (dist_layer_name %in% names(spatial_layers)) {
    p$avg_dist <- spatial_layers[[dist_layer_name]]
  }

  p$feedstock_cost <- BiocharAG::calculate_regional_feedstock_cost(row$region, p)

  # Execute All 3 Technologies
  res_bes <- BiocharAG::calculate_bes(p)
  res_beccs <- BiocharAG::calculate_beccs(p)
  res_bebcs <- BiocharAG::calculate_bebcs(p)

  # Stack NPVs and find winner
  npv_stack <- c(res_bes$net_value, res_beccs$net_value, res_bebcs$net_value)

  # Ensure we only evaluate areas with biomass
  bm_mask <- spatial_layers$biomass_density > 0
  opt_idx_raw <- terra::which.max(npv_stack)
  opt_idx <- terra::ifel(bm_mask, opt_idx_raw, NA)

  cell_area <- terra::cellSize(spatial_layers$biomass_density, unit = "km")
  biomass_amount <- spatial_layers$biomass_density * cell_area

  techs <- c("BES", "BECCS", "BEBCS")
  res_list <- list(res_bes, res_beccs, res_bebcs)

  for (t_idx in 1:3) {
    t_name <- techs[t_idx]
    tech_res <- res_list[[t_idx]]

    # Area and Biomass Calculations
    is_best <- opt_idx == t_idx
    is_viable <- is_best & (tech_res$net_value > 0)

    area_best_km2 <- terra::global(terra::ifel(is_best, cell_area, NA), fun = "sum", na.rm = TRUE)[[1]]
    area_viable_km2 <- terra::global(terra::ifel(is_viable, cell_area, NA), fun = "sum", na.rm = TRUE)[[1]]
    biomass_processed_yr <- terra::global(terra::ifel(is_viable, biomass_amount, NA), fun = "sum", na.rm = TRUE)[[1]]

    new_row <- data.frame(
      region = row$region,
      c_price = row$c_price,
      discount_rate = row$discount_rate,
      allow_eor = row$allow_eor,
      early_adoption = row$early_adoption,
      plant_mw_th = row$plant_mw_th,
      technology = t_name,
      area_best_km2 = area_best_km2,
      area_viable_km2 = area_viable_km2,
      biomass_processed_yr_mg = biomass_processed_yr,
      npv_min = extract_masked_min(tech_res$net_value, opt_idx, t_idx),
      npv_max = extract_masked_max(tech_res$net_value, opt_idx, t_idx),
      npv_mean = extract_masked_mean(tech_res$net_value, opt_idx, t_idx),
      mean_co2_transport_distance_km = extract_masked_mean(tech_res$co2_transport_distance_km, opt_idx, t_idx),
      mean_biomass_transport_distance_km = extract_masked_mean(tech_res$biomass_transport_distance_km, opt_idx, t_idx),
      mean_capital_cost_mg = extract_masked_mean(tech_res$capital_cost_mg, opt_idx, t_idx),
      mean_om_cost_mg = extract_masked_mean(tech_res$om_cost_mg, opt_idx, t_idx),
      mean_biomass_cost_mg = extract_masked_mean(tech_res$biomass_cost_mg, opt_idx, t_idx),
      mean_co2_transport_cost_mg = extract_masked_mean(tech_res$co2_transport_cost_mg, opt_idx, t_idx),
      mean_net_cdr = extract_masked_mean(tech_res$tot_c_abatement, opt_idx, t_idx),
      mean_carbon_removal_revenue_mg = extract_masked_mean(tech_res$abatement_revenue_mg, opt_idx, t_idx),
      mean_energy_production_mwh = extract_masked_mean(tech_res$energy_prod, opt_idx, t_idx),
      mean_energy_revenue_mg = extract_masked_mean(tech_res$energy_revenue_mg, opt_idx, t_idx),
      mean_agronomic_revenue_mg = extract_masked_mean(tech_res$agronomic_revenue_mg, opt_idx, t_idx),
      mean_lcoe_usd_mwh = extract_masked_mean(tech_res$lcoe, opt_idx, t_idx),
      mean_cost_of_co2_avoided = extract_masked_mean(tech_res$cost_of_co2_avoided, opt_idx, t_idx),
      mean_abatement_efficiency = extract_masked_mean(tech_res$abatement_efficiency, opt_idx, t_idx),
      mean_total_capex_m = extract_masked_mean(tech_res$total_capex_m, opt_idx, t_idx),
      stringsAsFactors = FALSE
    )

    results_df <- rbind(results_df, new_row)
  }
}

dir.create("results", showWarnings = FALSE)
write.csv(results_df, "results/factorial_analysis_results.csv", row.names = FALSE)
message("Factorial Analysis Complete. Results saved to results/factorial_analysis_results.csv")

### Content of file scripts/manuscript_figures_extra.R ###
# nolint start: indentation_linter, line_length_linter, object_usage_linter, commented_code_linter
# manuscript_figures.R
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

ggsave_with_scenario <- function(filename, plot, width, height, bg = "white", dpi = 300, scenario = "default") {
  if (scenario != "default") {
    ext_idx <- regexpr("\\.[^\\.]*$", filename)
    if (ext_idx > 0) {
      base_name <- substr(filename, 1, ext_idx - 1)
      ext <- substr(filename, ext_idx, nchar(filename))
      filename <- paste0(base_name, "_", scenario, ext)
    } else {
      filename <- paste0(filename, "_", scenario)
    }
  }

  ggplot2::ggsave(filename = filename, plot = plot, width = width, height = height, bg = bg, dpi = dpi)
}

# Linear interpolation for fast sweeps
# Net_Value(C) = Net_Value(0) + C * Abatement
get_linear_baseline <- function(template, layers, base_params, vec = NULL) {
  p0 <- base_params
  p0[["c_price"]] <- 0
  res0 <- run_scenario(template, layers, p0, vec = vec)
  res0 # Returns net at C=0, and abatement
}

# --- FIGURE GENERATORS ---

# Figure 1: Scale vs. Sink Bivariate Map
generate_fig1_phys_boundary <- function(dat, region_name, save_map = FALSE,
                                        scenario = "default") {
  params <- set_scenario(scenarios[[scenario]])
  message("Generating Figure 1: Physical Boundary for ", region_name, "...")
  params$region <- region_name
  res <- run_scenario(dat[["template", exact = TRUE]], dat[["layers", exact = TRUE]], params, vec = dat[["vec", exact = TRUE]])

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
    ggsave_with_scenario(
      paste0(out_dir, region_name, "_Fig1_Physical_Boundary.png"),
      p,
      scenario = scenario,
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
                                          scenario = "default") {
  params <- set_scenario(scenarios[[scenario]])
  message("Generating Figure 2: Booster Penalty CDF for ", region_name, "...")
  params$region <- region_name

  res <- run_scenario(dat[["template", exact = TRUE]], dat[["layers", exact = TRUE]], params, vec = dat[["vec", exact = TRUE]])
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
    ggsave_with_scenario(
      paste0(out_dir, region_name, "_Fig2_Booster_Penalty_CDF.png"),
      p,
      scenario = scenario,
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
  scenario = "default",
  metric = c("optimal_tech", "max_npv", "both")
) {
  metric <- match.arg(metric)
  params <- set_scenario(scenarios[[scenario]])
  message("Generating Figure 3: Evaporation Maps for ", region_name, " (Metric: ", metric, ")...")
  params$region <- region_name
  all_df <- data.frame()
  for (cp in c_prices) {
    for (dr in d_rates) {
      message("  Running DR: ", dr * 100, "%, C Price: $", cp)
      params$c_price <- cp
      params$discount_rate <- dr
      params$bc_valuation_method <- "advanced_mechanistic"

      res <- run_scenario(dat[["template", exact = TRUE]], dat[["layers", exact = TRUE]], params, vec = dat[["vec", exact = TRUE]])

      opt_raster <- res$opt
      if (!is.null(res$vec_res)) {
        max_npv_raster <- terra::rast(dat[["template", exact = TRUE]], nlyrs = 1, vals = NA)
        max_npv_raster[dat$vec$active_indices] <- pmax(
          res$vec_res$net[, 1],
          res$vec_res$net[, 2],
          res$vec_res$net[, 3],
          na.rm = TRUE
        )
      } else {
        max_npv_raster <- terra::app(res$net, max, na.rm = TRUE)
      }

      if (!is.null(dat$admin0)) {
        opt_raster <- terra::mask(opt_raster, terra::vect(dat$admin0))
        max_npv_raster <- terra::mask(max_npv_raster, terra::vect(dat$admin0))
      }
      comb_r <- c(opt_raster, max_npv_raster)
      names(comb_r) <- c("opt_tech", "max_npv")
      df <- terra::as.data.frame(comb_r, xy = TRUE, na.rm = TRUE)

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

  build_tech_plot <- function(df_data) {
    plt <- ggplot() +
      geom_tile(data = df_data, aes(x = .data$x, y = .data$y, fill = .data$tech))
    if (!is.null(dat$admin0)) {
      plt <- plt + geom_sf(
        data = dat$admin0,
        fill = NA, color = "black", linewidth = 0.5
      )
    }
    if (!is.null(dat$admin1)) {
      plt <- plt + geom_sf(
        data = dat$admin1,
        fill = NA, color = "black", linetype = "dotted", linewidth = 0.2
      )
    }
    plt +
      coord_sf(crs = 4326) +
      scale_fill_manual(
        values = c("BES" = "#1f77b4", "BECCS" = "#d62728", "BEBCS" = "#2ca02c")
      ) +
      facet_grid(cp_label ~ dr_label) +
      theme_void(base_size = 14) +
      theme(
        strip.text = element_text(face = "bold", margin = margin(b = 5, t = 5)),
        legend.position = "bottom"
      ) +
      labs(fill = "Optimal Technology")
  }

  build_npv_plot <- function(df_data) {
    plt <- ggplot() +
      geom_tile(data = df_data, aes(x = .data$x, y = .data$y, fill = .data$max_npv))
    if (!is.null(dat$admin0)) {
      plt <- plt + geom_sf(
        data = dat$admin0,
        fill = NA, color = "black", linewidth = 0.5
      )
    }
    if (!is.null(dat$admin1)) {
      plt <- plt + geom_sf(
        data = dat$admin1,
        fill = NA, color = "black", linetype = "dotted", linewidth = 0.2
      )
    }
    plt +
      coord_sf(crs = 4326) +
      scale_fill_viridis_c(option = "viridis", name = "Max NPV ($/Mg)") +
      facet_grid(cp_label ~ dr_label) +
      theme_void(base_size = 14) +
      theme(
        strip.text = element_text(face = "bold", margin = margin(b = 5, t = 5)),
        legend.position = "bottom"
      ) +
      labs(fill = "Max NPV ($/Mg)")
  }

  out_plot <- if (metric == "optimal_tech") {
    build_tech_plot(all_df)
  } else if (metric == "max_npv") {
    build_npv_plot(all_df)
  } else {
    # metric == "both"
    patchwork::wrap_plots(
      build_tech_plot(all_df) + labs(title = paste0("Optimal Technology - ", region_name)),
      build_npv_plot(all_df) + labs(title = paste0("Highest NPV - ", region_name)),
      ncol = 2
    )
  }

  if (save_map) {
    fname_suffix <- switch(metric,
      "optimal_tech" = "_Fig3_Evaporation_Maps.png",
      "max_npv"      = "_Fig3_Evaporation_NPV.png",
      "both"         = "_Fig3_Evaporation_Both.png"
    )
    save_w <- if (metric == "both") 18 else 10
    ggsave_with_scenario(
      paste0(out_dir, region_name, fname_suffix),
      out_plot,
      scenario = scenario,
      width = save_w,
      height = 7,
      bg = "white",
      dpi = 300
    )
  } else {
    print(out_plot)
  }
  out_plot
}

# Figure 4: Capital Lock-Out Wedge
generate_fig4_capital_wedge <- function(dat, region_name, save_map = FALSE,
                                        scenario = "default") {
  params <- set_scenario(scenarios[[scenario]])
  message("Generating Figure 4: Capital Lock-Out Wedge for ", region_name, "...")
  cell_area <- terra::cellSize(dat$template, unit = "km")

  # We loop over discount rates. C price fixed.
  dr_seq <- seq(0, 0.20, by = 0.02)
  results <- list()
  params$region <- region_name
  for (dr in dr_seq) {
    message("  Calculating DR: ", dr * 100, "%")
    params$discount_rate <- dr
    res <- run_scenario(dat[["template", exact = TRUE]], dat[["layers", exact = TRUE]], params, vec = dat[["vec", exact = TRUE]])
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
    ggsave_with_scenario(
      paste0(out_dir, region_name, "_Fig4_Capital_Wedge.png"),
      p,
      scenario = scenario,
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
                                           scenario = "default") {
  params <- set_scenario(scenarios[[scenario]])
  message(
    "Generating Figure 5: Carbon Price Threshold Map for ",
    region_name, "..."
  )

  # Get base NPV (at C=0) and Abatement using linear baseline
  params$region <- region_name
  base_res <- get_linear_baseline(dat[["template", exact = TRUE]], dat[["layers", exact = TRUE]], params, vec = dat[["vec", exact = TRUE]])

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
    ggsave_with_scenario(
      paste0(out_dir, region_name, "_Fig5_Threshold_Map.png"),
      p,
      scenario = scenario,
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
generate_fig6_macc <- function(save_map = FALSE, scenario = "default") {
  params <- set_scenario(scenarios[[scenario]])
  message("Generating Figure 6: Fractured Regional MACC (12-panel)...")

  regions_ordered <- c("US", "China", "Europe", "India")
  all_macc <- list()

  for (r in regions_ordered) {
    dat <- load_region_data(r)
    cell_area <- terra::cellSize(dat$template, unit = "km")

    params$region <- r
    base_res <- get_linear_baseline(dat[["template", exact = TRUE]], dat[["layers", exact = TRUE]], params, vec = dat[["vec", exact = TRUE]])

    npv0 <- base_res$net
    abate <- base_res$abate

    stack_df <- terra::as.data.frame(
      c(dat$layers$biomass_density, cell_area, npv0, abate),
      xy = TRUE,
      na.rm = TRUE
    )
    names(stack_df)[3:10] <- c(
      "biomass", "area", "NPV0_BES", "NPV0_BECCS", "NPV0_BEBCS",
      "A_BES", "A_BECCS", "A_BEBCS"
    )

    stack_df$cell_bm <- stack_df$biomass * stack_df$area

    c_prices <- seq(-50, 250, by = 1)
    results <- list()

    npv0_bes <- stack_df$NPV0_BES
    npv0_beccs <- stack_df$NPV0_BECCS
    npv0_bebcs <- stack_df$NPV0_BEBCS

    a_bes <- stack_df$A_BES
    a_beccs <- stack_df$A_BECCS
    a_bebcs <- stack_df$A_BEBCS

    total_a_bes <- a_bes * stack_df$cell_bm
    total_a_beccs <- a_beccs * stack_df$cell_bm
    total_a_bebcs <- a_bebcs * stack_df$cell_bm

    cell_area_vec <- stack_df$area
    cell_bm_vec <- stack_df$cell_bm

    for (cp in c_prices) {
      val_bes <- npv0_bes + cp * a_bes
      val_beccs <- npv0_beccs + cp * a_beccs
      val_bebcs <- npv0_bebcs + cp * a_bebcs

      max_val <- pmax(val_bes, val_beccs, val_bebcs, na.rm = TRUE)
      adopted <- !is.na(max_val) & (max_val >= 0)

      is_bes <- adopted & (max_val == val_bes)
      is_beccs <- adopted & (!is_bes) & (max_val == val_beccs)
      is_bebcs <- adopted & (!is_bes) & (!is_beccs) & (max_val == val_bebcs)

      sum_a_bes <- sum(total_a_bes[is_bes], na.rm = TRUE)
      sum_a_beccs <- sum(total_a_beccs[is_beccs], na.rm = TRUE)
      sum_a_bebcs <- sum(total_a_bebcs[is_bebcs], na.rm = TRUE)

      sum_area_bes <- sum(cell_area_vec[is_bes], na.rm = TRUE)
      sum_area_beccs <- sum(cell_area_vec[is_beccs], na.rm = TRUE)
      sum_area_bebcs <- sum(cell_area_vec[is_bebcs], na.rm = TRUE)

      sum_bm_bes <- sum(cell_bm_vec[is_bes], na.rm = TRUE)
      sum_bm_beccs <- sum(cell_bm_vec[is_beccs], na.rm = TRUE)
      sum_bm_bebcs <- sum(cell_bm_vec[is_bebcs], na.rm = TRUE)

      results[[length(results) + 1]] <- data.frame(
        Price = cp,
        Abatement_BES = sum_a_bes,
        Abatement_BECCS = sum_a_beccs,
        Abatement_BEBCS = sum_a_bebcs,
        Area_BES = sum_area_bes,
        Area_BECCS = sum_area_beccs,
        Area_BEBCS = sum_area_bebcs,
        Biomass_BES = sum_bm_bes,
        Biomass_BECCS = sum_bm_beccs,
        Biomass_BEBCS = sum_bm_bebcs
      )
    }

    macc_df <- dplyr::bind_rows(results)
    macc_long <- tidyr::pivot_longer(
      macc_df,
      cols = -Price,
      names_to = c("Metric", "Technology"),
      names_sep = "_",
      values_to = "Value"
    )
    
    macc_long$Value[macc_long$Metric == "Abatement"] <- macc_long$Value[macc_long$Metric == "Abatement"] / 1e6
    macc_long$Value[macc_long$Metric == "Area"] <- macc_long$Value[macc_long$Metric == "Area"] / 1e4 # km2 to Mha
    macc_long$Value[macc_long$Metric == "Biomass"] <- macc_long$Value[macc_long$Metric == "Biomass"] / 1e6
    
    macc_long$Region <- r
    all_macc[[r]] <- macc_long
  }

  combined_macc <- dplyr::bind_rows(all_macc)
  combined_macc$Technology <- factor(combined_macc$Technology, levels = c("BECCS", "BEBCS", "BES"))

  combined_macc$Region <- factor(combined_macc$Region, levels = c("US", "China", "Europe", "India"))
  
  metric_labels <- c(
    "Abatement" = "Abatement Potential\n(MtCO2e/yr)",
    "Area" = "Land Area Used\n(Mha)",
    "Biomass" = "Biomass Converted\n(Mt dry)"
  )
  combined_macc$Metric <- factor(combined_macc$Metric, levels = c("Abatement", "Area", "Biomass"), labels = metric_labels)

  if (sum(combined_macc$Value, na.rm = TRUE) > 0) {
    p <- ggplot(combined_macc, aes(x = Price, y = Value, fill = Technology)) +
      geom_area(alpha = 0.9, color = "black", linewidth = 0.2) +
      scale_fill_manual(values = TECH_COLORS) +
      ggh4x::facet_grid2(Region ~ Metric, scales = "free_y", independent = "y") +
      theme_minimal(base_size = 14) +
      labs(
        x = "Carbon Price ($/t)",
        y = ""
      ) +
      theme(
        legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 12),
        strip.background = element_rect(fill = "grey90", color = NA),
        plot.title = element_blank()
      )

    if (save_map) {
      ggsave_with_scenario(
        paste0(out_dir, "All_Fig6_MACC_12panel.png"),
        p,
        scenario = scenario,
        width = 12,
        height = 10,
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
                                           scenario = "default",
                                           c_price = 30) {
  params <- set_scenario(scenarios[[scenario]])
  message("Generating Figure 7: Agronomic Bridge for ", region_name, "...")

  # 1. With Ag Value
  params$c_price <- c_price
  params$region <- region_name
  res_ag <- run_scenario(dat[["template", exact = TRUE]], dat[["layers", exact = TRUE]], params, vec = dat[["vec", exact = TRUE]])

  # 2. Without Ag Value
  params$bc_valuation_method <- "ag_value"
  params$bc_ag_value <- 0
  res_no <- run_scenario(dat[["template", exact = TRUE]], dat[["layers", exact = TRUE]], params, vec = dat[["vec", exact = TRUE]])

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
    ggsave_with_scenario(
      paste0(out_dir, region_name, "_Fig7_Agronomic_Bridge.png"),
      p,
      scenario = scenario,
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

# Figure 8: Global Break-Even Carbon Price Grid
generate_fig8_breakeven_cprice <- function(save_map = FALSE,
                                           scenario = "default") {
  params <- set_scenario(scenarios[[scenario]])
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
    base_res <- get_linear_baseline(dat[["template", exact = TRUE]], dat[["layers", exact = TRUE]], params, vec = dat[["vec", exact = TRUE]])

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

  combined_legends <- cowplot::plot_grid(leg_cont, leg_cat, nrow = 1, rel_widths = c(1.5, 1)) + theme(plot.margin = margin(t = -1))

  combined_plot <- patchwork::wrap_elements(main_plot) / patchwork::wrap_elements(combined_legends) +
    patchwork::plot_layout(heights = c(1, 0.04))

  if (save_map) {
    ggsave_with_scenario(
      paste0(out_dir, "Global_Fig8_Breakeven_CPrice.png"),
      combined_plot,
      scenario = scenario,
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

# Figure 9: Optimal Scale per Tech Map
generate_fig9_optimal_scale_map <- function(dat, region_name, save_map = FALSE,
                                            scenario = "default") {
  params <- set_scenario(scenarios[[scenario]])
  message("Generating Figure 9: Optimal Scale Map for ", region_name, "...")
  params$region <- region_name

  # Run for each tech with optimize_scale = TRUE
  params$optimize_scale <- TRUE

  res_bes <- run_spatial_tea(
    dat$template, params, dat$layers,
    fun = calculate_bes
  )
  res_beccs <- run_spatial_tea(
    dat$template, params, dat$layers,
    fun = calculate_beccs
  )
  res_bebcs <- run_spatial_tea(
    dat$template, params, dat$layers,
    fun = calculate_bebcs
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
    ggsave_with_scenario(
      paste0(out_dir, region_name, "_Fig9_Optimal_Scale_Map.png"),
      p,
      scenario = scenario,
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


# Figure 10: Global biomass density map
generate_fig10_biomass_density <- function(save_map = FALSE) {
  message("Generating Figure 10: Biomass Density...")
  regions_ordered <- c("India", "China", "US", "Europe")
  df_all <- list()
  admin_all <- list()
  region_widths <- numeric(length(regions_ordered))

  for (i in seq_along(regions_ordered)) {
    r <- regions_ordered[i]
    # load biomass density raster for region
    dat <- load_region_data(r)
    bm_den_r <- dat$layers$biomass_density

    # Calculate bounding box width to preserve relative scales in patchwork
    e <- terra::ext(bm_den_r)
    region_widths[i] <- e$xmax - e$xmin

    # Apply admin0 mask if available
    if (!is.null(dat$admin0)) {
      bm_den_r <- terra::mask(bm_den_r, terra::vect(dat$admin0))
      admin_all[[r]] <- dat$admin0
    }

    # convert to dataframe
    df <- terra::as.data.frame(bm_den_r, xy = TRUE, na.rm = TRUE)
    names(df)[3] <- "biomass"
    df_all[[r]] <- df
  }

  # Find global min and max for synchronized color scales
  max_bm <- max(sapply(df_all, function(d) max(d$biomass, na.rm = TRUE)), na.rm = TRUE)
  min_bm <- min(sapply(df_all, function(d) min(d$biomass, na.rm = TRUE)), na.rm = TRUE)

  # Plot each region individually with enforced global scales
  plot_list <- list()
  for (r in regions_ordered) {
    p <- ggplot() +
      geom_tile(data = df_all[[r]], aes(x = x, y = y, fill = biomass))

    if (!is.null(admin_all[[r]])) {
      p <- p + geom_sf(data = admin_all[[r]], fill = NA, color = "black", linewidth = 0.2, inherit.aes = FALSE)
    }

    p <- p +
      scale_fill_viridis_c(
        option = "mako", direction = -1, trans = "log1p",
        limits = c(min_bm, max_bm), # Enforce global limits for patchwork collection
        name = expression("Biomass\n(Mg/km"^2 * ")")
      ) +
      theme_minimal(base_size = 14) +
      coord_sf() +
      ggtitle(r) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )

    plot_list[[r]] <- p
  }

  # Combine plots in a single row with relative widths preserved
  combined_plot <- patchwork::wrap_plots(plot_list, nrow = 1) +
    patchwork::plot_layout(guides = "collect", widths = region_widths) &
    theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))

  if (save_map) {
    ggplot2::ggsave(
      filename = paste0(out_dir, "Global_Fig10_Biomass_Density.png"),
      plot = combined_plot,
      width = 16,
      height = 5,
      bg = "white",
      dpi = 300
    )
  } else {
    print(combined_plot)
  }

  return(combined_plot)
}

run_all_manuscript_figures <- function(save_map = TRUE) { # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  for (scenario_name in .scenarios) {
    for (r in .regions) {
      dat <- load_region_data(r)
      # generate_fig1_phys_boundary(dat, r, save_map, scenario = scenario_name)
      # generate_fig2_booster_penalty(dat, r, save_map, scenario = scenario_name)
      generate_fig3_evaporation(dat, r, save_map, scenario = scenario_name)
      # generate_fig4_capital_wedge(dat, r, save_map, scenario = scenario_name)
      # generate_fig5_cprice_threshold(dat, r, save_map, scenario = scenario_name)
      # generate_fig7_agronomic_bridge(dat, r, save_map, scenario = scenario_name)
      # generate_fig9_optimal_scale_map(dat, r, save_map, scenario = scenario_name)
    }
    generate_fig6_macc(save_map, scenario = scenario_name)
    generate_fig8_breakeven_cprice(save_map, scenario = scenario_name)
    message(paste0("All figures generated successfully for scenario: ", scenario_name, "\n"))
  }
  # generate_fig10_biomass_density(save_map = TRUE)
}

# --- Execution block ---
if (sys.nframe() == 0) {
  # read parameters from file
  params <- BiocharAG::set_scenario()
  dir.create(out_dir, showWarnings = FALSE)
  .regions <- c("US", "China", "Europe", "India")
  .scenarios <- c("default", "CP100_MW250", "CP100_MW250_reg", "EA_CP100_MW250", "EA_CP100_MW250_reg")
  run_all_manuscript_figures(save_map = TRUE)
}
# nolint end

### Content of file scripts/manuscript_figures.R ###
# nolint start: indentation_linter, line_length_linter, object_usage_linter, commented_code_linter
# manuscript_figures.R
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

ggsave_with_scenario <- function(filename, plot, width, height, bg = "white", dpi = 300, scenario = "default") {
  if (scenario != "default") {
    ext_idx <- regexpr("\\.[^\\.]*$", filename)
    if (ext_idx > 0) {
      base_name <- substr(filename, 1, ext_idx - 1)
      ext <- substr(filename, ext_idx, nchar(filename))
      filename <- paste0(base_name, "_", scenario, ext)
    } else {
      filename <- paste0(filename, "_", scenario)
    }
  }

  ggplot2::ggsave(filename = filename, plot = plot, width = width, height = height, bg = bg, dpi = dpi)
}

# Linear interpolation for fast sweeps
# Net_Value(C) = Net_Value(0) + C * Abatement
get_linear_baseline <- function(template, layers, base_params, vec = NULL) {
  p0 <- base_params
  p0[["c_price"]] <- 0
  res0 <- run_scenario(template, layers, p0, vec = vec)
  res0 # Returns net at C=0, and abatement
}

# --- FIGURE GENERATORS ---

################ Figure: Evaporation Maps ################
generate_fig_evaporation <- function(
  dat, region_name, save_map = FALSE,
  d_rates = c(0.02, 0.08, 0.15), c_prices = c(30, 100, 150),
  scenario = "default",
  metric = c("optimal_tech", "max_npv", "both")
) {
  metric <- match.arg(metric)
  params <- set_scenario(scenarios[[scenario]])
  message("Generating Figure 3: Evaporation Maps for ", region_name, " (Metric: ", metric, ")...")
  params$region <- region_name
  all_df <- data.frame()
  for (cp in c_prices) {
    for (dr in d_rates) {
      message("  Running DR: ", dr * 100, "%, C Price: $", cp)
      params$c_price <- cp
      params$discount_rate <- dr
      params$bc_valuation_method <- "advanced_mechanistic"

      res <- run_scenario(dat[["template", exact = TRUE]], dat[["layers", exact = TRUE]], params, vec = dat[["vec", exact = TRUE]])

      opt_raster <- res$opt
      if (!is.null(res$vec_res)) {
        max_npv_raster <- terra::rast(dat[["template", exact = TRUE]], nlyrs = 1, vals = NA)
        max_npv_raster[dat$vec$active_indices] <- pmax(
          res$vec_res$net[, 1],
          res$vec_res$net[, 2],
          res$vec_res$net[, 3],
          na.rm = TRUE
        )
      } else {
        max_npv_raster <- terra::app(res$net, max, na.rm = TRUE)
      }

      if (!is.null(dat$admin0)) {
        opt_raster <- terra::mask(opt_raster, terra::vect(dat$admin0))
        max_npv_raster <- terra::mask(max_npv_raster, terra::vect(dat$admin0))
      }
      comb_r <- c(opt_raster, max_npv_raster)
      names(comb_r) <- c("opt_tech", "max_npv")
      df <- terra::as.data.frame(comb_r, xy = TRUE, na.rm = TRUE)

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

  build_tech_plot <- function(df_data) {
    plt <- ggplot() +
      geom_tile(data = df_data, aes(x = .data$x, y = .data$y, fill = .data$tech))
    if (!is.null(dat$admin0)) {
      plt <- plt + geom_sf(
        data = dat$admin0,
        fill = NA, color = "black", linewidth = 0.5
      )
    }
    if (!is.null(dat$admin1)) {
      plt <- plt + geom_sf(
        data = dat$admin1,
        fill = NA, color = "black", linetype = "dotted", linewidth = 0.2
      )
    }
    plt +
      coord_sf(crs = 4326) +
      scale_fill_manual(
        values = c("BES" = "#1f77b4", "BECCS" = "#d62728", "BEBCS" = "#2ca02c")
      ) +
      facet_grid(cp_label ~ dr_label) +
      theme_void(base_size = 14) +
      theme(
        strip.text = element_text(face = "bold", margin = margin(b = 5, t = 5)),
        legend.position = "bottom"
      ) +
      labs(fill = "Optimal Technology")
  }

  build_npv_plot <- function(df_data) {
    plt <- ggplot() +
      geom_tile(data = df_data, aes(x = .data$x, y = .data$y, fill = .data$max_npv))
    if (!is.null(dat$admin0)) {
      plt <- plt + geom_sf(
        data = dat$admin0,
        fill = NA, color = "black", linewidth = 0.5
      )
    }
    if (!is.null(dat$admin1)) {
      plt <- plt + geom_sf(
        data = dat$admin1,
        fill = NA, color = "black", linetype = "dotted", linewidth = 0.2
      )
    }
    plt +
      coord_sf(crs = 4326) +
      scale_fill_viridis_c(option = "viridis", name = "Max NPV ($/Mg)") +
      facet_grid(cp_label ~ dr_label) +
      theme_void(base_size = 14) +
      theme(
        strip.text = element_text(face = "bold", margin = margin(b = 5, t = 5)),
        legend.position = "bottom"
      ) +
      labs(fill = "Max NPV ($/Mg)")
  }

  out_plot <- if (metric == "optimal_tech") {
    build_tech_plot(all_df)
  } else if (metric == "max_npv") {
    build_npv_plot(all_df)
  } else {
    # metric == "both"
    patchwork::wrap_plots(
      build_tech_plot(all_df) + labs(title = paste0("Optimal Technology - ", region_name)),
      build_npv_plot(all_df) + labs(title = paste0("Highest NPV - ", region_name)),
      ncol = 2
    )
  }

  if (save_map) {
    fname_suffix <- switch(metric,
      "optimal_tech" = "_Evaporation_Maps.png",
      "max_npv"      = "_Evaporation_NPV.png",
      "both"         = "_Evaporation_Both.png"
    )
    save_w <- if (metric == "both") 18 else 10
    ggsave_with_scenario(
      paste0(out_dir, region_name, fname_suffix),
      out_plot,
      scenario = scenario,
      width = save_w,
      height = 7,
      bg = "white",
      dpi = 300
    )
  } else {
    print(out_plot)
  }
  out_plot
}

################ Figure: Regional MACC ################
generate_fig_macc <- function(save_map = FALSE, scenario = "default") {
  params <- set_scenario(scenarios[[scenario]])
  message("Generating Figure: Regional MACC (12-panel)...")

  regions_ordered <- c("US", "China", "Europe", "India")
  all_macc <- list()

  for (r in regions_ordered) {
    dat <- load_region_data(r)
    cell_area <- terra::cellSize(dat$template, unit = "km")

    params$region <- r
    base_res <- get_linear_baseline(dat[["template", exact = TRUE]], dat[["layers", exact = TRUE]], params, vec = dat[["vec", exact = TRUE]])

    npv0 <- base_res$net
    abate <- base_res$abate

    stack_df <- terra::as.data.frame(
      c(dat$layers$biomass_density, cell_area, npv0, abate),
      xy = TRUE,
      na.rm = TRUE
    )
    names(stack_df)[3:10] <- c(
      "biomass", "area", "NPV0_BES", "NPV0_BECCS", "NPV0_BEBCS",
      "A_BES", "A_BECCS", "A_BEBCS"
    )

    stack_df$cell_bm <- stack_df$biomass * stack_df$area

    c_prices <- seq(-50, 250, by = 1)
    results <- list()

    npv0_bes <- stack_df$NPV0_BES
    npv0_beccs <- stack_df$NPV0_BECCS
    npv0_bebcs <- stack_df$NPV0_BEBCS

    a_bes <- stack_df$A_BES
    a_beccs <- stack_df$A_BECCS
    a_bebcs <- stack_df$A_BEBCS

    total_a_bes <- a_bes * stack_df$cell_bm
    total_a_beccs <- a_beccs * stack_df$cell_bm
    total_a_bebcs <- a_bebcs * stack_df$cell_bm

    cell_area_vec <- stack_df$area
    cell_bm_vec <- stack_df$cell_bm

    for (cp in c_prices) {
      val_bes <- npv0_bes + cp * a_bes
      val_beccs <- npv0_beccs + cp * a_beccs
      val_bebcs <- npv0_bebcs + cp * a_bebcs

      max_val <- pmax(val_bes, val_beccs, val_bebcs, na.rm = TRUE)
      adopted <- !is.na(max_val) & (max_val >= 0)

      is_bes <- adopted & (max_val == val_bes)
      is_beccs <- adopted & (!is_bes) & (max_val == val_beccs)
      is_bebcs <- adopted & (!is_bes) & (!is_beccs) & (max_val == val_bebcs)

      sum_a_bes <- sum(total_a_bes[is_bes], na.rm = TRUE)
      sum_a_beccs <- sum(total_a_beccs[is_beccs], na.rm = TRUE)
      sum_a_bebcs <- sum(total_a_bebcs[is_bebcs], na.rm = TRUE)

      sum_area_bes <- sum(cell_area_vec[is_bes], na.rm = TRUE)
      sum_area_beccs <- sum(cell_area_vec[is_beccs], na.rm = TRUE)
      sum_area_bebcs <- sum(cell_area_vec[is_bebcs], na.rm = TRUE)

      sum_bm_bes <- sum(cell_bm_vec[is_bes], na.rm = TRUE)
      sum_bm_beccs <- sum(cell_bm_vec[is_beccs], na.rm = TRUE)
      sum_bm_bebcs <- sum(cell_bm_vec[is_bebcs], na.rm = TRUE)

      results[[length(results) + 1]] <- data.frame(
        Price = cp,
        Abatement_BES = sum_a_bes,
        Abatement_BECCS = sum_a_beccs,
        Abatement_BEBCS = sum_a_bebcs,
        Area_BES = sum_area_bes,
        Area_BECCS = sum_area_beccs,
        Area_BEBCS = sum_area_bebcs,
        Biomass_BES = sum_bm_bes,
        Biomass_BECCS = sum_bm_beccs,
        Biomass_BEBCS = sum_bm_bebcs
      )
    }

    macc_df <- dplyr::bind_rows(results)
    macc_long <- tidyr::pivot_longer(
      macc_df,
      cols = -Price,
      names_to = c("Metric", "Technology"),
      names_sep = "_",
      values_to = "Value"
    )

    macc_long$Value[macc_long$Metric == "Abatement"] <- macc_long$Value[macc_long$Metric == "Abatement"] / 1e6
    macc_long$Value[macc_long$Metric == "Area"] <- macc_long$Value[macc_long$Metric == "Area"] / 1e4 # km2 to Mha
    macc_long$Value[macc_long$Metric == "Biomass"] <- macc_long$Value[macc_long$Metric == "Biomass"] / 1e6

    macc_long$Region <- r
    all_macc[[r]] <- macc_long
  }

  combined_macc <- dplyr::bind_rows(all_macc)
  combined_macc$Technology <- factor(combined_macc$Technology, levels = c("BECCS", "BEBCS", "BES"))

  combined_macc$Region <- factor(combined_macc$Region, levels = c("US", "China", "Europe", "India"))

  metric_labels <- c(
    "Abatement" = "Abatement Potential\n(MtCO2e/yr)",
    "Area" = "Land Area Used\n(Mha)",
    "Biomass" = "Biomass Converted\n(Mt dry)"
  )
  combined_macc$Metric <- factor(combined_macc$Metric, levels = c("Abatement", "Area", "Biomass"), labels = metric_labels)

  if (sum(combined_macc$Value, na.rm = TRUE) > 0) {
    p <- ggplot(combined_macc, aes(x = Price, y = Value, fill = Technology)) +
      geom_area(alpha = 0.9, color = "black", linewidth = 0.2) +
      scale_fill_manual(values = TECH_COLORS) +
      ggh4x::facet_grid2(Region ~ Metric, scales = "free_y", independent = "y") +
      theme_minimal(base_size = 14) +
      labs(
        x = "Carbon Price ($/t)",
        y = ""
      ) +
      theme(
        legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 12),
        strip.background = element_rect(fill = "grey90", color = NA),
        plot.title = element_blank()
      )

    if (save_map) {
      ggsave_with_scenario(
        paste0(out_dir, "MACC.png"),
        p,
        scenario = scenario,
        width = 12,
        height = 10,
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

################ Figure: Break-Even Carbon Price ################
generate_fig_breakeven_cprice <- function(save_map = FALSE,
                                          scenario = "default") {
  params <- set_scenario(scenarios[[scenario]])
  message("Generating Figure: Break-Even Carbon Price Grid...")

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
    base_res <- get_linear_baseline(dat[["template", exact = TRUE]], dat[["layers", exact = TRUE]], params, vec = dat[["vec", exact = TRUE]])

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

  combined_legends <- cowplot::plot_grid(leg_cont, leg_cat, nrow = 1, rel_widths = c(1.5, 1)) + theme(plot.margin = margin(t = -1))

  combined_plot <- patchwork::wrap_elements(main_plot) / patchwork::wrap_elements(combined_legends) +
    patchwork::plot_layout(heights = c(1, 0.04))

  if (save_map) {
    ggsave_with_scenario(
      paste0(out_dir, "Breakeven_CPrice.png"),
      combined_plot,
      scenario = scenario,
      width = 8,
      height = 9,
      bg = "white",
      dpi = 300
    )
    message("Saved: Breakeven_CPrice.png")
  } else {
    print(combined_plot)
  }
  return(combined_plot)
}

run_all_manuscript_figures <- function(save_map = TRUE) { # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  for (scenario_name in .scenarios) {
    for (r in .regions) {
      dat <- load_region_data(r)
      generate_fig_evaporation(dat, r, save_map, scenario = scenario_name)
    }
    generate_fig_macc(save_map, scenario = scenario_name)
    generate_fig_breakeven_cprice(save_map, scenario = scenario_name)
    message(paste0("All figures generated successfully for scenario: ", scenario_name, "\n"))
  }
}

# --- Execution block ---
if (sys.nframe() == 0) {
  # read parameters from file
  params <- BiocharAG::set_scenario()
  dir.create(out_dir, showWarnings = FALSE)
  .regions <- c("US", "China", "Europe", "India")
  .scenarios <- c("default", "CP100_MW250", "CP100_MW250_reg", "EA_CP100_MW250", "EA_CP100_MW250_reg")
  run_all_manuscript_figures(save_map = TRUE)
}
# nolint end

### Content of file scripts/mc_analysis.R ###
# scripts/run_mc_analysis.R
# Executes Monte Carlo sensitivity and uncertainty analysis across scenario parameter combinations.
# Evaluates technologies competitively across spatial layers using randomized parameter draws.
# Highly optimized for performance using vectorization and multi-core parallelization.

library(terra)
library(parallel)
library(dplyr)
library(tidyr)

# Sourcing script for helper load function and devtools packages loading
source("scripts/manuscript_figures.R")

# Configuration
n_runs <- 500 # Number of MC iterations per scenario combination (default 20 for testing)
test_mode <- TRUE # Set to FALSE for full production run across all 720 scenario combinations
n_cores <- 12 # Set to integer to override default cores detection (detectCores() - 1)
append <- TRUE # Set to TRUE to append to existing results file

factorial_grid <- expand.grid(
  region = c("US", "China", "Europe", "India"),
  c_price = c(0, 50, 100, 150, 200),
  discount_rate = c(0.02, 0.08, 0.15),
  allow_eor = c(TRUE, FALSE),
  early_adoption = c(TRUE, FALSE),
  plant_mw_th = c(50, 100, 150, 250)
)

if (test_mode) {
  message("Running in TEST MODE: truncating factorial grid to 2 scenarios for speed.")
  factorial_grid <- head(factorial_grid, 2)
}

# Determine number of cores to use
if (is.null(n_cores)) {
  n_cores <- parallel::detectCores() - 1
}
if (is.na(n_cores) || n_cores < 1) {
  n_cores <- 1
}
message("Using ", n_cores, " core(s) for parallel processing.")

# 1. Load Parameter Definitions & Set Up Classifications
params_df <- read.csv("BiocharAG/inst/extdata/parameters.csv", stringsAsFactors = FALSE)

scenario_params <- c(
  "region", "c_price", "discount_rate", "allow_eor", "early_adoption", "plant_mw_th",
  "plant_sizes_mw_th", "optimize_scale", "use_flat_ci", "flat_ci_tCO2_GJ",
  "beccs_available", "bc_valuation_method", "time_frame", "n_app_rate", "rebound",
  "py_temp", "bm_feed_rate"
)

excluded_params <- c("bc_price", "bc_ag_value", "ccs_distance", "bc_stab_factor")
spatial_scalar_params <- c("ff_c_intensity")

# Filter uncertain extrinsic parameters to sample continuously
uncertain_params <- params_df %>%
  dplyr::filter(
    tolower(distribution) != "none",
    !is.na(distribution),
    distribution != "",
    !name %in% scenario_params,
    !name %in% excluded_params,
    !name %in% spatial_scalar_params
  )

# 2. Pre-load and vectorize region spatial data
message("Pre-loading and vectorizing spatial data for all regions...")
region_names <- unique(factorial_grid$region)
vectorized_regions <- list()

for (r in region_names) {
  message("  Loading vectorized data for: ", r)
  dat <- load_region_data(r)
  vectorized_regions[[r]] <- dat[["vec", exact = TRUE]]
}

# 3. Generate Randomized MC Parameter Tables per Region (Common Random Numbers across scenarios)
generate_param_draws <- function(row, n, local_mean = NULL) {
  dist <- tolower(gsub("[- ]", "", row$distribution))
  def_val <- suppressWarnings(as.numeric(row$default_value))
  target_val <- if (!is.null(local_mean) && !is.na(local_mean)) local_mean else def_val
  
  # Fetch target CV mapping
  cv_map <- c("low" = 0.05, "medium" = 0.20, "high" = 0.40)
  target_cv <- if (!is.null(row$uncertainty_level) && row$uncertainty_level != "" && !is.na(row$uncertainty_level)) {
    cv_map[trimws(tolower(row$uncertainty_level))]
  } else {
    NA_real_
  }
  
  if (is.na(target_cv) || dist == "none") {
      return(rep(target_val, n))
  }
  
  # Calculate dynamic dispersion and bounds based on target_val
  if (dist == "normal") {
      disp <- abs(target_val * target_cv)
      min_val <- target_val - (3 * disp)
      max_val <- target_val + (3 * disp)
      draws <- rnorm(n, mean = target_val, sd = disp)
  } else if (dist == "lognormal") {
      disp <- sqrt(log(1 + target_cv^2))
      meanlog <- log(target_val) - (disp^2) / 2
      min_val <- 0
      max_val <- exp(meanlog + 3*disp)
      draws <- rlnorm(n, meanlog = meanlog, sdlog = disp)
  } else if (dist == "uniform") {
      disp <- NA
      min_val <- target_val - (abs(target_val) * target_cv * sqrt(3))
      max_val <- target_val + (abs(target_val) * target_cv * sqrt(3))
      draws <- runif(n, min = min_val, max = max_val)
  } else {
      return(rep(target_val, n))
  }
  
  # Physical clamping
  if (!is.na(def_val) && def_val > 0 && min_val < 0) min_val <- 0
  if (grepl("fraction|%|ratio", row$units, ignore.case = TRUE) && max_val > 1) max_val <- 1.0

  if (!is.na(min_val)) draws <- pmax(draws, min_val)
  if (!is.na(max_val)) draws <- pmin(draws, max_val)

  return(draws)
}

set.seed(42) # For reproducible random draws
mc_tables_by_region <- list()

for (r in unique(factorial_grid$region)) {
  p_local <- BiocharAG::set_scenario(region = r)
  spatial_layers <- vectorized_regions[[r]]$layers
  mc_draws_list <- list()

  for (i in seq_len(nrow(uncertain_params))) {
    p_name <- uncertain_params$name[i]
    local_val <- if (!is.null(p_local[[p_name]])) p_local[[p_name]] else NULL
    mc_draws_list[[p_name]] <- generate_param_draws(uncertain_params[i, ], n_runs, local_mean = local_val)
  }

  # Generate scalar multiplier for ff_c_intensity (spatial raster parameter)
  ff_row <- params_df[params_df$name == "ff_c_intensity", ]
  if (nrow(ff_row) > 0) {
    ff_local_mean <- if (!is.null(spatial_layers$ff_c_intensity)) mean(spatial_layers$ff_c_intensity, na.rm=TRUE) else as.numeric(ff_row$default_value)
    ff_draws <- generate_param_draws(ff_row, n_runs, local_mean = ff_local_mean)
    mc_draws_list[["ff_ci_multiplier"]] <- ff_draws / ff_local_mean
  } else {
    mc_draws_list[["ff_ci_multiplier"]] <- rep(1.0, n_runs)
  }
  
  # Generate scalar multiplier for elec_price (spatial raster parameter)
  ep_row <- params_df[params_df$name == "elec_price", ]
  if (nrow(ep_row) > 0) {
    ep_local_mean <- if (!is.null(spatial_layers$elec_price)) mean(spatial_layers$elec_price, na.rm=TRUE) else as.numeric(ep_row$default_value)
    ep_draws <- generate_param_draws(ep_row, n_runs, local_mean = ep_local_mean)
    mc_draws_list[["elec_price_multiplier"]] <- ep_draws / ep_local_mean
  } else {
    mc_draws_list[["elec_price_multiplier"]] <- rep(1.0, n_runs)
  }

  mc_table_r <- as.data.frame(mc_draws_list, stringsAsFactors = FALSE)
  mc_table_r$mc_run_id <- seq_len(n_runs)
  mc_tables_by_region[[r]] <- mc_table_r
}

# Helper functions for spatial metrics extraction on vectors
extract_masked_vector_mean <- function(vec, is_best) {
  if (is.null(vec) || length(vec) == 0) {
    return(NA)
  }
  if (length(vec) == 1) {
    return(vec)
  }
  if (identical(is_best, FALSE)) is_best <- !is_best # Return all values if is_best is FALSE
  vals <- vec[is_best]
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) {
    return(NA)
  }
  return(mean(vals))
}

extract_masked_vector_min <- function(vec, is_best) {
  if (is.null(vec) || length(vec) == 0) {
    return(NA)
  }
  if (length(vec) == 1) {
    return(vec)
  }
  if (identical(is_best, FALSE)) is_best <- !is_best # Return all values if is_best is FALSE
  vals <- vec[is_best]
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) {
    return(NA)
  }
  return(min(vals))
}

extract_masked_vector_max <- function(vec, is_best) {
  if (is.null(vec) || length(vec) == 0) {
    return(NA)
  }
  if (length(vec) == 1) {
    return(vec)
  }
  if (identical(is_best, FALSE)) is_best <- !is_best # Return all values if is_best is FALSE
  vals <- vec[is_best]
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) {
    return(NA)
  }
  return(max(vals))
}

# 3. Pre-load and vectorize region spatial data
# (Moved to before generate_param_draws to allow spatial means for MC bounds)

message("Starting parallel Monte Carlo Analysis: ", nrow(factorial_grid), " scenario combinations x ", n_runs, " MC runs each.")

# Run scenario combinations in parallel
results_list <- parallel::mclapply(seq_len(nrow(factorial_grid)), function(s) {
  s_row <- factorial_grid[s, ]
  r_data <- vectorized_regions[[s_row$region]]
  spatial_layers <- r_data$layers
  cell_area <- r_data$cell_area

  # Create a scenario-specific results accumulator
  scenario_results <- data.frame()

  for (m in seq_len(n_runs)) {
    mc_row <- mc_tables_by_region[[s_row$region]][m, ]

    # Base Setup from Scenario
    p <- BiocharAG::set_scenario(region = s_row$region)
    p$c_price <- s_row$c_price
    p$discount_rate <- s_row$discount_rate
    p$allow_eor <- s_row$allow_eor
    p$early_adoption <- s_row$early_adoption
    p$plant_mw_th <- s_row$plant_mw_th

    # Inject all uncertain extrinsic scalar parameters from mc_row into p
    for (p_name in names(mc_row)) {
      if (!(p_name %in% c("mc_run_id", "ff_ci_multiplier", "elec_price_multiplier"))) {
        p[[p_name]] <- mc_row[[p_name]]
      }
    }

    # Inject spatial layers (overriding scalar defaults if layer exists)
    # TODO (Future): If spatially explicit parameters with strict physical boundaries 
    # (e.g. fractions strictly <= 1.0) are added and subjected to uncertainty multipliers, 
    # explicit terra::clamp() logic must be added below to prevent the multiplier from 
    # pushing pixel values out of bounds. Current spatial parameters (elec_price, ff_c_intensity) 
    # are unbounded upper-limit quantities, so proportional scaling is safe.
    
    if ("soil_temp" %in% names(spatial_layers)) p$soil_temp <- spatial_layers$soil_temp
    
    if ("elec_price" %in% names(spatial_layers)) {
      factor <- if (!is.null(p$wholesale_discount_factor)) p$wholesale_discount_factor else 0.4
      ep_mult <- if (!is.null(mc_row$elec_price_multiplier)) mc_row$elec_price_multiplier else 1.0
      p$elec_price <- spatial_layers$elec_price * ep_mult * factor
    } else if (!is.null(p$elec_price)) {
      ep_mult <- if (!is.null(mc_row$elec_price_multiplier)) mc_row$elec_price_multiplier else 1.0
      p$elec_price <- p$elec_price * ep_mult
    }
    
    if ("soil_ph" %in% names(spatial_layers)) p$soil_ph <- spatial_layers$soil_ph
    if ("soil_cec" %in% names(spatial_layers)) p$soil_cec <- spatial_layers$soil_cec
    if ("dist_sink_km" %in% names(spatial_layers)) p$dist_sink_km <- spatial_layers$dist_sink_km
    if ("dist_sink_saline_km" %in% names(spatial_layers)) p$dist_sink_saline_km <- spatial_layers$dist_sink_saline_km
    if ("sink_is_offshore" %in% names(spatial_layers)) p$sink_is_offshore <- spatial_layers$sink_is_offshore

    # Apply ff_ci_multiplier to ff_c_intensity (whether raster or scalar)
    ff_mult <- if (!is.null(mc_row$ff_ci_multiplier)) mc_row$ff_ci_multiplier else 1.0
    if ("ff_c_intensity" %in% names(spatial_layers)) {
      p$ff_c_intensity <- spatial_layers$ff_c_intensity * ff_mult
    } else if (!is.null(p$ff_c_intensity)) {
      p$ff_c_intensity <- p$ff_c_intensity * ff_mult
    }

    for (layer_name in c("cn_weather_risk", "cn_expansion_risk", "eu_base_eur", "us_base_cost")) {
      if (layer_name %in% names(spatial_layers)) p[[layer_name]] <- spatial_layers[[layer_name]]
    }

    dist_layer_name <- paste0("dist_", s_row$plant_mw_th, "MWth")
    if (dist_layer_name %in% names(spatial_layers)) {
      p$avg_dist <- spatial_layers[[dist_layer_name]]
    }

    p$feedstock_cost <- BiocharAG::calculate_regional_feedstock_cost(s_row$region, p)

    # Execute All 3 Technologies Competitively
    res_bes <- BiocharAG::calculate_bes(p)
    res_beccs <- BiocharAG::calculate_beccs(p)
    res_bebcs <- BiocharAG::calculate_bebcs(p)

    # Stack NPVs and find winner (in-memory matrix math)
    npv_matrix <- cbind(res_bes$net_value, res_beccs$net_value, res_bebcs$net_value)
    opt_idx <- max.col(npv_matrix, ties.method = "first")

    # If all NPVs are NA, opt_idx is NA
    opt_idx[rowSums(is.na(npv_matrix)) == 3] <- NA

    biomass_amount <- spatial_layers$biomass_density * cell_area

    techs <- c("BES", "BECCS", "BEBCS")
    res_list <- list(res_bes, res_beccs, res_bebcs)

    for (t_idx in 1:3) {
      t_name <- techs[t_idx]
      tech_res <- res_list[[t_idx]]

      # Area and Biomass Calculations
      is_best <- !is.na(opt_idx) & opt_idx == t_idx
      is_viable <- is_best & !is.na(tech_res$net_value) & (tech_res$net_value > 0)

      area_best_km2 <- sum(cell_area[is_best], na.rm = TRUE)
      area_viable_km2 <- sum(cell_area[is_viable], na.rm = TRUE)
      biomass_processed_yr <- sum(biomass_amount[is_viable], na.rm = TRUE)

      # Create result row combining scenario columns, MC parameter draws, and TEA results
      new_row <- data.frame(
        scenario_id = s,
        mc_run_id = m,
        region = s_row$region,
        c_price = s_row$c_price,
        discount_rate = s_row$discount_rate,
        allow_eor = s_row$allow_eor,
        early_adoption = s_row$early_adoption,
        plant_mw_th = s_row$plant_mw_th,
        technology = t_name,
        stringsAsFactors = FALSE
      )

      # Append MC parameter columns
      param_cols <- mc_row[, names(mc_row) != "mc_run_id", drop = FALSE]
      new_row <- cbind(new_row, param_cols)

      # Append TEA result columns
      tea_cols <- data.frame(
        area_best_km2 = if (area_best_km2 == 0) NA else area_best_km2,
        area_viable_km2 = if (area_viable_km2 == 0) NA else area_viable_km2,
        biomass_processed_yr_mg = if (area_viable_km2 == 0) NA else biomass_processed_yr,
        npv_min = extract_masked_vector_min(tech_res$net_value, is_best),
        npv_max = extract_masked_vector_max(tech_res$net_value, is_best),
        npv_mean = extract_masked_vector_mean(tech_res$net_value, is_best),
        mean_co2_transport_distance_km = extract_masked_vector_mean(tech_res$co2_transport_distance_km, is_best),
        mean_biomass_transport_distance_km = extract_masked_vector_mean(tech_res$biomass_transport_distance_km, is_best),
        mean_capital_cost_mg = extract_masked_vector_mean(tech_res$capital_cost_mg, is_best),
        mean_om_cost_mg = extract_masked_vector_mean(tech_res$om_cost_mg, is_best),
        mean_biomass_cost_mg = extract_masked_vector_mean(tech_res$biomass_cost_mg, is_best),
        mean_co2_transport_cost_mg = extract_masked_vector_mean(tech_res$co2_transport_cost_mg, is_best),
        mean_net_cdr = extract_masked_vector_mean(tech_res$tot_c_abatement, is_best),
        mean_carbon_removal_revenue_mg = extract_masked_vector_mean(tech_res$abatement_revenue_mg, is_best),
        mean_energy_production_mwh = extract_masked_vector_mean(tech_res$energy_prod, is_best),
        mean_energy_revenue_mg = extract_masked_vector_mean(tech_res$energy_revenue_mg, is_best),
        mean_agronomic_revenue_mg = extract_masked_vector_mean(tech_res$agronomic_revenue_mg, is_best),
        mean_lcoe_usd_mwh = extract_masked_vector_mean(tech_res$lcoe, is_best),
        mean_cost_of_co2_avoided = extract_masked_vector_mean(tech_res$cost_of_co2_avoided, is_best),
        mean_abatement_efficiency = extract_masked_vector_mean(tech_res$abatement_efficiency, is_best),
        mean_total_capex_m = extract_masked_vector_mean(tech_res$total_capex_m, is_best)
      )

      new_row <- cbind(new_row, tea_cols)
      scenario_results <- rbind(scenario_results, new_row)
    }
  }

  message(sprintf("Finished scenario %d of %d (Region: %s, C price: %d)", s, nrow(factorial_grid), s_row$region, s_row$c_price))
  return(scenario_results)
}, mc.cores = n_cores)

# Check for errors in parallel workers
errors <- sapply(results_list, inherits, "try-error")
if (any(errors)) {
  stop("One or more parallel workers failed. First error:\n", results_list[[which(errors)[1]]])
}

# Combine all parallel result chunks
results_df <- do.call(rbind, results_list)

dir.create("results", showWarnings = FALSE)
file_path <- "results/mc_analysis_results.csv"
file_exists <- file.exists(file_path)
write.table(
  results_df,
  file = file_path,
  row.names = FALSE,
  col.names = !file_exists || !append,
  sep = ",",
  dec = ".",
  qmethod = "double",
  append = append && file_exists
)
message("Monte Carlo Analysis Complete. Results saved to results/mc_analysis_results.csv")

### Content of file scripts/MC_shap.R ###
# Monte Carlo sensitivity analysis using xgboost and shapviz.
# Generates sensitivity evolution plot, beeswarm plot, and partial dependence plots.

library(data.table)
library(dplyr)
library(xgboost)
library(shapviz)
library(ggplot2)

plot_sensitivity_evolution <- function(
  data_path = "results/mc_analysis_results.csv",
  technology_name = "BECCS",
  region_name = "Europe",
  discount_rate = 0.08,
  target_c_price = 100
) {
  # 1. Load the Monte Carlo results
  message("Loading data from ", data_path, "...")
  df <- fread(data_path, data.table = FALSE)

  # Define the discrete scenario steps to track (carbon prices)
  c_prices <- sort(unique(df$c_price))
  message("Detected carbon prices: ", paste(c_prices, collapse = ", "))

  # Data structure to store the SHAP importance
  importance_list <- list()

  # Define columns to drop to isolate the purely uncertain input parameters and toggles
  cols_to_drop <- c(
    "scenario_id", "mc_run_id", "region", "technology", "c_price", "discount_rate",
    "area_best_km2", "area_viable_km2", "biomass_processed_yr_mg",
    "npv_min", "npv_max", "npv_mean"
  )

  # Also drop columns starting with mean_ or total_
  all_names <- names(df)
  cols_to_drop <- c(
    cols_to_drop,
    all_names[startsWith(all_names, "mean_") | startsWith(all_names, "total_")]
  )

  # Store the shapviz object for the target carbon price to plot beeswarm/dependence later
  target_shp <- NULL

  # 2. Iterate through Carbon Prices and fit SHAP model
  for (cp in c_prices) {
    # Filter conditionally to the specific scenario
    sub_df <- df %>%
      filter(
        technology == technology_name,
        region == region_name,
        discount_rate == !!discount_rate,
        c_price == !!cp,
        !is.na(npv_mean)
      )

    if (nrow(sub_df) < 30) {
      message(sprintf("Skipping c_price=%d: only %d runs available (locked out or inactive)", cp, nrow(sub_df)))
      next
    }

    message(sprintf("Processing c_price=%d (%d runs)...", cp, nrow(sub_df)))

    y <- sub_df$npv_mean
    X <- sub_df[, !(names(sub_df) %in% cols_to_drop), drop = FALSE]

    # Convert logical columns to numeric (0/1) for xgboost compatibility
    for (col in names(X)) {
      if (is.logical(X[[col]])) {
        X[[col]] <- as.numeric(X[[col]])
      } else if (is.character(X[[col]])) {
        # Try to map strings like TRUE/FALSE or convert to factor/numeric
        X[[col]] <- ifelse(X[[col]] %in% c("TRUE", "True", "T"), 1,
          ifelse(X[[col]] %in% c("FALSE", "False", "F"), 0,
            as.numeric(as.factor(X[[col]]))
          )
        )
      }
    }

    # Drop constant columns (variance <= 1e-8)
    variances <- sapply(X, var, na.rm = TRUE)
    constant_cols <- names(variances)[is.na(variances) | variances <= 1e-8]
    X <- X[, !(names(X) %in% constant_cols), drop = FALSE]

    # Train XGBoost model
    X_mat <- as.matrix(X)
    dtrain <- xgb.DMatrix(data = X_mat, label = y)

    params <- list(
      max_depth = 5,
      eta = 0.05,
      objective = "reg:squarederror",
      nthread = 1
    )

    model <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = 100,
      verbose = 0
    )

    # Calculate SHAP values
    shp <- shapviz(model, X_pred = X_mat)

    # Save target shapviz object if it matches the target carbon price
    if (cp == target_c_price) {
      target_shp <- shp
    }

    # Calculate Mean Absolute SHAP (Feature Importance)
    mean_abs_shap <- colMeans(abs(shp$S))

    # Save to list
    importance_df <- data.frame(
      c_price = cp,
      feature = names(mean_abs_shap),
      mean_abs_shap = as.numeric(mean_abs_shap),
      stringsAsFactors = FALSE
    )
    importance_list[[as.character(cp)]] <- importance_df
  }

  if (length(importance_list) == 0) {
    stop("Error: No data populated in sensitivity tracker. Verify scenario filters.")
  }

  # Combine importance data
  importance_all <- do.call(rbind, importance_list)

  # 3. Plot the Evolution
  # Filter to top 6 most important features overall to avoid chart clutter
  overall_importance <- importance_all %>%
    group_by(feature) %>%
    summarise(avg_importance = mean(mean_abs_shap), .groups = "drop") %>%
    arrange(desc(avg_importance)) %>%
    slice_head(n = 6)

  top_features <- overall_importance$feature
  message("Top 6 key features overall: ", paste(top_features, collapse = ", "))

  importance_top <- importance_all %>%
    filter(feature %in% top_features)

  # Evolution Plot
  p_ev <- ggplot(importance_top, aes(x = c_price, y = mean_abs_shap, color = feature, group = feature)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    theme_bw(base_size = 12) +
    labs(
      title = paste0(
        "Evolution of Parameter Sensitivity vs. Carbon Price (R Port)\n",
        technology_name, " in ", region_name, " (DR=", discount_rate * 100, "%)"
      ),
      x = "Carbon Price ($/tCO2e)",
      y = "Mean Absolute SHAP Value (Impact on NPV)",
      color = "Key Parameters / Toggles"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      legend.position = "right",
      panel.grid.minor = element_blank()
    )

  dir.create("results", showWarnings = FALSE)

  ev_png <- sprintf("results/sensitivity_evolution_R_%s_%s_toggles.png", technology_name, region_name)
  ggsave(ev_png, plot = p_ev, width = 10, height = 6, dpi = 300)
  message("Saved evolution plot to ", ev_png)

  # 4. Generate Beeswarm and Dependence Plots if target SHAP is available
  if (!is.null(target_shp)) {
    message("Generating SHAP diagnostic plots for target carbon price $", target_c_price, "...")

    # Beeswarm Plot
    p_bee <- sv_importance(target_shp, kind = "beeswarm") +
      theme_bw(base_size = 12) +
      labs(
        title = paste0(
          "SHAP Beeswarm Plot (Carbon Price = $", target_c_price, ")\n",
          technology_name, " in ", region_name
        )
      ) +
      theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5))

    bee_png <- sprintf("results/beeswarm_R_%s_%s_c_%d.png", technology_name, region_name, target_c_price)
    ggsave(bee_png, plot = p_bee, width = 9, height = 6, dpi = 300)
    message("Saved beeswarm plot to ", bee_png)

    # Partial Dependence / SHAP Dependence Plot for the top features
    # Find the top 2 features at this specific carbon price
    top_features_at_c <- importance_all %>%
      filter(c_price == target_c_price) %>%
      arrange(desc(mean_abs_shap)) %>%
      slice_head(n = 2) %>%
      pull(feature)

    for (feat in top_features_at_c) {
      if (feat %in% colnames(target_shp$X)) {
        p_dep <- sv_dependence(target_shp, v = feat) +
          theme_bw(base_size = 12) +
          labs(
            title = paste0(
              "SHAP Dependence Plot for ", feat, " (Carbon Price = $", target_c_price, ")\n",
              technology_name, " in ", region_name
            )
          ) +
          theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5))

        dep_png <- sprintf("results/dependence_R_%s_%s_%s_c_%d.png", technology_name, region_name, feat, target_c_price)
        ggsave(dep_png, plot = p_dep, width = 8, height = 5, dpi = 300)
        message("Saved dependence plot for ", feat, " to ", dep_png)
      }
    }
  } else {
    warning("Target carbon price $", target_c_price, " was not processed; skipping beeswarm and dependence plots.")
  }
}

plot_global_beeswarm <- function(
  data_path = "results/mc_analysis_results.csv",
  technology_name = "BECCS",
  discount_rate = 0.08,
  target_c_price = 100
) {
  # 1. Load the Monte Carlo results
  message("Loading data for global beeswarm from ", data_path, "...")
  df <- fread(data_path, data.table = FALSE)

  # Filter conditionally to the specific technology and carbon price across all regions
  sub_df <- df %>%
    filter(
      technology == technology_name,
      discount_rate == !!discount_rate,
      c_price == !!target_c_price,
      !is.na(npv_mean)
    )

  if (nrow(sub_df) < 30) {
    message(sprintf("Skipping global beeswarm for %s: only %d runs available", technology_name, nrow(sub_df)))
    return(NULL)
  }

  message(sprintf("Processing global beeswarm for %s (%d runs)...", technology_name, nrow(sub_df)))

  # Define columns to drop (note: region is NOT dropped!)
  cols_to_drop <- c(
    "scenario_id", "mc_run_id", "technology", "c_price", "discount_rate",
    "area_best_km2", "area_viable_km2", "biomass_processed_yr_mg",
    "npv_min", "npv_max", "npv_mean"
  )

  # Also drop columns starting with mean_ or total_
  all_names <- names(df)
  cols_to_drop <- c(
    cols_to_drop,
    all_names[startsWith(all_names, "mean_") | startsWith(all_names, "total_")]
  )

  y <- sub_df$npv_mean
  X <- sub_df[, !(names(sub_df) %in% cols_to_drop), drop = FALSE]

  # Convert logical and character columns to numeric for xgboost compatibility
  for (col in names(X)) {
    if (is.logical(X[[col]])) {
      X[[col]] <- as.numeric(X[[col]])
    } else if (is.character(X[[col]]) || is.factor(X[[col]])) {
      X[[col]] <- as.numeric(as.factor(X[[col]]))
    }
  }

  # Drop constant columns (variance <= 1e-8)
  variances <- sapply(X, var, na.rm = TRUE)
  constant_cols <- names(variances)[is.na(variances) | variances <= 1e-8]
  X <- X[, !(names(X) %in% constant_cols), drop = FALSE]

  # Train XGBoost model
  X_mat <- as.matrix(X)
  dtrain <- xgb.DMatrix(data = X_mat, label = y)

  params <- list(
    max_depth = 5,
    eta = 0.05,
    objective = "reg:squarederror",
    nthread = 1
  )

  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    verbose = 0
  )

  # Calculate SHAP values
  shp <- shapviz(model, X_pred = X_mat)

  # Beeswarm Plot
  p_bee <- sv_importance(shp, kind = "beeswarm") +
    theme_bw(base_size = 12) +
    labs(
      title = paste0(
        "Global SHAP Beeswarm Plot (Carbon Price = $", target_c_price, ")\n",
        technology_name, " - All Regions Aggregated (DR=", discount_rate * 100, "%)"
      )
    ) +
    theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5))

  # Add second color legend for region mapping
  opt <- getOption("shapviz.viridis_args", list(begin = 0.25, end = 0.85, option = "inferno"))
  cols <- viridisLite::viridis(100, begin = opt$begin, end = opt$end, option = opt$option)
  unique_regions <- sort(unique(na.omit(sub_df$region)))
  n_regions <- length(unique_regions)
  if (n_regions > 0) {
    region_colors <- cols[round(1 + seq(0, 1, length.out = n_regions) * 99)]
    names(region_colors) <- unique_regions

    legend_data <- data.frame(
      x = NA_real_,
      y = p_bee$data$feature[1],
      Region = factor(unique_regions, levels = unique_regions)
    )

    p_bee <- p_bee +
      geom_point(data = legend_data, aes(x = x, y = y, fill = Region), shape = 21, size = 3, stroke = 0) +
      scale_fill_manual(
        name = "Region",
        values = region_colors
      ) +
      guides(
        fill = guide_legend(override.aes = list(shape = 21, size = 4, stroke = 0))
      )
  }

  dir.create("results", showWarnings = FALSE)
  bee_png <- sprintf("results/beeswarm_global_%s_c_%d.png", technology_name, target_c_price)
  ggsave(bee_png, plot = p_bee, width = 10, height = 7, dpi = 300)
  message("Saved global beeswarm plot to ", bee_png)

  return(p_bee)
}

generate_evolution_plots <- function() {
  plot_sensitivity_evolution(
    data_path = "results/mc_analysis_results.csv",
    technology_name = "BECCS",
    region_name = "Europe",
    discount_rate = 0.08,
    target_c_price = 100
  )

  plot_sensitivity_evolution(
    data_path = "results/mc_analysis_results.csv",
    technology_name = "BECCS",
    region_name = "US",
    discount_rate = 0.08,
    target_c_price = 100
  )

  plot_sensitivity_evolution(
    data_path = "results/mc_analysis_results.csv",
    technology_name = "BECCS",
    region_name = "China",
    discount_rate = 0.08,
    target_c_price = 100
  )

  plot_sensitivity_evolution(
    data_path = "results/mc_analysis_results.csv",
    technology_name = "BECCS",
    region_name = "India",
    discount_rate = 0.08,
    target_c_price = 100
  )
}

generate_global_beeswarm_plots <- function() {
  plot_global_beeswarm(
    data_path = "results/mc_analysis_results.csv",
    technology_name = "BECCS",
    discount_rate = 0.08,
    target_c_price = 100
  )

  plot_global_beeswarm(
    data_path = "results/mc_analysis_results.csv",
    technology_name = "BES",
    discount_rate = 0.08,
    target_c_price = 100
  )

  plot_global_beeswarm(
    data_path = "results/mc_analysis_results.csv",
    technology_name = "BEBCS",
    discount_rate = 0.08,
    target_c_price = 100
  )
}

# Run the analysis
if (sys.nframe() == 0L) {
  generate_evolution_plots()
  generate_global_beeswarm_plots()
}

### Content of file scripts/run_analyses.R ###
library(BiocharAG)
library(terra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)

# 1) Manuscript main figures
#    Generates:
#    fig 3 evaporation maps
#    fig 6 MACC
#    fig 8 breakeven C price
{
    source("scripts/manuscript_figures.R")
    params <- BiocharAG::set_scenario()
    dir.create(out_dir, showWarnings = FALSE)
    .regions <- c("US", "China", "Europe", "India")
    .scenarios <- c("default", "CP100_MW250", "CP100_MW250_reg", "EA_CP100_MW250", "EA_CP100_MW250_reg")
    run_all_manuscript_figures(save_map = TRUE)
}

# 2) Factorial Analysis
#    Executes a full factorial spatial TEA across 960 factorial scenarios
#    Evaluates technologies competitively
#    Generates:
#      "results/factorial_analysis_results.csv"
{
    source("scripts/factorial_analysis.R")
}

# 3) Monte Carlo Simulations
#    Generates:
#    "results/mc_analysis_results.csv"
#    (very slow, comment out if not needed)
{
    source("scripts/mc_analysis.R")
}

# 4) Monte Carlo Shap Analysis
#    Generates:
#    Evolution Plot (shap value of highest features against carbon price)
#    Beeswarm of SHAP values
#    Dependence plots
{
    source("scripts/MC_shap.R")
    generate_evolution_plots()
    generate_global_beeswarm_plots()
}

# 5) Spatial sensitivity Analysis
#    Generates:
#    spatial_sensitivity_results.csv (economic and CO2 metrics for each grid cell)
#    spatial_shap_values_by_location.csv (shap values for each grid cell)
{
    source("scripts/spatial_sensitivity.R")
}

# 6) Spatial SHAP
#    Generates:
#    maps of dominant SHAP feature by location
{
    source("scripts/spatial_shap.R")
}

### Content of file scripts/shiny_app.R ###
# shiny_results.R
library(shiny)
library(shinyjs)
library(terra)
library(dplyr)
library(sf)
library(BiocharAG)
library(ggplot2)
library(tidyr)

# Silence linter warnings
if (getRversion() >= "2.15.1") {
    utils::globalVariables(c("x", "y", "tech"))
}

# Ensure data dictionary / parameters are available
if (!exists("set_scenario")) {
    message("Package loaded but set_scenario not found on search path.")
}

# 2. UI Definition
ui <- fluidPage(
    useShinyjs(),
    titlePanel("BiocharAG Spatial TEA explorer - Results"),
    sidebarLayout(
        sidebarPanel(
            width = 3,
            selectInput("region", "Region:",
                choices = c("US" = "US", "India" = "India", "China" = "China", "Europe" = "Europe"),
                selected = "US"
            ),
            hr(),
            conditionalPanel(
                condition = "input.main_tabs == 'Evaporation Maps'",
                sliderInput("dr_range", "Discount Rate Range (%):",
                    min = 0, max = 20, value = c(2, 15), step = 1
                ),
                sliderInput("cp_range", "Carbon Price Range ($/Mg CO2e):",
                    min = 0, max = 500, value = c(100, 150), step = 10
                ),
                actionButton("run_fig3_btn", "Generate Maps", class = "btn-primary", width = "100%")
            ),
            conditionalPanel(
                condition = "input.main_tabs == 'Interactive Map'",
                conditionalPanel(
                    condition = "input.optimize_scale == false",
                    numericInput("bes_plant_mw", "BES Plant Capacity (MWth):", value = 50, min = 5, step = 5),
                    numericInput("beccs_plant_mw", "BECCS Plant Capacity (MWth):", value = 250, min = 5, step = 5),
                    numericInput("bebcs_plant_mw", "BEBCS Plant Capacity (MWth):", value = 50, min = 5, step = 5)
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
                checkboxInput("optimize_scale", "Optimize Plant Scale?", value = TRUE),
                hr(),
                actionButton("run_btn", "Run Analysis", class = "btn-primary", width = "100%"),
                p(
                    class = "text-muted", style = "margin-top: 10px;",
                    "Note: Analysis may take 10-20 seconds to run across the full grid."
                )
            )
        ),
        mainPanel(
            tabsetPanel(
                id = "main_tabs",
                tabPanel(
                    "Evaporation Maps",
                    plotOutput("fig3_plot", height = "800px")
                ),
                tabPanel(
                    "Interactive Map",
                    plotOutput("map_plot", height = "800px")
                )
            )
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
    rv <- reactiveValues(map_data = NULL, fig3_data = NULL)

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


        if (input$region %in% c("India", "China", "Europe")) {
            prefix <- tolower(input$region) # "india", "china", "europe"

            bm <- terra::rast(paste0(gis_path, prefix, "_biomass.tif"))
            st <- terra::rast(paste0(gis_path, prefix, "_soil_temp.tif"))
            ep <- terra::rast(paste0(gis_path, prefix, "_elec_price.tif"))

            ph <- NULL
            if (file.exists(paste0(gis_path, prefix, "_soil_ph.tif"))) {
                ph <- terra::rast(paste0(gis_path, prefix, "_soil_ph.tif"))
            }
            cec <- NULL
            if (file.exists(paste0(gis_path, prefix, "_soil_cec.tif"))) {
                cec <- terra::rast(paste0(gis_path, prefix, "_soil_cec.tif"))
            }

            # Transport Layers
            dist_sink <- terra::rast(paste0(gis_path, prefix, "_dist_sink.tif"))
            dist_sink_saline <- terra::rast(paste0(gis_path, prefix, "_dist_sink_saline.tif"))
            sink_type <- terra::rast(paste0(gis_path, prefix, "_sink_type.tif"))

            processed_layers <- list(
                biomass_density = bm, soil_temp = st, elec_price = ep,
                dist_sink_km = dist_sink, dist_sink_saline_km = dist_sink_saline, sink_is_offshore = sink_type
            )
            if (!is.null(ph)) processed_layers$soil_ph <- ph
            if (!is.null(cec)) processed_layers$soil_cec <- cec

            ci_path <- paste0(gis_path, prefix, "_ff_c_intensity.tif")
            if (file.exists(ci_path)) processed_layers$ff_c_intensity <- terra::rast(ci_path)

            a0 <- NULL
            a1 <- NULL
            if (file.exists(paste0(gis_path, prefix, "_admin0.gpkg"))) a0 <- sf::st_read(paste0(gis_path, prefix, "_admin0.gpkg"), quiet = TRUE)
            if (file.exists(paste0(gis_path, prefix, "_admin1.gpkg"))) a1 <- sf::st_read(paste0(gis_path, prefix, "_admin1.gpkg"), quiet = TRUE)

            template <- bm
        } else {
            # US Logic
            bm <- terra::rast(paste0(gis_path, "us_biomass.tif"))
            st <- terra::rast(paste0(gis_path, "us_soil_temp.tif"))
            if (file.exists(paste0(gis_path, "us_elec_price.tif"))) {
                ep <- terra::rast(paste0(gis_path, "us_elec_price.tif"))
            } else {
                ep <- terra::rast(paste0(gis_path, "us_elec_price.tif")) # This logic is redundant now, but harmless
            }

            # Transport (US Demo)
            if (file.exists(paste0(gis_path, "us_dist_sink.tif"))) {
                dist_sink <- terra::rast(paste0(gis_path, "us_dist_sink.tif"))
                dist_sink_saline <- terra::rast(paste0(gis_path, "us_dist_sink_saline.tif"))
                sink_type <- terra::rast(paste0(gis_path, "us_sink_type.tif"))
            } else {
                # Fallback if US Transport layers missing (use demo defaults)
                dist_sink <- terra::rast(bm)
                values(dist_sink) <- 100
                dist_sink_saline <- terra::rast(bm)
                values(dist_sink_saline) <- 100
                sink_type <- terra::rast(bm)
                values(sink_type) <- 0
            }
            ph <- NULL
            cec <- NULL
            # Prefer Real Soil Data if available
            if (file.exists(paste0(gis_path, "soil_ph.tif"))) {
                ph <- terra::rast(paste0(gis_path, "soil_ph.tif"))
            } else {
                if (file.exists(paste0(gis_path, "us_soil_ph.tif"))) ph <- terra::rast(paste0(gis_path, "us_soil_ph.tif"))
            }

            if (file.exists(paste0(gis_path, "soil_cec.tif"))) {
                cec <- terra::rast(paste0(gis_path, "soil_cec.tif"))
            } else {
                if (file.exists(paste0(gis_path, "us_soil_cec.tif"))) cec <- terra::rast(paste0(gis_path, "us_soil_cec.tif"))
            }

            processed_layers <- list(
                biomass_density = bm, soil_temp = st, elec_price = ep,
                dist_sink_km = dist_sink, dist_sink_saline_km = dist_sink_saline, sink_is_offshore = sink_type
            )

            # DEBUG: Print Check
            if (!is.null(ph)) processed_layers$soil_ph <- ph
            if (!is.null(cec)) processed_layers$soil_cec <- cec

            ci_path <- paste0(gis_path, "us_ff_c_intensity.tif")
            if (file.exists(ci_path)) processed_layers$ff_c_intensity <- terra::rast(ci_path)

            a0 <- NULL
            a1 <- NULL
            if (file.exists(paste0(gis_path, "us_admin0.gpkg"))) a0 <- sf::st_read(paste0(gis_path, "us_admin0.gpkg"), quiet = TRUE)
            if (file.exists(paste0(gis_path, "us_admin1.gpkg"))) a1 <- sf::st_read(paste0(gis_path, "us_admin1.gpkg"), quiet = TRUE)

            template <- bm
        }

        prefix_for_dist <- if (input$region == "US") "us" else tolower(input$region)
        for (sz in c(5, 25, 50, 100, 250, 500)) {
            dist_name <- paste0("dist_", sz, "MWth")
            dist_file <- file.path(gis_path, paste0(prefix_for_dist, "_", dist_name, ".tif"))
            if (file.exists(dist_file)) {
                processed_layers[[dist_name]] <- terra::rast(dist_file)
            }
        }

        list(layers = processed_layers, template = template, admin0 = a0, admin1 = a1, gis_dir = gis_path, region_id = prefix_for_dist)
    })
    # Reactive values to modify params based on inputs
    params_r <- reactive({
        p <- set_scenario(region = input$region)
        p$c_price <- input$c_price
        p$discount_rate <- input$discount_rate / 100
        p$bc_ag_value <- input$bc_ag_value
        p$bc_valuation_method <- input$bc_valuation_method

        # Pass Region for Transport Cost Factors
        p$region <- input$region
        p$allow_eor <- input$allow_eor



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

            bes_sz <- if (!is.null(input$bes_plant_mw) && !is.na(input$bes_plant_mw)) input$bes_plant_mw else 50
            beccs_sz <- if (!is.null(input$beccs_plant_mw) && !is.na(input$beccs_plant_mw)) input$beccs_plant_mw else 250
            bebcs_sz <- if (!is.null(input$bebcs_plant_mw) && !is.na(input$bebcs_plant_mw)) input$bebcs_plant_mw else 50

            curr_params$plant_mw_th <- c(BES = bes_sz, BECCS = beccs_sz, BEBCS = bebcs_sz)
            curr_params$optimize_scale <- input$optimize_scale

            # 1. BES (Standard Radius: 50km)
            message(Sys.time(), " - Starting BES...")
            incProgress(0.1, detail = "Calculating BES...")
            bes_res <- run_spatial_tea(template, curr_params, processed_layers, fun = calculate_bes, region = dat$region_id, gis_dir = dat$gis_dir)

            # 2. BECCS (Large Radius: 100km to leverage economies of scale against CCS cost)
            message(Sys.time(), " - Starting BECCS...")
            incProgress(0.4, detail = "Calculating BECCS...")
            beccs_res <- run_spatial_tea(template, curr_params, processed_layers, fun = calculate_beccs, region = dat$region_id, gis_dir = dat$gis_dir)

            # 3. BEBCS (Distributed Radius: 50km)
            message(Sys.time(), " - Starting BEBCS...")
            incProgress(0.7, detail = "Calculating BEBCS...")
            bebcs_res <- run_spatial_tea(template, curr_params, processed_layers, fun = calculate_bebcs, region = dat$region_id, gis_dir = dat$gis_dir)

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
                ts_cost = ts_cost_layer,
                admin0 = dat$admin0,
                admin1 = dat$admin1
            )
        }) # End Progress
    })

    # Event: Run Fig 3 Button Clicked
    observeEvent(input$run_fig3_btn, {
        withProgress(message = "Generating Evaporation Maps...", value = 0, {
            dat <- data_r()

            c_prices <- c(input$cp_range[1], input$cp_range[2])
            d_rates <- c(input$dr_range[1], (input$dr_range[1] + input$dr_range[2]) / 2, input$dr_range[2]) / 100

            all_df <- data.frame()

            total_runs <- length(c_prices) * length(d_rates)
            run_count <- 0

            for (cp in c_prices) {
                for (dr in d_rates) {
                    run_count <- run_count + 1
                    incProgress(1 / total_runs, detail = paste0("Running DR: ", dr * 100, "%, CP: $", cp))

                    p <- set_scenario(region = input$region)
                    p$c_price <- cp
                    p$discount_rate <- dr
                    p$region <- input$region
                    p$bc_valuation_method <- "advanced_mechanistic"

                    p$plant_mw_th <- 50
                    p$optimize_scale <- TRUE

                    # Run spatial TEA
                    bes <- run_spatial_tea(dat$template, p, dat$layers, fun = calculate_bes, region = dat$region_id, gis_dir = dat$gis_dir)
                    beccs <- run_spatial_tea(dat$template, p, dat$layers, fun = calculate_beccs, region = dat$region_id, gis_dir = dat$gis_dir)
                    bebcs <- run_spatial_tea(dat$template, p, dat$layers, fun = calculate_bebcs, region = dat$region_id, gis_dir = dat$gis_dir)

                    net_stack <- c(bes[["Net_Value_USD"]], beccs[["Net_Value_USD"]], bebcs[["Net_Value_USD"]])
                    names(net_stack) <- c("BES", "BECCS", "BEBCS")

                    opt_idx <- terra::app(net_stack, which.max)

                    df <- terra::as.data.frame(opt_idx, xy = TRUE, na.rm = TRUE)
                    names(df)[3] <- "opt_tech"
                    tech_levels <- c("1" = "BES", "2" = "BECCS", "3" = "BEBCS")
                    df$tech <- tech_levels[as.character(df$opt_tech)]
                    df$dr_label <- paste0("Discount Rate: ", dr * 100, "%")
                    df$cp_label <- paste0("Carbon Price: $", cp, "/t")
                    all_df <- bind_rows(all_df, df)
                }
            }

            dr_levels <- paste0("Discount Rate: ", d_rates * 100, "%")
            all_df$dr_label <- factor(all_df$dr_label, levels = dr_levels)

            cp_levels <- paste0("Carbon Price: $", c_prices, "/t")
            all_df$cp_label <- factor(all_df$cp_label, levels = cp_levels)

            rv$fig3_data <- all_df
        })
    })

    output$fig3_plot <- renderPlot({
        req(rv$fig3_data)
        dat <- data_r()

        plt <- ggplot() +
            geom_tile(data = rv$fig3_data, aes(x = x, y = y, fill = tech))
        if (!is.null(dat$admin0)) {
            plt <- plt + geom_sf(data = dat$admin0, fill = NA, color = "black", linewidth = 0.5)
        }
        if (!is.null(dat$admin1)) {
            plt <- plt + geom_sf(data = dat$admin1, fill = NA, color = "black", linetype = "dotted", linewidth = 0.2)
        }

        plt +
            coord_sf(crs = 4326) +
            scale_fill_manual(values = c("BES" = "#1f77b4", "BECCS" = "#d62728", "BEBCS" = "#2ca02c")) +
            facet_grid(cp_label ~ dr_label) +
            theme_void(base_size = 14) +
            theme(
                strip.text = element_text(face = "bold", margin = margin(b = 5, t = 5)),
                legend.position = "bottom"
            ) +
            labs(fill = "Optimal Technology", title = paste0("Financial Gravity: Evaporation of BECCS - ", input$region))
    })

    output$map_plot <- renderPlot({
        req(rv$map_data)

        # Setup 3x2 layout
        # par(mfrow = c(3, 2))
        par(mfrow = c(2, 2))

        # Calculate common range for Net Value maps
        common_range <- range(minmax(rv$map_data$net_stack))

        # Helper to overlay borders
        add_borders <- function() {
            if (!is.null(rv$map_data$admin0)) plot(sf::st_geometry(rv$map_data$admin0), add = TRUE, lwd = 2, border = "black")
            if (!is.null(rv$map_data$admin1)) plot(sf::st_geometry(rv$map_data$admin1), add = TRUE, lwd = 0.5, lty = "dashed", border = "black")
        }

        # Row 1: BES & BECCS
        terra::plot(rv$map_data$net_stack[["BES"]], main = "BES Net Value ($)", range = common_range)
        add_borders()
        terra::plot(rv$map_data$net_stack[["BECCS"]], main = "BECCS Net Value ($)", range = common_range)
        add_borders()

        # Row 2: BEBCS & Optimal
        terra::plot(rv$map_data$net_stack[["BEBCS"]], main = "BEBCS Net Value ($)", range = common_range)
        add_borders()

        # Optimal Technology (RGB)
        terra::plot(rv$map_data$opt_colorize, main = "Optimal Tech (Sat = Excess Value)", legend = FALSE)
        add_borders()
        legend("topright",
            legend = c("BES", "BECCS", "BEBCS"),
            fill = c("blue", "red", "green"), bg = "white",
            xpd = TRUE, inset = 0.01
        )

        # Row 3: Transport Cost & Spare
        # Using a distinct color palette
        # terra::plot(rv$map_data$ts_cost, main = "BECCS Transport Cost ($/Mg)", col = terra::map.pal("viridis", 100))
        # Empty plot for the spare slot (optional, or just leave blank)
        # plot.new()
    })
}

# Run App
options(shiny.host = "0.0.0.0")
options(shiny.port = 8100)
shinyApp(ui = ui, server = server)

### Content of file scripts/spatial_sensitivity.R ###
# scripts/run_spatial_sensitivity.R
# Runs spatial Techno-Economic Assessment (TEA) on a selected scenario.
# Extracts spatial input layers and detailed NPV breakdowns for all active cells.

library(terra)
library(dplyr)
library(tidyr)

# Sourcing script for helper load function and package loading
source("scripts/manuscript_figures.R")

# --- Configuration ---
SCENARIO_NAME <- "CP100_MW250"
OUTPUT_FILE <- "results/spatial_sensitivity_results.csv"

message("Starting Spatial Sensitivity Analysis...")
message("Selected Scenario: ", SCENARIO_NAME)

# 1. Load Parameter Definitions & Scenario
if (SCENARIO_NAME %in% names(BiocharAG::scenarios)) {
  overrides <- BiocharAG::scenarios[[SCENARIO_NAME]]
  params <- BiocharAG::set_scenario(scenario = overrides)
  message("Successfully loaded scenario overrides.")
} else {
  stop("Scenario '", SCENARIO_NAME, "' not found in BiocharAG::scenarios.")
}

# Resolve general scenario parameters
c_price <- if (!is.null(params$c_price)) params$c_price else 150
tort <- if (!is.null(params$tortuosity)) params$tortuosity else 1.3
tf <- if (!is.null(params$bm_transport_fixed)) params$bm_transport_fixed else 5.0
tv <- if (!is.null(params$bm_transport_var)) params$bm_transport_var else 0.15
trans_em_factor <- if (!is.null(params$transport_emissions_factor)) params$transport_emissions_factor else 0.0001

# Function to run technology evaluation cell-by-cell using vectorization
evaluate_tech_vectorized <- function(tech_fun, tech_name, base_params, spatial_layers, cell_area, region_name) {
  optimize_scale <- if (!is.null(base_params$optimize_scale)) base_params$optimize_scale else FALSE
  plant_sizes_mw_th <- if (!is.null(base_params$plant_sizes_mw_th)) base_params$plant_sizes_mw_th else c(5, 25, 50, 100, 250, 500)

  p <- base_params
  p$region <- region_name

  # Inject spatial layers (as vectors)
  if ("soil_temp" %in% names(spatial_layers)) p$soil_temp <- spatial_layers$soil_temp
  if ("elec_price" %in% names(spatial_layers)) {
    factor <- if (!is.null(p$wholesale_discount_factor)) p$wholesale_discount_factor else 0.4
    p$elec_price <- spatial_layers$elec_price * factor
  }
  if ("soil_ph" %in% names(spatial_layers)) p$soil_ph <- spatial_layers$soil_ph
  if ("soil_cec" %in% names(spatial_layers)) p$soil_cec <- spatial_layers$soil_cec
  if ("dist_sink_km" %in% names(spatial_layers)) p$dist_sink_km <- spatial_layers$dist_sink_km
  if ("dist_sink_saline_km" %in% names(spatial_layers)) p$dist_sink_saline_km <- spatial_layers$dist_sink_saline_km
  if ("sink_is_offshore" %in% names(spatial_layers)) p$sink_is_offshore <- spatial_layers$sink_is_offshore
  if ("ff_c_intensity" %in% names(spatial_layers)) p$ff_c_intensity <- spatial_layers$ff_c_intensity

  for (layer_name in c("cn_weather_risk", "cn_expansion_risk", "eu_base_eur", "us_base_cost")) {
    if (layer_name %in% names(spatial_layers)) p[[layer_name]] <- spatial_layers[[layer_name]]
  }

  if (optimize_scale) {
    results_by_size <- list()
    npv_matrix <- matrix(NA, nrow = length(spatial_layers$biomass_density), ncol = length(plant_sizes_mw_th))

    for (i in seq_along(plant_sizes_mw_th)) {
      sz <- plant_sizes_mw_th[i]
      p_sz <- p
      p_sz$plant_mw_th <- sz

      dist_layer_name <- paste0("dist_", sz, "MWth")
      if (dist_layer_name %in% names(spatial_layers)) {
        p_sz$avg_dist <- spatial_layers[[dist_layer_name]]
      } else {
        stop("Missing spatial distance layer: ", dist_layer_name)
      }

      p_sz$feedstock_cost <- BiocharAG::calculate_regional_feedstock_cost(
        region_name,
        p_sz
      )

      res <- tech_fun(p_sz)
      results_by_size[[i]] <- res
      npv_matrix[, i] <- res$net_value
    }

    opt_size_idx <- max.col(npv_matrix, ties.method = "first")
    opt_size_idx[rowSums(is.na(npv_matrix)) == ncol(npv_matrix)] <- NA

    n_cells <- length(spatial_layers$biomass_density)
    out_res <- list()
    res_names <- names(results_by_size[[1]])

    for (name in res_names) {
      val1 <- results_by_size[[1]][[name]]
      if (length(val1) == 1 && is.na(val1)) {
        out_res[[name]] <- rep(NA, n_cells)
      } else if (length(val1) == 1) {
        out_res[[name]] <- rep(val1, n_cells)
      } else {
        out_res[[name]] <- rep(NA, n_cells)
      }
    }
    out_res[["plant_mw_th_chosen"]] <- rep(NA, n_cells)
    out_res[["avg_dist_chosen"]] <- rep(NA, n_cells)

    for (i in seq_along(plant_sizes_mw_th)) {
      cells_mask <- !is.na(opt_size_idx) & (opt_size_idx == i)
      if (any(cells_mask)) {
        for (name in res_names) {
          val <- results_by_size[[i]][[name]]
          if (length(val) > 1) {
            out_res[[name]][cells_mask] <- val[cells_mask]
          } else if (!is.na(val)) {
            out_res[[name]][cells_mask] <- val
          }
        }
        dist_layer_name <- paste0("dist_", plant_sizes_mw_th[i], "MWth")
        out_res[["plant_mw_th_chosen"]][cells_mask] <- plant_sizes_mw_th[i]
        out_res[["avg_dist_chosen"]][cells_mask] <- spatial_layers[[dist_layer_name]][cells_mask]
      }
    }
    return(out_res)
  } else {
    sz <- max(5, round(BiocharAG:::resolve_plant_mw_th(p$plant_mw_th, tech_name) / 5) * 5)
    p$plant_mw_th <- sz

    dist_layer_name <- paste0("dist_", sz, "MWth")
    if (dist_layer_name %in% names(spatial_layers)) {
      p$avg_dist <- spatial_layers[[dist_layer_name]]
    } else {
      stop("Missing spatial distance layer: ", dist_layer_name)
    }

    p$feedstock_cost <- BiocharAG::calculate_regional_feedstock_cost(
      region_name,
      p
    )

    res <- tech_fun(p)
    res[["plant_mw_th_chosen"]] <- rep(sz, length(spatial_layers$biomass_density))
    res[["avg_dist_chosen"]] <- p$avg_dist
    return(res)
  }
}

regions <- c("US", "China", "Europe", "India")
all_regions_results <- list()

for (r in regions) {
  message("Processing region: ", r)
  dat <- load_region_data(r)
  layers <- dat$layers

  # Identify active cells
  bm_vals <- terra::values(layers$biomass_density, mat = FALSE)
  active_indices <- which(!is.na(bm_vals) & bm_vals > 0)

  # Coordinates of active cells
  coords <- terra::xyFromCell(layers$biomass_density, active_indices)

  # Cell area in km2
  cell_area_raster <- terra::cellSize(layers$biomass_density, unit = "km")
  cell_area_vals <- terra::values(cell_area_raster, mat = FALSE)[active_indices]

  # Extract values of all layers for active indices
  spatial_layers <- list()
  for (layer_name in names(layers)) {
    vals <- terra::values(layers[[layer_name]], mat = FALSE)
    if (is.matrix(vals)) {
      spatial_layers[[layer_name]] <- vals[active_indices, 1]
    } else {
      spatial_layers[[layer_name]] <- vals[active_indices]
    }
  }

  message("  Running competitive vectorized spatial TEA...")
  res_bes <- evaluate_tech_vectorized(BiocharAG::calculate_bes, "BES", params, spatial_layers, cell_area_vals, r)
  res_beccs <- evaluate_tech_vectorized(BiocharAG::calculate_beccs, "BECCS", params, spatial_layers, cell_area_vals, r)
  res_bebcs <- evaluate_tech_vectorized(BiocharAG::calculate_bebcs, "BEBCS", params, spatial_layers, cell_area_vals, r)

  message("  Calculating economic metrics and breakdowns...")
  # Logistics transport and feedstock calculations
  effective_dist_bes <- res_bes$avg_dist_chosen * tort
  logistics_cost_bes <- tf + (tv * effective_dist_bes)
  feedstock_cost_bes <- res_bes$biomass_cost_mg - logistics_cost_bes

  effective_dist_beccs <- res_beccs$avg_dist_chosen * tort
  logistics_cost_beccs <- tf + (tv * effective_dist_beccs)
  feedstock_cost_beccs <- res_beccs$biomass_cost_mg - logistics_cost_beccs

  effective_dist_bebcs <- res_bebcs$avg_dist_chosen * tort
  logistics_cost_bebcs <- tf + (tv * effective_dist_bebcs)
  feedstock_cost_bebcs <- res_bebcs$biomass_cost_mg - logistics_cost_bebcs

  # Carbon abatement components
  ff_ci_vals <- if ("ff_c_intensity" %in% names(spatial_layers)) spatial_layers$ff_c_intensity else params$ff_c_intensity

  c_displaced_bes <- res_bes$energy_output * ff_ci_vals
  c_displaced_beccs <- res_beccs$energy_output * ff_ci_vals
  c_displaced_bebcs <- res_bebcs$energy_output * ff_ci_vals

  carbon_transport_emissions_cost_bes <- effective_dist_bes * trans_em_factor * c_price
  carbon_transport_emissions_cost_beccs <- effective_dist_beccs * trans_em_factor * c_price
  carbon_transport_emissions_cost_bebcs <- effective_dist_bebcs * trans_em_factor * c_price

  # 1. NPV under scenario carbon price
  npv_bes <- res_bes$net_value
  npv_beccs <- res_beccs$net_value
  npv_bebcs <- res_bebcs$net_value

  # 2. NPV at C=0
  npv0_bes <- npv_bes - c_price * res_bes$tot_c_abatement
  npv0_beccs <- npv_beccs - c_price * res_beccs$tot_c_abatement
  npv0_bebcs <- npv_bebcs - c_price * res_bebcs$tot_c_abatement

  # 3. Break-even carbon price per technology
  breakeven_cprice_bes <- ifelse(res_bes$tot_c_abatement <= 0, NA, -npv0_bes / res_bes$tot_c_abatement)
  breakeven_cprice_beccs <- ifelse(res_beccs$tot_c_abatement <= 0, NA, -npv0_beccs / res_beccs$tot_c_abatement)
  breakeven_cprice_bebcs <- ifelse(res_bebcs$tot_c_abatement <= 0, NA, -npv0_bebcs / res_bebcs$tot_c_abatement)

  # 4. Minimum break-even carbon price across the technologies
  c_matrix <- cbind(breakeven_cprice_bes, breakeven_cprice_beccs, breakeven_cprice_bebcs)
  min_breakeven_cprice <- apply(c_matrix, 1, function(row) {
    if (all(is.na(row))) {
      return(NA)
    }
    min(row, na.rm = TRUE)
  })

  # 5. Best Technology
  npv_matrix <- cbind(npv_bes, npv_beccs, npv_bebcs)
  best_idx <- max.col(npv_matrix, ties.method = "first")
  best_idx[rowSums(is.na(npv_matrix)) == 3] <- NA
  best_technology <- c("BES", "BECCS", "BEBCS")[best_idx]

  # Build region dataframe
  region_df <- data.frame(
    x = coords[, 1],
    y = coords[, 2],
    region = r,
    cell_area_km2 = cell_area_vals,

    # Spatial Inputs
    biomass_density = spatial_layers$biomass_density,
    soil_temp = if ("soil_temp" %in% names(spatial_layers)) spatial_layers$soil_temp else NA,
    elec_price = if ("elec_price" %in% names(spatial_layers)) spatial_layers$elec_price else NA,
    dist_sink_km = if ("dist_sink_km" %in% names(spatial_layers)) spatial_layers$dist_sink_km else NA,
    dist_sink_saline_km = if ("dist_sink_saline_km" %in% names(spatial_layers)) spatial_layers$dist_sink_saline_km else NA,
    sink_is_offshore = if ("sink_is_offshore" %in% names(spatial_layers)) spatial_layers$sink_is_offshore else NA,
    soil_ph = if ("soil_ph" %in% names(spatial_layers)) spatial_layers$soil_ph else NA,
    soil_cec = if ("soil_cec" %in% names(spatial_layers)) spatial_layers$soil_cec else NA,
    ff_c_intensity = ff_ci_vals,

    # Results
    min_breakeven_cprice = min_breakeven_cprice,
    best_technology = best_technology,

    # BES Metrics
    npv_BES = npv_bes,
    breakeven_cprice_BES = breakeven_cprice_bes,
    biomass_transport_cost_BES = logistics_cost_bes,
    feedstock_cost_BES = feedstock_cost_bes,
    capex_BES = res_bes$capital_cost_mg,
    opex_BES = res_bes$om_cost_mg,
    energy_revenue_BES = res_bes$energy_revenue_mg,
    fossil_fuel_offset_revenue_BES = c_displaced_bes * c_price,
    cdr_revenue_BES = 0,
    co2_transport_storage_cost_BES = 0,
    carbon_transport_emissions_cost_BES = carbon_transport_emissions_cost_bes,

    # BECCS Metrics
    npv_BECCS = npv_beccs,
    breakeven_cprice_BECCS = breakeven_cprice_beccs,
    biomass_transport_cost_BECCS = logistics_cost_beccs,
    feedstock_cost_BECCS = feedstock_cost_beccs,
    capex_BECCS = res_beccs$capital_cost_mg,
    opex_BECCS = res_beccs$om_cost_mg,
    energy_revenue_BECCS = res_beccs$energy_revenue_mg,
    fossil_fuel_offset_revenue_BECCS = c_displaced_beccs * c_price,
    cdr_revenue_BECCS = res_beccs$c_sequestered * c_price,
    co2_transport_storage_cost_BECCS = res_beccs$ts_cost,
    carbon_transport_emissions_cost_BECCS = carbon_transport_emissions_cost_beccs,

    # BEBCS Metrics
    npv_BEBCS = npv_bebcs,
    breakeven_cprice_BEBCS = breakeven_cprice_bebcs,
    biomass_transport_cost_BEBCS = logistics_cost_bebcs,
    feedstock_cost_BEBCS = feedstock_cost_bebcs,
    capex_BEBCS = res_bebcs$capital_cost_mg,
    opex_BEBCS = res_bebcs$om_cost_mg,
    energy_revenue_BEBCS = res_bebcs$energy_revenue_mg,
    fossil_fuel_offset_revenue_BEBCS = c_displaced_bebcs * c_price,
    cdr_revenue_BEBCS = res_bebcs$c_sequestered * c_price,
    agronomic_revenue_BEBCS = res_bebcs$agronomic_revenue_mg,
    soil_ghg_offset_revenue_BEBCS = 0.1 * c_price,
    co2_transport_storage_cost_BEBCS = 0,
    carbon_transport_emissions_cost_BEBCS = carbon_transport_emissions_cost_bebcs,
    stringsAsFactors = FALSE
  )

  all_regions_results[[r]] <- region_df
}

# Combine and save results
results_df <- do.call(rbind, all_regions_results)
dir.create("results", showWarnings = FALSE)
write.csv(results_df, OUTPUT_FILE, row.names = FALSE)
message("Spatial Sensitivity Analysis Complete. Results saved to: ", OUTPUT_FILE)

### Content of file scripts/spatial_shap.R ###
# scripts/run_spatial_shap.R
# XGBoost multi-class classification and SHAP attribution for spatial sensitivity results.
# Predicts optimal technology per location from spatial features and maps dominant SHAP drivers.

library(data.table)
library(dplyr)
library(xgboost)
library(shapviz)
library(ggplot2)
library(patchwork)
library(terra)
library(sf)

source("scripts/manuscript_figures.R")

INPUT_FILE <- "results/spatial_sensitivity_results.csv"
OUTPUT_CSV <- "results/spatial_shap_values_by_location.csv"

message("Loading spatial sensitivity results from: ", INPUT_FILE)
df <- fread(INPUT_FILE, data.table = FALSE)

# Filter to active viable cells where best_technology is defined
df_valid <- df %>% filter(!is.na(best_technology))

if (nrow(df_valid) == 0) {
  stop("No valid cells found in spatial sensitivity results.")
}

message("Total viable cells across all regions: ", nrow(df_valid))

# Define spatial features to use as predictors
features <- c(
  "biomass_density", "soil_temp", "elec_price", "dist_sink_km",
  "dist_sink_saline_km", "sink_is_offshore", "soil_ph", "soil_cec", "ff_c_intensity"
)

features <- intersect(features, names(df_valid))

# Prepare feature matrix X and target y
X_df <- df_valid[, features, drop = FALSE]

# Drop constant columns if any
variances <- sapply(X_df, var, na.rm = TRUE)
constant_cols <- names(variances)[is.na(variances) | variances <= 1e-8]
if (length(constant_cols) > 0) {
  message("Dropping constant predictors: ", paste(constant_cols, collapse = ", "))
  features <- setdiff(features, constant_cols)
  X_df <- X_df[, features, drop = FALSE]
}

X_mat <- as.matrix(X_df)

class_levels <- sort(unique(df_valid$best_technology))
y <- as.numeric(factor(df_valid$best_technology, levels = class_levels)) - 1

message("Training multi-class XGBoost classification tree...")
message("Target classes: ", paste(class_levels, collapse = ", "))

dtrain <- xgb.DMatrix(data = X_mat, label = y)

params <- list(
  objective = "multi:softprob",
  num_class = length(class_levels),
  max_depth = 6,
  eta = 0.05,
  nthread = 1
)

set.seed(42)
model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 120,
  verbose = 0
)

# Evaluate training accuracy
preds_prob <- predict(model, X_mat, reshape = TRUE)
preds_class <- class_levels[max.col(preds_prob, ties.method = "first")]
acc <- mean(preds_class == df_valid$best_technology)
message(sprintf("XGBoost Training Accuracy: %.2f%%", acc * 100))

message("Calculating SHAP values using shapviz...")
shp <- shapviz(model, X_pred = X_mat)
names(shp) <- class_levels

dir.create("results", showWarnings = FALSE)

# 1. Generate per-class SHAP Beeswarm Plots
message("Saving SHAP Beeswarm Plots...")
for (cls in class_levels) {
  p_bee <- sv_importance(shp[[cls]], kind = "beeswarm") +
    theme_bw(base_size = 12) +
    labs(
      title = paste0("SHAP Feature Importance for Predicting ", cls),
      x = "SHAP Value (Impact on Log-Odds of Class Prediction)"
    ) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))

  out_png <- sprintf("results/spatial_shap_beeswarm_%s.png", cls)
  ggsave(out_png, plot = p_bee, width = 8, height = 6, dpi = 300)
}

# 2. Extract cell-by-cell SHAP metrics for each location
message("Extracting location-level dominant SHAP features and maximum SHAP values...")
n_cells <- nrow(df_valid)
dominant_feat_winning <- character(n_cells)
max_shap_winning <- numeric(n_cells)
dominant_feat_overall <- character(n_cells)
max_shap_overall <- numeric(n_cells)

# Pre-extract SHAP matrices for speed
shap_matrices <- lapply(class_levels, function(cls) shp[[cls]]$S)
names(shap_matrices) <- class_levels

for (i in seq_len(n_cells)) {
  winning_cls <- df_valid$best_technology[i]
  S_win <- shap_matrices[[winning_cls]][i, ]

  idx_win <- which.max(abs(S_win))
  dominant_feat_winning[i] <- features[idx_win]
  max_shap_winning[i] <- S_win[idx_win]

  # Across all classes
  abs_all <- sapply(shap_matrices, function(M) abs(M[i, ]))
  # Find row (feature) with overall maximum absolute SHAP
  max_per_feat <- apply(abs_all, 1, max)
  idx_overall <- which.max(max_per_feat)
  dominant_feat_overall[i] <- features[idx_overall]
  max_shap_overall[i] <- max_per_feat[idx_overall]
}

df_shap_loc <- df_valid %>%
  select(x, y, region, best_technology) %>%
  mutate(
    dominant_feature_winning_class = dominant_feat_winning,
    max_shap_value_winning_class = max_shap_winning,
    dominant_feature_overall = dominant_feat_overall,
    max_shap_magnitude_overall = max_shap_overall
  )

write.csv(df_shap_loc, OUTPUT_CSV, row.names = FALSE)
message("Saved location-level SHAP table to: ", OUTPUT_CSV)

# 3. Plot Multi-Region Spatial Maps of Largest SHAP Value by Location
message("Generating multi-region spatial maps...")

regions <- c("US", "Europe", "China", "India")

# Color palette for features
feature_palette <- c(
  "biomass_density" = "#2ca02c",
  "soil_temp" = "#d62728",
  "elec_price" = "#1f77b4",
  "dist_sink_km" = "#9467bd",
  "dist_sink_saline_km" = "#8c564b",
  "sink_is_offshore" = "#e377c2",
  "soil_ph" = "#7f7f7f",
  "soil_cec" = "#bcbd22",
  "ff_c_intensity" = "#ff7f0e"
)

# Function to plot a 4-panel stitched map for categorical dominant feature
plot_dominant_feature_map <- function(data_loc, var_name, title_text, out_path) {
  plots <- list()
  for (r in regions) {
    reg_df <- data_loc %>% filter(region == r)
    dat <- load_region_data(r)

    p <- ggplot() +
      geom_tile(data = reg_df, aes(x = x, y = y, fill = .data[[var_name]]))

    if (!is.null(dat$admin0)) {
      p <- p + geom_sf(data = dat$admin0, fill = NA, color = "black", linewidth = 0.4)
    }

    p <- p +
      coord_sf(crs = 4326) +
      scale_fill_manual(
        name = "Dominant SHAP Predictor",
        values = feature_palette,
        na.value = "grey80"
      ) +
      theme_void(base_size = 11) +
      labs(subtitle = r) +
      theme(
        plot.subtitle = element_text(face = "bold", hjust = 0.5, margin = margin(b = 4))
      )

    plots[[r]] <- p
  }

  combined <- patchwork::wrap_plots(plots, ncol = 2) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = title_text,
      theme = theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.position = "bottom"
      )
    )

  ggsave(out_path, plot = combined, width = 12, height = 9, dpi = 300)
  message("Saved categorical dominant SHAP map to: ", out_path)
}

# Function to plot a 4-panel stitched map for numeric SHAP magnitude
plot_shap_magnitude_map <- function(data_loc, var_name, title_text, out_path) {
  plots <- list()
  for (r in regions) {
    reg_df <- data_loc %>% filter(region == r)
    dat <- load_region_data(r)

    p <- ggplot() +
      geom_tile(data = reg_df, aes(x = x, y = y, fill = abs(.data[[var_name]])))

    if (!is.null(dat$admin0)) {
      p <- p + geom_sf(data = dat$admin0, fill = NA, color = "black", linewidth = 0.4)
    }

    p <- p +
      coord_sf(crs = 4326) +
      scale_fill_viridis_c(
        name = "|SHAP Value|",
        option = "inferno",
        direction = 1
      ) +
      theme_void(base_size = 11) +
      labs(subtitle = r) +
      theme(
        plot.subtitle = element_text(face = "bold", hjust = 0.5, margin = margin(b = 4))
      )

    plots[[r]] <- p
  }

  combined <- patchwork::wrap_plots(plots, ncol = 2) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = title_text,
      theme = theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.position = "bottom"
      )
    )

  ggsave(out_path, plot = combined, width = 12, height = 9, dpi = 300)
  message("Saved numeric SHAP magnitude map to: ", out_path)
}

# Generate Maps
plot_dominant_feature_map(
  df_shap_loc,
  "dominant_feature_winning_class",
  "Dominant Spatial Driver of Optimal Technology (Largest SHAP Feature by Location)",
  "results/map_dominant_shap_feature.png"
)

plot_shap_magnitude_map(
  df_shap_loc,
  "max_shap_value_winning_class",
  "Magnitude of Strongest Spatial Driver (|SHAP Value| of Dominant Feature by Location)",
  "results/map_max_shap_magnitude.png"
)

message("XGBoost + SHAP spatial analysis and mapping complete!")

### Content of file scripts/test_rasters.R ###
source("manuscript_figures.R")

check_biomass_rasters <- function() {
  regions <- c("US", "China", "Europe", "India")
  
  cat("=== BIOMASS RASTER DIAGNOSTICS ===\n")
  for (r in regions) {
    dat <- load_region_data(r)
    bm_raster <- dat$layers$biomass_density
    
    # Extract active values
    vals <- terra::values(bm_raster, mat = FALSE)
    active_vals <- vals[!is.na(vals) & vals > 0]
    
    # Check for unit scaling (Max should be hundreds, not single digits)
    max_val <- max(active_vals, na.rm = TRUE)
    mean_val <- mean(active_vals, na.rm = TRUE)
    
    # Check for smearing (What % of active cells have near-zero density?)
    # E.g., less than 10 Mg/km2 (0.1 Mg/ha)
    ghost_cells <- sum(active_vals < 10) / length(active_vals)
    
    cat(sprintf("%-10s | Max: %8.2f | Mean: %8.2f | Cells < 10 Mg/km2: %5.1f%%\n", 
                r, max_val, mean_val, ghost_cells * 100))
  }
}
check_biomass_rasters()

