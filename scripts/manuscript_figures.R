# nolint start: indentation_linter, line_length_linter, object_usage_linter, commented_code_linter
# manuscript_figures.R
# Script to generate publication-quality display items for the
# BiocharAG manuscript.

library(terra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)

sf::sf_use_s2(FALSE)

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
  dat, region_name, save_map = FALSE, save_ai_data = FALSE,
  d_rates = c(0.02, 0.08, 0.15), c_prices = c(30, 100, 150),
  scenario = "default",
  metric = c("optimal_tech", "max_npv", "both")
) {
  metric <- match.arg(metric)
  params <- set_scenario(scenarios[[scenario]], region = region_name)
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

      if (save_ai_data && !is.null(dat$admin1) && requireNamespace("exactextractr", quietly = TRUE)) {
        admin1_polys <- sf::st_as_sf(dat$admin1)
        # Extract mean NPV
        admin1_polys$mean_max_npv <- exactextractr::exact_extract(max_npv_raster, admin1_polys, "mean")
        # Extract mode tech
        admin1_polys$majority_tech <- exactextractr::exact_extract(opt_raster, admin1_polys, "mode")
        admin1_polys$majority_tech_name <- tech_levels[as.character(admin1_polys$majority_tech)]

        # Save to CSV (dropping geometry)
        df_ai <- sf::st_drop_geometry(admin1_polys)
        df_ai$Discount_Rate <- dr
        df_ai$Carbon_Price <- cp

        ai_dir <- paste0(out_dir, "ai_summaries/")
        dir.create(ai_dir, showWarnings = FALSE)
        write.csv(df_ai, paste0(ai_dir, "evaporation_spatial_", region_name, "_DR", dr * 100, "_CP", cp, "_", scenario, ".csv"), row.names = FALSE)
      }
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
generate_fig_macc <- function(save_map = FALSE, save_ai_data = FALSE, scenario = "default") {
  message("Generating Figure: Regional MACC (12-panel)...")

  regions_ordered <- c("US", "China", "Europe", "India")
  all_macc <- list()

  for (r in regions_ordered) {
    dat <- load_region_data(r)
    cell_area <- terra::cellSize(dat$template, unit = "km")

    params <- set_scenario(scenarios[[scenario]], region = r)
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
    "Area" = "Grid-Cell Area Assigned\n(Mha)",
    "Biomass" = "Biomass Converted\n(Mt dry)"
  )
  combined_macc$Metric <- factor(combined_macc$Metric, levels = c("Biomass", "Area", "Abatement"), labels = metric_labels[c("Biomass", "Area", "Abatement")])

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

    if (save_ai_data) {
      ai_dir <- paste0(out_dir, "ai_summaries/")
      dir.create(ai_dir, showWarnings = FALSE)
      write.csv(combined_macc, paste0(ai_dir, "macc_data_", scenario, ".csv"), row.names = FALSE)
    }

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
                                          save_ai_data = FALSE,
                                          scenario = "default") {
  message("Generating Figure: Break-Even Carbon Price Grid...")

  # Ordered regions for columns
  regions_ordered <- c("India", "China", "US", "Europe")

  # Rows definitions
  techs <- c("BES", "BECCS", "BEBCS", "Best_Tech", "Best_C")
  row_labels <- c(
    "BES" = "Bioenergy", "BECCS" = "BECCS", "BEBCS" = "Biochar",
    "Best_Tech" = "Lowest\nBreak-even\nTech.", "Best_C" = "Lowest\nBreak-even\nPrice"
  )

  df_list <- list()
  admin_list <- list()
  df_ai_list <- list()

  for (r in regions_ordered) {
    message("  Processing Region for Fig 8: ", r)
    dat <- load_region_data(r)

    # Prepare parameters
    params <- set_scenario(scenarios[[scenario]], region = r)
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

    # Minimum break-even price across the 3 techs. Note: the technology with the lowest break-even price is
    # not necessarily the one with the highest NPV at a given carbon price (see generate_fig_evaporation).
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

    if (save_ai_data && !is.null(dat$admin1)) {
      admin1_polys <- sf::st_as_sf(dat$admin1)
      admin1_polys$mean_breakeven_c_bes <- exactextractr::exact_extract(full_stack$BES, admin1_polys, "mean")
      admin1_polys$mean_breakeven_c_beccs <- exactextractr::exact_extract(full_stack$BECCS, admin1_polys, "mean")
      admin1_polys$mean_breakeven_c_bebcs <- exactextractr::exact_extract(full_stack$BEBCS, admin1_polys, "mean")
      admin1_polys$mean_lowest_breakeven_c <- exactextractr::exact_extract(full_stack$Best_C, admin1_polys, "mean")

      majority_idx <- exactextractr::exact_extract(full_stack$Best_Tech, admin1_polys, "mode")
      admin1_polys$majority_lowest_breakeven_tech <- c("BES", "BECCS", "BEBCS")[majority_idx]

      df_ai_r <- sf::st_drop_geometry(admin1_polys)
      df_ai_r$Region <- r
      df_ai_list[[r]] <- df_ai_r
    }
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
  }

  if (save_ai_data && length(df_ai_list) > 0) {
    ai_dir <- paste0(out_dir, "ai_summaries/")
    dir.create(ai_dir, showWarnings = FALSE)
    df_ai_all <- dplyr::bind_rows(df_ai_list)
    write.csv(df_ai_all, paste0(ai_dir, "breakeven_data_", scenario, ".csv"), row.names = FALSE)
  }

  if (!save_map) {
    print(combined_plot)
  }
  return(combined_plot)
}

run_all_manuscript_figures <- function(save_map = TRUE, save_ai_data = TRUE) { # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  for (scenario_name in .scenarios) {
    for (r in .regions) {
      dat <- load_region_data(r)
      generate_fig_evaporation(dat, r, save_map, save_ai_data = save_ai_data, scenario = scenario_name)
    }
    generate_fig_macc(save_map, save_ai_data = save_ai_data, scenario = scenario_name)
    generate_fig_breakeven_cprice(save_map, save_ai_data = save_ai_data, scenario = scenario_name)
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
  run_all_manuscript_figures(save_map = TRUE, save_ai_data = TRUE)
}
# nolint end
