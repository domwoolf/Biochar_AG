#' Raster-Aware Conditional Element Selection (ifelse)
#'
#' Internal helper that delegates to terra::ifel if the test is a SpatRaster,
#' handles single logical scalar tests directly, and delegates to base::ifelse otherwise.
#'
#' @keywords internal
ifelse_raster <- function(test, yes, no) {
    if (inherits(test, "SpatRaster")) {
        terra::ifel(test, yes, no)
    } else if (is.logical(test) && length(test) == 1) {
        if (is.na(test)) {
            NA
        } else if (test) {
            yes
        } else {
            no
        }
    } else {
        ifelse(test, yes, no)
    }
}

#' Raster-Aware Parallel Minimum (pmin)
#'
#' Internal helper that delegates to terra::min if either input is a SpatRaster,
#' ensuring correct S3 dispatch by placing the SpatRaster first. Delegates to
#' base::pmin otherwise.
#'
#' @keywords internal
pmin_raster <- function(x, y) {
    if (inherits(x, "SpatRaster")) {
        min(x, y)
    } else if (inherits(y, "SpatRaster")) {
        min(y, x)
    } else {
        pmin(x, y)
    }
}

#' Raster-Aware Parallel Maximum (pmax)
#'
#' Internal helper that delegates to terra::max if either input is a SpatRaster,
#' ensuring correct S3 dispatch by placing the SpatRaster first. Delegates to
#' base::pmax otherwise.
#'
#' @keywords internal
pmax_raster <- function(x, y) {
    if (inherits(x, "SpatRaster")) {
        max(x, y)
    } else if (inherits(y, "SpatRaster")) {
        max(y, x)
    } else {
        pmax(x, y)
    }
}

#' Load Region Spatial Data and Pre-Extract 1D Vectors
#'
#' Loads GIS raster layers and administrative boundaries for a given region,
#' and pre-extracts 1D vectors for active grid cells to enable fast vectorized TEA calculations.
#'
#' @param region_name Character string ("US", "China", "Europe", "India").
#' @param gis_path Optional path to GIS/processed/ directory.
#' @return A list containing `template`, `layers`, `admin0`, `admin1`, and `vec`.
#' @export
load_region_data <- function(region_name, gis_path = NULL) {
    if (is.null(gis_path)) {
        candidates <- c("../GIS/processed/", "GIS/processed/", "/media/dominic/Data/git/Biochar_AG/GIS/processed/")
        for (cand in candidates) {
            if (dir.exists(cand)) {
                gis_path <- cand
                break
            }
        }
        if (is.null(gis_path)) {
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

    p_base <- prefix_map[[region_name, exact = TRUE]][["base", exact = TRUE]]
    p_dist <- prefix_map[[region_name, exact = TRUE]][["dist", exact = TRUE]]

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
        layers[["ff_c_intensity"]] <- ci
    }

    for (sz in c(5, 25, 50, 100, 250, 500, 1000)) {
        dist_name <- paste0("dist_", sz, "MWth")
        dist_file <- file.path(gis_path, paste0(p_dist, "_", dist_name, ".tif"))
        if (file.exists(dist_file)) {
            layers[[dist_name]] <- terra::rast(dist_file)
        }
    }

    # Pre-extract 1D vectors for active indices (biomass_density > 0 and not NA)
    bm_vals <- terra::values(layers[["biomass_density", exact = TRUE]], mat = FALSE)
    active_indices <- which(!is.na(bm_vals) & bm_vals > 0)
    xy_active <- terra::xyFromCell(layers[["biomass_density", exact = TRUE]], active_indices)
    cell_area_raster <- terra::cellSize(layers[["biomass_density", exact = TRUE]], unit = "km")
    cell_area_vals <- terra::values(cell_area_raster, mat = FALSE)[active_indices]

    vec_layers <- list()
    for (layer_name in names(layers)) {
        vals <- terra::values(layers[[layer_name, exact = TRUE]], mat = FALSE)
        if (is.matrix(vals)) {
            vec_layers[[layer_name]] <- vals[active_indices, 1]
        } else {
            vec_layers[[layer_name]] <- vals[active_indices]
        }
    }

    vec_data <- list(
        active_indices = active_indices,
        xy = xy_active,
        cell_area = cell_area_vals,
        layers = vec_layers
    )

    list(template = bm, layers = layers, admin0 = admin0, admin1 = admin1, vec = vec_data)
}

#' Run Scenario Spatial TEA
#'
#' Evaluates spatial TEA across BES, BECCS, and BEBCS for a scenario.
#' If `vec` (pre-extracted 1D spatial vectors) is provided, executes fast vectorized
#' calculations and maps the results onto SpatRaster objects matching `template`.
#' Otherwise, falls back to standard raster-based `run_spatial_tea`.
#'
#' @param template Reference SpatRaster template.
#' @param layers List of spatial layers (SpatRaster objects).
#' @param params Scenario parameter list.
#' @param vec Optional list of pre-extracted 1D spatial vectors from `load_region_data()$vec`.
#' @return A list containing `net` (SpatRaster stack), `abate` (SpatRaster stack), `opt` (SpatRaster), and optionally `vec_res`.
#' @export
run_scenario <- function(template, layers, params, vec = NULL) {
    if (!is.null(vec) && is.list(vec) && !is.null(vec[["active_indices", exact = TRUE]])) {
        spatial_layers <- vec[["layers", exact = TRUE]]
        p <- params

        if ("soil_temp" %in% names(spatial_layers)) p[["soil_temp"]] <- spatial_layers[["soil_temp", exact = TRUE]]
        if ("elec_price" %in% names(spatial_layers)) {
            factor <- if (!is.null(p[["wholesale_discount_factor", exact = TRUE]])) p[["wholesale_discount_factor", exact = TRUE]] else 0.4
            p[["elec_price"]] <- spatial_layers[["elec_price", exact = TRUE]] * factor
        }
        if ("soil_ph" %in% names(spatial_layers)) p[["soil_ph"]] <- spatial_layers[["soil_ph", exact = TRUE]]
        if ("soil_cec" %in% names(spatial_layers)) p[["soil_cec"]] <- spatial_layers[["soil_cec", exact = TRUE]]
        if ("dist_sink_km" %in% names(spatial_layers)) p[["dist_sink_km"]] <- spatial_layers[["dist_sink_km", exact = TRUE]]
        if ("dist_sink_saline_km" %in% names(spatial_layers)) p[["dist_sink_saline_km"]] <- spatial_layers[["dist_sink_saline_km", exact = TRUE]]
        if ("sink_is_offshore" %in% names(spatial_layers)) p[["sink_is_offshore"]] <- spatial_layers[["sink_is_offshore", exact = TRUE]]
        if ("ff_c_intensity" %in% names(spatial_layers)) p[["ff_c_intensity"]] <- spatial_layers[["ff_c_intensity", exact = TRUE]]

        for (layer_name in c("cn_weather_risk", "cn_expansion_risk", "eu_base_eur", "us_base_cost")) {
            if (layer_name %in% names(spatial_layers)) p[[layer_name]] <- spatial_layers[[layer_name, exact = TRUE]]
        }

        sz <- if (!is.null(p[["plant_mw_th", exact = TRUE]])) resolve_plant_mw_th(p[["plant_mw_th", exact = TRUE]], "BES") else 50
        dist_layer_name <- paste0("dist_", sz, "MWth")
        if (dist_layer_name %in% names(spatial_layers)) {
            p[["avg_dist"]] <- spatial_layers[[dist_layer_name, exact = TRUE]]
        }

        feedstock_region <- if (!is.null(p[["region", exact = TRUE]])) p[["region", exact = TRUE]] else "US"
        p[["feedstock_cost"]] <- calculate_regional_feedstock_cost(feedstock_region, p)

        res_bes <- calculate_bes(p)
        res_beccs <- calculate_beccs(p)
        res_bebcs <- calculate_bebcs(p)

        net_matrix <- cbind(res_bes[["net_value", exact = TRUE]], res_beccs[["net_value", exact = TRUE]], res_bebcs[["net_value", exact = TRUE]])
        abate_matrix <- cbind(res_bes[["tot_c_abatement", exact = TRUE]], res_beccs[["tot_c_abatement", exact = TRUE]], res_bebcs[["tot_c_abatement", exact = TRUE]])

        opt_vec <- max.col(net_matrix, ties.method = "first")
        opt_vec[rowSums(is.na(net_matrix)) == 3] <- NA

        active_idx <- vec[["active_indices", exact = TRUE]]

        opt_r <- terra::rast(template, nlyrs = 1, vals = NA)
        opt_r[active_idx] <- opt_vec
        names(opt_r) <- "Optimal_Tech"

        net_stack <- terra::rast(template, nlyrs = 3, vals = NA)
        net_stack[active_idx] <- net_matrix
        names(net_stack) <- c("BES", "BECCS", "BEBCS")

        abate_stack <- terra::rast(template, nlyrs = 3, vals = NA)
        abate_stack[active_idx] <- abate_matrix
        names(abate_stack) <- c("BES", "BECCS", "BEBCS")

        return(list(
            net = net_stack,
            abate = abate_stack,
            opt = opt_r,
            vec_res = list(net = net_matrix, abate = abate_matrix, opt = opt_vec)
        ))
    }

    bes <- run_spatial_tea(template, params, layers, fun = calculate_bes)
    beccs <- run_spatial_tea(template, params, layers, fun = calculate_beccs)
    bebcs <- run_spatial_tea(template, params, layers, fun = calculate_bebcs)

    net_stack <- c(bes[["Net_Value_USD", exact = TRUE]], beccs[["Net_Value_USD", exact = TRUE]], bebcs[["Net_Value_USD", exact = TRUE]])
    names(net_stack) <- c("BES", "BECCS", "BEBCS")

    abate_stack <- c(bes[["Abatement_tCO2", exact = TRUE]], beccs[["Abatement_tCO2", exact = TRUE]], bebcs[["Abatement_tCO2", exact = TRUE]])
    names(abate_stack) <- c("BES", "BECCS", "BEBCS")

    opt_idx <- terra::which.max(net_stack)

    list(net = net_stack, abate = abate_stack, opt = opt_idx)
}
