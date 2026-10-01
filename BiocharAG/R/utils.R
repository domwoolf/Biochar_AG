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
#' @param transport_version CO2 transport layers to use: "auto" (v2 `<prefix>_transport_layers.tif` if
#'   present, else v1), "v2" or "v1".
#' @return A list containing `template`, `layers`, `admin0`, `admin1`, and `vec`.
#' @export
load_region_data <- function(region_name, gis_path = NULL, transport_version = c("auto", "v2", "v1")) {
  transport_version <- match.arg(transport_version)
  if (is.null(gis_path)) {
    candidates <- c("GIS/processed/", "../GIS/processed/", "/media/dominic/Data/git/Biochar_AG/GIS/processed/")
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

  p_base <- prefix_map[[region_name]][["base"]]
  p_dist <- prefix_map[[region_name]][["dist"]]

  bm <- terra::rast(file.path(gis_path, paste0(p_base, "_biomass.tif"))) # Spatial density of available biomass (Mg/km2) [Source: Karan et al. (2023)]
  st <- terra::rast(file.path(gis_path, paste0(p_base, "_soil_temp.tif"))) # Soil temperature (degrees C) [Source: WorldClim/SBIO1]
  ep <- terra::rast(file.path(gis_path, paste0(p_base, "_elec_price.tif"))) # Wholesale electricity price ($/MWh) [Source: EIA/Eurostat/NDRC/CERC]
  # CO2 transport: v2 route layers (physical length, terrain multiplier, lift, per sink class) take
  # precedence over the v1 least-cost distance layers when both exist
  tl_path <- file.path(gis_path, paste0(p_dist, "_transport_layers.tif"))
  use_v2 <- transport_version != "v1" && file.exists(tl_path)
  if (transport_version == "v2" && !use_v2) stop("v2 transport layers not found: ", tl_path)
  opt_rast <- function(suffix) {
    f <- file.path(gis_path, paste0(p_dist, suffix))
    if (file.exists(f)) terra::rast(f) else NULL
  }
  ds <- opt_rast("_dist_sink.tif") # Distance to nearest CO2 sink (km, v1)
  dss <- opt_rast("_dist_sink_saline.tif") # Distance to nearest saline CO2 sink (km, v1)
  stype <- opt_rast("_sink_type.tif") # Nearest CO2 sink (incl. EOR) is offshore (1/0, v1)
  stype_saline <- opt_rast("_sink_type_saline.tif") # Nearest saline sink is offshore (1/0, v1)
  if (!use_v2 && (is.null(ds) || is.null(dss) || is.null(stype))) {
    stop("No CO2 transport layers for ", region_name, " (need ", basename(tl_path), " or the v1 *_dist_sink*.tif layers)")
  }
  ph <- terra::rast(file.path(gis_path, paste0(p_base, "_soil_ph.tif"))) # Soil pH [Source: ISRIC SoilGrids]
  cec <- terra::rast(file.path(gis_path, paste0(p_base, "_soil_cec.tif"))) # Soil cation exchange capacity (cmolc/kg) [Source: ISRIC SoilGrids]

  ci_path <- file.path(gis_path, paste0(p_base, "_ff_c_intensity.tif"))
  ci <- if (file.exists(ci_path)) terra::rast(ci_path) else NULL # Fossil fuel carbon intensity (tCO2eq/GJ) [Source: IPCC/Ember]

  a0_path <- file.path(gis_path, paste0(p_dist, "_admin0.gpkg"))
  a1_path <- file.path(gis_path, paste0(p_dist, "_admin1.gpkg"))

  # Administrative boundaries level 0 (e.g., countries)
  admin0 <- if (file.exists(a0_path)) {
    sf::st_read(a0_path, quiet = TRUE)
  } else {
    NULL
  }

  # Administrative boundaries level 1 (e.g., states/provinces)
  admin1 <- if (file.exists(a1_path)) {
    sf::st_read(a1_path, quiet = TRUE)
  } else {
    NULL
  }

  # The templates are bounding boxes; keep only biomass inside the region's countries (e.g. drop
  # Canada and Mexico from the US box, Russia and North Africa from the Europe box)
  if (!is.null(admin0)) bm <- terra::mask(bm, terra::vect(admin0))

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
  layers <- layers[!vapply(layers, is.null, logical(1))]

  if (!is.null(ci)) {
    layers[["ff_c_intensity"]] <- ci
  }
  if (!is.null(stype_saline)) {
    layers[["sink_is_offshore_saline"]] <- stype_saline
  }
  # Ship route legs for offshore sinks (pipeline to coast; sea voyage to the nearest / nearest saline sink)
  ship_layers <- c(dist_coast_km = "_dist_coast.tif", dist_sea_km = "_dist_sea.tif", dist_sea_saline_km = "_dist_sea_saline.tif")
  for (nm in names(ship_layers)) {
    f <- file.path(gis_path, paste0(p_dist, ship_layers[[nm]]))
    if (file.exists(f)) layers[[nm]] <- terra::rast(f)
  }
  if (use_v2) {
    tl <- terra::rast(tl_path)
    for (nm in intersect(transport_v2_layer_names(), names(tl))) layers[[nm]] <- tl[[nm]]
    # Storage cost of the sink each route reaches (issue #103): the routing records the sink index
    # (onshore *_target, offshore *_sink) into <prefix>_sinks_lookup.csv; costs come from co2_sinks.
    # NA where the sink is unclassified, so the regional storage parameter applies.
    lk_path <- file.path(gis_path, paste0(p_dist, "_sinks_lookup.csv"))
    sink_cost <- sink_storage_costs(lk_path)
    if (!is.null(sink_cost)) {
      # Storage_Cost is the saline cost; EOR routes (oneor) keep the regional parameter
      for (cls in c("onsal", "offship", "offpipe", "offship_any", "offpipe_any")) {
        idx_nm <- paste0(cls, if (startsWith(cls, "off")) "_sink" else "_target")
        if (!idx_nm %in% names(tl)) next
        layers[[paste0(cls, "_storage_cost")]] <- terra::classify(tl[[idx_nm]], cbind(seq_along(sink_cost), sink_cost), others = NA)
      }
    }
  }

  # Spatial field-side feedstock cost (2024 USD/Mg); built by data-raw/process_eu_feedstock.R (Europe)
  fc_path <- file.path(gis_path, paste0(p_base, "_feedstock_cost.tif"))
  if (file.exists(fc_path)) {
    fc <- terra::rast(fc_path)
    for (nm in names(fc)) layers[[nm]] <- fc[[nm]]
  }

  # Haulage terrain / road-network factors per plant size; built by data-raw/generate_logistics_layers.R
  hf_path <- file.path(gis_path, paste0(p_dist, "_haul_factors.tif"))
  if (file.exists(hf_path)) {
    hf <- terra::rast(hf_path)
    for (nm in names(hf)) layers[[paste0("haul_", nm)]] <- hf[[nm]]
  }

  # Biomass collection distance to satisfy each plant size (km); built by data-raw/generate_distance_rasters.R
  dist_files <- list.files(gis_path, pattern = paste0("^", p_dist, "_dist_[0-9]+MWth\\.tif$"), full.names = TRUE)
  for (dist_file in dist_files) {
    dist_name <- sub(paste0("^", p_dist, "_(dist_[0-9]+MWth)\\.tif$"), "\\1", basename(dist_file))
    layers[[dist_name]] <- terra::rast(dist_file)
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
      p[["elec_price"]] <- spatial_layers[["elec_price", exact = TRUE]]
    }
    if ("soil_ph" %in% names(spatial_layers)) p[["soil_ph"]] <- spatial_layers[["soil_ph", exact = TRUE]]
    if ("soil_cec" %in% names(spatial_layers)) p[["soil_cec"]] <- spatial_layers[["soil_cec", exact = TRUE]]
    for (nm in intersect(transport_layer_names(), names(spatial_layers))) p[[nm]] <- spatial_layers[[nm, exact = TRUE]]
    if (isTRUE(as.logical(p[["use_flat_ci", exact = TRUE]]))) {
      p[["ff_c_intensity"]] <- if (!is.null(p[["flat_ci_tCO2_GJ", exact = TRUE]])) p[["flat_ci_tCO2_GJ", exact = TRUE]] else 12 / 3600
    } else if ("ff_c_intensity" %in% names(spatial_layers)) {
      p[["ff_c_intensity"]] <- spatial_layers[["ff_c_intensity", exact = TRUE]]
    }

    for (layer_name in c("cn_weather_risk", "eu_feedstock_usd", "us_base_cost")) {
      if (layer_name %in% names(spatial_layers)) p[[layer_name]] <- spatial_layers[[layer_name, exact = TRUE]]
    }

    sz <- if (!is.null(p[["plant_mw_th", exact = TRUE]])) resolve_plant_mw_th(p[["plant_mw_th", exact = TRUE]], "BES") else 50
    p <- attach_size_layers(p, spatial_layers, sz)

    feedstock_region <- if (!is.null(p[["region", exact = TRUE]])) p[["region", exact = TRUE]] else "US"
    p[["feedstock_cost"]] <- calculate_regional_feedstock_cost(feedstock_region, p)

    res_bes <- calculate_bes(p)
    res_beccs <- calculate_beccs(p)
    res_bebcs <- calculate_bebcs(p)

    net_matrix <- cbind(res_bes[["net_value", exact = TRUE]],
      res_beccs[["net_value", exact = TRUE]], res_bebcs[["net_value", exact = TRUE]])

    abate_matrix <- cbind(res_bes[["tot_c_abatement", exact = TRUE]],
      res_beccs[["tot_c_abatement", exact = TRUE]],
      res_bebcs[["tot_c_abatement", exact = TRUE]])

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

#' CO2 Transport Layer Names
#'
#' Names of the spatial layers that carry CO2 transport information into the TEA functions.
#' v1: least-cost distances to the nearest sink and ship-route legs. v2
#' (`<prefix>_transport_layers.tif` from `data-raw/process_transport_layers.R`): per route class,
#' the physical land-route length, route-average terrain cost multiplier and highest point above
#' the source. Classes: onshore saline (`onsal`) and EOR (`oneor`) pipelines; offshore by ship via a
#' port (`offship`) or by subsea pipeline via a landfall (`offpipe`), each to the nearest saline
#' offshore sink and, where offshore EOR sinks exist, to any offshore sink (`*_any`). Offshore
#' classes also carry the sea distance from the port / landfall to the sink (`*_sea_km`).
#'
#' @param version "all", "v1" or "v2".
#' @return Character vector of layer names.
#' @export
transport_layer_names <- function(version = c("all", "v1", "v2")) {
  version <- match.arg(version)
  v1 <- c("dist_sink_km", "dist_sink_saline_km", "sink_is_offshore", "sink_is_offshore_saline",
          "dist_coast_km", "dist_sea_km", "dist_sea_saline_km")
  switch(version, v1 = v1, v2 = transport_v2_layer_names(), all = c(v1, transport_v2_layer_names()))
}

transport_v2_layer_names <- function() {
  off <- c("offship", "offpipe", "offship_any", "offpipe_any")
  c(as.vector(outer(c("onsal", "oneor", off), c("len_km", "terrain_mult", "hrel_max_m", "storage_cost"), paste, sep = "_")),
    paste0(off, "_sea_km"))
}

#' Storage Cost of Each Routed Sink
#'
#' Matches a region's sink lookup (written by data-raw/process_transport_layers.R, one row per sink in
#' routing order) to `co2_sinks` by basin and sub-unit and returns each sink's storage cost (2024 USD
#' per t CO2; NA for unclassified and EOR sinks).
#'
#' @param lookup_path Path to `<prefix>_sinks_lookup.csv`.
#' @return Numeric vector indexed by the lookup's `sink` column, or NULL if unavailable.
#' @keywords internal
sink_storage_costs <- function(lookup_path) {
  if (!file.exists(lookup_path)) return(NULL)
  env <- new.env()
  ok <- tryCatch({ utils::data("co2_sinks", package = "BiocharAG", envir = env); TRUE }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok || is.null(env$co2_sinks[["Storage_Cost"]])) return(NULL)
  sk <- sf::st_drop_geometry(env$co2_sinks)
  lk <- utils::read.csv(lookup_path, stringsAsFactors = FALSE)
  cost <- sk$Storage_Cost[match(paste(lk$Basin_Name, lk$Sub_Unit), paste(sk$Basin_Name, sk$Sub_Unit))]
  out <- rep(NA_real_, max(lk$sink))
  out[lk$sink] <- cost
  out
}

#' Plant O&M Fraction
#'
#' Annual all-in (fixed + variable) O&M as a fraction of total CAPEX, shared by every conversion
#' technology (`plant_om_factor`) so that O&M is treated consistently across BES, BECCS and BEBCS, and
#' sampled as one parameter in the Monte Carlo. A technology-specific override (e.g. `bes_om_factor`)
#' is used only if supplied explicitly.
#'
#' @param params Parameter list.
#' @param specific Optional name of a technology-specific override.
#' @return O&M fraction of CAPEX per year (before the regional O&M location factor).
#' @keywords internal
plant_om_fraction <- function(params, specific = NULL) {
  v <- if (!is.null(specific)) params[[specific, exact = TRUE]] else NULL
  if (is.null(v)) v <- params[["plant_om_factor", exact = TRUE]]
  if (is.null(v)) 0.04 else v
}

#' Regional Cost Location Factor
#'
#' @param params Parameter list.
#' @param type One of "capex", "om" or "haulage" (reads `<type>_location_factor`).
#' @return The regional multiplier (1 if not set).
#' @keywords internal
location_factor <- function(params, type) {
  v <- params[[paste0(type, "_location_factor"), exact = TRUE]]
  if (is.null(v)) 1 else v
}

#' Biomass Collection Logistics
#'
#' Road haulage cost and emissions for delivering feedstock to the plant (or returning biochar to
#' fields). The average collection distance (`avg_dist`, straight-line) is converted to road distance
#' with `tortuosity`. The variable (per km) trucking cost is split into time-based costs (driver, truck
#' capital, insurance: `haul_time_share`), fuel (`haul_fuel_share`) and other distance-based costs
#' (repairs, tyres, tolls). Where terrain / road-network factors from `data-raw/generate_logistics_layers.R`
#' are supplied (`haul_kt` travel time, `haul_kd` road distance, `haul_g` climb fuel; each normalised to
#' a regional mean of 1), the time share scales with `haul_kt`, fuel with `haul_kd * haul_g` and other
#' distance costs with `haul_kd`. Emissions scale with fuel. Costs are scaled by the regional haulage
#' location factor. Per-km costs include the empty return trip.
#'
#' @param params Parameter list.
#' @param mass Mg hauled per Mg feed (1 for feedstock; biochar yield for biochar return).
#' @return A list with `effective_dist` (km), `cost` ($/Mg feed) and `emissions` (Mg CO2e/Mg feed).
#' @keywords internal
biomass_logistics <- function(params, mass = 1) {
  pv <- function(n, d) if (!is.null(params[[n, exact = TRUE]])) params[[n, exact = TRUE]] else d
  avg_dist <- if (!is.null(params$avg_dist)) params$avg_dist else (2 / 3) * pv("collection_radius", 50)
  effective_dist <- avg_dist * pv("tortuosity", 1.3)
  fac <- function(n) {
    v <- params[[n, exact = TRUE]]
    if (is.null(v)) 1 else ifelse_raster(is.na(v), 1, v)
  }
  kt <- fac("haul_kt")
  kd <- fac("haul_kd")
  g <- fac("haul_g")
  s_t <- pv("haul_time_share", 0.64)
  s_f <- pv("haul_fuel_share", 0.24)
  var_mult <- s_t * kt + s_f * kd * g + max(0, 1 - s_t - s_f) * kd
  list(
    effective_dist = effective_dist,
    cost = mass * (pv("bm_transport_fixed", 6.27) + pv("bm_transport_var", 0.15) * effective_dist * var_mult) *
      location_factor(params, "haulage"),
    emissions = mass * effective_dist * kd * g * pv("transport_emissions_factor", 0.0001)
  )
}

#' Attach Size-Specific Collection Distance and Haulage Factors
#'
#' Sets `avg_dist` from the `dist_<sz>MWth` layer and, where present, `haul_kt`, `haul_kd` and
#' `haul_g` from the `haul_<kt|kd|g>_<sz>` layers (see `data-raw/generate_logistics_layers.R`).
#'
#' @param p Parameter list.
#' @param spatial_layers Named list of layers (rasters or vectors).
#' @param sz Plant size (MWth).
#' @return The updated parameter list.
#' @export
attach_size_layers <- function(p, spatial_layers, sz) {
  dist_layer_name <- paste0("dist_", sz, "MWth")
  if (!dist_layer_name %in% names(spatial_layers)) {
    stop("Missing spatial distance layer: ", dist_layer_name, " (run data-raw/generate_distance_rasters.R)")
  }
  p[["avg_dist"]] <- spatial_layers[[dist_layer_name, exact = TRUE]]
  for (nm in c("kt", "kd", "g")) {
    ln <- paste0("haul_", nm, "_", sz)
    p[[paste0("haul_", nm)]] <- if (ln %in% names(spatial_layers)) spatial_layers[[ln, exact = TRUE]] else NULL
  }
  p
}

#' Counterfactual Residue GHG Effects of Removal
#'
#' GHG effects common to all pathways when crop residue is removed for energy: a net soil GHG penalty
#' (forgone soil organic carbon minus the soil N2O that retained residues would have caused) and the
#' CH4 and N2O from open field burning that is avoided for the regional share of residues otherwise
#' burned (a gain). CO2 from burning is biogenic and not counted. The soil GHG penalty defaults to zero:
#' McClelland et al. (2025) found that the SOC gain and the N2O increase from retaining residues
#' approximately cancel through 2100 in all regions studied except the Brazilian Cerrado.
#'
#' @param params Parameter list.
#' @return Net abatement in Mg CO2e / Mg feed (positive = avoided emissions exceed the soil penalty).
#' @keywords internal
residue_counterfactual_ghg <- function(params) {
  pv <- function(n, d) if (!is.null(params[[n, exact = TRUE]])) params[[n, exact = TRUE]] else d
  soil_ghg_penalty <- pv("residue_soil_ghg_penalty", 0) # Mg CO2e / Mg feed removed
  burn_ghg <- pv("residue_burn_fraction", 0) * pv("residue_burn_cf", 0.8) *
    (pv("residue_burn_ch4_ef", 2.7) * pv("gwp_ch4", 27) + pv("residue_burn_n2o_ef", 0.07) * pv("gwp_n2o", 273)) / 1000
  burn_ghg - soil_ghg_penalty
}
