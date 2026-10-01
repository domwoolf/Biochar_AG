# nolint start: indentation_linter, line_length_linter, object_usage_linter, commented_code_linter
library(terra)
library(sf)
library(dplyr)
library(geodata)

# ==============================================================================
# process_transport_layers.R  -- v2: route on one surface, measure on another
# ------------------------------------------------------------------------------
# Changes relative to v1 (see docs/design/co2_transport_routing.md, "effective km"; issue #17):
#
#  1. Two friction surfaces on the same routing grid.
#       F_cost  : bounded construction-cost multiplier (x flat-terrain cost per km)
#       F_route : F_cost x bounded planning-risk premium, plus hard barriers
#     Routes are CHOSEN on F_route. The chosen route is then MEASURED:
#     physical length, cost-equivalent length (line integral of F_cost),
#     cumulative climb and maximum elevation above the source. The routing
#     integral itself is only reported as a diagnostic, never used as km.
#  2. Nonlinear terrain functions are evaluated at the native DEM/slope
#     resolution (ideally 90-250 m) and only then averaged to the routing grid.
#     v1 aggregated ~1 km slope to ~10 km blocks by the 95th percentile and
#     bilinearly smoothed those blocks back onto the ~1 km routing grid, which
#     both underestimated true slopes and closed valleys narrower than ~10 km.
#  3. Protected areas are tiered: IUCN Ia/Ib/II (+ natural World Heritage)
#     are barriers; all other terrestrial PAs are crossable at a premium;
#     marine PAs are ignored for land routing. v1 made every PA a wall, which
#     turns long, narrow riverine PAs into continuous barriers.
#  4. Land routing never crosses the sea. Offshore sinks are reached by a land
#     route to a coastal cell plus a separately computed sea distance (land
#     impassable). v1 routed over water at friction 5, which (a) inflated
#     offshore distances and (b) made sea cells cheaper than any land cell
#     with > 6.4 deg of p95 slope. The port / landfall is chosen jointly with
#     the land route: each coastal cell starts the land Dijkstra with its sea
#     leg priced in flat-pipeline-km equivalents (sea_route_weight_*), so the
#     route minimises land + sea cost rather than land distance alone. Two
#     offshore modes are routed: ship (pipeline to port, liquefaction, voyage)
#     and subsea pipeline (pipeline to landfall, then subsea to the sink).
#  5. One costDist per sink CLASS (multi-target). The sink actually reached is
#     recovered from the route tree, so per-sink runs are unnecessary.
#  6. Sinks may be points (optionally buffered) or basin polygons.
#
# Output: one multi-layer GeoTIFF per region, <prefix>_transport_layers.tif,
# plus <prefix>_sinks_lookup.csv and <prefix>_ports_lookup.csv. Sink choice
# (onshore vs offshore, saline vs EOR) is left to the TEA, which should
# compare total transport + injection cost (see transport_cost_reference.R).
#
# Run self_test_path_integration() after changes; it checks the route-tree reconstruction
# and the offset Dijkstra against terra::costDist.
# ==============================================================================

if (!exists("co2_sinks")) {
    source("data-raw/generate_sinks.R")
}

raw_dir <- "../GIS/raw"
proc_dir <- "../GIS/processed"
if (!dir.exists(raw_dir)) dir.create(raw_dir, recursive = TRUE)
if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)

#' @importFrom rlang .data
utils::globalVariables(c("co2_sinks"))

# ==============================================================================
# Parameters
# ==============================================================================
#' Default parameters. Override with transport_params(name = value, ...).
#'
#' All cost multipliers are relative to the flat-terrain unit pipeline cost
#' used in the TEA. Values are defensible starting points, NOT calibrated:
#' see the calibration notes in the accompanying discussion.
transport_params <- function(...) {
    p <- list(
        # --- Terrain inputs ----------------------------------------------------
        # Fine slope raster in degrees (e.g. Geomorpho90m 'slope', 90 m or 250 m,
        # as a VRT over the tiles). Preferred: avoids recomputing slope.
        slope_path = NULL,
        slope_units = "degrees", # or "percent"
        slope_scale = 1, # multiply stored values by this (Geomorpho90m 250 m Int16 = degrees x 100 -> 0.01)
        # Fine DEM (e.g. MERIT DEM, Copernicus GLO-90). Used for slope if
        # slope_path is NULL, and for elevation / land-sea mask.
        dem_path = NULL,
        # Fallback only: geodata::elevation_global() (arc-minutes, ~1 km).
        fallback_dem_res = 0.5,

        # --- Construction-cost multiplier vs fine-pixel slope (degrees) --------
        # Piecewise linear, evaluated per fine pixel, then averaged.
        # Anchors: ~17 deg (30 % grade) is where winch-assisted construction
        # typically starts; route-average factors for mostly mountainous routes
        # in the IEA (2002) terrain table are x1.3-1.5.
        cost_slope_deg  = c(0,    5,    10,   17,   25,   35,   45),
        cost_slope_mult = c(1.00, 1.05, 1.20, 1.60, 2.30, 3.20, 4.00),

        # --- Planning-risk premium (ROUTING ONLY) ------------------------------
        # F_route_pixel = F_cost_pixel * (1 + r_max * logistic((s - s_mid) / w))
        # r_max bounds how strongly steep terrain is avoided; calibrate against
        # existing pipeline alignments.
        route_risk_max = 2.0,
        route_risk_mid_deg = 20,
        route_risk_width_deg = 3,
        steep_threshold_deg = 17, # diagnostic layer: share of fine pixels >= this

        # --- Altitude premiums (additive, x flat-terrain cost) -----------------
        # Equipment derating, crew productivity/acclimatisation, short seasons.
        alt_m = c(2500, 3500, 4500, 5500),
        alt_cost_premium = c(0, 0.15, 0.40, 0.80),
        alt_route_premium = c(0, 0.50, 1.50, 4.00),
        route_friction_cap = 25,

        # --- Protected areas ---------------------------------------------------
        use_wdpa = TRUE,
        # wdpar::wdpa_clean() is slow on large regions and failed in v1 (lon/lat CRS; topology errors),
        # so PAs were never applied there. Default: status / UNESCO-MAB filters + st_make_valid.
        wdpa_use_wdpar = FALSE,
        # Simplify PA polygons to this fraction of a routing cell before repair (speeds loading; detail finer
        # than the routing grid is lost in rasterisation anyway). 0 disables.
        wdpa_simplify_cells = 0.25,
        wdpa_cores = max(1L, min(16L, parallel::detectCores() - 2L)), # parallel simplify/repair
        wdpa_cache = TRUE, # cache cleaned, tiered PAs per region in proc_dir (<prefix>_wdpa_tiers.gpkg)
        file_prefix_hint = NULL,
        pa_strict_iucn = c("Ia", "Ib", "II"),
        pa_whs_as_strict = TRUE,
        pa_strict_barrier = TRUE,
        pa_strict_cost_premium = 0.5,  # applies only to cells a route must use (e.g. source inside)
        pa_strict_route_premium = 9,   # used only if pa_strict_barrier = FALSE
        pa_other_cost_premium = 0.25,  # HDD / mitigation / permitting
        pa_other_route_premium = 1.0,
        pa_touches = FALSE,            # cell-centre rasterisation: narrow PAs do not form walls

        # --- Optional extra layers ---------------------------------------------
        # list(list(name=, path=, method="average", cost=function(x) premium_raster,
        #           route=function(x) premium_raster), ...)   premiums additive, >= 0
        # Examples (paths are placeholders):
        # list(name = "road_access", path = "../GIS/processed/dist_to_road_km.tif",
        #      cost  = function(x) 0.30 * terra::clamp(x / 50, 0, 1),
        #      route = function(x) 0.50 * terra::clamp(x / 50, 0, 1)),
        # list(name = "wetland", path = "../GIS/processed/wetland_fraction.tif",
        #      cost  = function(x) 0.6 * x, route = function(x) 1.0 * x),
        # list(name = "urban", path = "../GIS/processed/built_fraction.tif",
        #      cost  = function(x) 2.0 * x, route = function(x) 10 * x)
        modifiers = list(),
        hires_factor_hint = 10, # routing grid = template / this (used only to size PA simplification)

        # --- Sinks, ports, sea -------------------------------------------------
        sink_point_buffer_km = 0,  # >0 turns point sinks into discs of target cells
        sea_agg_factor = 2,        # sea routing grid = routing grid x this factor
        # Sea-leg price in flat onshore-pipeline-km per sea km, used ONLY to choose the port / landfall.
        # Ship: voyage ~0.035 $/t/km vs ~0.05 $/t/km for a shared onshore trunk (hub-and-spoke at
        # 3 Mt/yr, 10 %, 20 yr) -> ~0.7. Terminal and liquefaction are fixed per tonne, so they do not
        # affect which port is best. Subsea pipeline: offshore/onshore CAPEX ratio (keep equal to the
        # TEA parameter co2_subsea_capex_factor).
        sea_route_weight_ship = 0.7,
        sea_route_weight_pipe = 1.5,
        # Storage cost seeds the route search (issue #104): each sink starts with its storage cost
        # (co2_sinks$Storage_Cost, else the defaults below, 2024 USD/t) converted to flat-pipeline km at
        # the reference trunk cost, so a farther but cheaper sink can win. Same 0.05 $/t/km reference
        # as the sea-leg weights. Set storage_route_offsets = FALSE to route on transport cost only.
        storage_route_offsets = TRUE,
        storage_route_cost_per_km = 0.05,
        storage_default_onshore = 10,  # = ccs_storage_cost
        storage_default_offshore = 20, # = cost_offshore_storage

        # --- Numerics / outputs ------------------------------------------------
        costdist_maxiter = 500,
        diagnose_decoupling = TRUE, # also compute the cost-optimal path length for comparison
        write_debug = TRUE
    )
    utils::modifyList(p, list(...))
}

# ==============================================================================
# Small helpers
# ==============================================================================
.OFFSETS <- list(
    c(-1L, -1L), c(-1L, 0L), c(-1L, 1L), c(0L, -1L),
    c(0L, 1L), c(1L, -1L), c(1L, 0L), c(1L, 1L)
)

pwl_vec <- function(v, x, y) {
    out <- rep(NA_real_, length(v))
    ok <- !is.na(v)
    if (any(ok)) out[ok] <- stats::approx(x, y, xout = v[ok], rule = 2)$y
    out
}

grid_geometry <- function(r) {
    nr <- nrow(r)
    nc <- ncol(r)
    rs <- terra::res(r)
    lonlat <- isTRUE(terra::is.lonlat(r, warn = FALSE))
    if (lonlat) {
        R <- 6371008.8
        lat <- terra::yFromRow(r, seq_len(nr)) * pi / 180
        dx_row <- rs[1] * pi / 180 * R * cos(lat)
        dy <- rs[2] * pi / 180 * R
    } else {
        lu <- tryCatch(terra::linearUnits(r), error = function(e) 1)
        if (!isTRUE(all.equal(lu, 1))) warning("Routing CRS linear unit is not metres (", lu, "); lengths will be scaled accordingly.")
        dx_row <- rep(rs[1] * lu, nr)
        dy <- rs[2] * lu
    }
    list(
        nr = nr, nc = nc, lonlat = lonlat, dx_row = dx_row, dy = dy,
        row = rep(seq_len(nr), each = nc), col = rep.int(seq_len(nc), nr)
    )
}

straight_km <- function(xy1, xy2, lonlat) {
    if (!lonlat) return(sqrt((xy1[, 1] - xy2[, 1])^2 + (xy1[, 2] - xy2[, 2])^2) / 1000)
    k <- pi / 180
    la1 <- xy1[, 2] * k
    la2 <- xy2[, 2] * k
    a <- sin((la2 - la1) / 2)^2 + cos(la1) * cos(la2) * sin((xy2[, 1] - xy1[, 1]) * k / 2)^2
    2 * 6371.0088 * asin(pmin(1, sqrt(a)))
}

neighbor_any <- function(mask, geo) {
    res <- rep(FALSE, length(mask))
    for (o in .OFFSETS) {
        i <- which(geo$row + o[1] >= 1L & geo$row + o[1] <= geo$nr & geo$col + o[2] >= 1L & geo$col + o[2] <= geo$nc)
        j <- i + o[1] * geo$nc + o[2]
        res[i] <- res[i] | (mask[j] %in% TRUE)
    }
    res
}

#' Nearest cell (Euclidean, map units) where mask is TRUE; expanding window search.
snap_to_mask <- function(r, mask, x, y) {
    nr <- nrow(r)
    nc <- ncol(r)
    col <- terra::colFromX(r, x)
    row <- terra::rowFromY(r, y)
    if (is.na(col)) col <- if (x < terra::xmin(r)) 1L else nc
    if (is.na(row)) row <- if (y > terra::ymax(r)) 1L else nr
    k <- 1L
    repeat {
        r0 <- max(1L, row - k); r1 <- min(nr, row + k)
        c0 <- max(1L, col - k); c1 <- min(nc, col + k)
        rr <- rep(r0:r1, each = c1 - c0 + 1L)
        cc <- rep.int(c0:c1, r1 - r0 + 1L)
        cid <- (rr - 1L) * nc + cc
        hit <- cid[which(mask[cid])]
        if (length(hit)) {
            xy <- terra::xyFromCell(r, hit)
            return(hit[which.min((xy[, 1] - x)^2 + (xy[, 2] - y)^2)])
        }
        if (k >= max(nr, nc)) return(NA_integer_)
        k <- k * 2L
    }
}

run_costdist <- function(fr_r, params) {
    A <- withCallingHandlers(
        terra::costDist(fr_r, target = 0, maxiter = params$costdist_maxiter),
        warning = function(w) {
            message("  costDist warning: ", conditionMessage(w), "  -> consider raising costdist_maxiter")
            invokeRestart("muffleWarning")
        }
    )
    terra::values(A, mat = FALSE)
}

#' Multi-source Dijkstra on the 8-neighbour grid with a starting cost per source (e.g. the
#' priced sea leg at each coastal cell); terra::costDist cannot seed targets with offsets.
#' Edge cost = step length x mean friction of its two cells (as costDist and build_parent).
#' Source cells keep their friction: with many adjacent sources (every coastal cell), zero-friction
#' sources would form a free corridor along the coast.
#' Returns accumulated cost A, 1-based parent (self at roots and unreached cells) and step (m).
.dijkstra_src <- '
#include <Rcpp.h>
#include <queue>
using namespace Rcpp;
// [[Rcpp::export]]
List dijkstra_offsets_cpp(NumericVector fr, int nr, int nc, NumericVector dx_row, double dy,
                          IntegerVector src, NumericVector off) {
    const int n = fr.size();
    NumericVector A(n, R_PosInf);
    IntegerVector parent(n);
    NumericVector step(n);
    std::vector<double> f(n);
    std::vector<char> done(n, 0);
    for (int i = 0; i < n; i++) { parent[i] = i + 1; f[i] = ISNAN(fr[i]) ? -1.0 : fr[i]; }
    typedef std::pair<double, int> P;
    std::priority_queue<P, std::vector<P>, std::greater<P> > pq;
    for (int k = 0; k < src.size(); k++) {
        int c = src[k] - 1;
        if (off[k] < A[c]) { A[c] = off[k]; pq.push(P(off[k], c)); }
    }
    const int dr[8] = {-1, -1, -1, 0, 0, 1, 1, 1};
    const int dc[8] = {-1, 0, 1, -1, 1, -1, 0, 1};
    while (!pq.empty()) {
        P t = pq.top(); pq.pop();
        int c = t.second;
        if (done[c]) continue;
        done[c] = 1;
        int r = c / nc, cc = c % nc;
        for (int k = 0; k < 8; k++) {
            int rr = r + dr[k], c2 = cc + dc[k];
            if (rr < 0 || rr >= nr || c2 < 0 || c2 >= nc) continue;
            int j = rr * nc + c2;
            if (done[j] || f[j] < 0) continue;
            double sx = dc[k] * dx_row[rr], sy = dr[k] * dy;
            double s = std::sqrt(sx * sx + sy * sy);
            double nd = t.first + s * (f[c] + f[j]) / 2.0;
            if (nd < A[j]) { A[j] = nd; parent[j] = c + 1; step[j] = s; pq.push(P(nd, j)); }
        }
    }
    return List::create(_["A"] = A, _["parent"] = parent, _["step"] = step);
}'

dijkstra_offsets <- function(fr, geo, src, off) {
    if (!exists("dijkstra_offsets_cpp", mode = "function")) Rcpp::sourceCpp(code = .dijkstra_src, env = globalenv())
    dijkstra_offsets_cpp(as.numeric(fr), geo$nr, geo$nc, as.numeric(geo$dx_row), geo$dy, as.integer(src), as.numeric(off))
}

zone_mean <- function(v, zone, n_t) {
    ok <- is.finite(v)
    s <- rowsum(v[ok], zone[ok])
    n <- rowsum(rep(1, sum(ok)), zone[ok])
    out <- rep(NA_real_, n_t)
    out[as.integer(rownames(s))] <- s[, 1] / n[, 1]
    out
}

# ==============================================================================
# Route tree and path integrals (validated against Dijkstra in a Python prototype)
# ==============================================================================
#' Parent of each cell in the least-cost tree implied by an accumulated-cost vector A.
#' parent(c) = argmin over 8-neighbours n with A(n) < A(c) of A(n) + d(c,n) (F(c)+F(n))/2.
#' Strict descent guarantees an acyclic tree even if costDist's internal edge rule
#' differs slightly from the one assumed here.
build_parent <- function(A, fr, geo) {
    n <- length(A)
    valid <- is.finite(A)
    parent <- seq_len(n)
    step <- numeric(n)
    best <- rep(Inf, n)
    for (o in .OFFSETS) {
        dr <- o[1]; dc <- o[2]
        i <- which(valid & geo$row + dr >= 1L & geo$row + dr <= geo$nr & geo$col + dc >= 1L & geo$col + dc <= geo$nc)
        j <- i + dr * geo$nc + dc
        keep <- is.finite(A[j]) & A[j] < A[i]
        i <- i[keep]; j <- j[keep]
        s <- sqrt((dc * geo$dx_row[geo$row[i]])^2 + (dr * geo$dy)^2)
        cand <- A[j] + s * (fr[i] + fr[j]) / 2
        b <- which(cand < best[i])
        ib <- i[b]
        best[ib] <- cand[b]
        parent[ib] <- j[b]
        step[ib] <- s[b]
    }
    orphan <- valid & A > 0 & parent == seq_len(n)
    if (any(orphan)) {
        warning(sum(orphan), " reachable cells have no lower neighbour; treated as unreachable (costDist not converged?)")
        valid[orphan] <- FALSE
    }
    list(parent = parent, step = step, valid = valid)
}

#' Pointer jumping: sums of edge values and maxima of node values along every
#' cell's path to its root, in O(log path length) vectorised passes.
#' Edge values must be 0 at roots (parent == self).
path_integrate <- function(parent, sums, maxs = list(), max_iter = 64L) {
    P <- parent
    for (it in seq_len(max_iter)) {
        PP <- P[P]
        if (all(PP == P)) break
        for (k in names(sums)) sums[[k]] <- sums[[k]] + sums[[k]][P]
        for (k in names(maxs)) maxs[[k]] <- pmax(maxs[[k]], maxs[[k]][P])
        P <- PP
    }
    if (!all(P[P] == P)) stop("path_integrate did not converge: route tree has a cycle")
    c(sums, maxs, list(root = P))
}

representative_subcells <- function(A, zone) {
    ok <- which(is.finite(A))
    o <- ok[order(zone[ok], A[ok])]
    keep <- !duplicated(zone[o])
    list(zone = zone[o][keep], cell = o[keep])
}

#' Route every routing cell to the nearest target (by F_route), measure the
#' route, and summarise to the template grid using, per template cell, the
#' sub-cell with the lowest routing cost (plant sited at the best spot in its cell).
#' With `offset` (one starting cost per target, in route-friction metres), routing uses
#' dijkstra_offsets(), so each target competes on offset + land route cost.
route_and_summarise <- function(label, target_cells, target_id, ctx, params, offset = NULL) {
    message("  Routing class '", label, "' to ", length(target_cells), " target cells...")
    fr <- ctx$Fr
    if (is.null(offset)) fr[target_cells] <- 0 # offset mode: targets keep their friction (see dijkstra_offsets)
    if (is.null(offset)) {
        A <- run_costdist(terra::setValues(ctx$r_geom, fr), params)
        tree <- build_parent(A, fr, ctx$geo)
    } else {
        dj <- dijkstra_offsets(fr, ctx$geo, target_cells, offset)
        A <- dj$A
        tree <- list(parent = dj$parent, step = dj$step, valid = is.finite(A))
        rm(dj)
    }
    valid <- tree$valid
    p <- tree$parent
    s <- tree$step
    root_edge <- s == 0

    eLc <- s * (ctx$Fc_s + ctx$Fc_s[p]) / 2
    eLr <- s * (fr + fr[p]) / 2
    eUp <- pmax(0, ctx$h_s[p] - ctx$h_s)
    eLc[root_edge] <- 0; eLr[root_edge] <- 0; eUp[root_edge] <- 0
    hnode <- ctx$h_s
    hnode[!valid] <- -Inf

    pj <- path_integrate(p, sums = list(L = s, Lc = eLc, Lr = eLr, climb = eUp), maxs = list(hmax = hnode))
    rm(eLc, eLr, eUp, hnode, tree)

    if (is.null(offset)) {
        chk <- abs(pj$Lr[valid] - A[valid]) / pmax(A[valid], 1)
        message(sprintf("    route tree vs costDist: median rel. diff %.1e, 99th pct %.1e", stats::median(chk), stats::quantile(chk, 0.99)))
    } else {
        off_c <- rep(NA_real_, length(A))
        off_c[target_cells] <- offset
        chk <- abs(pj$Lr[valid] + off_c[pj$root[valid]] - A[valid]) / pmax(A[valid], 1)
        message(sprintf("    route integral + offset vs Dijkstra: max rel. diff %.1e", max(chk)))
    }

    A[!valid] <- NA
    rep_sc <- representative_subcells(A, ctx$zone)
    cells <- rep_sc$cell
    root <- pj$root[cells]
    L <- pj$L[cells]
    Lc <- pj$Lc[cells]
    eu <- straight_km(terra::xyFromCell(ctx$r_geom, cells), terra::xyFromCell(ctx$r_geom, root), ctx$geo$lonlat)

    vals <- list(
        len_km = L / 1000,
        costlen_km = Lc / 1000,
        routecost_km = A[cells] / 1000,
        terrain_mult = ifelse(L > 0, Lc / L, 1),
        detour = ifelse(eu > 0.5, (L / 1000) / eu, NA_real_),
        hrel_max_m = pj$hmax[cells] - ctx$h_s[cells],
        climb_m = pj$climb[cells],
        target = as.numeric(target_id[match(root, target_cells)])
    )
    if (isTRUE(params$diagnose_decoupling)) {
        fc <- ctx$Fc_barrier
        if (is.null(offset)) fc[target_cells] <- 0
        Ac <- if (is.null(offset)) run_costdist(terra::setValues(ctx$r_geom, fc), params) else dijkstra_offsets(fc, ctx$geo, target_cells, offset)$A
        vals$costopt_km <- Ac[cells] / 1000
    }
    out <- lapply(vals, function(v) {
        o <- rep(NA_real_, ctx$n_t)
        o[rep_sc$zone] <- v
        o
    })
    names(out) <- paste0(label, "_", names(out))
    out
}

na_class_layers <- function(label, ctx, params, offshore = FALSE) {
    nm <- c("len_km", "costlen_km", "routecost_km", "terrain_mult", "detour", "hrel_max_m", "climb_m", "target")
    if (isTRUE(params$diagnose_decoupling)) nm <- c(nm, "costopt_km")
    if (offshore) nm <- c(nm, "sea_km", "sink")
    out <- replicate(length(nm), rep(NA_real_, ctx$n_t), simplify = FALSE)
    names(out) <- paste0(label, "_", nm)
    out
}

# ==============================================================================
# Inputs: terrain, protected areas, sinks
# ==============================================================================
load_fine_terrain <- function(poly_ll, params) {
    crop_to <- function(r) terra::crop(r, terra::ext(terra::project(poly_ll, terra::crs(r))))
    dem <- if (!is.null(params$dem_path)) crop_to(terra::rast(params$dem_path)) else NULL
    slope <- NULL
    if (!is.null(params$slope_path)) {
        slope <- crop_to(terra::rast(params$slope_path))
        if (!isTRUE(all.equal(params$slope_scale, 1))) slope <- slope * params$slope_scale
        if (identical(params$slope_units, "percent")) slope <- atan(slope / 100) * 180 / pi
    }
    if (is.null(dem)) {
        message("No dem_path: using geodata::elevation_global() for elevation and land/sea mask.")
        dem <- crop_to(geodata::elevation_global(res = params$fallback_dem_res, path = raw_dir))
    }
    if (is.null(slope)) {
        if (is.null(params$dem_path)) {
            warning("Slope derived from the ~1 km fallback DEM: slopes will be strongly underestimated in rugged terrain. ",
                    "Supply slope_path (e.g. Geomorpho90m) or dem_path (MERIT DEM / Copernicus GLO-90).")
        }
        message("Computing slope at native DEM resolution...")
        slope <- terra::terrain(dem, v = "slope", unit = "degrees")
    }
    list(dem = dem, slope = slope)
}

#' Evaluate nonlinear functions per fine pixel, then average to the routing grid.
terrain_to_grid <- function(ft, r_geom, params) {
    f_cost <- function(s) pwl_vec(s, params$cost_slope_deg, params$cost_slope_mult)
    f_route <- function(s) f_cost(s) * (1 + params$route_risk_max * stats::plogis((s - params$route_risk_mid_deg) / params$route_risk_width_deg))
    f_steep <- function(s) as.numeric(s >= params$steep_threshold_deg)
    message("Evaluating terrain functions at native slope resolution and averaging to routing grid...")
    fine <- c(terra::lapp(ft$slope, f_cost), terra::lapp(ft$slope, f_route), terra::lapp(ft$slope, f_steep))
    names(fine) <- c("m_cost", "m_route", "frac_steep")
    g <- terra::project(fine, r_geom, method = "average")
    elev <- terra::project(ft$dem, r_geom, method = "average")
    names(elev) <- "elev"
    c(g, elev)
}

load_wdpa <- function(r_template, params) {
    wdpa_dir <- file.path(raw_dir, "WDPA")
    zip_files <- list.files(wdpa_dir, pattern = "\\.zip$", full.names = TRUE)
    if (length(zip_files) == 0) {
        warning("No WDPA zip files found in ", wdpa_dir, " - skipping PA integration.")
        return(NULL)
    }
    zip_file <- zip_files[which.max(file.info(zip_files)$mtime)]
    cache_path <- if (isTRUE(params$wdpa_cache) && !is.null(params$file_prefix_hint)) {
        file.path(proc_dir, paste0(params$file_prefix_hint, "_wdpa_tiers.gpkg"))
    } else {
        NULL
    }
    if (!is.null(cache_path) && file.exists(cache_path) && file.mtime(cache_path) > file.mtime(zip_file)) {
        message("Loading cached protected-area tiers: ", cache_path)
        return(sf::st_read(cache_path, quiet = TRUE))
    }
    message("Loading local WDPA database from: ", zip_file)
    tryCatch({
        gdb_name <- gsub("\\.zip$", ".gdb", basename(zip_file))
        vsi_path <- paste0("/vsizip/", zip_file, "/", gdb_name)
        layers <- sf::st_layers(vsi_path)$name
        poly_layer <- layers[grepl("poly", layers, ignore.case = TRUE)][1]
        e_poly_wgs84 <- sf::st_transform(sf::st_as_sfc(sf::st_bbox(r_template)), 4326)
        # Only the attributes used for tiering; skip GDAL's slow multipart polygon reorganisation
        old_cfg <- Sys.getenv("OGR_ORGANIZE_POLYGONS", unset = NA)
        Sys.setenv(OGR_ORGANIZE_POLYGONS = "SKIP")
        on.exit(if (is.na(old_cfg)) Sys.unsetenv("OGR_ORGANIZE_POLYGONS") else Sys.setenv(OGR_ORGANIZE_POLYGONS = old_cfg), add = TRUE)
        # WDPA schema changed over time (MARINE -> REALM); select only fields that exist
        fields <- names(sf::st_read(vsi_path, query = paste0("SELECT * FROM \"", poly_layer, "\" LIMIT 1"), quiet = TRUE))
        want <- intersect(c("IUCN_CAT", "DESIG_ENG", "STATUS", "MARINE", "REALM"), fields)
        pa <- sf::st_read(vsi_path, query = paste0("SELECT ", paste(want, collapse = ", "), " FROM \"", poly_layer, "\""),
                          wkt_filter = sf::st_as_text(e_poly_wgs84), quiet = TRUE)
        pa <- sf::st_transform(pa, terra::crs(r_template))
        # Marine PAs are ignored for land routing (coastal PAs straddle the shoreline and are kept)
        if ("REALM" %in% names(pa)) pa <- pa[!(pa$REALM %in% "Marine"), ]
        if ("MARINE" %in% names(pa)) pa <- pa[!(as.character(pa$MARINE) %in% c("2", "marine")), ]
        manual_clean <- function(pa) {
            if ("STATUS" %in% names(pa)) pa <- pa[pa$STATUS %in% c("Designated", "Inscribed", "Established"), ]
            if ("DESIG_ENG" %in% names(pa)) pa <- pa[!grepl("Biosphere Reserve", pa$DESIG_ENG, ignore.case = TRUE), ]
            old_s2 <- sf::sf_use_s2()
            sf::sf_use_s2(FALSE)
            on.exit(sf::sf_use_s2(old_s2))
            tol <- params$wdpa_simplify_cells * min(terra::res(r_template)) / params$hires_factor_hint
            clean_chunk <- function(x) {
                sf::sf_use_s2(FALSE)
                if (tol > 0) x <- suppressWarnings(sf::st_simplify(x, preserveTopology = TRUE, dTolerance = tol))
                x <- x[!sf::st_is_empty(x), ]
                sf::st_collection_extract(sf::st_make_valid(x), "POLYGON")
            }
            n_cores <- max(1L, as.integer(params$wdpa_cores))
            if (n_cores > 1L && nrow(pa) > 1000L) {
                chunks <- split(seq_len(nrow(pa)), cut(seq_len(nrow(pa)), n_cores * 4L, labels = FALSE))
                parts <- parallel::mclapply(chunks, function(i) clean_chunk(pa[i, ]), mc.cores = n_cores)
                failed <- vapply(parts, inherits, logical(1), "try-error")
                if (any(failed)) stop("parallel PA cleaning failed: ", parts[[which(failed)[1]]])
                do.call(rbind, parts)
            } else {
                clean_chunk(pa)
            }
        }
        if (isTRUE(params$wdpa_use_wdpar) && requireNamespace("wdpar", quietly = TRUE)) {
            old_s2 <- sf::sf_use_s2()
            sf::sf_use_s2(FALSE)
            # wdpa_clean buffers geometries, so it needs a projected CRS; fall back to the manual filters on failure
            pa <- tryCatch(
                sf::st_transform(wdpar::wdpa_clean(pa, crs = "ESRI:54017", erase_overlaps = FALSE), terra::crs(r_template)),
                error = function(e) {
                    warning("wdpa_clean failed (", conditionMessage(e), "); using manual filters.")
                    manual_clean(pa)
                },
                finally = sf::sf_use_s2(old_s2)
            )
        } else {
            pa <- manual_clean(pa)
        }
        # Tiering
        iucn <- if ("IUCN_CAT" %in% names(pa)) as.character(pa$IUCN_CAT) else rep(NA_character_, nrow(pa))
        strict <- iucn %in% params$pa_strict_iucn
        if (isTRUE(params$pa_whs_as_strict) && "DESIG_ENG" %in% names(pa)) {
            strict <- strict | grepl("World Heritage", pa$DESIG_ENG, ignore.case = TRUE)
        }
        pa$pa_class <- ifelse(strict, 2L, 1L)
        message(sprintf("  PAs retained: %d strict (barrier), %d other (crossable at a premium)", sum(strict), sum(!strict)))
        pa <- pa[, "pa_class"]
        if (!is.null(cache_path)) sf::st_write(pa, cache_path, delete_dsn = TRUE, quiet = TRUE)
        pa
    }, error = function(e) {
        warning("Failed to load WDPA data: ", conditionMessage(e))
        NULL
    })
}

#' Target cells for a set of sinks on grid r (a SpatRaster with values).
#' Polygons -> all passable cells in the polygon; points -> the cell (or a disc).
#' Sinks with no passable cell are snapped to the nearest passable cell.
sink_target_cells <- function(sinks_b, r, passable, params) {
    gt <- as.character(sf::st_geometry_type(sinks_b))
    cxy <- sf::st_coordinates(suppressWarnings(sf::st_centroid(sf::st_geometry(sinks_b))))
    out <- vector("list", nrow(sinks_b))
    for (i in seq_len(nrow(sinks_b))) {
        g <- sf::st_geometry(sinks_b)[i]
        if (gt[i] %in% c("POLYGON", "MULTIPOLYGON")) {
            cc <- terra::cells(r, terra::vect(g), touches = TRUE)[, "cell"]
        } else if (params$sink_point_buffer_km > 0) {
            cc <- terra::cells(r, terra::vect(sf::st_buffer(g, params$sink_point_buffer_km * 1000)), touches = TRUE)[, "cell"]
        } else {
            cc <- terra::cellFromXY(r, cxy[i, 1:2, drop = FALSE])
        }
        cc <- cc[!is.na(cc)]
        cc <- cc[passable[cc] %in% TRUE]
        if (length(cc) == 0) cc <- snap_to_mask(r, passable, cxy[i, 1], cxy[i, 2])
        cc <- cc[!is.na(cc)]
        if (length(cc)) out[[i]] <- data.frame(cell = cc, sink = i)
    }
    df <- do.call(rbind, out)
    if (is.null(df)) return(data.frame(cell = integer(0), sink = integer(0)))
    df[!duplicated(df$cell), ]
}

#' Sea distance (km, land impassable) from each port to each offshore sink.
sea_distances_at <- function(port_xy, sinks_off_b, dem_ll, r_geom, params) {
    rs <- terra::res(r_geom) * params$sea_agg_factor
    e <- terra::ext(r_geom)
    bb <- sf::st_bbox(sinks_off_b)
    pad <- 10 * rs[1]
    e_u <- terra::ext(
        min(e$xmin, bb[["xmin"]]) - pad, max(e$xmax, bb[["xmax"]]) + pad,
        min(e$ymin, bb[["ymin"]]) - pad, max(e$ymax, bb[["ymax"]]) + pad
    )
    g <- terra::rast(e_u, resolution = rs, crs = terra::crs(r_geom))
    elev_g <- terra::project(dem_ll, g, method = "average")
    sea_v <- is.na(terra::values(elev_g, mat = FALSE))
    fr0 <- ifelse(sea_v, 1, NA_real_)
    tg <- sink_target_cells(sinks_off_b, elev_g, sea_v, params)
    port_cell_g <- terra::cellFromXY(g, port_xy)
    D <- matrix(NA_real_, nrow(port_xy), nrow(sinks_off_b))
    for (k in seq_len(nrow(sinks_off_b))) {
        tc <- tg$cell[tg$sink == k]
        if (!length(tc)) next
        fr <- fr0
        fr[tc] <- 0
        d <- terra::setValues(g, run_costdist(terra::setValues(g, fr), params))
        # ports are land cells: take the nearest sea value within 2 cells
        d <- terra::cover(d, terra::focal(d, w = 5, fun = "min", na.rm = TRUE))
        D[, k] <- terra::values(d, mat = FALSE)[port_cell_g] / 1000
    }
    D
}

best_col <- function(D, cols) {
    if (!length(cols)) return(list(d = rep(NA_real_, nrow(D)), k = rep(NA_integer_, nrow(D))))
    sub <- D[, cols, drop = FALSE]
    sub[!is.finite(sub)] <- Inf
    k <- max.col(-sub, ties.method = "first")
    d <- sub[cbind(seq_len(nrow(sub)), k)]
    d[!is.finite(d)] <- NA_real_
    list(d = d, k = cols[k])
}

# ==============================================================================
# Main
# ==============================================================================
#' Generate transport layers for a region.
#' @param hires_factor routing grid = template disaggregated by this factor
#'   (aim for ~1 km routing cells; memory scales with the number of routing cells).
#' @param ports optional sf POINTs of candidate CO2 export ports (e.g. World Port
#'   Index). If NULL, every coastal land cell is a candidate port.
process_transport_layers <- function(region_name, template_path, file_prefix,
                                     hires_factor = 10, params = transport_params(),
                                     ports = NULL) {
    message(paste0("\n=== Transport layers (route/cost decoupled) for: ", region_name, " ==="))
    if (!file.exists(template_path)) {
        warning("Template not found: ", template_path, " - skipping.")
        return(NULL)
    }
    r_template <- terra::rast(template_path)
    f <- as.integer(hires_factor)
    r_geom <- terra::rast(terra::ext(r_template), nrows = nrow(r_template) * f, ncols = ncol(r_template) * f, crs = terra::crs(r_template))
    n_t <- terra::ncell(r_template)

    # --- Sinks -----------------------------------------------------------------
    sinks_sub <- co2_sinks |> dplyr::filter(.data$Region == region_name)
    if (nrow(sinks_sub) == 0) {
        warning("No sinks found for this region in co2_sinks database.")
        return(NULL)
    }
    sinks_b <- sf::st_transform(sinks_sub, terra::crs(r_geom))
    is_off <- sinks_b$Type == "Offshore"
    is_eor <- !is.na(sinks_b$Is_EOR) & as.logical(sinks_b$Is_EOR)
    # Saline and EOR are separate flags (issue #105): a basin can be both a saline and an EOR target
    has_sal <- if ("Has_Saline" %in% names(sinks_b)) !is.na(sinks_b$Has_Saline) & as.logical(sinks_b$Has_Saline) else !is_eor
    cls_onsal <- which(!is_off & has_sal)
    cls_oneor <- which(!is_off & is_eor)
    cls_off <- which(is_off)
    # Storage-cost offsets in route-friction metres (flat-pipeline km x 1000)
    stor <- if ("Storage_Cost" %in% names(sinks_b)) sinks_b$Storage_Cost else rep(NA_real_, nrow(sinks_b))
    stor <- ifelse(is.na(stor), ifelse(is_off, params$storage_default_offshore, params$storage_default_onshore), stor)
    stor_m <- if (isTRUE(params$storage_route_offsets)) stor / params$storage_route_cost_per_km * 1000 else rep(0, nrow(sinks_b))
    message(sprintf("Sinks: %d onshore saline, %d onshore EOR, %d offshore.", length(cls_onsal), length(cls_oneor), length(cls_off)))
    message("Storage offsets (flat-pipeline km): ", paste(sprintf("%s %.0f", sinks_b$Basin_Name, stor_m / 1000), collapse = "; "))

    # --- Processing extent (routing grid + sinks + margin), in lon/lat --------
    e <- terra::ext(r_geom)
    bb <- sf::st_bbox(sinks_b)
    pad <- 25 * terra::res(r_geom)[1] * max(1, params$sea_agg_factor)
    e_proc <- terra::ext(min(e$xmin, bb[["xmin"]]) - pad, max(e$xmax, bb[["xmax"]]) + pad,
                         min(e$ymin, bb[["ymin"]]) - pad, max(e$ymax, bb[["ymax"]]) + pad)
    poly_ll <- terra::project(terra::as.polygons(e_proc, crs = terra::crs(r_geom)), "EPSG:4326")

    # --- Terrain -----------------------------------------------------------------
    ft <- load_fine_terrain(poly_ll, params)
    tg <- terrain_to_grid(ft, r_geom, params)
    h_r <- tg$elev

    # --- Protected areas -------------------------------------------------------
    pa_r <- NULL
    if (isTRUE(params$use_wdpa)) {
        pa <- load_wdpa(r_template, utils::modifyList(params, list(hires_factor_hint = f, file_prefix_hint = file_prefix)))
        if (!is.null(pa) && nrow(pa) > 0) {
            # Two passes (other, then strict on top) = max class, but far faster than fun = "max"
            pa_r <- terra::rasterize(terra::vect(pa[pa$pa_class == 1L, ]), r_geom, field = 1, background = 0, touches = params$pa_touches)
            if (any(pa$pa_class == 2L)) {
                pa_r <- terra::rasterize(terra::vect(pa[pa$pa_class == 2L, ]), pa_r, field = 2, update = TRUE, touches = params$pa_touches)
            }
        }
    }

    # --- Friction surfaces -----------------------------------------------------
    message("Assembling F_cost and F_route...")
    m_cost <- terra::subst(tg$m_cost, NA, 1)
    m_route <- terra::subst(tg$m_route, NA, 1)
    P_cost <- terra::lapp(h_r, function(v) pwl_vec(v, params$alt_m, params$alt_cost_premium))
    P_route <- terra::lapp(h_r, function(v) pwl_vec(v, params$alt_m, params$alt_route_premium))
    if (!is.null(pa_r)) {
        P_cost <- P_cost + (pa_r == 1) * params$pa_other_cost_premium + (pa_r == 2) * params$pa_strict_cost_premium
        P_route <- P_route + (pa_r == 1) * params$pa_other_route_premium
        if (!isTRUE(params$pa_strict_barrier)) P_route <- P_route + (pa_r == 2) * params$pa_strict_route_premium
    }
    for (m in params$modifiers) {
        message("  Applying modifier: ", m$name)
        x <- terra::project(terra::rast(m$path), r_geom, method = if (is.null(m$method)) "average" else m$method)
        if (!is.null(m$cost)) P_cost <- P_cost + terra::subst(m$cost(x), NA, 0)
        if (!is.null(m$route)) P_route <- P_route + terra::subst(m$route(x), NA, 0)
    }
    F_cost <- terra::mask(m_cost + P_cost, h_r)
    F_route <- terra::mask((m_route + P_cost) * (1 + P_route), h_r)
    F_route <- terra::clamp(F_route, upper = params$route_friction_cap, values = TRUE)
    if (!is.null(pa_r) && isTRUE(params$pa_strict_barrier)) F_route <- terra::ifel(pa_r == 2, NA, F_route)
    names(F_cost) <- "F_cost"
    names(F_route) <- "F_route"

    if (isTRUE(params$write_debug)) {
        terra::writeRaster(F_cost, file.path(proc_dir, paste0(file_prefix, "_debug_F_cost.tif")), overwrite = TRUE)
        terra::writeRaster(F_route, file.path(proc_dir, paste0(file_prefix, "_debug_F_route.tif")), overwrite = TRUE)
        terra::writeRaster(tg$frac_steep, file.path(proc_dir, paste0(file_prefix, "_debug_frac_steep.tif")), overwrite = TRUE)
        if (!is.null(pa_r)) terra::writeRaster(pa_r, file.path(proc_dir, paste0(file_prefix, "_debug_pa_class.tif")), overwrite = TRUE)
    }

    # --- Vector context ----------------------------------------------------------
    geo <- grid_geometry(r_geom)
    Fr <- terra::values(F_route, mat = FALSE)
    Fc <- terra::values(F_cost, mat = FALSE)
    h <- terra::values(h_r, mat = FALSE)
    Fc_s <- Fc; Fc_s[!is.finite(Fc_s)] <- 1
    h_s <- h; h_s[!is.finite(h_s)] <- 0
    Fc_barrier <- Fc; Fc_barrier[!is.finite(Fr)] <- NA
    zone <- as.integer(((geo$row - 1L) %/% f) * ncol(r_template) + ((geo$col - 1L) %/% f) + 1L)
    ctx <- list(r_geom = r_geom, geo = geo, Fr = Fr, Fc_s = Fc_s, Fc_barrier = Fc_barrier, h_s = h_s, zone = zone, n_t = n_t)
    passable <- is.finite(Fr)

    cell_layers <- list(
        terrain_cost_index = zone_mean(Fc, zone, n_t),
        route_friction_index = zone_mean(Fr, zone, n_t),
        frac_steep = zone_mean(terra::values(tg$frac_steep, mat = FALSE), zone, n_t),
        elev_mean_m = zone_mean(h, zone, n_t)
    )

    # --- Onshore classes ---------------------------------------------------------
    onshore_class <- function(label, idx, seed_storage = FALSE) {
        if (!length(idx)) return(na_class_layers(label, ctx, params))
        tc <- sink_target_cells(sinks_b[idx, ], F_route, passable, params)
        sid <- idx[tc$sink]
        off <- if (seed_storage && length(unique(stor_m[sid])) > 1) stor_m[sid] - min(stor_m[sid]) else NULL
        route_and_summarise(label, tc$cell, sid, ctx, params, offset = off)
    }
    lay_onsal <- onshore_class("onsal", cls_onsal, seed_storage = TRUE)
    gc()
    lay_oneor <- onshore_class("oneor", cls_oneor)
    gc()

    # --- Offshore: land route to port / landfall + sea leg ------------------------
    # Classes: offship / offpipe (nearest saline offshore sink by sea) and, when offshore EOR sinks
    # exist, offship_any / offpipe_any (any offshore sink). Each coastal cell seeds the land Dijkstra
    # with its sea leg priced at sea_route_weight_* flat-pipeline km per sea km.
    off_modes <- c(ship = params$sea_route_weight_ship, pipe = params$sea_route_weight_pipe)
    off_sets <- list(sal = which(has_sal[cls_off]))
    if (any(is_eor[cls_off] & !has_sal[cls_off])) off_sets$any <- seq_along(cls_off)
    off_label <- function(mode, set) paste0("off", mode, if (set == "any") "_any" else "")
    lay_off <- list()
    for (set in names(off_sets)) for (mode in names(off_modes)) lay_off <- c(lay_off, na_class_layers(off_label(mode, set), ctx, params, offshore = TRUE))
    ports_tbl <- NULL
    if (length(cls_off)) {
        message("Offshore sinks: locating coastal cells and computing sea distances...")
        coast <- passable & neighbor_any(!is.finite(h), geo)
        if (!is.null(ports)) {
            pb <- sf::st_transform(ports, terra::crs(r_geom))
            pxy <- sf::st_coordinates(pb)
            port_cells <- unique(stats::na.omit(vapply(seq_len(nrow(pxy)), function(i) snap_to_mask(r_geom, coast, pxy[i, 1], pxy[i, 2]), integer(1))))
        } else {
            port_cells <- which(coast)
        }
        port_xy <- terra::xyFromCell(r_geom, port_cells)
        D <- sea_distances_at(port_xy, sinks_b[cls_off, ], ft$dem, r_geom, params)
        ports_tbl <- data.frame(port = seq_along(port_cells), cell = port_cells, x = port_xy[, 1], y = port_xy[, 2])
        for (set in names(off_sets)) {
            b <- best_col(D, off_sets[[set]])
            ports_tbl[[paste0("sea_km_", set)]] <- b$d
            ports_tbl[[paste0("sink_", set)]] <- cls_off[b$k]
            ok_port <- which(is.finite(b$d))
            message(sprintf("  set '%s': %d coastal cells, %d with sea access to an offshore sink", set, length(port_cells), length(ok_port)))
            if (!length(ok_port)) next
            for (mode in names(off_modes)) {
                lab <- off_label(mode, set)
                # Each port's sink minimises sea-leg + storage cost for this mode (issue #104)
                Cm <- sweep(off_modes[[mode]] * D * 1000, 2, stor_m[cls_off], "+")
                bm <- best_col(Cm, off_sets[[set]])
                bm$sea <- D[cbind(seq_len(nrow(D)), bm$k)]
                bm$off <- bm$d - min(bm$d[ok_port], na.rm = TRUE)
                lay <- route_and_summarise(lab, port_cells[ok_port], ok_port, ctx, params, offset = bm$off[ok_port])
                pidx <- as.integer(lay[[paste0(lab, "_target")]])
                lay[[paste0(lab, "_sea_km")]] <- bm$sea[pidx]
                lay[[paste0(lab, "_sink")]] <- as.numeric(cls_off[bm$k[pidx]])
                lay_off[names(lay)] <- lay
                gc()
            }
        }
    }

    # --- Write -------------------------------------------------------------------
    all_layers <- c(cell_layers, lay_onsal, lay_oneor, lay_off)
    stack <- terra::rast(r_template, nlyrs = length(all_layers))
    terra::values(stack) <- do.call(cbind, all_layers)
    names(stack) <- names(all_layers)
    out_path <- file.path(proc_dir, paste0(file_prefix, "_transport_layers.tif"))
    terra::writeRaster(stack, out_path, overwrite = TRUE, datatype = "FLT4S", gdal = c("COMPRESS=DEFLATE"))

    sinks_tbl <- sf::st_drop_geometry(sinks_b)
    sinks_tbl$sink <- seq_len(nrow(sinks_tbl))
    utils::write.csv(sinks_tbl, file.path(proc_dir, paste0(file_prefix, "_sinks_lookup.csv")), row.names = FALSE)
    if (!is.null(ports_tbl)) utils::write.csv(ports_tbl, file.path(proc_dir, paste0(file_prefix, "_ports_lookup.csv")), row.names = FALSE)
    message("Saved: ", out_path)

    report_transport_diagnostics(stack, r_template, params)
    invisible(stack)
}

# ==============================================================================
# Diagnostics
# ==============================================================================
report_transport_diagnostics <- function(stack, r_template, params) {
    bio <- terra::values(r_template, mat = FALSE)
    bio <- is.finite(bio) & bio > 0
    q <- function(x) {
        x <- x[bio & is.finite(x)]
        if (!length(x)) return(c(median = NA, p95 = NA, max = NA))
        c(median = stats::median(x), p95 = unname(stats::quantile(x, 0.95)), max = max(x))
    }
    v <- terra::values(stack, dataframe = TRUE)
    rows <- c("onsal_len_km", "onsal_costlen_km", "onsal_terrain_mult", "onsal_detour", "onsal_hrel_max_m",
              "offship_len_km", "offship_sea_km", "offpipe_len_km", "offpipe_sea_km")
    rows <- rows[rows %in% names(v)]
    tab <- t(vapply(rows, function(n) q(v[[n]]), numeric(3)))
    if (isTRUE(params$diagnose_decoupling) && all(c("onsal_costlen_km", "onsal_costopt_km") %in% names(v))) {
        # Skip routes < 10 km: costDist charges half a step into the target, so the ratio is meaningless there
        prem <- ifelse(v$onsal_costopt_km >= 10, v$onsal_costlen_km / v$onsal_costopt_km, NA_real_)
        tab <- rbind(tab, onsal_routing_premium = q(prem))
    }
    message("Diagnostics over biomass cells (median / p95 / max):")
    print(round(tab, 2))
    invisible(tab)
}

#' Synthetic check of the route-tree reconstruction against terra::costDist.
self_test_path_integration <- function(seed = 1) {
    set.seed(seed)
    r <- terra::rast(nrows = 60, ncols = 80, xmin = 0, xmax = 80000, ymin = 0, ymax = 60000, crs = "EPSG:3857")
    geo <- grid_geometry(r)
    tgt <- c(terra::cellFromRowCol(r, 5, 75), terra::cellFromRowCol(r, 55, 75))

    # (1) uniform friction: physical length must match costDist distance
    fr <- rep(1, terra::ncell(r)); fr[tgt] <- 0
    A <- terra::values(terra::costDist(terra::setValues(r, fr), target = 0, maxiter = 500), mat = FALSE)
    tr <- build_parent(A, fr, geo)
    pj <- path_integrate(tr$parent, list(L = tr$step))
    d1 <- max(abs(pj$L - A)[tr$valid]) / terra::res(r)[1]

    # (2) random friction with barriers: route integral must match costDist
    fr <- stats::runif(terra::ncell(r), 1, 10)
    fr[sample(terra::ncell(r), 400)] <- NA
    fr[tgt] <- 0
    A <- terra::values(terra::costDist(terra::setValues(r, fr), target = 0, maxiter = 500), mat = FALSE)
    tr <- build_parent(A, fr, geo)
    e <- tr$step * (fr + fr[tr$parent]) / 2
    e[tr$step == 0] <- 0
    pj <- path_integrate(tr$parent, list(Lr = e))
    rel <- abs(pj$Lr - A)[tr$valid] / pmax(A[tr$valid], 1)
    roots_ok <- all(pj$root[tr$valid] %in% tgt)

    # (3) offset Dijkstra: zero offsets reproduce costDist; with offsets, A = min over sources of
    #     (offset + single-source distance)
    dj <- dijkstra_offsets(fr, geo, tgt, c(0, 0))
    ok3 <- is.finite(A) & is.finite(dj$A)
    rel3 <- abs(dj$A - A)[ok3] / pmax(A[ok3], 1)
    off <- c(0, 20000)
    dj2 <- dijkstra_offsets(fr, geo, tgt, off)
    a1 <- dijkstra_offsets(fr, geo, tgt[1], 0)$A
    a2 <- dijkstra_offsets(fr, geo, tgt[2], 0)$A
    brute <- pmin(a1 + off[1], a2 + off[2])
    ok4 <- is.finite(brute)
    rel4 <- max(abs(dj2$A - brute)[ok4] / pmax(brute[ok4], 1))

    message(sprintf("Test 1 (uniform friction): max |L - costDist| = %.2f cells (expect <= 0.71: costDist charges half of the final step into a zero-friction target)", d1))
    message(sprintf("Test 2 (random friction): median rel. diff %.1e, max %.1e; all roots are targets: %s", stats::median(rel), max(rel), roots_ok))
    message(sprintf("Test 3 (offset Dijkstra, zero offsets vs costDist): median rel. diff %.1e, max %.1e; reachability identical: %s",
                    stats::median(rel3), max(rel3), identical(is.finite(A), is.finite(dj$A))))
    message(sprintf("Test 4 (offset Dijkstra vs brute force min over sources): max rel. diff %.1e", rel4))
    invisible(list(uniform_max_cells = d1, random_rel = rel, roots_ok = roots_ok, dijkstra_rel = rel3, offset_rel = rel4))
}

# ==============================================================================
# Execution
# ==============================================================================
# Production settings (September 2026): Geomorpho90m 250 m slope (Amatulli et al. 2020,
# doi:10.1594/PANGAEA.899135; Int16 degrees x 100); elevation and land/sea mask from the ~1 km
# geodata::elevation_global() fallback. Routing grid = template x 10 (0.02 deg US, 0.01 deg elsewhere).
# Run from BiocharAG/ with the package loaded (co2_sinks). Regions are independent and can run in
# parallel (5-20 GB each).
# self_test_path_integration()

p <- transport_params(
    slope_path = file.path(raw_dir, "geomorpho90m", "dtm_slope_merit.dem_m_250m_s0..0cm_2018_v1.0.tif"),
    slope_scale = 0.01
)

process_transport_layers("North America", "../GIS/processed/us_biomass.tif", "us", hires_factor = 10, params = p)
process_transport_layers("China", "../GIS/processed/china_biomass.tif", "china", hires_factor = 10, params = p)
process_transport_layers("India", "../GIS/processed/india_biomass.tif", "india", hires_factor = 10, params = p)
process_transport_layers("Europe", "../GIS/processed/europe_biomass.tif", "europe", hires_factor = 10, params = p)
# nolint end
