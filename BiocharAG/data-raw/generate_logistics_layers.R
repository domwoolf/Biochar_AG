# nolint start: indentation_linter, line_length_linter, object_usage_linter, commented_code_linter
# ==============================================================================
# generate_logistics_layers.R -- terrain / road-network factors for biomass haulage
# ------------------------------------------------------------------------------
# For every candidate plant cell (template grid) and plant size, travel from all
# fields in the collection area to the plant is routed on the Weiss et al. (2020)
# motorised friction surface (minutes per metre, 30 arc-seconds; Malaria Atlas
# Project, 202001_Global_Motorized_Friction_Surface). Biomass-weighted over the
# collection area this gives, per unit straight-line distance:
#   time      T / E   (min per straight km)  -> speed and connectivity
#   road path L / E   (circuity)
#   climb     cumulative ascent loaded (field -> plant) and empty (plant -> field)
# Outputs (<prefix>_haul_factors.tif), per plant size <sz> MWth:
#   kt_<sz>  time factor      = (T/E) / regional biomass-weighted mean
#   kd_<sz>  distance factor  = (L/E) / regional biomass-weighted mean
#   g_<sz>   fuel grade factor = (1 + climb fuel / flat-road fuel) / regional mean
#            climb fuel = m g (ascent - descent_recovery x descent) / (engine eff x diesel LHV) per leg
# kt, kd and g are normalised within each region because the regional level of
# haulage cost (road quality, speeds, wages) is already in haulage_location_factor,
# which is based on market road-freight rates, and the base fuel rate already reflects
# typical rolling terrain. They redistribute cost within a
# region, from well-connected flat areas to rugged or poorly connected ones.
#
# The plant is placed at the fastest (lowest-friction) sub-cell of its template
# cell. Biomass density is spread uniformly over the sub-cells of each template
# cell. Collection radius = 1.5 x avg_dist (avg_dist = 2/3 radius; see
# generate_distance_rasters.R); the routing window is capped at window_cap_km,
# beyond which the factors are estimated from the fields inside the cap.
#
# Run from BiocharAG/ with the friction clips in ../GIS/raw/map_friction/
# (<prefix>_motorized_friction_2020.tif, WCS subset of the global surface).
# ==============================================================================
library(terra)

raw_dir <- "../GIS/raw"
proc_dir <- "../GIS/processed"

logistics_params <- function(...) {
    p <- list(
        sizes_mw_th = c(5, 25, 50, 100, 125, 150, 250, 500),
        window_cap_km = 200,
        # Climb fuel (heavy truck, baled residue is volume-limited)
        payload_t = 20, tare_t = 15,
        engine_eff = 0.40, diesel_mj_per_l = 36,
        descent_recovery = 0.5, # share of descent energy offsetting climbs on the same leg (coasting)
        flat_l_per_km_loaded = 0.38, flat_l_per_km_empty = 0.28,
        cores = max(1L, min(20L, parallel::detectCores() - 4L)),
        test_n = NULL # timing test: route only this many random plant cells, write nothing
    )
    utils::modifyList(p, list(...))
}

# Single-source Dijkstra on a window (8-neighbour, edge cost = step x mean friction of its two
# cells, as terra::costDist). Accumulates along each least-time path: time, path length, ascent
# and descent (from the plant outward).
.haul_src <- '
#include <Rcpp.h>
#include <queue>
using namespace Rcpp;
// [[Rcpp::export]]
List haul_dijkstra(NumericVector fr, NumericVector h, int nr, int nc, NumericVector dx_row, double dy, int src) {
    const int n = fr.size();
    NumericVector T(n, R_PosInf), L(n, NA_REAL), up(n, NA_REAL), dn(n, NA_REAL);
    std::vector<char> done(n, 0);
    typedef std::pair<double, int> P;
    std::priority_queue<P, std::vector<P>, std::greater<P> > pq;
    int s0 = src - 1;
    if (ISNAN(fr[s0])) return List::create(_["T"] = T, _["L"] = L, _["up"] = up, _["dn"] = dn);
    T[s0] = 0; L[s0] = 0; up[s0] = 0; dn[s0] = 0;
    pq.push(P(0.0, s0));
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
            if (done[j] || ISNAN(fr[j])) continue;
            double sx = dc[k] * dx_row[rr], sy = dr[k] * dy;
            double s = std::sqrt(sx * sx + sy * sy);
            double nd = t.first + s * (fr[c] + fr[j]) / 2.0;
            if (nd < T[j]) {
                T[j] = nd; L[j] = L[c] + s;
                double dh = (ISNAN(h[j]) || ISNAN(h[c])) ? 0.0 : h[j] - h[c];
                up[j] = up[c] + (dh > 0 ? dh : 0); dn[j] = dn[c] + (dh < 0 ? -dh : 0);
                pq.push(P(nd, j));
            }
        }
    }
    return List::create(_["T"] = T, _["L"] = L, _["up"] = up, _["dn"] = dn);
}'
Rcpp::sourceCpp(code = .haul_src)

generate_logistics_layers <- function(prefix, params = logistics_params()) {
    message("\n=== Haulage factors: ", prefix, " ===")
    t0 <- Sys.time()
    tmpl <- rast(file.path(proc_dir, paste0(prefix, "_biomass.tif")))
    fr <- rast(file.path(raw_dir, "map_friction", paste0(prefix, "_motorized_friction_2020.tif")))
    fr <- classify(fr, cbind(-Inf, 0, NA)) # -9999 nodata / non-positive -> impassable
    dem <- geodata::elevation_global(res = 0.5, path = raw_dir)
    dem <- project(crop(dem, ext(fr) + 0.1), fr, method = "bilinear")
    dens <- project(tmpl, fr, method = "near") # Mg/km2, uniform within template cells
    area <- cellSize(fr, unit = "km")
    nr <- nrow(fr); nc <- ncol(fr)
    Fv <- values(fr, mat = FALSE)
    Hv <- values(dem, mat = FALSE)
    Mv <- values(dens * area, mat = FALSE); Mv[!is.finite(Mv) | Mv < 0] <- 0
    R_earth <- 6371.0088
    lat <- yFromRow(fr, seq_len(nr))
    dx_km <- res(fr)[1] * pi / 180 * R_earth * cos(lat * pi / 180)
    dy_km <- res(fr)[2] * pi / 180 * R_earth

    # Plant cells and radii per size
    bio <- values(tmpl, mat = FALSE)
    plant_cells <- which(is.finite(bio) & bio > 0)
    radius <- sapply(params$sizes_mw_th, function(s) {
        values(rast(file.path(proc_dir, paste0(prefix, "_dist_", s, "MWth.tif"))), mat = FALSE)[plant_cells] * 1.5
    })
    colnames(radius) <- params$sizes_mw_th
    xy <- xyFromCell(tmpl, plant_cells)
    half <- res(tmpl) / 2

    one_plant <- function(i) {
        out <- matrix(NA_real_, 5, ncol(radius)) # sumT, sumL, sumE, sumClimbFuel, sumFlatFuel (weighted)
        rmax <- suppressWarnings(max(radius[i, ], na.rm = TRUE))
        if (!is.finite(rmax)) return(out)
        W <- min(rmax, params$window_cap_km)
        # Plant at the fastest sub-cell of its template cell
        r_a <- rowFromY(fr, xy[i, 2] + half[2] - 1e-9); r_b <- rowFromY(fr, xy[i, 2] - half[2] + 1e-9)
        c_a <- colFromX(fr, xy[i, 1] - half[1] + 1e-9); c_b <- colFromX(fr, xy[i, 1] + half[1] - 1e-9)
        if (anyNA(c(r_a, r_b, c_a, c_b))) return(out)
        sub <- as.vector(outer((r_a:r_b - 1L) * nc, c_a:c_b, "+"))
        if (all(!is.finite(Fv[sub]))) return(out)
        pc <- sub[which.min(Fv[sub])]
        pr <- (pc - 1L) %/% nc + 1L; pcc <- (pc - 1L) %% nc + 1L
        wy <- ceiling(W / dy_km); wx <- ceiling(W / dx_km[pr])
        rows <- max(1L, pr - wy):min(nr, pr + wy); cols <- max(1L, pcc - wx):min(nc, pcc + wx)
        idx <- as.vector(t(outer((rows - 1L) * nc, cols, "+"))) # row-major window
        wnr <- length(rows); wnc <- length(cols)
        src <- (pr - rows[1]) * wnc + (pcc - cols[1]) + 1L
        d <- haul_dijkstra(Fv[idx], Hv[idx], wnr, wnc, dx_km[rows] * 1000, dy_km * 1000, src)
        rr <- rep(rows, each = wnc); cc <- rep.int(cols, wnr)
        E <- sqrt(((cc - pcc) * dx_km[rr])^2 + ((rr - pr) * dy_km)^2)
        m <- Mv[idx]
        ok <- is.finite(d$T) & m > 0 & E > 0
        # Round-trip fuel per trip (per tonne payload; common factors cancel in the ratio)
        g0 <- 9.81 / (params$engine_eff * params$diesel_mj_per_l * 1e6) # litres per (kg m)
        # Loaded leg field -> plant climbs the forward descent (dn); empty leg plant -> field climbs up
        rec <- params$descent_recovery
        climb_l <- g0 * ((params$payload_t + params$tare_t) * 1000 * pmax(0, d$dn - rec * d$up) + params$tare_t * 1000 * pmax(0, d$up - rec * d$dn))
        flat_l <- (params$flat_l_per_km_loaded + params$flat_l_per_km_empty) * d$L / 1000
        for (k in seq_len(ncol(radius))) {
            R <- min(radius[i, k], params$window_cap_km)
            if (!is.finite(R)) next
            sel <- ok & E <= max(R, 1.5 * dy_km)
            if (!any(sel)) next
            w <- m[sel]
            out[, k] <- c(sum(w * d$T[sel]), sum(w * d$L[sel] / 1000), sum(w * E[sel]), sum(w * climb_l[sel]), sum(w * flat_l[sel]))
        }
        out
    }

    message(sprintf("  %d plant cells, %d cores", length(plant_cells), params$cores))
    if (!is.null(params$test_n)) {
        ii <- sample(seq_along(plant_cells), min(params$test_n, length(plant_cells)))
        tt <- system.time(r <- parallel::mclapply(ii, one_plant, mc.cores = params$cores))
        A <- simplify2array(r)
        message(sprintf("  test: %d cells in %.1f s -> full region ~%.1f min", length(ii), tt[["elapsed"]], tt[["elapsed"]] * length(plant_cells) / length(ii) / 60))
        k <- which(params$sizes_mw_th == 125)
        print(summary(data.frame(kt_raw = A[1, k, ] / A[3, k, ], circuity = A[2, k, ] / A[3, k, ], g = 1 + A[4, k, ] / A[5, k, ])))
        return(invisible(A))
    }
    res_list <- parallel::mclapply(seq_along(plant_cells), one_plant, mc.cores = params$cores, mc.preschedule = TRUE)
    bad <- vapply(res_list, function(x) !is.matrix(x), logical(1))
    if (any(bad)) stop(sum(bad), " plant cells failed: ", conditionMessage(attr(res_list[[which(bad)[1]]], "condition")))
    A <- simplify2array(res_list) # 5 x sizes x cells

    mass_cell <- bio[plant_cells] * values(cellSize(tmpl, unit = "km"), mat = FALSE)[plant_cells]
    layers <- list(); norm <- data.frame()
    for (k in seq_along(params$sizes_mw_th)) {
        sz <- params$sizes_mw_th[k]
        kt_raw <- A[1, k, ] / A[3, k, ]
        kd_raw <- A[2, k, ] / A[3, k, ]
        g <- 1 + A[4, k, ] / A[5, k, ]
        okn <- is.finite(kt_raw) & is.finite(kd_raw)
        mt <- sum(mass_cell[okn] * kt_raw[okn]) / sum(mass_cell[okn])
        md <- sum(mass_cell[okn] * kd_raw[okn]) / sum(mass_cell[okn])
        mg <- sum(mass_cell[okn] * g[okn]) / sum(mass_cell[okn])
        norm <- rbind(norm, data.frame(size_mw_th = sz, min_per_straight_km = mt, circuity = md,
                                       implied_speed_kmh = 60 * md / mt, grade_fuel_factor = mg))
        for (nm in c("kt", "kd", "g")) {
            v <- rep(NA_real_, ncell(tmpl))
            v[plant_cells] <- switch(nm, kt = kt_raw / mt, kd = kd_raw / md, g = g / mg)
            layers[[paste0(nm, "_", sz)]] <- v
        }
    }
    st <- rast(tmpl, nlyrs = length(layers))
    values(st) <- do.call(cbind, layers)
    names(st) <- names(layers)
    out <- file.path(proc_dir, paste0(prefix, "_haul_factors.tif"))
    writeRaster(st, out, overwrite = TRUE, datatype = "FLT4S", gdal = "COMPRESS=DEFLATE")
    utils::write.csv(norm, file.path(proc_dir, paste0(prefix, "_haul_factors_norm.csv")), row.names = FALSE)
    message("  Saved ", out, sprintf(" (%.1f min)", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    print(norm, digits = 3)
    for (sz in c(25, 125, 500)) {
        q <- sapply(c("kt", "kd", "g"), function(nm) stats::quantile(layers[[paste0(nm, "_", sz)]], c(.05, .5, .95), na.rm = TRUE))
        message(sprintf("  %d MWth quantiles (p5/p50/p95):", sz)); print(round(q, 3))
    }
    invisible(st)
}

# ==============================================================================
# Execution
# ==============================================================================
if (sys.nframe() == 0L) {
    args <- commandArgs(trailingOnly = TRUE)
    for (pre in if (length(args)) args else c("us", "europe", "china", "india")) generate_logistics_layers(pre)
}
# nolint end
