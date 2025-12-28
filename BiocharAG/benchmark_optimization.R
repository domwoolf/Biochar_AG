# benchmark_optimization.R

devtools::load_all()
if (!requireNamespace("microbenchmark", quietly = TRUE)) install.packages("microbenchmark")
library(microbenchmark)

# --- 1. Define Prototype Functions for 1D Optimization ---

#' Pre-calculate the Fperm vector for a fixed H/C or PyTemp
#' This corresponds to "slicing" the surface at x = val
prep_fperm_1d <- function(val, method = "HC") {
    lut <- BiocharAG::fperm_lut

    if (method == "HC") {
        grid_z <- lut$hc_grid
        x_grid <- lut$hc_vals
    } else {
        grid_z <- lut$temp_grid
        x_grid <- lut$py_temps
    }

    # Clamp val
    val <- pmax(min(x_grid), pmin(max(x_grid), val))

    # Find X Interval (once)
    idx_x <- findInterval(val, x_grid, all.inside = TRUE)
    x0 <- x_grid[idx_x]
    x1 <- x_grid[idx_x + 1]
    wx <- (val - x0) / (x1 - x0)

    # Interpolate the entire column vector for this val
    # V[j] = Z[x0, j]*(1-w) + Z[x1, j]*w
    # grid_z is [X, Y]
    col0 <- grid_z[idx_x, ]
    col1 <- grid_z[idx_x + 1, ]

    fperm_vec <- col0 * (1 - wx) + col1 * wx

    list(vec = fperm_vec, y_grid = lut$soil_temps)
}

#' Calculate from 1D vector
calc_fperm_1d <- function(prep, soil_temp) {
    y_grid <- prep$y_grid
    vec <- prep$vec

    # Clamp Temp
    soil_temp <- pmax(min(y_grid), pmin(max(y_grid), soil_temp))

    # 1D Interpolation
    idx_y <- findInterval(soil_temp, y_grid, all.inside = TRUE)
    y0 <- y_grid[idx_y]
    y1 <- y_grid[idx_y + 1]
    wy <- (soil_temp - y0) / (y1 - y0)

    # Interpolate
    v0 <- vec[idx_y]
    v1 <- vec[idx_y + 1]

    res <- v0 * (1 - wy) + v1 * wy
    return(res)
}

# --- 2. Setup Benchmark ---

# Scenario:
# Fixed H:C = 0.35
# Run for 10,000 randomized soil temperatures (representing global grid cells)
hc_val <- 0.35
n_points <- 10000
temps <- runif(n_points, -10, 35)

cat("Generating 1D Pre-calc object...\n")
prep_obj <- prep_fperm_1d(hc_val, "HC")

# Wrapper for bilinear (current approx)
# We assume the user would pass the fixed 'val' repeatedly
run_bilinear <- function(t_vec) {
    # Emulate vectorized call for varying temp, fixed val
    # calculate_fperm_approx handles vectorized inputs usually?
    # Let's verify our fperm_approx handles vectorized soil_temp with scalar val
    calculate_fperm_approx(hc_val, "HC", t_vec)
}

# Wrapper for 1D
run_1d <- function(t_vec) {
    # We need to vectorize 'calc_fperm_1d' or implement it vectorized
    # Implementing vectorized version inside here for fair comparison
    y_grid <- prep_obj$y_grid
    vec <- prep_obj$vec

    t_vec <- pmax(min(y_grid), pmin(max(y_grid), t_vec))

    idx_y <- findInterval(t_vec, y_grid, all.inside = TRUE)
    # y0 <- y_grid[idx_y] # Slower indexing?
    # Optimization: y_grid is regular step? yes, 1.0 degree.
    # If regular step, we can calc index directly: idx = floor(t - min) + 1
    # But let's stick to generic 'findInterval' speed first.

    y0 <- y_grid[idx_y]
    y1 <- y_grid[idx_y + 1]
    wy <- (t_vec - y0) / (y1 - y0)

    v0 <- vec[idx_y]
    v1 <- vec[idx_y + 1]

    v0 * (1 - wy) + v1 * wy
}

# Verify Correctness
cat("Verifying correctness...\n")
check_temps <- c(10, 20, 30)
exact_check <- calculate_fperm_approx(hc_val, "HC", check_temps)
opt_check <- run_1d(check_temps)
diffs <- abs(exact_check - opt_check)
cat(sprintf("Max diff: %f\n", max(diffs)))
if (max(diffs) > 1e-10) warning("Logic mismatch!")

# Benchmark
cat("\nRunning Benchmark (Vectorized N=10,000)...\n")
mb <- microbenchmark(
    Bilinear_2D = run_bilinear(temps),
    PreCalc_1D = run_1d(temps),
    times = 100
)

print(mb)

cat("\nAnalysis:\n")
med_2d <- summary(mb)$median[1]
med_1d <- summary(mb)$median[2]
cat(sprintf("2D Median: %.2f us\n", med_2d))
cat(sprintf("1D Median: %.2f us\n", med_1d))
cat(sprintf("Speedup: %.2fx\n", med_2d / med_1d))
