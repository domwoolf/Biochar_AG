#' Calculate Approximate Biochar Permanence (Fast)
#'
#' Calculates Fperm using bilinear interpolation over pre-calculated look-up tables (LUTs).
#' This function is significantly faster than `calculate_fperm` but approximates the result.
#'
#' @param val The independent variable value (H/Corg ratio or Pyrolysis Temperature).
#' @param method Character string specifying the method: "HC" (Hydrogen:Organic Carbon ratio) or "Temp" (Pyrolysis Temperature). Default is "HC".
#' @param soil_temp Mean annual soil temperature (Celsius).
#' @return Numeric fraction (0 to 1).
#' @export
calculate_fperm_approx <- function(val, method = "HC", soil_temp) {
    # Ensure LUT is loaded
    if (!exists("fperm_lut")) {
        try(utils::data("fperm_lut", package = "BiocharAG", envir = environment()), silent = TRUE)
    }

    if (!exists("fperm_lut")) {
        stop("Dataset 'fperm_lut' not found. Ensure BiocharAG is properly loaded or data/fperm_lut.rda exists.")
    }

    lut <- fperm_lut

    # Select Grid
    if (method == "HC") {
        grid_z <- lut$hc_grid
        x_grid <- lut$hc_vals

        # Bound input val (H:C should be 0 - 0.7 roughly)
        # If val is outside range, we clamp it to the nearest edge
        # Warning: extrapolation is dangerous, clamping is safer for Fperm.
        val <- pmax(min(x_grid), pmin(max(x_grid), val))
    } else if (method == "Temp") {
        grid_z <- lut$temp_grid
        x_grid <- lut$py_temps

        val <- pmax(min(x_grid), pmin(max(x_grid), val))
    } else {
        stop("Invalid method. Choose 'HC' or 'Temp'.")
    }

    y_grid <- lut$soil_temps
    soil_temp <- pmax(min(y_grid), pmin(max(y_grid), soil_temp))

    # Bilinear Interpolation

    # Find indices
    # findInterval returns index i such that vec[i] <= x < vec[i+1]
    # We need x0 and x1

    idx_x <- findInterval(val, x_grid, all.inside = TRUE)
    idx_y <- findInterval(soil_temp, y_grid, all.inside = TRUE)

    x0 <- x_grid[idx_x]
    x1 <- x_grid[idx_x + 1]

    y0 <- y_grid[idx_y]
    y1 <- y_grid[idx_y + 1]

    # Values at 4 corners
    # Grid structure: Rows = X (Val), Cols = Y (Temp)
    q11 <- grid_z[idx_x, idx_y] # x0, y0
    q21 <- grid_z[idx_x + 1, idx_y] # x1, y0
    q12 <- grid_z[idx_x, idx_y + 1] # x0, y1
    q22 <- grid_z[idx_x + 1, idx_y + 1] # x1, y1

    # Weights
    wx <- (val - x0) / (x1 - x0)
    wy <- (soil_temp - y0) / (y1 - y0)

    # Interpolate
    # Linear in X at y0
    r1 <- q11 * (1 - wx) + q21 * wx
    # Linear in X at y1
    r2 <- q12 * (1 - wx) + q22 * wx

    # Linear in Y
    res <- r1 * (1 - wy) + r2 * wy

    return(as.numeric(res))
}
