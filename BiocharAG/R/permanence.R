#' Calculate Biochar Permanence (Fperm)
#'
#' Calculates the fraction of biochar carbon remaining after a specified time frame (Fperm),
#' using the method described by Woolf et al. (2021).
#' The function dynamically adjusts reference decay rates to the local soil temperature
#' before fitting a regression model (H:C ratio or Pyrolysis Temperature) to predict stability.
#'
#' @param val The independent variable value (H/Corg ratio or Pyrolysis Temperature).
#' @param method Character string specifying the method: "HC" (Hydrogen:Organic Carbon ratio) or "Temp" (Pyrolysis Temperature). Default is "HC".
#' @param soil_temp Mean annual soil temperature (Celsius). Default is 14.9.
#' @param time_years Time frame for permanence calculation (years). Default is 100.
#' @return Numeric fraction (0 to 1).
#' @importFrom utils read.csv
#' @importFrom stats lm predict coefficients
#' @export
calculate_fperm <- function(val, method = "HC", soil_temp = 14.9, time_years = 100) {
    # 1. Load Reference Data
    # Access the lazy-loaded dataset 'fperm_data'
    # In some testing contexts, it might need explicit loading
    if (!exists("fperm_data")) {
        try(utils::data("fperm_data", package = "BiocharAG", envir = environment()), silent = TRUE)
    }

    # Check if loaded successfully
    if (!exists("fperm_data")) {
        stop("Dataset 'fperm_data' not found. Ensure BiocharAG is properly loaded.")
    }

    # 2. Adjust Reference Data to Target Soil Temperature
    # Q10 function: Q10(T) = 1.1 + 12.0 * exp(-0.19 * T)
    # We need to shift from T_expt to soil_temp.

    # Check if T_expt column exists, default to 20 or similar if missing?
    # The CSV has "Temp_of_experiment".
    t_expt <- fperm_data$Temp_of_experiment
    # Handle NAs? Rmd didn't show NAs handling for T_expt but let's assume valid.

    # Vectorized calculation for efficiency (avoid data.table dependency for now)

    # Calculate Q10_avg (q10)
    # q10 = (Integral(T_soil to T_expt) / (T_expt - T_soil))
    # Integral: 1.1*T - (12/0.19)*exp(-0.19*T)
    # Term at T: 1.1*T + 63.1579*exp(-0.19*T) ... wait sign.
    # Rmd: 1.1(T2 - T1) - 63.1579(exp(-0.19T2) - exp(-0.19T1))
    # Rmd q10 formula line 88:
    # (1.1 *(T_expt-soil_temperature) - 63.1579 * exp(-0.19*T_expt) + 63.1579 *exp(-0.19*soil_temperature)) / (T_expt-soil_temperature)

    delta_t <- t_expt - soil_temp

    # Handle case where delta_t is close to 0 to avoid division by zero
    q10 <- ifelse(abs(delta_t) < 0.001,
        1.1 + 12.0 * exp(-0.19 * soil_temp), # Limit as T_expt -> soil_temp
        (1.1 * delta_t - 63.15789 * (exp(-0.19 * t_expt) - exp(-0.19 * soil_temp))) / delta_t
    )

    # Shift factor fT
    # fT = exp(log(q10) * (soil_temp - T_expt) / 10)
    # Note: shift FROM expt TO soil.
    # If soil < expt (cooling), rate should decrease. Q10 > 1. (Soil - Expt) is negative.
    fT <- exp(log(q10) * (soil_temp - t_expt) / 10)

    # Adjust decay rates k (k1, k2, k3) parameters in CSV
    # Note: C1, C2, C3 are percentages in CSV (based on Rmd line 80 dividing by 100).
    c1 <- fperm_data$C1 / 100
    c2 <- fperm_data$C2 / 100
    c3 <- fperm_data$C3 / 100

    k1_adj <- fperm_data$k1 * fT
    k2_adj <- fperm_data$k2 * fT
    k3_adj <- fperm_data$k3 * fT

    # Calculate Reference Fperm at t = time_years
    fperm_ref <- c1 * exp(-k1_adj * time_years) +
        c2 * exp(-k2_adj * time_years) +
        c3 * exp(-k3_adj * time_years)

    # 3. Fit Regression Model
    if (method == "HC") {
        # Use H:C ratio (Column "H_to_Corg" or "H_C_used"?)
        # Rmd line 79: H_to_Corg renamed to H_C.
        # We'll use H_to_Corg.
        x_var <- fperm_data$H_to_Corg
        valid <- !is.na(x_var) & !is.na(fperm_ref)

        model <- lm(fperm_ref[valid] ~ x_var[valid])

        # Predict for input val
        pred <- coef(model)[1] + coef(model)[2] * val
    } else if (method == "Temp") {
        # Use Pyrolysis Temperature
        x_var <- fperm_data$Pyrolysis_temperature

        # Rmd filtered for Temp >= 350 for regression logic usually?
        # Rmd line 143: Pyrolysis_temperature > 350
        valid <- !is.na(x_var) & !is.na(fperm_ref) & x_var >= 350

        model <- lm(fperm_ref[valid] ~ x_var[valid])

        pred <- coef(model)[1] + coef(model)[2] * val
    } else {
        stop("Invalid method. Choose 'HC' or 'Temp'.")
    }

    # 4. Return Result (clamped 0-1)
    result <- as.numeric(pred) # Drop names
    result <- pmin(1.0, pmax(0.0, result))

    return(result)
}

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

#' Prepare Fperm 1D Vector (Optimization)
#'
#' Pre-calculates a 1D vector of Fperm values across the entire soil temperature range
#' for a fixed input value (H:C ratio or Pyrolysis Temperature). This object can be
#' passed to `calculate_fperm_vectorized` for highly efficient repeated calculations.
#'
#' @param val The fixed independent variable value (H/Corg ratio or Pyrolysis Temperature).
#' @param method Character string: "HC" or "Temp". Default is "HC".
#' @return A list containing the `vec` (Fperm values) and `y_grid` (Soil Temperatures).
#' @export
prep_fperm_vector <- function(val, method = "HC") {
    # Ensure LUT is loaded
    if (!exists("fperm_lut")) {
        try(utils::data("fperm_lut", package = "BiocharAG", envir = environment()), silent = TRUE)
    }

    if (!exists("fperm_lut")) {
        stop("Dataset 'fperm_lut' not found.")
    }

    lut <- fperm_lut

    if (method == "HC") {
        grid_z <- lut$hc_grid
        x_grid <- lut$hc_vals
    } else if (method == "Temp") {
        grid_z <- lut$temp_grid
        x_grid <- lut$py_temps
    } else {
        stop("Invalid method.")
    }

    # Clamp val to grid bounds
    val <- pmax(min(x_grid), pmin(max(x_grid), val))

    # Find X Interval (once)
    idx_x <- findInterval(val, x_grid, all.inside = TRUE)
    x0 <- x_grid[idx_x]
    x1 <- x_grid[idx_x + 1]
    wx <- (val - x0) / (x1 - x0)

    # Interpolate the entire column vector for this val
    # grid_z is [X, Y], so we take rows idx_x and idx_x+1
    col0 <- grid_z[idx_x, ]
    col1 <- grid_z[idx_x + 1, ]

    fperm_vec <- col0 * (1 - wx) + col1 * wx

    list(vec = fperm_vec, y_grid = lut$soil_temps)
}

#' Calculate Fperm from Pre-calculated Vector (Optimization)
#'
#' Calculates Fperm for a vector of soil temperatures using 1D linear interpolation
#' on a pre-calculated Fperm vector (created by `prep_fperm_vector`).
#'
#' @param prep A list object returned by `prep_fperm_vector`.
#' @param soil_temp Numeric vector of soil temperatures (Celsius).
#' @return Numeric vector of Fperm fractions.
#' @export
calculate_fperm_vectorized <- function(prep, soil_temp) {
    y_grid <- prep$y_grid
    vec <- prep$vec

    # Clamp Temp
    soil_temp <- pmax(min(y_grid), pmin(max(y_grid), soil_temp))

    # 1D Linear Interpolation
    idx_y <- findInterval(soil_temp, y_grid, all.inside = TRUE)

    y0 <- y_grid[idx_y]
    y1 <- y_grid[idx_y + 1]

    # Calculate weight
    wy <- (soil_temp - y0) / (y1 - y0)

    # Interpolate values
    v0 <- vec[idx_y]
    v1 <- vec[idx_y + 1]

    res <- v0 * (1 - wy) + v1 * wy
    return(as.numeric(res))
}
