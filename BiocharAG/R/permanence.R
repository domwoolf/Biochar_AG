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
