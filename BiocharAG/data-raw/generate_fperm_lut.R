# data-raw/generate_fperm_lut.R

# Load the current package development version
devtools::load_all()

# Define Ranges
soil_temps <- seq(-55, 40, by = 1) # Soil temperatures
hc_vals <- seq(0, 0.7, by = 0.02) # H:C org ratios
py_temps <- seq(350, 1000, by = 10) # Pyrolysis temperatures

message("Generating H:C Look-Up Table...")

# Function wrapper to ensure scalar inputs for calculate_fperm if strictly needed
calc_hc <- function(h, t) {
    calculate_fperm(val = h, method = "HC", soil_temp = t)
}
calc_hc_vec <- Vectorize(calc_hc)

# Outer product for H:C grid
# Rows = H:C values, Cols = Soil Temps
hc_grid <- outer(hc_vals, soil_temps, calc_hc_vec)
dimnames(hc_grid) <- list(as.character(hc_vals), as.character(soil_temps))

message("Generating Pyrolysis Temp Look-Up Table...")

# Wrapper for PyTemp
calc_py <- function(p, t) {
    calculate_fperm(val = p, method = "Temp", soil_temp = t)
}
calc_py_vec <- Vectorize(calc_py)

# Outer product for PyTemp grid
# Rows = PyTemp values, Cols = Soil Temps
temp_grid <- outer(py_temps, soil_temps, calc_py_vec)
dimnames(temp_grid) <- list(as.character(py_temps), as.character(soil_temps))

# Combine into a list
fperm_lut <- list(
    hc_grid = hc_grid,
    temp_grid = temp_grid,
    soil_temps = soil_temps,
    hc_vals = hc_vals,
    py_temps = py_temps
)

message("Saving fperm_lut to data/...")
usethis::use_data(fperm_lut, overwrite = TRUE)
message("Done.")
