library(dplyr)

#' Populate Uncertainty Distributions in Parameters CSV
#'
#' @param input_file Path to the input parameters.csv
#' @param output_file Path to save the populated csv
#' @return A populated dataframe
populate_uncertainty_bounds <- function(input_file, output_file = "parameters_populated.csv") {
  
  # 1. Define CV mapping based on uncertainty_level
  cv_map <- c("low" = 0.05, "medium" = 0.20, "high" = 0.40)
  
  # Read the CSV
  df <- read.csv(input_file, stringsAsFactors = FALSE)
  
  # 2. Process the dataframe
  df_populated <- df %>%
    mutate(
      # Ensure numeric typing for calculations without overwriting non-numeric default values
      default_val_num = suppressWarnings(as.numeric(default_value)),
      
      # Extract the target CV
      target_cv = cv_map[trimws(tolower(uncertainty_level))],
      
      # Calculate Dispersion
      dispersion = case_when(
        is.na(target_cv) | tolower(distribution) == "none" ~ NA_real_,
        
        # Normal: Dispersion is the Standard Deviation (sigma)
        tolower(distribution) == "normal" ~ abs(default_val_num * target_cv),
        
        # Lognormal: Dispersion is the shape parameter (sigma_log)
        tolower(gsub("[- ]", "", distribution)) == "lognormal" ~ sqrt(log(1 + target_cv^2)),
        
        # Uniform/Triangular: Dispersion usually left NA as we use min/max
        TRUE ~ NA_real_
      ),
      
      # Calculate Minimum Bound
      minimum = case_when(
        is.na(target_cv) | tolower(distribution) == "none" ~ NA_real_,
        
        # Uniform: Min = Mean - (Mean * CV * sqrt(3))
        tolower(distribution) == "uniform" ~ default_val_num - (abs(default_val_num) * target_cv * sqrt(3)),
        
        # Normal (Truncated): Bound at -3 standard deviations
        tolower(distribution) == "normal" ~ default_val_num - (3 * dispersion),
        
        # Lognormal: Bounded by 0 mathematically, but can set physical lower limits if desired
        tolower(gsub("[- ]", "", distribution)) == "lognormal" ~ 0,
        
        TRUE ~ NA_real_
      ),
      
      # Calculate Maximum Bound
      maximum = case_when(
        is.na(target_cv) | tolower(distribution) == "none" ~ NA_real_,
        
        # Uniform: Max = Mean + (Mean * CV * sqrt(3))
        tolower(distribution) == "uniform" ~ default_val_num + (abs(default_val_num) * target_cv * sqrt(3)),
        
        # Normal (Truncated): Bound at +3 standard deviations
        tolower(distribution) == "normal" ~ default_val_num + (3 * dispersion),
        
        # Lognormal: Bound at +3 sigma_log equivalent (or leave NA to let long tail ride)
        tolower(gsub("[- ]", "", distribution)) == "lognormal" ~ exp(log(default_val_num) - (dispersion^2)/2 + 3*dispersion),
        
        TRUE ~ NA_real_
      )
    ) %>%
    
    # 3. Sanity Checks & Physical Clamping
    mutate(
      # Prevent strictly positive variables from having negative minimums
      # (assuming any parameter that defaults > 0 shouldn't drop below 0 in physics/economics)
      minimum = ifelse(!is.na(minimum) & !is.na(default_val_num) & default_val_num > 0 & minimum < 0, 0, minimum),
      
      # For fractional units, clamp maximum to 1.0
      maximum = ifelse(!is.na(maximum) & grepl("fraction|%|ratio", units, ignore.case = TRUE) & maximum > 1, 1.0, maximum)
    ) %>%
    
    # Cleanup temporary columns
    select(-target_cv, -default_val_num)

  # Write back to disk
  write.csv(df_populated, output_file, row.names = FALSE)
  message(paste("Successfully populated parameter uncertainties and saved to", output_file))
  
  return(df_populated)
}

# Execution
if (sys.nframe() == 0L) {
  df <- populate_uncertainty_bounds("parameters.csv", "parameters_mc_ready.csv")
}