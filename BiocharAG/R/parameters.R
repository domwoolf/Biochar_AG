#' Default Parameters Dataset
#'
#' A list containing the default parameters for the BiocharAG model.
#'
#' @format A named list.
"default_parameters"

#' Set Scenario Parameters
#'
#' Returns a list of default parameters overridden by a specific scenario.
#' Values inferred from op_space_2.41.xlsm and tea_literature_review.md.
#'
#' @param scenario A named list of parameters to override the defaults. Defaults to an empty list.
#' @return A named list of parameters.
#' @export
set_scenario <- function(scenario = list()) {
  params <- BiocharAG::default_parameters
  if (length(scenario) > 0) {
    params[names(scenario)] <- scenario
  }
  return(params)
}

#' Load Parameters from Configuration CSV
#'
#' @param file Path to the parameters.csv file.
#' @param as_dataframe Logical. If TRUE, returns the full dataframe with all columns for sensitivity/MC analysis. If FALSE, returns a standard named list of default values.
#' @return A list or dataframe of parameters.
#' @export
load_parameters <- function(file, as_dataframe = FALSE) {
  df <- utils::read.csv(file, stringsAsFactors = FALSE)
  if (as_dataframe) {
    return(df)
  }
  
  params <- BiocharAG::default_parameters
  for (i in seq_len(nrow(df))) {
    name <- df$name[i]
    val_str <- df$default_value[i]
    
    if (name %in% names(params)) {
      orig_val <- params[[name]]
      if (is.logical(orig_val)) {
        params[[name]] <- as.logical(val_str)
      } else if (is.numeric(orig_val)) {
        if (length(orig_val) > 1) {
           params[[name]] <- as.numeric(trimws(unlist(strsplit(val_str, ","))))
        } else {
           params[[name]] <- as.numeric(val_str)
        }
      } else {
        params[[name]] <- val_str
      }
    } else {
      params[[name]] <- utils::type.convert(val_str, as.is = TRUE)
    }
  }
  return(params)
}

#' Resolve plant_mw_th for a specific technology
#' @param plant_mw_th A single numeric value or named vector of numeric values.
#' @param tech Character string ("BES", "BECCS", or "BEBCS").
#' @return A single numeric value.
resolve_plant_mw_th <- function(plant_mw_th, tech) {
  if (is.null(plant_mw_th)) {
    return(50)
  }
  if (length(plant_mw_th) > 1 && !is.null(names(plant_mw_th))) {
    if (!is.na(tech) && tech %in% names(plant_mw_th)) {
      return(plant_mw_th[[tech]])
    } else {
      return(plant_mw_th[1])
    }
  }
  return(plant_mw_th)
}

#' Scenarios List
#'
#' A predefined list of scenarios used to override default parameters.
#' @export
scenarios <- list(
  default = list(),
  CP100_MW250 = list(
    c_price = 100,
    plant_mw_th = c(BES = 250, BECCS = 250, BEBCS = 250)
  ),
  EA = list(
    early_adoption = TRUE
  ),
  EA_CP100_MW250 = list(
    c_price = 100,
    plant_mw_th = c(BES = 250, BECCS = 250, BEBCS = 250),
    early_adoption = TRUE
  ),
  EA_CP100_MW250_EOR = list(
    c_price = 100,
    plant_mw_th = c(BES = 250, BECCS = 250, BEBCS = 250),
    early_adoption = TRUE,
    allow_eor = TRUE
  )

)
