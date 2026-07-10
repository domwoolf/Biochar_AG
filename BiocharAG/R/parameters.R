#' Default Parameters Dataset
#'
#' A list containing the default parameters for the BiocharAG model.
#'
#' @format A named list.
"default_parameters"

#' Normalize Region Name
#'
#' Standardizes regional names and aliases to match `regional_overrides` keys.
#'
#' @param region Character string indicating region.
#' @return Normalized region character string ("US", "India", "China", "Europe").
#' @export
normalize_region_name <- function(region) {
  if (is.null(region) || is.na(region)) {
    return("US")
  }
  r <- trimws(region)
  if (r %in% c("EU", "Europe")) {
    return("Europe")
  }
  if (r %in% c("US", "USA", "North America")) {
    return("US")
  }
  return(r)
}

#' Regional Overrides List
#'
#' A predefined list of regional parameter overrides for non-spatial parameters
#' (financial, capital cost modifiers, O&M labor factors, and fertilizer prices).
#' @export
regional_overrides <- list(
  US = list(),
  India = list(
    discount_rate = 0.12,
    bes_capital_cost = 3000 * 0.7,
    beccs_capital_cost = 4000 * 0.7,
    bes_om_factor = 0.025,
    beccs_om_factor = 0.03,
    price_n = 0.30,
    price_p = 0.80,
    price_k = 0.40,
    price_lime = 40,
    soil_ph_target = 6.5
  ),
  China = list(
    discount_rate = 0.10,
    bes_capital_cost = 3000 * 0.75,
    beccs_capital_cost = 4000 * 0.75,
    bes_om_factor = 0.03,
    beccs_om_factor = 0.035,
    price_n = 0.70,
    price_p = 0.90,
    price_k = 0.55
  ),
  Europe = list(
    discount_rate = 0.07,
    bes_capital_cost = 3000 * 1.15,
    beccs_capital_cost = 4000 * 1.15,
    bes_om_factor = 0.045,
    beccs_om_factor = 0.055,
    price_n = 1.05,
    price_p = 1.25,
    price_k = 0.75
  )
)

#' Apply Regional Overrides
#'
#' Applies regional non-spatial overrides to a parameter list.
#'
#' @param params Parameter list.
#' @param region Optional region string ("US", "India", "China", "Europe"). If NULL, checks `params$region`.
#' @return Parameter list with regional overrides applied.
#' @export
apply_regional_overrides <- function(params, region = NULL) {
  if (is.null(region)) {
    region <- params$region
  }
  if (is.null(region) || is.na(region)) {
    return(params)
  }
  r_key <- normalize_region_name(region)
  if (r_key %in% names(BiocharAG::regional_overrides)) {
    overrides <- BiocharAG::regional_overrides[[r_key]]
    if (length(overrides) > 0) {
      params[names(overrides)] <- overrides
    }
  }
  params$region <- r_key
  return(params)
}

#' Set Scenario Parameters
#'
#' Returns a list of default parameters overridden by a specific scenario
#' and optional regional parameter overrides.
#' Order of precedence: default_parameters -> regional_overrides -> scenario overrides.
#' Values inferred from op_space_2.41.xlsm and tea_literature_review.md.
#'
#' @param scenario A named list of parameters to override the defaults. Defaults to an empty list.
#' @param region Optional character string specifying a region ("US", "India", "China", "Europe").
#' @return A named list of parameters.
#' @export
set_scenario <- function(scenario = list(), region = NULL) {
  params <- BiocharAG::default_parameters
  if (is.null(region) && !is.null(scenario$region)) {
    region <- scenario$region
  }
  if (!is.null(region)) {
    params <- apply_regional_overrides(params, region = region)
  }
  if (length(scenario) > 0) {
    params[names(scenario)] <- scenario
  }
  if (!is.null(region)) {
    params$region <- normalize_region_name(region)
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
