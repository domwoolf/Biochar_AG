#' Default Parameters Dataset
#'
#' A list containing the default parameters for the BiocharAG model.
#'
#' @format A named list.
"default_parameters"

#' Regional Overrides List
#'
#' A predefined list of regional parameter overrides for non-spatial parameters
#' (financial, capital cost modifiers, O&M labor factors, and fertilizer prices).
#' @export
regional_overrides <- list(
  US = list(
    discount_rate = 0.05,
    bes_capital_cost = 3000 * 1.25,
    beccs_capital_cost = 4000 * 1.25,
    bes_om_factor = 0.045,
    beccs_om_factor = 0.055,
    price_n = 1.59,
    price_p = 2.08,
    price_k = 0.82,
    price_lime = 45,
    soil_ph_target = 6.5
  ),
  India = list(
    discount_rate = 0.10,
    bes_capital_cost = 3000 * 0.65,
    beccs_capital_cost = 4000 * 0.65,
    bes_om_factor = 0.025,
    beccs_om_factor = 0.03,
    price_n = 0.14,
    price_p = 0.70,
    price_k = 0.68,
    price_lime = 35,
    soil_ph_target = 6.5
  ),
  China = list(
    discount_rate = 0.045,
    bes_capital_cost = 3000 * 0.7,
    beccs_capital_cost = 4000 * 0.7,
    bes_om_factor = 0.03,
    beccs_om_factor = 0.035,
    price_n = 0.79,
    price_p = 1.10,
    price_k = 0.55,
    price_lime = 35,
    soil_ph_target = 6.5
  ),
  Europe = list(
    discount_rate = 0.045,
    bes_capital_cost = 3000 * 1.15,
    beccs_capital_cost = 4000 * 1.15,
    bes_om_factor = 0.045,
    beccs_om_factor = 0.055,
    price_n = 1.75,
    price_p = 2.29,
    price_k = 0.9,
    price_lime = 50,
    soil_ph_target = 6.5
  )
)

#' Scenarios List
#'
#' A predefined list of scenarios used to override default parameters.
scenarios_base <- list(
  default = list(),
  CP100_MW250 = list(
    c_price = 100,
    plant_mw_th = c(BES = 250, BECCS = 250, BEBCS = 250)
  ),
  CP100_MW150 = list(
    c_price = 100,
    plant_mw_th = c(BES = 150, BECCS = 150, BEBCS = 150)
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

scenarios_reg <- lapply(scenarios_base, \(s) c(s, list(regional = regional_overrides)))
names(scenarios_reg) <- paste0(names(scenarios_reg), "_reg")
#' Regionalized Scenarios
#'
#' A predefined list of scenarios that includes global and regional parameter overrides.
#' @export
scenarios <- c(scenarios_base, scenarios_reg)


#' Normalize Region Name
#'
#' Standardizes regional names and aliases to match `regional_overrides` keys.
#'
#' @param region Character string indicating region.
#' @return Normalized region character string ("US", "India", "China", "Europe").
#' @export
normalize_region_name <- function(region) {
  if (is.null(region) || length(region) != 1 || is.na(region)) {
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
    region <- params[["region", exact = TRUE]]
  }
  if (is.null(region) || length(region) != 1 || is.na(region)) {
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
#' 4-tier precedence hierarchy:
#'   1. default_parameters (global defaults)
#'   2. regional_overrides for region (baseline regional overrides)
#'   3. scenario (global scenario overrides)
#'   4. scenario regional sublist for region (scenario-specific regional variants)
#' Values inferred from op_space_2.41.xlsm and tea_literature_review.md.
#'
#' @param scenario A named list of parameters to override the defaults. Defaults to an empty list.
#' @param region Optional character string specifying a region ("US", "India", "China", "Europe").
#' @return A named list of parameters.
#' @export
set_scenario <- function(scenario = list(), region = NULL) {
  params <- BiocharAG::default_parameters

  # 1. Determine region
  if (is.null(region) && !is.null(scenario[["region", exact = TRUE]])) {
    region <- scenario[["region", exact = TRUE]]
  }
  r_key <- if (!is.null(region)) normalize_region_name(region) else NULL

  # 2. Apply baseline regional overrides (Tier 2)
  if (!is.null(r_key)) {
    params <- apply_regional_overrides(params, region = r_key)
  }

  # 3. Apply global scenario overrides (Tier 3), excluding regional variant sublists
  if (length(scenario) > 0) {
    global_scen <- scenario[setdiff(names(scenario), c("regional", "regions"))]
    if (length(global_scen) > 0) {
      params[names(global_scen)] <- global_scen
    }
  }

  # 4. Apply scenario-specific regional variants (Tier 4)
  if (!is.null(r_key) && length(scenario) > 0) {
    reg_variants <- NULL
    if (!is.null(scenario$regional)) {
      reg_variants <- scenario$regional
    } else if (!is.null(scenario$regions)) {
      reg_variants <- scenario$regions
    }

    if (!is.null(reg_variants) && is.list(reg_variants)) {
      # Match against exact key or normalized aliases
      matched_var <- reg_variants[[r_key]]
      if (is.null(matched_var)) {
        for (k in names(reg_variants)) {
          if (normalize_region_name(k) == r_key) {
            matched_var <- reg_variants[[k]]
            break
          }
        }
      }
      if (!is.null(matched_var) && is.list(matched_var) && length(matched_var) > 0) {
        params[names(matched_var)] <- matched_var
      }
    }
  }

  if (!is.null(r_key)) {
    params$region <- r_key
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
