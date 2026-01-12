#' Adjust TEA Costs based on Fuel Quality (Ash Content)
#'
#' Applies cost penalties for high-ash biomass (e.g., crop residues) which require
#' more expensive boilers (CFB vs Stoker) and have higher O&M/Lower Efficiency.
#'
#' @param params A list of TEA parameters including `bm_ash` (fraction).
#' @return The modified parameter list with updated `bes_capital_cost`, `beccs_capital_cost`,
#' `bes_om_factor`, `beccs_om_factor`, `bes_energy_efficiency`, and `beccs_efficiency`.
#' @export
adjust_costs_for_fuel <- function(params) {
    # Default ash to low (wood chip) if missing
    ash <- if (!is.null(params$bm_ash)) params$bm_ash else 0.01

    # Base Multipliers (1.0 = No Penalty)
    capex_mult <- 1.0
    om_mult <- 1.0
    eff_mult <- 1.0

    # Thresholds based on Literature (Wood < 1-2%, Straw > 5-10%)
    if (ash > 0.05) {
        # High Ash Regime (Straw, Corn Stover)
        # Significant fouling, slagging risk. Requires Fluidized Bed (CFB).
        capex_mult <- 1.25
        om_mult <- 1.50
        eff_mult <- 0.90
    } else if (ash > 0.02) {
        # Medium Ash (Bark, Forest Residues with dirt)
        capex_mult <- 1.10
        om_mult <- 1.20
        eff_mult <- 0.95
    }

    # Apply Multipliers to BES
    if (!is.null(params$bes_capital_cost)) {
        params$bes_capital_cost <- params$bes_capital_cost * capex_mult
    }
    if (!is.null(params$bes_om_factor)) {
        params$bes_om_factor <- params$bes_om_factor * om_mult
    }
    if (!is.null(params$bes_energy_efficiency)) {
        params$bes_energy_efficiency <- params$bes_energy_efficiency * eff_mult
    }

    # Apply Multipliers to BECCS
    if (!is.null(params$beccs_capital_cost)) {
        params$beccs_capital_cost <- params$beccs_capital_cost * capex_mult
    }
    if (!is.null(params$beccs_om_factor)) {
        params$beccs_om_factor <- params$beccs_om_factor * om_mult
    }
    if (!is.null(params$beccs_efficiency)) {
        params$beccs_efficiency <- params$beccs_efficiency * eff_mult
    }

    # Store multipliers for transparency/debugging if needed
    params$fuel_penalty_capex <- capex_mult

    params
}
