#' Calculate Biochar Economic Value
#'
#' Determines the economic value of the biochar fraction based on the selected valuation method.
#' Handles the mutually exclusive logic between Market Sales (Revenue) and Agronomic Value (Shadow Price).
#' ensuring values are normalized to $/Mg Feedstock.
#'
#' @param params List of parameters including `bc_valuation_method`, `bc_price`, `bc_ag_value`, etc.
#' @param bc_yield Numeric. Biochar yield fraction (Mg Biochar / Mg Feedstock).
#'
#' @return A list containing:
#' \item{value_usd_per_mg_feedstock}{Total economic value per Mg of biomass feedstock.}
#' \item{method_used}{Character string indicating the method ("market_price" or "ag_value").}
#' \item{detail}{Intermediate values (e.g. unit price per Mg char).}
#' @export
calculate_biochar_value <- function(params, bc_yield) {
    method <- if (!is.null(params$bc_valuation_method)) params$bc_valuation_method else "ag_value"

    # Initialize
    val_per_mg_feedstock <- 0
    detail <- list()

    if (method == "market_price") {
        # Method A: Market Sale
        # Value = Yield * Price
        # Units: (Mg BC / Mg Feed) * ($ / Mg BC) = $ / Mg Feed
        bc_price <- if (!is.null(params$bc_price)) params$bc_price else 0

        val_per_mg_feedstock <- bc_yield * bc_price

        detail <- list(
            unit_price_char = bc_price,
            type = "Sales Revenue"
        )
    } else {
        # Method B: Agronomic Value (Shadow Price)
        # Value = Yield * NPV of agronomic benefit per Mg Char

        # 1. Get Params
        bc_ag_value <- if (!is.null(params$bc_ag_value)) params$bc_ag_value else 0 # $/Mg Char/Year
        discount_rate <- if (!is.null(params$discount_rate)) params$discount_rate else 0.1
        bc_stab_factor <- if (!is.null(params$bc_stab_factor)) params$bc_stab_factor else 4.6

        # 2. Calculate Decay/Persistence
        # Half Life T1/2 = 10^(factor * (1 - 0.1)) (Approximation)
        # Note: ensure consistency with fperm logic if possible, but using established Excel formula linkage here
        bc_half_life <- 10^(bc_stab_factor * 0.9)
        decay_rate <- log(2) / bc_half_life

        # 3. Calculate NPV of Agg Benefit per Mg Char
        # Perpetuity with decay: Value / (r + decay)
        nbcf_per_mg_char <- bc_ag_value / (discount_rate + decay_rate)

        # 4. Convert to $/Mg Feedstock
        val_per_mg_feedstock <- bc_yield * nbcf_per_mg_char

        detail <- list(
            annual_benefit_char = bc_ag_value,
            half_life = bc_half_life,
            npv_per_mg_char = nbcf_per_mg_char,
            type = "Agronomic Shadow Price"
        )
    }

    list(
        value_usd_per_mg_feedstock = val_per_mg_feedstock,
        method_used = method,
        detail = detail
    )
}
