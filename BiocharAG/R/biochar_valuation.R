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
        bc_price <- if (!is.null(params$bc_price)) params$bc_price else 0
        val_per_mg_feedstock <- bc_yield * bc_price

        detail <- list(unit_price_char = bc_price, type = "Sales Revenue")
    } else if (method == "ag_value") {
        # Method B: Legacy Simple Ag Value
        bc_ag_value <- if (!is.null(params$bc_ag_value)) params$bc_ag_value else 0
        discount_rate <- if (!is.null(params$discount_rate)) params$discount_rate else 0.1
        # Use simple decay model
        bc_stab_factor <- if (!is.null(params$bc_stab_factor)) params$bc_stab_factor else 4.6
        bc_half_life <- 10^(bc_stab_factor * 0.9)
        decay_rate <- log(2) / bc_half_life

        nbcf_per_mg_char <- bc_ag_value / (discount_rate + decay_rate)
        val_per_mg_feedstock <- bc_yield * nbcf_per_mg_char

        detail <- list(type = "Static Ag Value", value = nbcf_per_mg_char)
    } else if (method == "advanced_mechanistic") {
        # Method C: Mechanistic Substitution Model (Advanced)

        # 1. Liming Value (Substitution)
        soil_ph <- if (!is.null(params$soil_ph)) params$soil_ph else 6.5
        target_ph <- 6.5
        price_lime <- if (!is.null(params$price_lime)) params$price_lime else 60
        bc_cce <- if (!is.null(params$bc_cce)) params$bc_cce else 0.15

        v_lime_per_mg_char <- 0
        if (soil_ph < target_ph) {
            v_lime_per_mg_char <- bc_cce * price_lime
        }

        # 2. Nutrient Value (Substitution)
        p_n <- if (!is.null(params$price_n)) params$price_n else 0.92
        p_p <- if (!is.null(params$price_p)) params$price_p else 1.10
        p_k <- if (!is.null(params$price_k)) params$price_k else 0.62

        c_n <- if (!is.null(params$bc_n_content)) params$bc_n_content else 0.005
        c_p <- if (!is.null(params$bc_p_content)) params$bc_p_content else 0.002
        c_k <- if (!is.null(params$bc_k_content)) params$bc_k_content else 0.005

        # Availability Factors
        avail_n <- 0.1
        avail_p <- 0.5
        avail_k <- 0.8

        v_nut_per_mg_char <- (c_n * avail_n * p_n * 1000) +
            (c_p * avail_p * p_p * 1000) +
            (c_k * avail_k * p_k * 1000)

        # 3. Physical/CEC Value (Yield Efficiency)
        soil_cec <- if (!is.null(params$soil_cec)) params$soil_cec else 20
        # Heuristic: Value is proportional to CEC deficit (Sandier = More value)
        # Assume $50/Mg annual benefit in pure sand (CEC=5), $0 in clay (CEC>30)
        # Linear ramp: (30 - CEC) * 2
        cec_val_annual <- max(0, (30 - soil_cec) * 2)

        # Discounted over impact duration
        dur <- if (!is.null(params$ag_impact_duration)) params$ag_impact_duration else 10
        dr <- if (!is.null(params$discount_rate)) params$discount_rate else 0.1
        apv <- (1 - (1 + dr)^-dur) / dr

        v_phys_per_mg_char <- cec_val_annual * apv

        # Total
        total_val_per_mg_char <- v_lime_per_mg_char + v_nut_per_mg_char + v_phys_per_mg_char
        val_per_mg_feedstock <- bc_yield * total_val_per_mg_char

        detail <- list(
            v_lime = v_lime_per_mg_char,
            v_nut = v_nut_per_mg_char,
            v_phys = v_phys_per_mg_char,
            type = "Mechanistic Substitutes"
        )
    }

    list(
        value_usd_per_mg_feedstock = val_per_mg_feedstock,
        method_used = method,
        detail = detail
    )
}
