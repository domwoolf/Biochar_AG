#' Calculate Biochar Economic Value
#'
#' Determines the economic value of the biochar fraction based on the selected valuation method.
#' Handles the mutually exclusive logic between Market Sales (Revenue) and Agronomic Value (Shadow Price).
#' ensuring values are normalized to $/Mg Feedstock.
#'
#' @param params List of parameters including `bc_valuation_method`, `bc_price`, `bc_ag_value`, etc.
#' @param bc_yield Numeric. Biochar yield fraction (Mg Biochar / Mg Feedstock).
#' @param bc_c_content Biochar carbon content (Mg C / Mg biochar), used to convert the soil physical
#'   benefit (`bc_cec_value`, per Mg biochar C) to a value per Mg biochar. Defaults to
#'   `params$bc_c_content`, else 0.75.
#' @param bc_decay_rate Decay rate of biochar carbon (1/yr), used in the perpetuity for the soil
#'   physical benefit. Defaults to `params$bc_decay_rate`, else 0.003.
#'
#' @return A list containing:
#' \item{value_usd_per_mg_feedstock}{Total economic value per Mg of biomass feedstock.}
#' \item{method_used}{Character string indicating the method ("market_price" or "ag_value").}
#' \item{detail}{Intermediate values (e.g. unit price per Mg char).}
#' @export
calculate_biochar_value <- function(params, bc_yield, bc_c_content = NULL, bc_decay_rate = NULL) {
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
        target_ph <- if (!is.null(params$target_ph)) params$target_ph else 6.5
        price_lime <- if (!is.null(params$price_lime)) params$price_lime else 59
        bc_cce <- if (!is.null(params$bc_cce)) params$bc_cce else 0.15

        v_lime_per_mg_char <- ifelse_raster(soil_ph < target_ph, bc_cce * price_lime, 0)

        # 2. Nutrient Value (Substitution)
        p_n <- if (!is.null(params$price_n)) params$price_n else 1.06
        p_p <- if (!is.null(params$price_p)) params$price_p else 1.27
        p_k <- if (!is.null(params$price_k)) params$price_k else 0.86

        c_n <- if (!is.null(params$bc_n_content)) params$bc_n_content else 0.005
        c_p <- if (!is.null(params$bc_p_content)) params$bc_p_content else 0.002
        c_k <- if (!is.null(params$bc_k_content)) params$bc_k_content else 0.005

        # Availability Factors
        avail_n <- if (!is.null(params$avail_n)) params$avail_n else 0.1
        avail_p <- if (!is.null(params$avail_p)) params$avail_p else 0.5
        avail_k <- if (!is.null(params$avail_k)) params$avail_k else 0.8

        kg_to_mg_conv <- if (!is.null(params$kg_to_mg)) params$kg_to_mg else 1000 # 1000 converts kg to Mg
        # Contents are elemental (kg P, kg K per kg biochar); prices are per kg P2O5 and K2O
        v_nut_per_mg_char <- (c_n * avail_n * p_n * kg_to_mg_conv) +
            (c_p * P_TO_P2O5 * avail_p * p_p * kg_to_mg_conv) +
            (c_k * K_TO_K2O * avail_k * p_k * kg_to_mg_conv)

        # 3. Physical/CEC Value (Yield Efficiency)
        soil_cec <- if (!is.null(params$soil_cec)) params$soil_cec else 20
        # Annual yield benefit from raising soil CEC, proportional to the CEC deficit: the full value
        # bc_cec_value ($/Mg biochar C/yr; Woolf et al. 2016) at CEC <= 5 cmol/kg (sands), falling linearly
        # to 0 at CEC >= 30. Valued as a yield increment, not a substitute input: higher CEC raises the
        # whole fertilizer response curve.
        cec_value <- if (!is.null(params$bc_cec_value)) params$bc_cec_value else 21.1
        bc_c <- if (!is.null(bc_c_content)) bc_c_content else if (!is.null(params$bc_c_content)) params$bc_c_content else 0.75
        cec_frac <- pmin_raster(pmax_raster((30 - soil_cec) / 25, 0), 1)
        cec_val_annual <- cec_value * bc_c * cec_frac # $/Mg biochar/yr

        # Biochar CEC rises with ageing (surface oxidation), so the benefit is treated as a perpetuity that
        # lasts as long as the biochar carbon: present value = annual value / (discount rate + decay rate)
        dr <- if (!is.null(params$discount_rate)) params$discount_rate else 0.1
        k <- if (!is.null(bc_decay_rate)) bc_decay_rate else if (!is.null(params$bc_decay_rate)) params$bc_decay_rate else 0.003
        v_phys_per_mg_char <- cec_val_annual / (dr + k)

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

#' Agronomic Value of Recycled Combustion Ash
#'
#' Value of returning BES/BECCS bottom and fly ash to cropland (when `ash_recycling` is TRUE), by
#' substitution for agricultural lime (only where soil pH is below `target_ph`) and phosphorus
#' fertiliser. P is largely retained in the ash; N and most K are volatilised in combustion, so they
#' are not credited.
#'
#' @param params Parameter list (`ash_recycling`, `bm_ash`, `ash_cce`, `ash_p_content`, `avail_p`,
#'   `price_lime`, `price_p`, `soil_ph`, `target_ph`).
#' @return Value in $/Mg feedstock (0 when ash is not recycled).
#' @export
calculate_ash_value <- function(params) {
    recycle <- if (!is.null(params$ash_recycling)) as.logical(params$ash_recycling) else TRUE
    if (!isTRUE(recycle)) {
        return(0)
    }
    ash <- if (!is.null(params$bm_ash)) params$bm_ash else 0.05
    ash_mass <- ash / (1 - ash) # Mg ash / Mg daf feed
    soil_ph <- if (!is.null(params$soil_ph)) params$soil_ph else 6.5
    target_ph <- if (!is.null(params$target_ph)) params$target_ph else 6.5
    price_lime <- if (!is.null(params$price_lime)) params$price_lime else 59
    ash_cce <- if (!is.null(params$ash_cce)) params$ash_cce else 0.85
    ash_p <- if (!is.null(params$ash_p_content)) params$ash_p_content else 0.012
    avail_p <- if (!is.null(params$avail_p)) params$avail_p else 0.5
    price_p <- if (!is.null(params$price_p)) params$price_p else 1.27

    v_lime <- ifelse_raster(soil_ph < target_ph, ash_cce * price_lime, 0)
    v_p <- ash_p * P_TO_P2O5 * avail_p * price_p * 1000 # elemental P content; price per kg P2O5
    ash_mass * (v_lime + v_p)
}

# Mass conversion from elemental nutrient to fertiliser oxide basis (fertiliser prices are quoted
# per kg P2O5 and K2O): P2O5/2P = 141.94/61.95; K2O/2K = 94.20/78.20
P_TO_P2O5 <- 2.291
K_TO_K2O <- 1.205
