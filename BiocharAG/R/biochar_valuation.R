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

        # Liming and nutrient value of the minerals the biochar returns, per Mg feed (charge balance; see
        # residue_minerals()). Liming is credited only where soil pH is below target_ph; P and K at total
        # content; biochar N has no fertiliser value. The crop yield response, spreading and soil N2O are in
        # the field-application model (biochar_field_table()); the charge for nutrients removed with the
        # residue is in residue_removal_charge().
        m <- residue_minerals(params)
        v_lime_feed <- lime_value(params, m$anc_bc)
        v_nut_feed <- nutrient_value(params, p = m$p_bc, k = m$k_bc)
        val_per_mg_feedstock <- v_lime_feed + v_nut_feed
        detail <- list(v_lime_feed = v_lime_feed, v_nut_feed = v_nut_feed, type = "Mechanistic Substitutes")
    }

    list(
        value_usd_per_mg_feedstock = val_per_mg_feedstock,
        method_used = method,
        detail = detail
    )
}

#' Mineral Partition of Crop Residue Between Biochar, Bottom Ash and Fly Ash
#'
#' Charge-balance accounting of the residue's mineral fraction (issue #110), per Mg of dry, ash-free
#' (daf) feed, the basis of the conversion models. Feedstock contents (`bm_ca`, `bm_mg`, `bm_k`,
#' `bm_na`, `bm_p`, `bm_n`, `bm_cl`, `bm_s`; kg per Mg dry matter, residue-weighted regional means of
#' the available cereal residue from Phyllis2) are converted to the daf basis with `bm_ash`.
#'
#' Acid-neutralising capacity (ANC) is the charge balance 2Ca + 2Mg + K + Na - Cl - 2S - P (mol_c),
#' expressed as kg CaCO3-equivalent. Base cations balanced by weak-acid anions (carbonate, oxide,
#' silicate, organic anions) neutralise soil acidity; those balanced by chloride or sulphate do not, and
#' phosphate counts as one equivalent because H2PO4- is the dominant species at soil pH. Silica carries
#' no alkalinity.
#'
#' - Biochar retains all Ca, Mg, K, Na and P, the fractions `bc_cl_retention` and `bc_s_retention` of
#'   Cl and S (the rest leaves as HCl and sulphur gases, raising the char's ANC) and `bc_n_retention`
#'   of N (biochar N is held in heterocyclic structures and is given no fertiliser value).
#' - Combustion: fly ash is KCl and K2SO4 carrying the fractions `ash_cl_fly` and `ash_s_fly` of Cl and
#'   S with the K they bind, the fraction `ash_p_fly` of P, and 18% calcium phosphate and insoluble
#'   matter (Avedøre straw fly ash), so it has negligible ANC. Bottom ash keeps Ca, Mg, Na, the
#'   remaining K and P and the fraction `ash_s_bottom` of S; the rest of the Cl and S leaves in the
#'   flue gas.
#'
#' @param params Parameter list.
#' @return List per Mg daf feed: `anc_bc`, `anc_ba`, `anc_res` (kg CaCO3-eq: biochar, bottom ash and the
#'   residue itself), `k_bc`, `k_ba`, `k_fly`, `p_bc`, `p_ba`, `p_fly`, `n_res`, `p_res`, `k_res` (kg) and
#'   `ash_bottom`, `ash_fly` (Mg).
#' @export
residue_minerals <- function(params) {
    pv <- function(n, d) if (!is.null(params[[n, exact = TRUE]])) params[[n, exact = TRUE]] else d
    ash <- pv("bm_ash", 0.05)
    daf <- 1 / (1 - ash) # Mg DM per Mg daf
    ca <- pv("bm_ca", 2.9) * daf; mg <- pv("bm_mg", 0.72) * daf; k <- pv("bm_k", 8.7) * daf
    na <- pv("bm_na", 0.2) * daf; p <- pv("bm_p", 0.88) * daf; n <- pv("bm_n", 6.2) * daf
    cl <- pv("bm_cl", 2.9) * daf; s <- pv("bm_s", 1.07) * daf
    # kg CaCO3-eq per kg element: 50.04 g CaCO3 per mol of charge
    f_ca <- 2.4973; f_mg <- 4.1180; f_k <- 1.2799; f_na <- 2.1767; f_cl <- 1.4115; f_s <- 3.1219; f_p <- 1.6156
    base <- ca * f_ca + mg * f_mg + na * f_na

    anc_bc <- base + k * f_k - pv("bc_cl_retention", 0.45) * cl * f_cl - pv("bc_s_retention", 0.4) * s * f_s - p * f_p

    cl_fly <- pv("ash_cl_fly", 0.9) * cl
    s_fly <- pv("ash_s_fly", 0.5) * s
    k_fly <- pmin(k, 39.098 * (cl_fly / 35.453 + 2 * s_fly / 32.06)) # K bound as KCl and K2SO4
    p_fly <- pv("ash_p_fly", 0.05) * p
    k_ba <- k - k_fly
    p_ba <- p - p_fly
    anc_ba <- base + k_ba * f_k - pv("ash_s_bottom", 0.1) * s * f_s - p_ba * f_p
    ash_fly <- (cl_fly * 74.551 / 35.453 + s_fly * 174.26 / 32.06) / 0.82 / 1000 # Mg / Mg daf
    ash_bottom <- pmax(ash * daf - ash_fly, 0)

    anc_res <- base + k * f_k - cl * f_cl - s * f_s - p * f_p # the residue itself (all Cl and S present)

    list(anc_bc = anc_bc, anc_ba = anc_ba, anc_res = anc_res, k_bc = k, k_ba = k_ba, k_fly = k_fly, p_bc = p,
         p_ba = p_ba, p_fly = p_fly, n_res = n, p_res = p, k_res = k, ash_bottom = ash_bottom, ash_fly = ash_fly)
}

#' Fertiliser Substitution Value of N, P and K
#'
#' Nutrients are valued at their total content: P and K returned in residue, biochar or ash remain in the
#' soil and become available over the following years, so the removal charge and the return credits are
#' on the same basis (docs/biochar_agronomy_handover/AGRONOMY_SPEC.md, section 3). N is priced as urea;
#' callers apply any fertiliser-replacement fraction.
#'
#' @param params Parameter list (`price_n`, `price_p`, `price_k` per kg N, P2O5 and K2O).
#' @param n,p,k Elemental N, P and K (kg per Mg feed).
#' @return Value in $/Mg feed.
#' @keywords internal
nutrient_value <- function(params, n = 0, p = 0, k = 0) {
    pv <- function(nm, d) if (!is.null(params[[nm, exact = TRUE]])) params[[nm, exact = TRUE]] else d
    n * pv("price_n", 1.06) + p * P_TO_P2O5 * pv("price_p", 1.27) + k * K_TO_K2O * pv("price_k", 0.86)
}

#' Liming Value of an Acid-Neutralising Capacity
#'
#' Credited at the lime price (per Mg CaCO3-eq, delivered and spread) where soil pH is below `target_ph`.
#'
#' @param params Parameter list.
#' @param anc Acid-neutralising capacity (kg CaCO3-eq per Mg feed).
#' @return Value in $/Mg feed.
#' @keywords internal
lime_value <- function(params, anc) {
    pv <- function(nm, d) if (!is.null(params[[nm, exact = TRUE]])) params[[nm, exact = TRUE]] else d
    soil_ph <- pv("soil_ph", 6.5)
    ifelse_raster(soil_ph < pv("target_ph", 6.5), pv("lime_effectiveness", 1) * anc / 1000 * pv("price_lime", 59), 0)
}

#' Charge for the Nutrients and Alkalinity Removed with the Residue
#'
#' Common to all pathways: the counterfactual leaves the residue in the field or burns it there, returning
#' its P, K and alkalinity (in the ash, if burned) and, for the retained share, N at its fertiliser
#' replacement value `n_fert_replacement`. Each pathway is credited separately for what it returns in
#' biochar or ash.
#'
#' @param params Parameter list.
#' @return Cost in $/Mg dry ash-free feed.
#' @export
residue_removal_charge <- function(params) {
    pv <- function(nm, d) if (!is.null(params[[nm, exact = TRUE]])) params[[nm, exact = TRUE]] else d
    m <- residue_minerals(params)
    f_n <- (1 - residue_burn_share(params)) * pv("n_fert_replacement", 0)
    nutrient_value(params, n = f_n * m$n_res, p = m$p_res, k = m$k_res) + lime_value(params, m$anc_res)
}

#' Agronomic Value of Recycled Combustion Ash
#'
#' Value of returning BES/BECCS bottom ash to cropland (when `ash_recycling` is TRUE): substitution for
#' agricultural lime (only where soil pH is below `target_ph`) and for P and K fertiliser, from the
#' mineral partition in [residue_minerals()]. Fly ash is landfilled; the fraction `fly_ash_recycled`
#' (default 0) is returned with the bottom ash and credited for its K and P (its ANC is negligible).
#' P and K are credited at total content (see [nutrient_value()]). N is lost in combustion. The cost of
#' returning the ash is in [ash_return()]; the charge for the nutrients removed with the residue is in
#' [residue_removal_charge()].
#'
#' @param params Parameter list.
#' @return Value in $/Mg feed (0 when ash is not recycled).
#' @export
calculate_ash_value <- function(params) {
    recycle <- if (!is.null(params$ash_recycling)) as.logical(params$ash_recycling) else TRUE
    if (!isTRUE(recycle)) {
        return(0)
    }
    m <- residue_minerals(params)
    rho <- if (!is.null(params$fly_ash_recycled)) params$fly_ash_recycled else 0
    lime_value(params, m$anc_ba) + nutrient_value(params, p = m$p_ba + rho * m$p_fly, k = m$k_ba + rho * m$k_fly)
}

#' Cost and Emissions of Returning Combustion Ash to Cropland
#'
#' Bottom ash (plus the recycled fraction of fly ash) returns to the fields as a backhaul in the
#' feedstock trucks, as biochar does: loading and handling are charged per Mg (`bm_transport_fixed`),
#' and one spreading and incorporation pass per hectare ([bc_pass_cost()], [bc_pass_diesel()]) at the ash
#' application rate `ash_app_rate` (Mg/ha).
#'
#' @param params Parameter list.
#' @return List with `cost` ($/Mg feed), `emissions` (Mg CO2e/Mg feed) and `mass` (Mg ash/Mg feed).
#' @keywords internal
ash_return <- function(params) {
    recycle <- if (!is.null(params$ash_recycling)) as.logical(params$ash_recycling) else TRUE
    if (!isTRUE(recycle)) {
        return(list(cost = 0, emissions = 0, mass = 0))
    }
    pv <- function(n, d) if (!is.null(params[[n, exact = TRUE]])) params[[n, exact = TRUE]] else d
    m <- residue_minerals(params)
    mass <- m$ash_bottom + pv("fly_ash_recycled", 0) * m$ash_fly
    rate <- pv("ash_app_rate", 5)
    haul <- if (isFALSE(as.logical(params$bc_return_haul))) 0 else mass * pv("bm_transport_fixed", 6.27)
    list(cost = haul * location_factor(params, "haulage") + mass / rate * bc_pass_cost(params),
         emissions = mass / rate * bc_pass_diesel(params) * 2.68e-3, mass = mass)
}

# Mass conversion from elemental nutrient to fertiliser oxide basis (fertiliser prices are quoted
# per kg P2O5 and K2O): P2O5/2P = 141.94/61.95; K2O/2K = 94.20/78.20
P_TO_P2O5 <- 2.291
K_TO_K2O <- 1.205
