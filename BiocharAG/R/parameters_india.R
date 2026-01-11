#' Default Parameters for India (North-West)
#'
#' Returns a list of parameters customized for the Indian context (Punjab/Haryana).
#' Key Differences from US:
#' - Lower Labor Costs (affects O&M)
#' - Lower Capital Costs (Construction factor ~0.7)
#' - High Discount Rate (Developing market risk)
#' - Fertilizer Subsidies (Low N price)
#' - Negative/Zero Feedstock Cost (Crop Residue burning prevention)
#'
#' @return A named list of parameters.
#' @export
parameters_india <- function() {
    p <- default_parameters()

    # 1. Financial
    # Higher cost of capital in India
    p$discount_rate <- 0.12

    # 2. Technology / CAPEX
    # Construction in India is cheaper, technology might be imported or domestic.
    # Assume 70% of US Capital Cost context.
    capex_factor <- 0.7
    p$bes_capital_cost <- 3000 * capex_factor
    p$beccs_capital_cost <- 4000 * capex_factor

    # O&M: significantly lower due to labor
    p$bes_om_factor <- 0.025 # vs 0.04
    p$beccs_om_factor <- 0.03 # vs 0.05

    # 3. Biomass Feedstock
    # KEY ASSUMPTION: Crop residue is a nuisance (Stubble Burning).
    # Farmers burn it to clear fields quickly.
    # Cost structure: Collection & Transport only.
    # Payment to farmer might be 0 or negative (subsidy to remove).
    # We assume $0 'stumpage' but standard transport cost logic applies.
    # In our model, 'bc_price' is sale price.
    # Feedstock cost in spatial_tea usually comes from 'feedstock_cost' map or fixed.
    # If we want to simulate negative cost, we can treat it later.
    # For now, let's assume the "Price of Biomass" at field edge is $0.

    # 4. Electricity
    # India Wholesale (APPC): ~ Rs 4-6 / kWh -> ~$0.05 - $0.07 / kWh
    p$elec_price <- 0.06 * 1000 # $/MWh = 60
    # Wholesale factor is 1.0 because we are inputting the Generator price directly
    p$wholesale_discount_factor <- 1.0

    # 5. Fertilizer Substitutes (Subsidized)
    # Urea is heavily subsidized in India. Market price might be $400, Farmer pays $70.
    # Substitution value to farmer is based on SUBSIDIZED price (low).
    # Substitution value to Society (Social Cost) is full price.
    # Let's assess Private Value first (Farmer perspective).
    p$price_n <- 0.30 # $/kg N (Very low due to subsidy)
    p$price_p <- 0.80 # $/kg P
    p$price_k <- 0.40 # $/kg K
    p$price_lime <- 40 # $/Mg (Locally available)

    # 6. Soil
    # Soils in Punjab are often alkaline (pH > 7) -> No Liming Value!
    # But they are low in Organics.
    p$soil_ph_target <- 6.5 # If soil is 7.5, value is 0.

    # 7. Feedstock Characteristics (Rice Straw)
    p$bm_ash <- 0.15 # High Ash triggers BES Cost Penalty (but not BEBCS)

    return(p)
}
