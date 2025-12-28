#' Calculate Annuity Factor
#'
#' Calculates the Present Value of an Annuity Factor.
#' @param discount_rate Discount rate (decimal).
#' @param lifetime Lifetime in years.
#' @return The annuity factor.
#' @export
calculate_annuity_factor <- function(discount_rate, lifetime) {
  if (discount_rate == 0) return(lifetime)
  (1 - (1 / ((1 + discount_rate)^lifetime))) / discount_rate
}

#' Calculate Net Present Value
#'
#' @param cash_flows Vector of cash flows.
#' @param discount_rate Discount rate.
#' @return NPV.
#' @export
calculate_npv <- function(cash_flows, discount_rate) {
  t <- seq_along(cash_flows) - 1 # Assuming start at year 0 or 1? Excel formula suggests annuity.
  # If using annuity factor, we deal with annualized costs.
  # This function is a placeholder for direct cash flow streams if needed.
  sum(cash_flows / (1 + discount_rate)^t)
}
