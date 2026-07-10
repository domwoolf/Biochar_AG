#' Default Parameters for India (North-West)
#'
#' Deprecated: Returns a list of parameters customized for the Indian context
#' by calling `set_scenario(region = "India")`.
#'
#' @return A named list of parameters.
#' @export
parameters_india <- function() {
    .Deprecated("set_scenario(region = 'India')")
    set_scenario(region = "India")
}
