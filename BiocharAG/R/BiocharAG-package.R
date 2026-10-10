#' BiocharAG: Spatial Techno-Economic Model of Crop-Residue Bioenergy, BECCS and PyCCS
#'
#' Implements the C-SCAPE model. Compiled code: the cohort loop of the biochar field-application model
#' ([field_cohort_sums_cpp()], src/field_cohorts.cpp).
#'
#' @useDynLib BiocharAG, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @keywords internal
"_PACKAGE"
