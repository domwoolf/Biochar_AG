#' Raster-Aware Conditional Element Selection (ifelse)
#'
#' Internal helper that delegates to terra::ifel if the test is a SpatRaster,
#' handles single logical scalar tests directly, and delegates to base::ifelse otherwise.
#'
#' @keywords internal
ifelse_raster <- function(test, yes, no) {
    if (inherits(test, "SpatRaster")) {
        terra::ifel(test, yes, no)
    } else if (is.logical(test) && length(test) == 1) {
        if (is.na(test)) {
            NA
        } else if (test) {
            yes
        } else {
            no
        }
    } else {
        ifelse(test, yes, no)
    }
}

#' Raster-Aware Parallel Minimum (pmin)
#'
#' Internal helper that delegates to terra::min if either input is a SpatRaster,
#' ensuring correct S3 dispatch by placing the SpatRaster first. Delegates to
#' base::pmin otherwise.
#'
#' @keywords internal
pmin_raster <- function(x, y) {
    if (inherits(x, "SpatRaster")) {
        min(x, y)
    } else if (inherits(y, "SpatRaster")) {
        min(y, x)
    } else {
        pmin(x, y)
    }
}

#' Raster-Aware Parallel Maximum (pmax)
#'
#' Internal helper that delegates to terra::max if either input is a SpatRaster,
#' ensuring correct S3 dispatch by placing the SpatRaster first. Delegates to
#' base::pmax otherwise.
#'
#' @keywords internal
pmax_raster <- function(x, y) {
    if (inherits(x, "SpatRaster")) {
        max(x, y)
    } else if (inherits(y, "SpatRaster")) {
        max(y, x)
    } else {
        pmax(x, y)
    }
}
