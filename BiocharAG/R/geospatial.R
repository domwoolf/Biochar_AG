#' Find Nearest CO2 Sink
#'
#' Calculates the geodesic distance from a given projected location to the nearest
#' potential CO2 storage basin in the database.
#'
#' @param lat Latitude of the project location (decimal degrees).
#' @param lon Longitude of the project location (decimal degrees).
#' @return A list containing:
#'   - `distance_km`: Distance to the nearest sink (numeric).
#'   - `sink_name`: Name of the nearest sink (character).
#'   - `sink_region`: Region of the nearest sink (character).
#' @export
#' @importFrom sf st_as_sf st_nearest_feature st_distance
find_nearest_sink <- function(lat, lon) {
    # Load internal data
    if (!exists("co2_sinks")) {
        try(utils::data("co2_sinks", package = "BiocharAG", envir = environment()), silent = TRUE)
    }

    if (!exists("co2_sinks")) {
        warning("Dataset 'co2_sinks' not found. Using default distance of 100 km.")
        return(list(distance_km = 100, sink_name = "Default", sink_region = "Unknown"))
    }

    # Create point from input
    pt <- sf::st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326)

    # Find nearest feature calculation
    # st_nearest_feature returns index
    nearest_idx <- sf::st_nearest_feature(pt, co2_sinks)
    nearest_sink <- co2_sinks[nearest_idx, ]

    # Calculate distance
    dist_m <- sf::st_distance(pt, nearest_sink)
    dist_km <- as.numeric(dist_m) / 1000

    list(
        distance_km = dist_km,
        sink_name = nearest_sink$Basin,
        sink_region = nearest_sink$Region
    )
}
