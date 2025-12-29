# data-raw/generate_sinks.R
library(sf)
library(dplyr)

# Approximate centroids of major CO2 storage basins (NETL / CO2StoP / Global)
sinks_list <- tribble(
    ~Region, ~Basin, ~Lat, ~Lon,
    "North America", "Illinois Basin", 39.0, -89.0,
    "North America", "Permian Basin", 31.5, -103.0,
    "North America", "Gulf Coast", 29.5, -95.0,
    "North America", "Williston Basin", 47.5, -103.5,
    "North America", "Alberta Basin", 54.0, -114.0,
    "Europe", "North Sea (Sleipner/Aurora)", 58.5, 1.9,
    "Europe", "Rotterdam/Porthos", 51.9, 4.0,
    "Europe", "Adriatic", 44.5, 13.0,
    "Asia", "Ordos Basin (China)", 38.0, 109.0,
    "Asia", "Songliao Basin (China)", 45.0, 125.0,
    "Asia", "Cambay Basin (India)", 22.5, 72.5,
    "Asia", "Bombay High (India)", 19.5, 71.3
)

# Convert to sf object
co2_sinks <- st_as_sf(sinks_list, coords = c("Lon", "Lat"), crs = 4326)

usethis::use_data(co2_sinks, overwrite = TRUE)
