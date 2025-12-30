# data-raw/generate_elec_price_layer.R
library(terra)
library(sf)

# libraries
library(terra)
library(sf)

# 1. EIA 2023 Average Retail Price of Electricity (cents/kWh)
# Source: EIA 861M (from search)
# Values rounded/approx from 2023 Annual Data
data <- data.frame(
    name = c(
        "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
        "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
        "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
        "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
        "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
        "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
        "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
        "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
        "Washington", "West Virginia", "Wisconsin", "Wyoming"
    ),
    price_cents = c(
        14.15, 23.95, 13.91, 10.97, 27.50, 13.25,
        24.80, 14.16, 15.65, 15.11, 13.33,
        42.60, 10.28, 12.29, 12.88, 8.40, 11.92, 12.16,
        11.23, 20.54, 15.54, 25.58, 13.37, 12.98,
        13.51, 10.87, 11.60, 10.51, 13.88, 25.82,
        14.68, 12.56, 18.09, 12.44, 10.02,
        11.02, 10.97, 12.75, 13.14, 23.30, 13.53,
        10.45, 11.63, 12.93, 11.59, 17.26, 12.69,
        10.23, 12.99, 12.40, 10.74
    )
)

# Convert to $/MWh
# 1 cent/kWh = $10 / MWh
data$price_mwh <- data$price_cents * 10

# 2. Get US Map (Download Census Shapefile)
# Using low-res (20m) generalized file
shp_url <- "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_20m.zip"
gis_raw <- "../GIS/raw"
dir.create(gis_raw, showWarnings = FALSE, recursive = TRUE)
shp_file <- file.path(gis_raw, "cb_2018_us_state_20m.shp")

if (!file.exists(shp_file)) {
    message("Downloading US State Shapefile...")
    zip_dest <- file.path(gis_raw, "us_states.zip")
    download.file(shp_url, zip_dest, mode = "wb")
    unzip(zip_dest, exdir = gis_raw)
}

us_states <- sf::read_sf(shp_file)

# 3. Join Result
# Census uses "NAME"
setdiff(data$name, us_states$NAME)

us_elec <- merge(us_states, data, by.x = "NAME", by.y = "name", all.x = TRUE)

# Fill NA (e.g. Puerto Rico or mismatched)?
# Assume National Avg (~12.7 cents = $127) for missing
us_elec$price_mwh[is.na(us_elec$price_mwh)] <- 127

# 4. Rasterize
# Load Template
template_path <- "../GIS/processed/demo_biomass.tif"
if (file.exists(template_path)) {
    template <- terra::rast(template_path)
} else {
    # Fallback template
    template <- terra::rast(ext(-125, -66, 24, 50), res = 0.2, crs = "EPSG:4326")
}

message("Rasterizing Electricity Price...")
# Rasterize "price_mwh" field
r_elec <- terra::rasterize(us_elec, template, field = "price_mwh")

# 5. Save
out_path <- "../GIS/processed/demo_elec_price.tif"
terra::writeRaster(r_elec, out_path, overwrite = TRUE)

message("Electricity Price Layer Saved: ", out_path)
plot(r_elec, main = "Electricity Price ($/MWh)")
