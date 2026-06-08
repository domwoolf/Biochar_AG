library(sf)
library(terra)

r_template <- terra::rast("../GIS/processed/us_biomass.tif")
zip_file <- "../GIS/raw/WDPA/WDPA_Jun2026_Public.zip"
gdb_name <- gsub("\\.zip$", ".gdb", basename(zip_file))
vsi_path <- paste0("/vsizip/", zip_file, "/", gdb_name)

layers <- sf::st_layers(vsi_path)$name
poly_layer <- layers[grepl("poly", layers, ignore.case = TRUE)][1]

e_poly <- sf::st_as_sfc(sf::st_bbox(r_template))
e_poly_wgs84 <- sf::st_transform(e_poly, 4326)

t0 <- Sys.time()
pa_raw_cropped <- sf::st_read(vsi_path, layer = poly_layer, wkt_filter = sf::st_as_text(e_poly_wgs84), quiet = TRUE)
t1 <- Sys.time()

print(t1 - t0)
print(nrow(pa_raw_cropped))

