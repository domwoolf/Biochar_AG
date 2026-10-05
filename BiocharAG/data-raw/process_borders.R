# data-raw/process_borders.R
# Downloads World Bank Admin 0 (National) and Admin 1 (State/Province) boundaries
# Subsets them by country name, crops them to the region's analysis bounding box,
# and saves them as fast-loading GeoPackages.

library(terra)

# 1. Setup Data Paths
gis_raw <- "../GIS/raw"
gis_proc <- "../GIS/processed"
if (!dir.exists(gis_raw)) dir.create(gis_raw, recursive = TRUE)

a0_url <- "https://datacatalogfiles.worldbank.org/ddh-published/0038272/5/DR0095370/World Bank Official Boundaries (GeoPackage)/World Bank Official Boundaries - Admin 0.gpkg"
a1_url <- "https://datacatalogfiles.worldbank.org/ddh-published/0038272/5/DR0095370/World Bank Official Boundaries (GeoPackage)/World Bank Official Boundaries - Admin 1.gpkg"

a0_file <- file.path(gis_raw, "WB_Admin0.gpkg")
a1_file <- file.path(gis_raw, "WB_Admin1.gpkg")

options(timeout = max(3600, getOption("timeout"))) 

# 2. Download Files (only if missing)
if (!file.exists(a0_file)) {
    message("Downloading Admin 0 boundaries (this may take a while)...")
    download.file(URLencode(a0_url), a0_file, mode = "wb")
}
if (!file.exists(a1_file)) {
    message("Downloading Admin 1 boundaries (this may take a while)...")
    download.file(URLencode(a1_url), a1_file, mode = "wb")
}

# 3. Load Global Vectors
message("Loading global vectors into memory...")
v_a0 <- terra::vect(a0_file)
v_a1 <- terra::vect(a1_file)

# Helper function to dynamically search all character columns for a country name
# This avoids needing to hardcode the exact World Bank metadata column names.
filter_by_name <- function(v, search_term) {
    if (is.null(search_term)) return(v)
    df <- as.data.frame(v)
    # Identify character or factor columns
    char_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    
    idx <- integer(0)
    for (col in char_cols) {
        found <- grep(search_term, as.character(df[[col]]), ignore.case = TRUE)
        idx <- unique(c(idx, found))
    }
    
    if (length(idx) > 0) {
        message("  -> Found ", length(idx), " polygons matching '", search_term, "'.")
        return(v[idx, ])
    } else {
        warning("  -> No polygons matched '", search_term, "'. Returning un-filtered vector.")
        return(v)
    }
}

# 4. Process Each Region
regions <- list(
    USA = list(
        prefix = "us",
        filter = "United States",
        template = file.path(gis_proc, "us_biomass.tif")
    ),
    India = list(
        prefix = "india",
        filter = "India",
        template = file.path(gis_proc, "india_biomass.tif")
    ),
    China = list(
        prefix = "china",
        filter = "China",
        # Mainland China: the World Bank boundary of China includes Taiwan (Admin 1 "Taiwan Sheng"),
        # and the "China" name filter also matches Hong Kong and Macau. The model's China data
        # (prices, fertilizer, grid) are for mainland China, so all three are excluded.
        exclude_iso = c("HKG", "MAC"),
        erase_adm1 = "Taiwan Sheng",
        template = file.path(gis_proc, "china_biomass.tif")
    ),
    Europe = list(
        prefix = "europe",
        filter = NULL, # All nations inside the Europe bounding box, except those below
        # Outside the study region but inside the bounding box: Russia, North Africa and the Middle East
        exclude_iso = c("RUS", "DZA", "MAR", "TUN", "SYR"),
        template = file.path(gis_proc, "europe_biomass.tif")
    )
)

for (r_name in names(regions)) {
    message("\n==================================")
    message("Processing Boundaries: ", r_name)
    reg <- regions[[r_name]]
    
    if (!file.exists(reg$template)) {
        warning("Template raster missing: ", reg$template, " - Skipping this region.")
        next
    }
    
    # Load template to get bounding box Extent
    r_template <- terra::rast(reg$template)
    e_box <- terra::ext(r_template)
    
    # ---- Process Admin 0 ----
    message("Filtering Admin 0...")
    sub_a0 <- filter_by_name(v_a0, reg$filter)
    if (!is.null(reg$exclude_iso)) sub_a0 <- sub_a0[!(sub_a0$ISO_A3 %in% reg$exclude_iso), ]
    if (!is.null(reg$erase_adm1)) {
        # Erase Admin 1 units from the national polygon, with a 10 km margin so that no coastal slivers
        # remain where the Admin 0 and Admin 1 coastlines differ
        erase_v <- v_a1[v_a1$NAM_1 %in% reg$erase_adm1, ]
        sub_a0 <- terra::erase(sub_a0, terra::buffer(erase_v, 10000))
    }
    
    message("Cropping Admin 0 to Bounding Box...")
    # Wrap in tryCatch as cropping can occasionally fail if geometries are invalid
    crop_a0 <- tryCatch({
        terra::crop(sub_a0, e_box)
    }, error = function(e) {
        warning("Cropping Admin 0 failed: ", e$message)
        sub_a0 # fallback to just the filtered one
    })
    
    a0_out <- file.path(gis_proc, paste0(reg$prefix, "_admin0.gpkg"))
    terra::writeVector(crop_a0, a0_out, overwrite = TRUE)
    message("Saved: ", a0_out)
    
    # ---- Process Admin 1 ----
    # Admin 1 (States) are really only legible and useful for the single-country regions
    # But we'll do it for Europe too if available.
    message("Filtering Admin 1...")
    sub_a1 <- filter_by_name(v_a1, reg$filter)
    if (!is.null(reg$exclude_iso)) sub_a1 <- sub_a1[!(sub_a1$ISO_A3 %in% reg$exclude_iso), ]
    if (!is.null(reg$erase_adm1)) sub_a1 <- sub_a1[!(sub_a1$NAM_1 %in% reg$erase_adm1), ]
    
    message("Cropping Admin 1 to Bounding Box...")
    crop_a1 <- tryCatch({
        terra::crop(sub_a1, e_box)
    }, error = function(e) {
        warning("Cropping Admin 1 failed: ", e$message)
        sub_a1
    })
    
    a1_out <- file.path(gis_proc, paste0(reg$prefix, "_admin1.gpkg"))
    terra::writeVector(crop_a1, a1_out, overwrite = TRUE)
    message("Saved: ", a1_out)
}

message("\nAll boundary processing completed successfully.")
