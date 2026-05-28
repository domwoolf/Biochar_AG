# data-raw/generate_elec_price_layer.R
# This script applies high-resolution state/provincial electricity prices (USD/MWh) 
# onto the spatial models by joining price lookups with World Bank boundaries 
# and rasterizing them against the regional templates.

library(terra)
library(dplyr)
library(tibble)

# ==============================================================================
# 1. Industrial Electricity Price Database (2024/2025 Estimates)
#    Units: USD per MWh
# ==============================================================================

us_prices <- tribble(
  ~name, ~price_mwh,
  "Alabama", 75, "Alaska", 180, "Arizona", 85, "Arkansas", 72, "California", 195,
  "Colorado", 90, "Connecticut", 165, "Delaware", 95, "Florida", 90, "Georgia", 75,
  "Hawaii", 320, "Idaho", 68, "Illinois", 85, "Indiana", 88, "Iowa", 65,
  "Kansas", 82, "Kentucky", 70, "Louisiana", 68, "Maine", 120, "Maryland", 100,
  "Massachusetts", 185, "Michigan", 95, "Minnesota", 92, "Mississippi", 78,
  "Missouri", 80, "Montana", 70, "Nebraska", 75, "Nevada", 85, "New Hampshire", 170,
  "New Jersey", 130, "New Mexico", 75, "New York", 95, "North Carolina", 75,
  "North Dakota", 78, "Ohio", 85, "Oklahoma", 65, "Oregon", 72, "Pennsylvania", 85,
  "Rhode Island", 175, "South Carolina", 78, "South Dakota", 82, "Tennessee", 85,
  "Texas", 68, "Utah", 70, "Vermont", 125, "Virginia", 78, "Washington", 60,
  "West Virginia", 75, "Wisconsin", 90, "Wyoming", 72
)

cn_prices <- tribble(
  ~name, ~price_mwh,
  "Shanghai", 105, "Zhejiang", 108, "Jiangsu", 102, "Guangdong", 100, "Shandong", 95,
  "Beijing", 110, "Tianjin", 100, "Fujian", 98, "Henan", 90, "Hubei", 88, "Hunan", 92, 
  "Anhui", 95, "Jiangxi", 95, "Inner Mongolia", 65, "Shanxi", 70, "Hebei", 85, 
  "Liaoning", 80, "Jilin", 82, "Heilongjiang", 80, "Xinjiang", 55, "Qinghai", 50, 
  "Gansu", 55, "Ningxia", 60, "Shaanxi", 70, "Sichuan", 65, "Yunnan", 60, 
  "Guizhou", 75, "Tibet", 60, "Chongqing", 85, "Guangxi", 90
)

in_prices <- tribble(
  ~name, ~price_mwh,
  "Maharashtra", 130, "Gujarat", 95, "Tamil Nadu", 105, "Karnataka", 100,
  "Andhra Pradesh", 95, "Telangana", 100, "Uttar Pradesh", 105, "Punjab", 95,
  "Haryana", 95, "Rajasthan", 98, "Madhya Pradesh", 90, "West Bengal", 110,
  "Odisha", 75, "Chhattisgarh", 80, "Jharkhand", 85, "Bihar", 100,
  "Kerala", 105, "Assam", 95, "Delhi", 120
)

eu_prices <- tribble(
  ~name, ~price_mwh,
  "Germany", 190, "France", 150, "United Kingdom", 220, "Italy", 200, "Spain", 110,
  "Poland", 140, "Sweden", 85, "Norway", 75, "Finland", 80, "Netherlands", 160,
  "Belgium", 155, "Austria", 170, "Switzerland", 160, "Portugal", 105, "Ireland", 240,
  "Denmark", 110, "Czech Rep.", 160, "Hungary", 170, "Romania", 150, "Greece", 165
)

price_lut <- bind_rows(us_prices, cn_prices, in_prices, eu_prices)

# ==============================================================================
# 2. Geometry Matcher Helper Function
# ==============================================================================

# Iterates over attribute columns and securely matches names inside strings (e.g. "Zhejiang" in "Zhejiang Province")
merge_prices <- function(v, lut) {
    df <- as.data.frame(v)
    price_col <- rep(NA_real_, nrow(df))
    char_cols <- names(df)[sapply(df, is.character) | sapply(df, is.factor)]
    
    for (i in seq_len(nrow(lut))) {
        search_name <- lut$name[i]
        price <- lut$price_mwh[i]
        
        for (col in char_cols) {
            # Partial but robust string matching
            idx <- grep(search_name, as.character(df[[col]]), ignore.case = TRUE)
            
            if (length(idx) > 0) {
                # Assign price only where currently NA to prevent double-overwrites
                na_idx <- idx[is.na(price_col[idx])]
                if (length(na_idx) > 0) {
                    price_col[na_idx] <- price
                }
            }
        }
    }
    return(price_col)
}

# ==============================================================================
# 3. Process the Regions
# ==============================================================================
gis_proc <- "../GIS/processed"

# Note: US uses "us" prefix for its template.
region_config <- list(
    USA = list(prefix = "us", tpl = "us", admin = "admin1", default = 90),
    China = list(prefix = "china", tpl = "china", admin = "admin1", default = 85),
    India = list(prefix = "india", tpl = "india", admin = "admin1", default = 95),
    Europe = list(prefix = "europe", tpl = "europe", admin = "admin0", default = 160)
)

for (r in names(region_config)) {
    message("\n==================================")
    message("Generating Electricity Price Layer: ", r)
    reg <- region_config[[r]]
    
    # 3a. Load Template
    tpl_path <- file.path(gis_proc, paste0(reg$tpl, "_biomass.tif"))
    if (!file.exists(tpl_path)) {
        warning(paste("Template missing:", tpl_path, "- Skipping."))
        next
    }
    tpl <- terra::rast(tpl_path)
    
    # 3b. Load Admin Boundaries
    v_path <- file.path(gis_proc, paste0(reg$prefix, "_", reg$admin, ".gpkg"))
    if (!file.exists(v_path)) {
        warning(paste("Boundaries missing:", v_path, "- Skipping."))
        next
    }
    v <- terra::vect(v_path)
    
    # 3c. Extract and Attach Prices
    message("Matching names to attributes...")
    v$price_mwh <- merge_prices(v, price_lut)
    
    # Handle regions that failed to match any prices
    matched_pct <- sum(!is.na(v$price_mwh)) / nrow(v) * 100
    message(sprintf("Matched %.1f%% of regions natively.", matched_pct))
    
    # Fallback to regional default for any NAs
    v$price_mwh[is.na(v$price_mwh)] <- reg$default
    
    # 3d. Rasterize
    message("Rasterizing to template grid...")
    # `background = reg$default` ensures oceans and blank edges catch the baseline price
    # so that the TEA model doesn't crash from NA division.
    r_elec <- terra::rasterize(v, tpl, field = "price_mwh", background = reg$default)
    names(r_elec) <- "elec_price"
    
    # 3e. Export
    out_path <- file.path(gis_proc, paste0(reg$prefix, "_elec_price.tif"))
    terra::writeRaster(r_elec, out_path, overwrite = TRUE)
    message("Saved: ", out_path)
}

message("\nAll heterogeneous electricity price layers parsed and securely rasterized.")
