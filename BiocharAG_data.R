==> BiocharAG/data-raw/download_and_process_spatial.R <==
library(jsonlite)
library(terra)
library(sf)

# 0. Setup
dir.create("data-raw/external", showWarnings = FALSE, recursive = TRUE)
options(timeout = 3600) # 60 mins

# 1. Get Dataverse File ID
meta <- tryCatch(jsonlite::fromJSON("dataverse_meta.json"), error = function(e) null)

res_avail_url <- NULL
if (!is.null(meta)) {
    files <- meta$data$latestVersion$files
    # Look for res_avail.tif
    idx <- grep("res_avail.tif", files$label, ignore.case = TRUE)
    if (length(idx) > 0) {
        fid <- files$dataFile$id[idx[1]]
        res_avail_url <- paste0("https://dataverse.harvard.edu/api/access/datafile/", fid)
        message("Found res_avail.tif ID: ", fid)
    }
}

# 2. Download Files
# Biomass (Skip if exists and > 1MB)
dest_biomass <- "data-raw/external/res_avail.tif"
if (file.exists(dest_biomass) && file.size(dest_biomass) > 1000000) {
    message("Biomass layer exists (", round(file.size(dest_biomass) / 1024 / 1024, 1), " MB). Skipping download.")
} else {
    if (!is.null(res_avail_url)) {
        message("Downloading Biomass Density...")
        download.file(res_avail_url, dest_biomass, mode = "wb")
    } else {
        warning("Could not find res_avail.tif in metadata. Skipping download.")
    }
}

# Soil Temp (Zenodo)
dest_soil <- "data-raw/external/sbio1.tif"
sbio_url <- "https://zenodo.org/records/7134169/files/SBIO1_0_5cm_Annual_Mean_Temperature.tif?download=1"

# Check if complete (approx 335 MB). If < 300MB, re-download.
if (file.exists(dest_soil) && file.size(dest_soil) > 300 * 1024 * 1024) {
    message("Soil Temp layer exists and appears complete (", round(file.size(dest_soil) / 1024 / 1024, 1), " MB). Skipping download.")
} else {
    message("Downloading Soil Temp...")
    tryCatch(
        download.file(sbio_url, dest_soil, mode = "wb"),
        error = function(e) warning("Failed to download Soil Temp: ", e$message)
    )
}

# 3. Process Layers (US 20km Grid)
message("Processing Layers...")

# Create Template Grid (US Extent, ~20km ~ 0.2 deg)
# Approx US BBox: -125, 24, -66, 50
us_extent <- ext(-125, -66, 24, 50)
us_template <- rast(us_extent, res = 0.2, crs = "EPSG:4326")

processed_layers <- list()

if (file.exists(dest_biomass)) {
    r_bm <- rast(dest_biomass)
    r_bm_us <- project(r_bm, us_template)

    v_max <- global(r_bm_us, "max", na.rm = TRUE)$max
    message("Biomass Max Value: ", v_max)

    # Assuming kg/ha. Goal: Mg/km2.
    # 1 kg/ha = 0.001 Mg / 0.01 km2 = 0.1 Mg/km2.
    # Using Factor = 0.1
    if (v_max > 10000 && v_max < 50000) {
        r_bm_us <- r_bm_us * 0.1
        message("Assuming kg/ha input. Converting to Mg/km2 (Factor 0.1). Max: ", global(r_bm_us, "max", na.rm = TRUE)$max)
    } else if (v_max < 50) {
        r_bm_us <- r_bm_us * 100 # t/ha -> Mg/km2
        message("Creating Mg/km2 from t/ha assumption")
    }

    names(r_bm_us) <- "biomass_density"
    processed_layers$biomass_density <- r_bm_us
}

if (file.exists(dest_soil) && file.size(dest_soil) > 1000000) {
    r_st <- rast(dest_soil)
    # Resample
    r_st_us <- project(r_st, us_template)

    # Unit Check
    # SBIO1 is usually x10 degC? Or just degC?
    # Chelsa/WorldClim often x10.
    v_mm <- minmax(r_st_us)
    message("Soil Temp Range: ", v_mm[1], " - ", v_mm[2])

    # If range is 0 - 300, divide by 10.
    if (v_mm[2] > 60) {
        r_st_us <- r_st_us / 10
        message("Dividing Soil Temp by 10")
    }

    names(r_st_us) <- "soil_temp"
    processed_layers$soil_temp <- r_st_us
}

# 4. Save
save(processed_layers, file = "data/spatial_demo_layers.rda")
terra::writeRaster(processed_layers$biomass_density, "data/demo_biomass.tif", overwrite = TRUE)
terra::writeRaster(processed_layers$soil_temp, "data/demo_soil_temp.tif", overwrite = TRUE)

message("Done.")

==> BiocharAG/data-raw/generate_ci_layers.R <==
# data-raw/generate_ci_layers.R
# This script rasterizes the marginal carbon intensity by country/state 
# into regional `_ff_c_intensity.tif` layers for the TEA pipeline.

library(terra)
library(sf)
library(dplyr)

message("======================================================================")
message("Generating Spatial Carbon Intensity Layers...")
message("======================================================================")

gis_path <- "GIS/processed/"
csv_path <- "BiocharAG/data-raw/marginal_ci_by_country.csv"

if (!file.exists(csv_path)) {
  stop("Input CSV not found at: ", csv_path)
}

df <- read.csv(csv_path, stringsAsFactors = FALSE)

# Check for the expected column
if (!"Merged_CI_tCO2_GJ" %in% names(df)) {
  stop("Missing 'Merged_CI_tCO2_GJ' column in the CSV data.")
}

# ------------------------------------------------------------------------------
# 1. Process US
# ------------------------------------------------------------------------------
message("Processing US carbon intensity layer...")
us_admin <- st_read(paste0(gis_path, "us_admin1.gpkg"), quiet = TRUE)
us_df <- df %>% filter(grepl("^US-", Code))

# Join by state name
us_admin <- merge(us_admin, us_df, by.x = "NAM_1", by.y = "Name", all.x = TRUE)

us_bm <- rast(paste0(gis_path, "us_biomass.tif"))
us_ci <- rasterize(us_admin, us_bm, field = "Merged_CI_tCO2_GJ")

writeRaster(us_ci, paste0(gis_path, "us_ff_c_intensity.tif"), overwrite = TRUE, gdal = c("COMPRESS=LZW"))
message("  -> Created us_ff_c_intensity.tif")

# ------------------------------------------------------------------------------
# 2. Process Europe
# ------------------------------------------------------------------------------
message("Processing Europe carbon intensity layer...")
eu_admin <- st_read(paste0(gis_path, "europe_admin0.gpkg"), quiet = TRUE)

# Join by country name. The CSV has names like "Germany", "France", etc.
eu_admin <- merge(eu_admin, df, by.x = "NAM_0", by.y = "Name", all.x = TRUE)

eu_bm <- rast(paste0(gis_path, "europe_biomass.tif"))
eu_ci <- rasterize(eu_admin, eu_bm, field = "Merged_CI_tCO2_GJ")

writeRaster(eu_ci, paste0(gis_path, "europe_ff_c_intensity.tif"), overwrite = TRUE, gdal = c("COMPRESS=LZW"))
message("  -> Created europe_ff_c_intensity.tif")

# ------------------------------------------------------------------------------
# 3. Process China
# ------------------------------------------------------------------------------
message("Processing China carbon intensity layer...")
cn_val <- df$Merged_CI_tCO2_GJ[df$Code == "CN"]

if (length(cn_val) > 0) {
  cn_bm <- rast(paste0(gis_path, "china_biomass.tif"))
  cn_ci <- rast(cn_bm)
  values(cn_ci) <- cn_val[1]
  
  # Mask to the country extent
  cn_admin <- st_read(paste0(gis_path, "china_admin0.gpkg"), quiet = TRUE)
  cn_ci <- mask(cn_ci, cn_admin)
  
  writeRaster(cn_ci, paste0(gis_path, "china_ff_c_intensity.tif"), overwrite = TRUE, gdal = c("COMPRESS=LZW"))
  message("  -> Created china_ff_c_intensity.tif")
} else {
  warning("  -> Skipping China: No matching data in CSV.")
}

# ------------------------------------------------------------------------------
# 4. Process India
# ------------------------------------------------------------------------------
message("Processing India carbon intensity layer...")
in_val <- df$Merged_CI_tCO2_GJ[df$Code == "IN"]

if (length(in_val) > 0) {
  in_bm <- rast(paste0(gis_path, "india_biomass.tif"))
  in_ci <- rast(in_bm)
  values(in_ci) <- in_val[1]
  
  # Mask to the country extent
  in_admin <- st_read(paste0(gis_path, "india_admin0.gpkg"), quiet = TRUE)
  in_ci <- mask(in_ci, in_admin)
  
  writeRaster(in_ci, paste0(gis_path, "india_ff_c_intensity.tif"), overwrite = TRUE, gdal = c("COMPRESS=LZW"))
  message("  -> Created india_ff_c_intensity.tif")
} else {
  warning("  -> Skipping India: No matching data in CSV.")
}

message("======================================================================")
message("Carbon Intensity Layer Processing Complete!")
message("======================================================================")

==> BiocharAG/data-raw/generate_distance_rasters.R <==
# data-raw/generate_distance_rasters.R
library(terra)

# Assuming working directory is Biochar_AG/BiocharAG/
gis_dir <- "../GIS/processed/"

regions <- c("us", "china", "europe", "india")
sizes_mw_th <- c(5, 25, 50, 100, 250, 500)
radii_km <- c(5, 10, 25, 50, 100, 150, 250, 500)

bm_lhv <- 18.6 # Default LHV
capacity_factor <- 0.85

# Equal-Area Projections for each region to ensure accurate circular buffers
proj_dict <- list(
    us = "EPSG:5070", # NAD83 / Conus Albers
    europe = "EPSG:3035", # ETRS89 / LAEA Europe
    china = "+proj=aea +lat_1=25 +lat_2=47 +lat_0=30 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
    india = "+proj=aea +lat_1=12 +lat_2=28 +lat_0=24 +lon_0=80 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
)

# Resolution in meters (10 km x 10 km) to balance speed and accuracy
res_m <- 10000
cell_area_km2 <- (res_m / 1000)^2

for (region in regions) {
    bm_file <- file.path(gis_dir, paste0(region, "_biomass.tif"))
    if (!file.exists(bm_file)) {
        message("Skipping ", region, " - biomass file not found.")
        next
    }

    message("\nProcessing region: ", region)

    dens_wgs84 <- terra::rast(bm_file)
    
    # Reclassify values <= 0 and NAs to 0.0
    rcl <- matrix(c(-Inf, 0, 0, NA, NA, 0), ncol = 3, byrow = TRUE)
    dens_wgs84 <- terra::classify(dens_wgs84, rcl)

    # Project to Equal Area
    proj_str <- proj_dict[[region]]
    dens_ea <- terra::project(dens_wgs84, proj_str, res = res_m, method = "bilinear")

    # Calculate mass per cell (Mg)
    mass_ea <- dens_ea * cell_area_km2

    # Compute focal sums for all anchor radii
    focal_list <- list()
    message("  Computing focal sums...")
    for (r_km in radii_km) {
        if (r_km <= (res_m / 2000)) {
            f_sum <- mass_ea
        } else {
            w <- terra::focalMat(mass_ea, r_km * 1000, type = "circle")
            w[w > 0] <- 1
            f_sum <- terra::focal(mass_ea, w = w, na.rm = TRUE)
        }
        names(f_sum) <- paste0("r_", r_km)
        focal_list[[paste0("r_", r_km)]] <- f_sum
    }
    focal_stack <- terra::rast(focal_list)

    # Interpolate for each target size
    for (sz in sizes_mw_th) {
        message("  Interpolating for size: ", sz, " MW_th")
        target_mass <- (sz * 8760 * capacity_factor) / (bm_lhv * 0.277778)

        # Initialize output radius raster with NA
        # (This enforces the constraint: If target_mass > max available mass, it stays NA)
        out_radius <- terra::rast(mass_ea, nlyrs = 1, vals = NA)

        # Piecewise Interpolation
        rad_lower <- 0
        mass_lower <- mass_ea * 0

        for (i in seq_along(radii_km)) {
            rad_upper <- radii_km[i]
            mass_upper <- focal_stack[[i]]

            # Mask where target falls in this bin
            mask <- (target_mass > mass_lower) & (target_mass <= mass_upper)

            # Correct Quadratic Interpolation (Area -> Radius)
            fraction <- (target_mass - mass_lower) / (mass_upper - mass_lower)

            # Prevent division by zero errors where mass is perfectly flat
            fraction <- terra::ifel(mass_upper == mass_lower, 0, fraction)

            # Interpolate based on the square of the radius (Area)
            r_interp <- sqrt(rad_lower^2 + fraction * (rad_upper^2 - rad_lower^2))

            out_radius <- terra::ifel(mask, r_interp, out_radius)

            rad_lower <- rad_upper
            mass_lower <- mass_upper
        }

        # Convert to avg_dist (2/3 of collection radius for a circle)
        avg_dist_ea <- (2 / 3) * out_radius

        # Reproject back to WGS84 template
        avg_dist_wgs84 <- terra::project(avg_dist_ea, dens_wgs84, method = "bilinear")

        out_file <- file.path(gis_dir, paste0(region, "_dist_", sz, "MWth.tif"))
        terra::writeRaster(avg_dist_wgs84, out_file, overwrite = TRUE)
        message("    Saved ", out_file)
    }
}

==> BiocharAG/data-raw/generate_elec_price_layer.R <==
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

==> BiocharAG/data-raw/generate_fperm_lut.R <==
# data-raw/generate_fperm_lut.R

# Load the current package development version
devtools::load_all()

# Define Ranges
soil_temps <- seq(-55, 40, by = 1) # Soil temperatures
hc_vals <- seq(0, 0.7, by = 0.02) # H:C org ratios
py_temps <- seq(350, 1000, by = 10) # Pyrolysis temperatures

message("Generating H:C Look-Up Table...")

# Function wrapper to ensure scalar inputs for calculate_fperm if strictly needed
calc_hc <- function(h, t) {
    calculate_fperm(val = h, method = "HC", soil_temp = t)
}
calc_hc_vec <- Vectorize(calc_hc)

# Outer product for H:C grid
# Rows = H:C values, Cols = Soil Temps
hc_grid <- outer(hc_vals, soil_temps, calc_hc_vec)
dimnames(hc_grid) <- list(as.character(hc_vals), as.character(soil_temps))

message("Generating Pyrolysis Temp Look-Up Table...")

# Wrapper for PyTemp
calc_py <- function(p, t) {
    calculate_fperm(val = p, method = "Temp", soil_temp = t)
}
calc_py_vec <- Vectorize(calc_py)

# Outer product for PyTemp grid
# Rows = PyTemp values, Cols = Soil Temps
temp_grid <- outer(py_temps, soil_temps, calc_py_vec)
dimnames(temp_grid) <- list(as.character(py_temps), as.character(soil_temps))

# Combine into a list
fperm_lut <- list(
    hc_grid = hc_grid,
    temp_grid = temp_grid,
    soil_temps = soil_temps,
    hc_vals = hc_vals,
    py_temps = py_temps
)

message("Saving fperm_lut to data/...")
usethis::use_data(fperm_lut, overwrite = TRUE)
message("Done.")

==> BiocharAG/data-raw/generate_marginal_ci.R <==
# data-raw/generate_marginal_ci.R
# This script calculates the marginal carbon intensity (CI) of new electricity generation capacity 
# by country and US state by analyzing recent growth (2019-2024) across generation sources.
# It also ingests average grid CIs as a fallback for regions without growth or missing data.

library(dplyr)
library(tidyr)

message("======================================================================")
message("Starting Marginal Carbon Intensity Data Processing Pipeline...")
message("======================================================================")

# ------------------------------------------------------------------------------
# 1. Load Datasets
# ------------------------------------------------------------------------------
gen_path <- "BiocharAG/data-raw/generation-including-net-imports-monthly.csv"
avg_ci_path <- "BiocharAG/data-raw/carbon-intensity-electricity.csv"

if (!file.exists(gen_path)) stop("Monthly generation dataset not found at: ", gen_path)
if (!file.exists(avg_ci_path)) stop("Average carbon intensity dataset not found at: ", avg_ci_path)

message("Loading monthly generation data...")
df_gen <- read.csv(gen_path, check.names = FALSE, stringsAsFactors = FALSE)

message("Loading average carbon intensity data...")
df_avg_ci <- read.csv(avg_ci_path, stringsAsFactors = FALSE)

# Rename the first three columns of the generation dataset for easier access
colnames(df_gen)[1] <- "Code"
colnames(df_gen)[2] <- "Name"
colnames(df_gen)[3] <- "Source"

# ------------------------------------------------------------------------------
# 2. Reshape and Melt Monthly Data
# ------------------------------------------------------------------------------
message("Melting monthly generation data into long format...")
month_cols <- grep("^20", colnames(df_gen), value = TRUE)

df_melt <- df_gen %>%
  select(Code, Name, Source, all_of(month_cols)) %>%
  pivot_longer(
    cols = all_of(month_cols),
    names_to = "Month",
    values_to = "Generation"
  ) %>%
  mutate(
    Generation = as.numeric(Generation),
    Year = substr(Month, 1, 4)
  ) %>%
  filter(!is.na(Generation))

# ------------------------------------------------------------------------------
# 3. Standardize Names and US State Mapping
# ------------------------------------------------------------------------------
message("Standardizing region and US state names...")

us_state_map <- c(
  "AL" = "Alabama", "AK" = "Alaska", "AZ" = "Arizona", "AR" = "Arkansas", "CA" = "California",
  "CO" = "Colorado", "CT" = "Connecticut", "DE" = "Delaware", "FL" = "Florida", "GA" = "Georgia",
  "HI" = "Hawaii", "ID" = "Idaho", "IL" = "Illinois", "IN" = "Indiana", "IA" = "Iowa",
  "KS" = "Kansas", "KY" = "Kentucky", "LA" = "Louisiana", "ME" = "Maine", "MD" = "Maryland",
  "MA" = "Massachusetts", "MI" = "Michigan", "MN" = "Minnesota", "MS" = "Mississippi", "MO" = "Missouri",
  "MT" = "Montana", "NE" = "Nebraska", "NV" = "Nevada", "NH" = "New Hampshire", "NJ" = "New Jersey",
  "NM" = "New Mexico", "NY" = "New York", "NC" = "North Carolina", "ND" = "North Dakota", "OH" = "Ohio",
  "OK" = "Oklahoma", "OR" = "Oregon", "PA" = "Pennsylvania", "RI" = "Rhode Island", "SC" = "South Carolina",
  "SD" = "South Dakota", "TN" = "Tennessee", "TX" = "Texas", "UT" = "Utah", "VT" = "Vermont",
  "VA" = "Virginia", "WA" = "Washington", "WV" = "West Virginia", "WI" = "Wisconsin", "WY" = "Wyoming",
  "DC" = "District of Columbia"
)

standardize_us_state <- function(code) {
  suffix <- sub("US-", "", code)
  if (suffix %in% names(us_state_map)) {
    return(us_state_map[[suffix]])
  }
  return(code)
}

df_melt <- df_melt %>%
  mutate(Clean_Name = case_when(
    Name == "People's Republic of China" ~ "China",
    Name == "Republic of China (Taiwan)" ~ "Taiwan",
    Name == "Bosnia & Herzegovina" ~ "Bosnia and Herzegovina",
    grepl("^Special region: US-", Name) ~ sapply(Code, standardize_us_state),
    TRUE ~ Name
  ))

# ------------------------------------------------------------------------------
# 4. Filter and Aggregate by Year
# ------------------------------------------------------------------------------
# IPCC default values of lifecycle carbon intensity by generation type (gCO2eq / kWh)
ipcc_ci <- c(
  coal = 820,
  gas = 490,
  biofuels = 230,
  geothermal = 38,
  hydro = 24,
  nuclear = 12,
  solar = 48,
  wind = 11.5,
  oil = 700
)

message("Filtering primary mutually-exclusive sources and aggregating by year...")
df_primary <- df_melt %>%
  filter(Source %in% names(ipcc_ci))

df_yr <- df_primary %>%
  group_by(Code, Clean_Name, Source, Year) %>%
  summarize(Generation = sum(Generation, na.rm = TRUE), .groups = "drop")

# ------------------------------------------------------------------------------
# 5. Extract Recent 5-Year Window (2019 to 2024)
# ------------------------------------------------------------------------------
start_year <- "2019"
end_year <- "2024"
message(sprintf("Calculating generation change (delta) between %s and %s...", start_year, end_year))

df_start <- df_yr %>%
  filter(Year == start_year) %>%
  select(Code, Clean_Name, Source, Gen_Start = Generation)

df_end <- df_yr %>%
  filter(Year == end_year) %>%
  select(Code, Clean_Name, Source, Gen_End = Generation)

# Calculate the change in generation (delta)
df_delta <- full_join(df_start, df_end, by = c("Code", "Clean_Name", "Source")) %>%
  mutate(
    Gen_Start = replace_na(Gen_Start, 0),
    Gen_End = replace_na(Gen_End, 0),
    Delta_Gen = Gen_End - Gen_Start
  )

# Isolate sources adding new capacity (Delta_Gen > 0)
df_growth <- df_delta %>%
  filter(Delta_Gen > 0) %>%
  mutate(CI = ipcc_ci[Source]) %>%
  mutate(Emissions_Added = Delta_Gen * CI)

# Aggregate by country/state to find the weighted marginal CI
df_marginal <- df_growth %>%
  group_by(Code, Clean_Name) %>%
  summarize(
    Total_Delta_Gen = sum(Delta_Gen, na.rm = TRUE),
    Total_Emissions_Added = sum(Emissions_Added, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Marginal_CI = Total_Emissions_Added / Total_Delta_Gen)

# ------------------------------------------------------------------------------
# 6. Process Average Grid Carbon Intensity Fallback
# ------------------------------------------------------------------------------
message("Processing average grid carbon intensity fallback layer...")
df_avg_latest <- df_avg_ci %>%
  group_by(Entity) %>%
  filter(Year == max(Year)) %>%
  ungroup() %>%
  select(Entity, Avg_Code = Code, Avg_Year = Year, Average_CI = Carbon.intensity.of.electricity.per.kWh) %>%
  mutate(Clean_Name = case_when(
    Entity == "United States" ~ "United States",
    Entity == "China" ~ "China",
    Entity == "India" ~ "India",
    TRUE ~ Entity
  ))

# ------------------------------------------------------------------------------
# 7. Merge and Apply Fallback Logic
# ------------------------------------------------------------------------------
message("Merging marginal and average datasets...")

df_merged <- full_join(
  df_marginal, 
  df_avg_latest, 
  by = "Clean_Name"
)

# Populate Code and Name for any rows that were only present in average dataset
df_merged <- df_merged %>%
  mutate(
    Code = if_else(is.na(Code), Avg_Code, Code),
    Code = if_else(Clean_Name == "China", "CN", Code),
    Code = if_else(Clean_Name == "India", "IN", Code),
    Code = if_else(Clean_Name == "United States", "US", Code)
  )

# Extract national average grid CI for United States to use as fallback for US States
us_avg_ci <- df_avg_latest %>%
  filter(Clean_Name == "United States") %>%
  pull(Average_CI)

if (length(us_avg_ci) > 0) {
  us_avg_ci <- us_avg_ci[1]
} else {
  us_avg_ci <- 370.0 # Default fallback if missing
}

# Fill missing Average_CI for US states with US national average
df_merged <- df_merged %>%
  mutate(
    Average_CI = if_else(is.na(Average_CI) & grepl("^US-", Code), us_avg_ci, Average_CI)
  )

# Calculate final merged CI (Marginal CI as primary, Average CI as fallback)
df_merged <- df_merged %>%
  mutate(
    Merged_CI = case_when(
      !is.na(Marginal_CI) & Marginal_CI > 0 ~ Marginal_CI,
      !is.na(Average_CI) ~ Average_CI,
      TRUE ~ 400.0 # Global fallback baseline
    )
  )

# ------------------------------------------------------------------------------
# 8. Convert Units and Select Output Columns
# ------------------------------------------------------------------------------
message("Converting units and formatting output...")
# 1 gCO2eq/kWh = 1/3600 tCO2eq/GJ
df_final <- df_merged %>%
  mutate(
    Marginal_CI_gCO2_kWh = Marginal_CI,
    Marginal_CI_tCO2_GJ  = Marginal_CI / 3600,
    Average_CI_gCO2_kWh  = Average_CI,
    Average_CI_tCO2_GJ   = Average_CI / 3600,
    Merged_CI_gCO2_kWh   = Merged_CI,
    Merged_CI_tCO2_GJ    = Merged_CI / 3600
  ) %>%
  select(
    Code,
    Name = Clean_Name,
    Total_Delta_Gen,
    Total_Emissions_Added,
    Marginal_CI_gCO2_kWh,
    Marginal_CI_tCO2_GJ,
    Average_CI_gCO2_kWh,
    Average_CI_tCO2_GJ,
    Merged_CI_gCO2_kWh,
    Merged_CI_tCO2_GJ
  ) %>%
  filter(!is.na(Code) & Code != "") %>%
  arrange(Code)

# ------------------------------------------------------------------------------
# 9. Save Output CSV
# ------------------------------------------------------------------------------
out_csv <- "BiocharAG/data-raw/marginal_ci_by_country.csv"
message("Saving results to: ", out_csv)
write.csv(df_final, out_csv, row.names = FALSE)

message("======================================================================")
message("Marginal CI data processing completed successfully!")
message("======================================================================")

==> BiocharAG/data-raw/generate_sinks.R <==
# # data-raw/generate_sinks.R

# Block below commented out, as deprecated.  TODO: Remove later
# library(sf)
# library(dplyr)
# # Approximate centroids of major CO2 storage basins (NETL / CO2StoP / Global)
# sinks_list <- tribble(
#     ~Region, ~Basin, ~Lat, ~Lon,
#     "North America", "Illinois Basin", 39.0, -89.0,
#     "North America", "Permian Basin", 31.5, -103.0,
#     "North America", "Gulf Coast", 29.5, -95.0,
#     "North America", "Williston Basin", 47.5, -103.5,
#     "North America", "Alberta Basin", 54.0, -114.0,
#     "Europe", "North Sea (Sleipner/Aurora)", 58.5, 1.9,
#     "Europe", "Rotterdam/Porthos", 51.9, 4.0,
#     "Europe", "Adriatic", 44.5, 13.0,
#     "Asia", "Ordos Basin (China)", 38.0, 109.0,
#     "Asia", "Songliao Basin (China)", 45.0, 125.0,
#     "Asia", "Cambay Basin (India)", 22.5, 72.5,
#     "Asia", "Bombay High (India)", 19.5, 71.3
# )
# # Convert to sf object
# co2_sinks <- st_as_sf(sinks_list, coords = c("Lon", "Lat"), crs = 4326)
# usethis::use_data(co2_sinks, overwrite = TRUE)

library(sf)
library(dplyr)
library(tibble)
library(usethis)

# ==============================================================================
# Global Carbon Sink Database
# Source: Global Geologic Carbon Storage Assessment & Technoeconomic Transport Modeling
# Table 1: Comprehensive Global Storage Basin and Sink List
# ==============================================================================

sinks_list <- tribble(
    ~Region, ~Basin_Name, ~Sub_Unit, ~Type, ~Lat, ~Lon, ~Regional_Factor, ~Is_EOR, ~Notes,

    # --- NORTH AMERICA (USA) ---
    "North America", "Gulf Coast Basin", "Frio/Miocene Sands", "Onshore", 29.5, -95.0, 1.0, FALSE, "Premier global hub; <$10/t transport",
    "North America", "Permian Basin", "San Andres/Clearfork", "Onshore", 31.5, -103.5, 1.0, TRUE, "EOR & Saline; Existing pipeline network",
    "North America", "Illinois Basin", "Mt. Simon Sandstone", "Onshore", 39.8, -89.0, 1.0, FALSE, "Proven by Decatur ADM project",
    "North America", "Williston Basin", "Madison/Broom Creek", "Onshore", 47.5, -103.0, 1.0, TRUE, "Weyburn-Midale region (EOR)",
    "North America", "Michigan Basin", "St. Peter Sandstone", "Onshore", 44.0, -85.0, 1.0, FALSE, "Saline capacity",
    "North America", "Appalachian Basin", "Oriskany/Rose Run", "Onshore", 40.0, -80.0, 1.0, FALSE, "Critical for East Coast ind. corridor",
    "North America", "Powder River Basin", "Muddy Sandstone", "Onshore", 44.5, -105.5, 1.0, TRUE, "Wyoming coal/EOR belt",
    "North America", "San Juan Basin", "Entrada Sandstone", "Onshore", 36.5, -107.5, 1.0, FALSE, "Four Corners region",
    "North America", "Anadarko Basin", "Granite Wash", "Onshore", 35.5, -99.0, 1.0, TRUE, "Oklahoma/Texas Panhandle (EOR)",

    # --- EUROPE (Offshore Focus) ---
    "Europe", "Northern North Sea (NO)", "Utsira Formation", "Offshore", 58.4, 1.9, 1.2, FALSE, "Sleipner site; Massive aquifer",
    "Europe", "Northern North Sea (NO)", "Johansen Formation", "Offshore", 60.5, 3.5, 1.2, FALSE, "Northern Lights / Aurora",
    "Europe", "Southern North Sea (NL)", "P18/P15 Fields", "Offshore", 52.0, 3.5, 1.2, FALSE, "Porthos (Rotterdam)",
    "Europe", "Southern North Sea (UK)", "Goldeneye/Viking", "Offshore", 53.5, 2.0, 1.2, FALSE, "UK Sector depleted gas",
    "Europe", "North German Basin", "Mid. Buntsandstein", "Onshore", 53.0, 10.0, 1.2, FALSE, "Onshore Germany",
    "Europe", "Paris Basin", "Keuper/Dogger", "Onshore", 48.5, 3.0, 1.2, FALSE, "France industrial hub",
    "Europe", "Pannonian Basin", "Sava/Drava Depr.", "Onshore", 46.0, 17.0, 1.2, TRUE, "Croatia/Hungary EOR",

    # --- CHINA (Source-Sink Mismatch) ---
    "China", "Ordos Basin", "Triassic Liujiagou", "Onshore", 39.33, 110.15, 0.7, TRUE, "Shenhua region; EOR Potential",
    "China", "Songliao Basin", "Cretaceous Sands", "Onshore", 45.0, 125.0, 0.7, TRUE, "Daqing Oilfield (EOR)",
    "China", "Bohai Bay Basin", "Shahejie Formation", "Offshore", 38.5, 119.5, 0.7, TRUE, "Shengli/Dagang Oilfields (EOR)",
    "China", "Tarim Basin", "Carboniferous", "Onshore", 40.0, 84.0, 0.7, TRUE, "Deep saline & EOR",
    "China", "Subei Basin", "Paleogene Sands", "Onshore", 33.0, 119.5, 0.7, FALSE, "Near Yangtze Delta",
    "China", "Junggar Basin", "Jurassic/Triassic", "Onshore", 45.0, 86.0, 0.7, TRUE, "Xinjiang Oilfield (EOR)",
    "China", "Pearl River Mouth", "Enping 15-1", "Offshore", 21.5, 114.5, 0.7, FALSE, "Greater Bay Area",

    # --- INDIA (Emerging / Data Poor) ---
    "India", "Cambay Basin", "Gandhar/Ankleshwar", "Onshore", 21.7, 72.9, 0.7, TRUE, "Gujarat industrial belt; EOR Potential",
    "India", "Krishna-Godavari", "Syn-rift sediments", "Offshore", 16.5, 82.0, 0.7, FALSE, "East Coast (Visakhapatnam)",
    "India", "Assam-Arakan", "Barail/Tipam", "Onshore", 27.5, 95.5, 0.7, TRUE, "Northeast; Older oilfields",
    "India", "Cauvery Basin", "Cretaceous Sands", "Onshore", 11.0, 79.5, 0.7, FALSE, "Tamil Nadu region",
    "India", "Rajasthan Basin", "Barmer/Jaisalmer", "Onshore", 26.0, 71.0, 0.7, TRUE, "Northwest desert (Cairn Oil)",
    "India", "Mahanadi Basin", "Mesozoic Sediments", "Onshore", 20.0, 87.0, 0.7, FALSE, "Odisha region"
)

# Convert to sf object (CRS 4326 for WGS84)
co2_sinks <- st_as_sf(sinks_list, coords = c("Lon", "Lat"), crs = 4326)

# Save to package data
usethis::use_data(co2_sinks, overwrite = TRUE)

# Print Summary
message("Sinks database updated with ", nrow(sinks_list), " entries.")
print(table(sinks_list$Region, sinks_list$Type))

==> BiocharAG/data-raw/generate_us_soil_layers.R <==
# Generate Demo Soil Layers for Advanced Valuation
# Creating plausible synthetic data for US Demo
#
# Real implementation would use:
# geodata::soil_world(var="phh2o", depth=15, path="GIS/raw")
#
# Current Demo:
# pH: Gradient from East (Low/Acidic) to West (High/Alkaline)
# CEC: Random variation with spatial autocorrelation

library(terra)

# 1. Load Template
template_path <- "../GIS/processed/us_biomass.tif"
if (file.exists(template_path)) {
    r_template <- terra::rast(template_path)
} else {
    stop("Template biomass raster not found. Run previous processing scripts.")
}

# 2. Generate pH Layer
# Hypothesis: US pH roughly correlates with longitude (Rainfall)
# East (Wet) -> Acidic (~5.5 - 6.5)
# West (Arid) -> Alkaline (~7.0 - 8.0)
coords <- terra::xyFromCell(r_template, 1:ncell(r_template))
lons <- coords[, 1]

# Normalize Lon: -125 (West) to -70 (East)
# Scale: (lon - min) / (max - min)
lon_min <- -125
lon_max <- -70
lon_norm <- (lons - lon_min) / (lon_max - lon_min) # 0 (West) to 1 (East)

# pH Model: West=7.5, East=5.5
# ph = 7.5 - (2.0 * lon_norm) + Noise
set.seed(42)
noise <- rnorm(length(lons), mean = 0, sd = 0.5)
ph_vals <- 7.5 - (2.0 * lon_norm) + noise

r_ph <- terra::rast(r_template)
values(r_ph) <- ph_vals
names(r_ph) <- "soil_ph"

# 3. Generate CEC Layer (cmol/kg)
# Range: Sand (~5) to Clay (~30-40)
# Make it somewhat spatially correlated (smooth noise)
r_noise <- terra::rast(r_template)
values(r_noise) <- rnorm(ncell(r_template))
# Smooth it to create "regions"
r_cec_spatial <- terra::focal(r_noise, w = 5, fun = mean)

# Normalize 0-1
v_min <- minmax(r_cec_spatial)[1]
v_max <- minmax(r_cec_spatial)[2]
r_cec_norm <- (r_cec_spatial - v_min) / (v_max - v_min)

# Scale to typical CEC 5 -> 30
r_cec <- 5 + (r_cec_norm * 25)
names(r_cec) <- "soil_cec"

# 4. Save
out_dir <- "../GIS/processed"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

terra::writeRaster(r_ph, file.path(out_dir, "us_soil_ph.tif"), overwrite = TRUE)
terra::writeRaster(r_cec, file.path(out_dir, "us_soil_cec.tif"), overwrite = TRUE)

message("Demo Soil Layers Generated: pH and CEC")
plot(c(r_ph, r_cec), main = c("Demo Soil pH", "Demo Soil CEC"))

==> BiocharAG/data-raw/patch_biomass_units.R <==
library(terra)

gis_dir <- "GIS/processed/"
regions <- c("us", "china", "europe", "india")

for (r in regions) {
    file_path <- file.path(gis_dir, paste0(r, "_biomass.tif"))
    if (!file.exists(file_path)) {
        message("File not found: ", file_path)
        next
    }
    
    message("Patching units for ", r, "...")
    bm <- terra::rast(file_path)
    
    # Check if we already patched this by looking at max values
    max_val <- max(terra::global(bm, "max", na.rm=TRUE)[[1]])
    
    if (max_val > 1000) {
        message("  Detected unpatched raster (max > 1000). Applying area division.")
        
        # Calculate latitude for every cell
        lat <- terra::init(bm, "y")
        
        # The area of the original 5-arcmin pixel at latitude `y`
        # 5-arcmin = 0.08333 degrees
        # Equator width = 111.32 km * (5/60) = 9.276 km
        # Equator area = 9.276 * 9.276 = 86.05 km2
        r_area <- 86.05 * cos(lat * pi / 180)
        
        # Convert Total Mg to Mg / km2
        bm_density <- bm / r_area
        
        # Write directly back to disk
        terra::writeRaster(bm_density, file_path, overwrite = TRUE)
        message("  -> Successfully converted to Mg/km2")
    } else {
        message("  Raster appears already patched (max <= 1000). Skipping.")
    }
}
message("Done.")

==> BiocharAG/data-raw/process_borders.R <==
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
        template = file.path(gis_proc, "china_biomass.tif")
    ),
    Europe = list(
        prefix = "europe",
        filter = NULL, # Render all nations inside the Europe bounding box
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

==> BiocharAG/data-raw/process_china_europe.R <==
# Process Spatial Data for China and Europe
# Extracts templates and base layers from global datasets

library(terra)

# 1. Define Paths
gis_proc <- "../GIS/processed"
gis_raw <- "../GIS/raw"
soil_raw <- "../GIS/raw/soilgrids/files.isric.org/soilgrids/latest/data"

# Global Biomass
bm_global_path <- file.path(gis_raw, "res_avail.tif")
if (!file.exists(bm_global_path)) stop("Global biomass map not found in: ", bm_global_path)

message("Loading Global Biomass...")
r_bm_global <- terra::rast(bm_global_path)

# Regions definition
regions <- list(
    China = list(
        prefix = "china",
        ext = terra::ext(73, 135, 18, 54) # Approx BBox
    ),
    Europe = list(
        prefix = "europe",
        ext = terra::ext(-11, 40, 35, 71) # Approx BBox
    )
)

for (r_name in names(regions)) {
    prefix <- regions[[r_name]]$prefix
    e_box <- regions[[r_name]]$ext
    
    message("\n==================================")
    message("Processing Region: ", r_name)
    message("Bounding Box: ", e_box)
    
    # Process Biomass / Template
    r_bm <- terra::crop(r_bm_global, e_box)
    
    target_res <- 0.1
    message("Resampling biomass to ", target_res, " degree resolution...")
    r_template <- terra::rast(e_box, res = target_res)
    terra::crs(r_template) <- terra::crs(r_bm_global)
    
    r_bm_resampled <- terra::resample(r_bm, r_template, method = "bilinear")
    names(r_bm_resampled) <- "biomass_density"
    
    out_bm <- file.path(gis_proc, paste0(prefix, "_biomass.tif"))
    terra::writeRaster(r_bm_resampled, out_bm, overwrite = TRUE)
    message("Saved: ", out_bm)
    
    template <- r_bm_resampled
    
    # Process SoilGrids (CEC and pH)
    process_sg <- function(vrt_path, name, scaler = 0.1) {
        if (!file.exists(vrt_path)) {
            message("  Skipping missing VRT: ", vrt_path)
            return(NULL)
        }
        message("  Processing Soil Grid: ", name)
        r_vrt <- terra::rast(vrt_path)
        r_out <- terra::project(r_vrt, template)
        r_out <- r_out * scaler
        names(r_out) <- name
        out_p <- file.path(gis_proc, paste0(prefix, "_", name, ".tif"))
        terra::writeRaster(r_out, out_p, overwrite = TRUE)
        message("  Saved: ", out_p)
    }
    
    cec_vrt <- file.path(soil_raw, "cec/cec_0-5cm_mean.vrt")
    ph_vrt <- file.path(soil_raw, "phh2o/phh2o_0-5cm_mean.vrt")
    process_sg(cec_vrt, "soil_cec", 0.1)
    process_sg(ph_vrt, "soil_ph", 0.1)
    
    # Process Soil Temp
    temp_path <- file.path(gis_raw, "SBIO1_0_5cm_Annual_Mean_Temperature.tif")
    if (file.exists(temp_path)) {
        message("  Processing Soil Temperature...")
        r_temp <- terra::rast(temp_path)
        r_temp_proj <- terra::project(r_temp, template)
        names(r_temp_proj) <- "soil_temp"
        out_temp <- file.path(gis_proc, paste0(prefix, "_soil_temp.tif"))
        terra::writeRaster(r_temp_proj, out_temp, overwrite = TRUE)
        message("  Saved: ", out_temp)
    } else {
        message("  Soil Temp not found, using constant fallback...")
        r_temp_proj <- terra::rast(template)
        values(r_temp_proj) <- if (r_name == "Europe") 10 else 15
        names(r_temp_proj) <- "soil_temp"
        terra::writeRaster(r_temp_proj, file.path(gis_proc, paste0(prefix, "_soil_temp.tif")), overwrite = TRUE)
    }
    
    # Generate constant Electricity Price Layer
    r_elec <- terra::rast(template)
    values(r_elec) <- if (r_name == "Europe") 150 else 60 # Default scalar guesses $/MWh
    names(r_elec) <- "elec_price"
    terra::writeRaster(r_elec, file.path(gis_proc, paste0(prefix, "_elec_price.tif")), overwrite = TRUE)
    message("  Saved: ", paste0(prefix, "_elec_price.tif"))
}

message("Finished generating templates for missing regions.")

==> BiocharAG/data-raw/process_eu_feedstock.R <==
# data-raw/process_eu_feedstock.R
# Processes European country-level biomass prices into a spatial raster layer
# for the TEA model.

library(terra)
library(sf)
library(dplyr)
library(giscoR) # For fetching European country geometries

# ==============================================================================
# Setup: Define Paths and Load Template
# ==============================================================================
raw_dir <- "../GIS/raw"
proc_dir <- "../GIS/processed"

if (!dir.exists(raw_dir)) dir.create(raw_dir, recursive = TRUE)
if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)

message("Loading Europe Biomass Template...")
template_path <- file.path(proc_dir, "europe_biomass.tif")
if (!file.exists(template_path)) stop("Template raster not found. Run process_china_europe.R first.")
r_template <- terra::rast(template_path)

# ==============================================================================
# 1. Prepare Country Geometries
# ==============================================================================
message("Fetching Europe Country Boundaries...")
# Fetch countries using giscoR (includes all of Europe/Eurasia)
eu_countries <- gisco_get_countries(region = c("Europe", "Asia")) %>%
  sf::st_as_sf()

# Project countries to match the raster template
eu_countries_proj <- sf::st_transform(eu_countries, terra::crs(r_template))

# ==============================================================================
# 2. Process Biomass Prices
# ==============================================================================
message("Processing Biomass Prices...")

# Read the CSV with NUTS-3 baseline prices per country
csv_path <- "Biomass_price_Europe.csv"
if (!file.exists(csv_path)) {
  stop(sprintf("Prices CSV not found at: %s", csv_path))
}

price_data <- read.csv(csv_path, stringsAsFactors = FALSE)

# Clean up column names since it has spaces
names(price_data) <- gsub("\\.", "_", make.names(names(price_data)))

# Map specific country names to match the Eurostat (giscoR) NAME_ENGL column
price_data <- price_data %>%
  dplyr::mutate(Country_Name = dplyr::case_when(
    Country_Name == "Czech Republic (Czechia)" ~ "Czechia",
    Country_Name == "Russia" ~ "Russian Federation",
    Country_Name == "Republic of Ireland" ~ "Ireland",
    TRUE ~ Country_Name
  ))

# Join the price data to the spatial geometries
counties_cost <- eu_countries_proj %>%
  dplyr::left_join(price_data, by = c("NAME_ENGL" = "Country_Name")) %>%
  # Keep only those that have a price mapping, or keep all to allow NA areas
  # We will just pass the ones we joined, and fill the rest as NA during rasterization
  dplyr::select(NAME_ENGL, eu_base_eur = Price)

# Ensure the column to be rasterized is numeric
counties_cost$eu_base_eur <- as.numeric(counties_cost$eu_base_eur)

# Rasterize the spatial polygons to the template grid
# We rasterize the 'eu_base_eur' field
r_base_cost <- terra::rasterize(
  terra::vect(counties_cost), 
  r_template, 
  field = "eu_base_eur", 
  background = NA
)

# Mask out areas outside the biomass template
r_base_cost <- terra::mask(r_base_cost, r_template)

# Name the layer explicitly
names(r_base_cost) <- "eu_base_eur"

# Write the final raster to disk
out_path <- file.path(proc_dir, "europe_eu_base_eur.tif")
terra::writeRaster(r_base_cost, out_path, overwrite = TRUE)
message("  -> Saved: ", basename(out_path))

message("=== Europe Feedstock Processing Complete ===")

==> BiocharAG/data-raw/process_fperm.R <==
## data-raw/process_fperm.R

# Read the CSV data
fperm_data <- read.csv("data-raw/FpermData.csv", stringsAsFactors = FALSE)

# clean up or process if necessary (e.g. check types)
# For now, just save it as is.

# Save to data/ directory as internal package data
usethis::use_data(fperm_data, overwrite = TRUE)

==> BiocharAG/data-raw/process_india.R <==
# Process Spatial Data for India (North-West)
# Comparison Scenario for BiocharAG

library(terra)

# 1. Define Paths
gis_proc <- "../GIS/processed"
gis_raw <- "../GIS/raw"
soil_raw <- "../GIS/raw/soilgrids/files.isric.org/soilgrids/latest/data"

# 2. Define Extent: Whole India
# Approx: 6N - 37N, 68E - 98E
e_india <- terra::ext(68, 98, 6, 38)

message("Processing India Region (Whole Country): ", e_india)

# 3. Process Biomass (Global Crop Residue)
bm_global_path <- file.path(gis_raw, "res_avail.tif")
if (!file.exists(bm_global_path)) stop("Biomass map not found: ", bm_global_path)

message("Loading Global Biomass...")
r_bm_global <- terra::rast(bm_global_path)
r_india_bm <- terra::crop(r_bm_global, e_india)

# Resample to coarser resolution for Interactive Demo Performance
# Target resolution: 0.1 degrees (~10km)
target_res <- 0.1
message("Resampling to ", target_res, " degree resolution...")
r_template <- terra::rast(e_india, res = target_res)
terra::crs(r_template) <- terra::crs(r_bm_global)

r_india_bm <- terra::resample(r_india_bm, r_template, method = "bilinear")

# Resample to match SoilGrids resolution (approx 0.0025 deg)?
# Or stick to Biomass resolution?
# SoilGrids is 250m. Beccs optimization is fast enough for fine resolution.
# Let's target the Biomass resolution or 1km to save demo time?
# Let's keep the native biomass resolution for now.

names(r_india_bm) <- "biomass_density"
# Handle Units: Dataverse dataset often Mg C / km2 or Mg / ha?
# User said "global extent". Assuming it is Mg/km2 or similar.
# We'll treat values as "Mg/km2" (feedstock density) for now.
# If values are too low/high we will know in the app.

out_bm <- file.path(gis_proc, "india_biomass.tif")
terra::writeRaster(r_india_bm, out_bm, overwrite = TRUE)
message("Saved: ", out_bm)

# Define Template from Biomass
template <- r_india_bm

# 4. Process Soil Layers (SoilGrids)
process_sg <- function(vrt_path, name, scaler = 0.1) {
    if (!file.exists(vrt_path)) {
        message("Skipping missing VRT: ", vrt_path)
        return(NULL)
    }
    r_vrt <- terra::rast(vrt_path)
    # Project to template (handles cropping and resolution)
    r_out <- terra::project(r_vrt, template)
    r_out <- r_out * scaler
    names(r_out) <- name

    out_p <- file.path(gis_proc, paste0("india_", name, ".tif"))
    terra::writeRaster(r_out, out_p, overwrite = TRUE)
    message("Saved: ", out_p)
}

cec_vrt <- file.path(soil_raw, "cec/cec_0-5cm_mean.vrt")
ph_vrt <- file.path(soil_raw, "phh2o/phh2o_0-5cm_mean.vrt")

process_sg(cec_vrt, "soil_cec", 0.1)
process_sg(ph_vrt, "soil_ph", 0.1)

# 5. Process Soil Temp (WorldClim / SBIO1)
temp_path <- file.path(gis_raw, "SBIO1_0_5cm_Annual_Mean_Temperature.tif")
if (file.exists(temp_path)) {
    r_temp <- terra::rast(temp_path)
    r_india_temp <- terra::project(r_temp, template)
    names(r_india_temp) <- "soil_temp"
    terra::writeRaster(r_india_temp, file.path(gis_proc, "india_soil_temp.tif"), overwrite = TRUE)
    message("Saved: india_soil_temp.tif")
} else {
    # Fallback: Constant 25C (Warm)
    r_india_temp <- terra::rast(template)
    values(r_india_temp) <- 25
    names(r_india_temp) <- "soil_temp"
    terra::writeRaster(r_india_temp, file.path(gis_proc, "india_soil_temp.tif"), overwrite = TRUE)
}

# 6. Generate APPC Electricity Price Layer
# Constant ~ $0.06/kWh = 60 $/MWh
# But parameters_india has this value, spatial_tea expects a map.
r_india_elec <- terra::rast(template)
values(r_india_elec) <- 60
names(r_india_elec) <- "elec_price"
terra::writeRaster(r_india_elec, file.path(gis_proc, "india_elec_price.tif"), overwrite = TRUE)
message("Saved: india_elec_price.tif")

message("India Data Processing Complete.")

==> BiocharAG/data-raw/process_local_spatial.R <==
# data-raw/process_local_spatial.R
library(terra)
library(sf)

# 1. Paths
# Input: Raw data in Root/GIS/raw/
# Output: Processed data in Root/GIS/processed/

raw_dir <- "../GIS/raw"
proc_dir <- "../GIS/processed"

if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)

# File names
biomass_file <- file.path(raw_dir, "res_avail.tif")
temp_file <- file.path(raw_dir, "SBIO1_0_5cm_Annual_Mean_Temperature.tif")

# 2. Check Exists
if (!file.exists(biomass_file) || !file.exists(temp_file)) {
    stop("Input files not found in ", raw_dir)
}

# 1. Prepare Template
message("Processing Layers from Local Source...")

# Create Template Grid (US Extent, ~20km ~ 0.2 deg)
# Approx US BBox: -125, 24, -66, 50
us_extent <- ext(-125, -66, 24, 50)
us_template <- rast(us_extent, res = 0.2, crs = "EPSG:4326")

processed_layers <- list()

# 2. Process Biomass
message("Processing Biomass...")
r_bm <- rast(biomass_file)
r_bm_us <- project(r_bm, us_template)

v_max <- global(r_bm_us, "max", na.rm = TRUE)$max
message("Biomass Max Value: ", v_max)

# Unit Conversion Logic (kg/ha or t/ha -> Mg/km2)
if (v_max > 10000 && v_max < 50000) {
    # Assuming kg/ha
    # 1 kg/ha = 0.1 Mg/km2
    r_bm_us <- r_bm_us * 0.1
    message("Converted kg/ha to Mg/km2 (Factor 0.1)")
} else if (v_max < 50) {
    # Assuming t/ha
    # 1 t/ha = 100 Mg/km2
    r_bm_us <- r_bm_us * 100
    message("Converted t/ha to Mg/km2 (Factor 100)")
}

names(r_bm_us) <- "biomass_density"
processed_layers$biomass_density <- r_bm_us

# 3. Process Soil Temp
message("Processing Soil Temp...")
r_st <- rast(temp_file)
r_st_us <- project(r_st, us_template)

v_mM <- minmax(r_st_us)
message("Soil Temp Range: ", v_mM[1], " - ", v_mM[2])

# Unit Check (x10 scaling common in bioclim)
if (v_mM[2] > 60) {
    r_st_us <- r_st_us / 10
    message("Dividing Soil Temp by 10 (Deci-degrees correction)")
}

names(r_st_us) <- "soil_temp"
processed_layers$soil_temp <- r_st_us

# 4. Save
# save(processed_layers, file = "data/spatial_demo_layers.rda") # Optional

# Export Tifs for quick check
terra::writeRaster(processed_layers$biomass_density, file.path(proc_dir, "demo_biomass.tif"), overwrite = TRUE)
terra::writeRaster(processed_layers$soil_temp, file.path(proc_dir, "demo_soil_temp.tif"), overwrite = TRUE)

message("Done. Layers saved to ", proc_dir)

==> BiocharAG/data-raw/process_soilgrids.R <==
# Process SoilGrids VRTs (CEC, pH) to match Project Grid
#
# Inputs:
# - GIS/processed/us_biomass.tif (Template)
# - GIS/raw/soilgrids/.../cec_0-5cm_mean.vrt
# - GIS/raw/soilgrids/.../phh2o_0-5cm_mean.vrt
#
# Outputs:
# - GIS/processed/soil_cec.tif (cmol/kg)
# - GIS/processed/soil_ph.tif (pH)

library(terra)

# 1. Define Paths
gis_proc <- "../GIS/processed"
gis_raw <- "../GIS/raw/soilgrids/files.isric.org/soilgrids/latest/data"

cec_vrt <- file.path(gis_raw, "cec/cec_0-5cm_mean.vrt")
ph_vrt <- file.path(gis_raw, "phh2o/phh2o_0-5cm_mean.vrt")

if (!file.exists(cec_vrt)) stop("CEC VRT not found at: ", cec_vrt)
if (!file.exists(ph_vrt)) stop("pH VRT not found at: ", ph_vrt)

regions <- c("us", "china", "india", "europe")

process_and_save <- function(r_in, r_template, out_name, scale_factor = 0.1, var_name) {
    message("  Processing ", var_name, "...")
    message("   - Projecting and Resampling (average)...")
    r_out <- terra::project(r_in, r_template, method = "average")
    message("   - Scaling units (x", scale_factor, ")...")
    r_out <- r_out * scale_factor
    r_out <- terra::mask(r_out, r_template)
    names(r_out) <- var_name
    
    out_path <- file.path(gis_proc, out_name)
    message("   - Saving to ", out_path)
    terra::writeRaster(r_out, out_path, overwrite = TRUE, gdal = c("COMPRESS=ZSTD", "PREDICTOR=2"))
    return(r_out)
}

# Load VRTs once
r_cec_raw <- terra::rast(cec_vrt)
r_ph_raw <- terra::rast(ph_vrt)

for (region in regions) {
    message("\n=== Processing SoilGrids for Region: ", toupper(region), " ===")
    template_path <- file.path(gis_proc, paste0(region, "_biomass.tif"))
    
    if (!file.exists(template_path)) {
        warning("Template raster missing for ", region, ": ", template_path, ". Skipping.")
        next
    }
    
    r_template <- terra::rast(template_path)
    message("  Template loaded: ", paste(dim(r_template), collapse = "x"), " | CRS: ", crs(r_template, proj = TRUE))
    
    # CEC: mmol(c)/kg -> cmol(+)/kg. Factor = 0.1
    process_and_save(r_cec_raw, r_template, paste0(region, "_soil_cec.tif"), 0.1, "soil_cec")
    
    # pH: pH*10 -> pH. Factor = 0.1
    process_and_save(r_ph_raw, r_template, paste0(region, "_soil_ph.tif"), 0.1, "soil_ph")
}

message("\nProcessing Complete for all regions.")

==> BiocharAG/data-raw/process_transport_layers.R <==
# nolint start: indentation_linter, line_length_linter, object_usage_linter, commented_code_linter
library(terra)
library(sf)
library(dplyr)
library(geodata)

# ==============================================================================
# Setup: Load Sinks and Define Regions
# ==============================================================================

# 1. Load the Sinks (generated in Step 1)
if (!exists("co2_sinks")) {
    source("data-raw/generate_sinks.R")
}

# 2. Define File Paths
raw_dir <- "../GIS/raw"
proc_dir <- "../GIS/processed"

if (!dir.exists(raw_dir)) dir.create(raw_dir, recursive = TRUE)
if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)

#' @importFrom rlang .data
utils::globalVariables(c("co2_sinks"))

# ==============================================================================
# Helper Function: Process Region
# ==============================================================================
#' Generate Transport Layers for a Region (Terrain-Optimized)
#' @param region_name String matching the 'Region' column in co2_sinks
#' @param template_path Path to the biomass template raster for this region
#' @param file_prefix Prefix for output files (e.g., "us")
#' @param hires_factor Factor by which to disaggregate the grid for
#' high-res routing.
#' e.g. fact=10 turns a 20km grid into a 2km grid. 1 = Native resolution.
#' @param pa_cost_multiplier Multiplier for WDPA Protected Areas.
#' NA = absolute barrier.
process_transport_layers <- function(region_name, template_path, file_prefix,
                                     hires_factor = 10,
                                     pa_cost_multiplier = NA) {
    message(paste0("\n=== Processing Terrain-Optimized Transport for: ", region_name, " ==="))

    # 1. Load Template
    if (!file.exists(template_path)) {
        warning(paste("Template not found:", template_path, "- Skipping."))
        return(NULL)
    }
    r_template <- terra::rast(template_path)

    # 2. Filter Sinks
    sinks_sub <- co2_sinks |> dplyr::filter(.data$Region == region_name)
    if (nrow(sinks_sub) == 0) {
        warning("No sinks found for this region in co2_sinks database.")
        return(NULL)
    }
    message(paste0("Found ", nrow(sinks_sub), " sinks."))

    # 2.5 WDPA Protected Areas
    pa_clean <- NULL
    if (hires_factor > 1) {
        message("Downloading/loading WDPA Protected Areas...")
        wdpa_dir <- file.path(raw_dir, "WDPA")
        zip_files <- list.files(wdpa_dir, pattern = "\\.zip$", full.names = TRUE)

        if (length(zip_files) > 0) {
            # Use the most recent zip file if there are multiple
            zip_file <- zip_files[which.max(file.info(zip_files)$mtime)]
            message("Loading local WDPA database from: ", zip_file)

            tryCatch(
                {
                    # Construct GDAL virtual file system path for the geodatabase inside the zip
                    gdb_name <- gsub("\\.zip$", ".gdb", basename(zip_file))
                    vsi_path <- paste0("/vsizip/", zip_file, "/", gdb_name)

                    # Find the polygon layer automatically (usually WDPA_poly_MonYYYY)
                    layers <- sf::st_layers(vsi_path)$name
                    poly_layer <- layers[grepl("poly", layers, ignore.case = TRUE)][1]

                    # Ensure the bbox matches the CRS of the WDPA database (EPSG:4326) for on-the-fly cropping
                    message("Cropping WDPA polygons directly from disk (this is extremely fast)...")
                    e_poly <- sf::st_as_sfc(sf::st_bbox(r_template))
                    e_poly_wgs84 <- sf::st_transform(e_poly, 4326)

                    # Read only the polygons within the bounding box
                    pa_raw_cropped <- sf::st_read(vsi_path,
                        layer = poly_layer,
                        wkt_filter = sf::st_as_text(e_poly_wgs84),
                        quiet = TRUE
                    )

                    # Project back to our native template CRS
                    pa_raw_cropped <- sf::st_transform(pa_raw_cropped, terra::crs(r_template))

                    message("Cleaning cropped WDPA data (this may take a minute)...")
                    if (requireNamespace("wdpar", quietly = TRUE)) {
                        # S2 geometry engine frequently throws topology errors on messy WDPA polygons
                        # Disable it temporarily and rely on GEOS planar geometry instead
                        old_s2 <- sf::sf_use_s2()
                        sf::sf_use_s2(FALSE)

                        pa_clean <- tryCatch({
                            # Avoid 24+ hour execution time by skipping erase_overlaps (not needed for rasterization)
                            # Pass the template CRS to prevent wdpa_clean from defaulting back to ESRI:54017
                            wdpar::wdpa_clean(pa_raw_cropped,
                                crs = terra::crs(r_template),
                                erase_overlaps = FALSE
                            )
                        }, finally = {
                            sf::sf_use_s2(old_s2)
                        })
                    } else {
                        warning("wdpar package not installed. Using simple st_make_valid.")
                        pa_clean <- sf::st_make_valid(pa_raw_cropped)
                    }
                },
                error = function(e) {
                    warning("Failed to load local WDPA data: ", e$message)
                }
            )
        } else {
            warning(paste("No WDPA zip files found in", wdpa_dir, "- Skipping PA integration."))
        }
    }

    # 3. Create High-Resolution Base Grid
    # --------------------------------------------------------------------------
    if (hires_factor > 1) {
        message(paste0("Disaggregating grid by factor of ", hires_factor, " for micro-routing..."))
        r_base <- terra::disagg(r_template, fact = hires_factor)
    } else {
        r_base <- r_template
    }

    # 4. Generate Topographic Friction
    # --------------------------------------------------------------------------
    # 4. Generate Topographic Friction (High-Res to Low-Res)
    # --------------------------------------------------------------------------
    message("Fetching Global DEM and computing slope...")
    r_dem <- geodata::elevation_global(res = 0.5, path = raw_dir) # Pull higher res if possible

    # Calculate Slope at native resolution FIRST
    r_slope_hires <- terra::terrain(r_dem, v = "slope", unit = "degrees")

    # Aggregate to working grid using the Percolation Threshold (95th percentile)
    # Use 95th percentile to capture linear barriers
    message("Aggregating slope via 95th percentile threshold. This may take some time...")
    r_slope_10km <- terra::aggregate(r_slope_hires,
        fact = hires_factor,
        fun = function(x) quantile(x, probs = 0.95, na.rm = TRUE)
    )

    # Project to working grid
    r_slope_proj <- terra::project(r_slope_10km, r_base)

    # Apply Exponential Cost Function
    # M(theta) = exp(0.25 * theta)
    friction_surface <- exp(0.25 * r_slope_proj)
    friction_surface <- terra::subst(friction_surface, NA, 5) # Handle water/NAs safely

    # --- DEBUG SAVES ---
    terra::writeRaster(r_dem, file.path(proc_dir, paste0(file_prefix, "_debug_dem.tif")), overwrite = TRUE)
    terra::writeRaster(r_slope_hires, file.path(proc_dir, paste0(file_prefix, "_debug_slope_raw.tif")), overwrite = TRUE)
    terra::writeRaster(r_slope_proj, file.path(proc_dir, paste0(file_prefix, "_debug_slope.tif")), overwrite = TRUE)
    terra::writeRaster(r_slope_proj, file.path(proc_dir, paste0(file_prefix, "_debug_slope_percolation.tif")), overwrite = TRUE)
    terra::writeRaster(friction_surface, file.path(proc_dir, paste0(file_prefix, "_debug_friction_pre_pa.tif")), overwrite = TRUE)
    # -------------------

    # 5. Apply Protected Areas Penalty
    # --------------------------------------------------------------------------
    if (!is.null(pa_clean) && nrow(pa_clean) > 0) {
        message("Rasterizing Protected Areas to friction surface...")

        # Crop PA to the raster extent to save memory during rasterize
        pa_cropped <- sf::st_crop(pa_clean, terra::ext(r_base))

        if (nrow(pa_cropped) > 0) {
            r_pa <- terra::rasterize(terra::vect(pa_cropped), friction_surface, field = 1, background = 0)

            if (is.na(pa_cost_multiplier)) {
                # Absolute barrier
                friction_surface <- terra::ifel(r_pa == 1, NA, friction_surface)
            } else {
                # Cost multiplier
                friction_surface <- terra::ifel(r_pa == 1, friction_surface * pa_cost_multiplier, friction_surface)
            }
            # --- DEBUG SAVES ---
            terra::writeRaster(r_pa, file.path(proc_dir, paste0(file_prefix, "_debug_pa.tif")), overwrite = TRUE)
            terra::writeRaster(friction_surface, file.path(proc_dir, paste0(file_prefix, "_debug_friction_post_pa.tif")), overwrite = TRUE)
            # -------------------
        }
    }

    # 6. Calculate Cost-Distance to EACH Sink Individually
    # --------------------------------------------------------------------------
    message("Calculating Least Cost Paths to individual sinks...")

    dist_list <- list()
    for (i in seq_len(nrow(sinks_sub))) {
        sink_pt <- sinks_sub[i, ]
        sink_coords <- sf::st_coordinates(sink_pt)
        sink_cell <- terra::cellFromXY(friction_surface, sink_coords)

        # If sink is slightly outside the bounding box, snap it to the nearest edge
        if (is.na(sink_cell)) {
            e <- terra::ext(friction_surface)
            x <- max(min(sink_coords[1], e$xmax), e$xmin)
            y <- max(min(sink_coords[2], e$ymax), e$ymin)
            sink_cell <- terra::cellFromXY(friction_surface, cbind(x, y))
        }

        # terra::costDist calculates distance to a target VALUE in the raster
        fs <- friction_surface
        fs[sink_cell] <- 0
        cost_m <- terra::costDist(fs, target = 0)

        # Convert to Kilometers
        cost_km <- cost_m / 1000

        # Aggregate back to native resolution if we used hires
        if (hires_factor > 1) {
            cost_km <- terra::aggregate(cost_km, fact = hires_factor, fun = mean, na.rm = TRUE)
            # Ensure perfect alignment
            cost_km <- terra::resample(cost_km, r_template, method = "bilinear")
        }

        dist_list[[i]] <- cost_km
    }

    # 7. Stack and Determine Winners
    # --------------------------------------------------------------------------
    dist_stack <- terra::rast(dist_list)
    names(dist_stack) <- sinks_sub$Basin_Name

    message("Determining optimal routing and sink allocations...")

    # Minimum cost-distance across all sinks
    r_min_dist <- terra::app(dist_stack, min, na.rm = TRUE)
    names(r_min_dist) <- "dist_sink_km"

    # Index of the winning sink
    r_winner_idx <- terra::app(dist_stack, which.min)

    # 8. Map Sink Properties based on the Winner
    # --------------------------------------------------------------------------
    # Initialize rasters for properties
    r_offshore_flag <- terra::rast(r_template)
    terra::values(r_offshore_flag) <- NA # Default NA
    r_saline_dist <- terra::rast(r_template)
    terra::values(r_saline_dist) <- NA # Default NA

    # Extract properties from the vector database
    is_offshore_vec <- ifelse(sinks_sub$Type == "Offshore", 1, 0)
    is_eor_vec <- sinks_sub$Is_EOR

    # Loop over the indices to populate the property maps
    for (i in seq_len(nrow(sinks_sub))) {
        mask_winner <- (r_winner_idx == i)

        # Assign Offshore Flag
        r_offshore_flag[mask_winner] <- is_offshore_vec[i]

        # Determine Saline Distance
        if (is_eor_vec[i] == FALSE) {
            r_saline_dist[mask_winner] <- r_min_dist[mask_winner]
        }
    }

    names(r_offshore_flag) <- "sink_is_offshore"

    # 9. Handle Saline routing for regions where EOR won
    # --------------------------------------------------------------------------
    saline_indices <- which(sinks_sub$Is_EOR == FALSE)
    if (length(saline_indices) > 0) {
        saline_stack <- dist_stack[[saline_indices]]
        r_min_saline <- terra::app(saline_stack, min, na.rm = TRUE)

        # Fill in the NA gaps in r_saline_dist where EOR won the primary routing
        mask_needs_saline <- is.na(r_saline_dist)
        r_saline_dist[mask_needs_saline] <- r_min_saline[mask_needs_saline]
    }
    names(r_saline_dist) <- "dist_sink_saline_km"

    # ==============================================================================
    # Final Output
    # ==============================================================================
    out_dist <- file.path(proc_dir, paste0(file_prefix, "_dist_sink.tif"))
    out_dist_saline <- file.path(proc_dir, paste0(file_prefix, "_dist_sink_saline.tif"))
    out_type <- file.path(proc_dir, paste0(file_prefix, "_sink_type.tif"))

    terra::writeRaster(r_min_dist, out_dist, overwrite = TRUE)
    terra::writeRaster(r_saline_dist, out_dist_saline, overwrite = TRUE)
    terra::writeRaster(r_offshore_flag, out_type, overwrite = TRUE)

    message(paste("Saved:", out_dist))
    list(dist = r_min_dist, dist_saline = r_saline_dist, type = r_offshore_flag)
}

# ==============================================================================
# Execution: Run for All Study Areas
# ==============================================================================

# 1. United States (Coterminous)
# Note: PA fetch and calculation might take ~10-15 minutes first time for USA due to WDPA size
process_transport_layers(
    region_name = "North America",
    template_path = "../GIS/processed/us_biomass.tif",
    file_prefix = "us",
    hires_factor = 10,
    pa_cost_multiplier = NA
)

# 2. China
process_transport_layers(
    region_name = "China",
    template_path = "../GIS/processed/china_biomass.tif",
    file_prefix = "china",
    hires_factor = 10,
    pa_cost_multiplier = NA
)

# 3. India
process_transport_layers(
    region_name = "India",
    template_path = "../GIS/processed/india_biomass.tif",
    file_prefix = "india",
    hires_factor = 10,
    pa_cost_multiplier = NA
)

# 4. Europe
process_transport_layers(
    region_name = "Europe",
    template_path = "../GIS/processed/europe_biomass.tif",
    file_prefix = "europe",
    hires_factor = 10,
    pa_cost_multiplier = NA
)

==> BiocharAG/data-raw/process_us_feedstock.R <==
library(terra)
library(sf)
library(dplyr)
library(tigris) # For fetching US county boundaries

# ==============================================================================
# Setup: Define Paths and Load Template
# ==============================================================================
raw_dir <- "../GIS/raw"
proc_dir <- "../GIS/processed"

if (!dir.exists(raw_dir)) dir.create(raw_dir, recursive = TRUE)
if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)

message("Loading US Biomass Template...")
template_path <- file.path(proc_dir, "us_biomass.tif")
if (!file.exists(template_path)) stop("Template raster not found.")
r_template <- terra::rast(template_path)

# ==============================================================================
# 1. Prepare Base County Geometries
# ==============================================================================
message("Fetching US County Boundaries...")
# Fetch US counties at 1:20 million scale for speed, omitting non-CONUS territories
us_counties <- tigris::counties(cb = TRUE, resolution = "20m", class = "sf") %>%
    dplyr::filter(!(STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")))

# Project counties to match the raster template (EPSG:5070 for US Equal Area)
us_counties_proj <- sf::st_transform(us_counties, terra::crs(r_template))

# Create a unique ID for joining (GEOID is standard: State FIPS + County FIPS)
# Ensure area is calculated for density metrics
us_counties_proj$area_km2 <- as.numeric(sf::st_area(us_counties_proj)) / 1e6

# ==============================================================================
# 2. Process Cattle Density (Opportunity Cost Trigger)
# ==============================================================================
message("Processing Cattle Density Mask...")

# NOTE: Download the USDA NASS Census data for "CATTLE, INCL CALVES - INVENTORY"
# Save it to GIS/raw/nass_cattle_inventory.csv
cattle_csv_path <- file.path(raw_dir, "nass_cattle_inventory.csv")

if (file.exists(cattle_csv_path)) {
    cattle_data <- read.csv(cattle_csv_path, stringsAsFactors = FALSE) %>%
        # Ensure GEOID matches tigris (string, 5 chars, leading zeros)
        dplyr::mutate(GEOID = sprintf("%02d%03d", State.ANSI, County.ANSI)) %>%
        dplyr::select(GEOID, inventory = Value) %>%
        # Clean numeric formatting (remove commas)
        dplyr::mutate(inventory = as.numeric(gsub(",", "", inventory)))
    
    # Join to spatial counties
    counties_cattle <- us_counties_proj %>%
        dplyr::left_join(cattle_data, by = "GEOID") %>%
        dplyr::mutate(
            inventory = ifelse(is.na(inventory), 0, inventory),
            density_head_per_km2 = inventory / area_km2
        )
    
    # Define threshold for "High Cattle" (e.g., top 25% of counties, or a hard limit like > 50 head/km2)
    threshold <- quantile(counties_cattle$density_head_per_km2, probs = 0.75, na.rm = TRUE)
    counties_cattle$is_high_cattle <- ifelse(counties_cattle$density_head_per_km2 >= threshold, 1, 0)
    
    # Rasterize
    r_high_cattle <- terra::rasterize(
        terra::vect(counties_cattle), 
        r_template, 
        field = "is_high_cattle", 
        background = 0
    )
    
    # Write to disk
    terra::writeRaster(r_high_cattle, file.path(proc_dir, "us_is_high_cattle.tif"), overwrite = TRUE)
    message("  -> Saved: us_is_high_cattle.tif")
} else {
    warning("Cattle inventory CSV not found. Skipping cattle density layer.")
}

# ==============================================================================
# 3. Process POLYSYS Baseline Cost
# ==============================================================================
message("Processing POLYSYS Baseline Costs...")

# NOTE: Download the Bioenergy KDF county-level supply curve data
# Save it to GIS/raw/polysys_stover_cost.csv
polysys_csv_path <- file.path(raw_dir, "polysys_stover_cost.csv")

if (file.exists(polysys_csv_path)) {
    cost_data <- read.csv(polysys_csv_path, stringsAsFactors = FALSE) %>%
        dplyr::mutate(GEOID = sprintf("%05d", FIPS)) %>%
        # Assuming you extract the baseline price column at your target supply volume
        # Rename your specific price column to 'base_cost_usd'
        dplyr::select(GEOID, base_cost_usd = Price_USD_per_Mg)
    
    # Join to spatial counties
    counties_cost <- us_counties_proj %>%
        dplyr::left_join(cost_data, by = "GEOID") %>%
        # Fill missing counties with the national default ($60) to prevent NAs from breaking the TEA
        dplyr::mutate(base_cost_usd = ifelse(is.na(base_cost_usd), 60.0, base_cost_usd))
    
    # Rasterize
    r_base_cost <- terra::rasterize(
        terra::vect(counties_cost), 
        r_template, 
        field = "base_cost_usd", 
        background = NA # Areas outside the US borders will be NA
    )
    
    # Mask out non-biomass pixels (oceans, lakes) using the template
    r_base_cost <- terra::mask(r_base_cost, r_template)
    
    # Write to disk
    terra::writeRaster(r_base_cost, file.path(proc_dir, "us_base_cost.tif"), overwrite = TRUE)
    message("  -> Saved: us_base_cost.tif")
} else {
    warning("POLYSYS cost CSV not found. Skipping baseline cost layer.")
}

message("=== US Feedstock Processing Complete ===")
