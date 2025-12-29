# data-raw/download_and_process_spatial.R
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

    # Biomass Unit Logic
    # Value ~15219. Assuming kg/ha.
    # Goal: Mg/km2.
    # 1 kg/ha = 0.001 Mg / 0.01 km2 = 0.1 Mg/km2.
    # Factor = 0.1
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
    v_mM <- minmax(r_st_us)
    message("Soil Temp Range: ", v_mM[1], " - ", v_mM[2])

    # If range is 0 - 300, divide by 10.
    if (v_mM[2] > 60) {
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
