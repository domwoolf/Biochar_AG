library(terra)
library(dplyr)

# Setup terra to use local temp directory to avoid sandbox permission errors
dir.create("/media/dominic/Data/git/Biochar_AG/BiocharAG/scratch/terra_temp", showWarnings = FALSE, recursive = TRUE)
terraOptions(tempdir = "/media/dominic/Data/git/Biochar_AG/BiocharAG/scratch/terra_temp")

# Load package
library(BiocharAG)

run_tea <- function(...) suppressMessages(BiocharAG::run_spatial_tea(...))

# Load data like generate_manuscript_figures.R
gis_path <- "/media/dominic/Data/git/Biochar_AG/GIS/processed/"
p_base <- "demo"
p_dist <- "us"

bm <- terra::rast(file.path(gis_path, paste0(p_base, "_biomass.tif")))
st <- terra::rast(file.path(gis_path, paste0(p_base, "_soil_temp.tif")))
ep <- terra::rast(file.path(gis_path, paste0(p_base, "_elec_price.tif")))
ds_onshore <- terra::rast(file.path(gis_path, paste0(p_dist, "_dist_onshore.tif")))
ds_offshore <- terra::rast(file.path(gis_path, paste0(p_dist, "_dist_offshore.tif")))
ph <- terra::rast(file.path(gis_path, paste0(p_base, "_soil_ph.tif")))
cec <- terra::rast(file.path(gis_path, paste0(p_base, "_soil_cec.tif")))

layers <- list(
    biomass_density = bm,
    soil_temp = st,
    elec_price = ep,
    dist_onshore = ds_onshore,
    dist_offshore = ds_offshore,
    soil_ph = ph,
    soil_cec = cec
)

template <- bm

# Scenario 1: exact generated script logic
# C_price = 150, dr = 8.5%
p_fig <- BiocharAG::default_parameters()
p_fig$c_price <- 150
p_fig$discount_rate <- 0.085 # let's use the shiny one to standardize
p_fig$region <- "US"
p_fig$bc_valuation_method <- "advanced_mechanistic"

bes_fig <- run_tea(template, p_fig, layers, fun = BiocharAG::calculate_bes) # uses default r=50
beccs_fig <- run_tea(template, p_fig, layers, fun = BiocharAG::calculate_beccs)
bebcs_fig <- run_tea(template, p_fig, layers, fun = BiocharAG::calculate_bebcs) # uses default r=50

net_fig <- c(bes_fig[["Net_Value_USD"]], beccs_fig[["Net_Value_USD"]], bebcs_fig[["Net_Value_USD"]])
opt_fig <- terra::app(net_fig, which.max)

cat("\n--- Figure Script Logic (r=50 for all, region='US') ---\n")
print(table(values(opt_fig)))

# Scenario 2: shiny script logic
p_shiny <- BiocharAG::default_parameters()
p_shiny$c_price <- 150
p_shiny$discount_rate <- 0.085
p_shiny$region <- "North America"
p_shiny$bc_valuation_method <- "advanced_mechanistic"

bes_shiny <- run_tea(template, p_shiny, layers, fun = BiocharAG::calculate_bes, collection_radius_km = 50)
beccs_shiny <- run_tea(template, p_shiny, layers, fun = BiocharAG::calculate_beccs, collection_radius_km = 50)
bebcs_shiny <- run_tea(template, p_shiny, layers, fun = BiocharAG::calculate_bebcs, collection_radius_km = 40)

net_shiny <- c(bes_shiny[["Net_Value_USD"]], beccs_shiny[["Net_Value_USD"]], bebcs_shiny[["Net_Value_USD"]])
opt_shiny <- terra::app(net_shiny, which.max)

cat("\n--- Shiny Script Logic (r=40 for BEBCS, region='North America') ---\n")
print(table(values(opt_shiny)))

# Scenario 3: Shiny with r=50 for BEBCS
bebcs_shiny_50 <- run_tea(template, p_shiny, layers, fun = BiocharAG::calculate_bebcs, collection_radius_km = 50)
net_shiny_50 <- c(bes_shiny[["Net_Value_USD"]], beccs_shiny[["Net_Value_USD"]], bebcs_shiny_50[["Net_Value_USD"]])
opt_shiny_50 <- terra::app(net_shiny_50, which.max)
cat("\n--- Shiny Logic but r=50 for BEBCS (Testing Radius Effect) ---\n")
print(table(values(opt_shiny_50)))

# Scenario 4: Shiny with r=40 for BEBCS, but region='US'
p_shiny_us <- p_shiny
p_shiny_us$region <- "US"
bes_shiny_us <- run_tea(template, p_shiny_us, layers, fun = BiocharAG::calculate_bes, collection_radius_km = 50)
beccs_shiny_us <- run_tea(template, p_shiny_us, layers, fun = BiocharAG::calculate_beccs, collection_radius_km = 50)
bebcs_shiny_us <- run_tea(template, p_shiny_us, layers, fun = BiocharAG::calculate_bebcs, collection_radius_km = 40)
net_shiny_us <- c(bes_shiny_us[["Net_Value_USD"]], beccs_shiny_us[["Net_Value_USD"]], bebcs_shiny_us[["Net_Value_USD"]])
opt_shiny_us <- terra::app(net_shiny_us, which.max)
cat("\n--- Shiny Logic but region='US' (Testing Region Effect) ---\n")
print(table(values(opt_shiny_us)))

# Scenario 5: Check data loading differences
if (file.exists(paste0(gis_path, "us_elec_price.tif"))) {
    ep_shiny <- terra::rast(paste0(gis_path, "us_elec_price.tif"))
} else {
    ep_shiny <- terra::rast(paste0(gis_path, "demo_elec_price.tif"))
}
cat("\n--- Elec Price Diff ---\n")
cat("Figure uses demo_elec_price.tif\n")
cat("Shiny uses us_elec_price.tif if it exists. Does it exist? ", file.exists(paste0(gis_path, "us_elec_price.tif")), "\n")

if (file.exists(paste0(gis_path, "us_elec_price.tif"))) {
    layers_shiny <- layers
    layers_shiny$elec_price <- ep_shiny
    
    bes_shiny_data <- run_tea(template, p_shiny, layers_shiny, fun = BiocharAG::calculate_bes, collection_radius_km = 50)
    beccs_shiny_data <- run_tea(template, p_shiny, layers_shiny, fun = BiocharAG::calculate_beccs, collection_radius_km = 50)
    bebcs_shiny_data <- run_tea(template, p_shiny, layers_shiny, fun = BiocharAG::calculate_bebcs, collection_radius_km = 40)
    
    net_shiny_data <- c(bes_shiny_data[["Net_Value_USD"]], beccs_shiny_data[["Net_Value_USD"]], bebcs_shiny_data[["Net_Value_USD"]])
    opt_shiny_data <- terra::app(net_shiny_data, which.max)
    cat("\n--- Shiny Logic + Shiny Data Loading (Testing Data Effect) ---\n")
    print(table(values(opt_shiny_data)))
}
