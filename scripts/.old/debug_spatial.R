devtools::load_all(".")
source("../figures/generate_manuscript_figures.R")
dat <- load_region_data("US")

params <- BiocharAG::default_parameters()
params$c_price <- 150
params$region <- "US"
params$bc_valuation_method <- "advanced_mechanistic"

tryCatch(run_scenario(dat$template, dat$layers, params), error = function(e) { print(e); traceback(); quit(status=1) })
