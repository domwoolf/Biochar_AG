## data-raw/process_fperm.R

# Read the CSV data
fperm_data <- read.csv("data-raw/FpermData.csv", stringsAsFactors = FALSE)

# clean up or process if necessary (e.g. check types)
# For now, just save it as is.

# Save to data/ directory as internal package data
usethis::use_data(fperm_data, overwrite = TRUE)
