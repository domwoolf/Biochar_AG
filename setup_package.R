
if (!requireNamespace("usethis", quietly = TRUE)) install.packages("usethis", repos = "http://cran.us.r-project.org")
library(usethis)

# Create package in subdirectory
create_package("BiocharAG", open = FALSE)

setwd("BiocharAG")
# Add dependencies
use_package("readxl")
use_package("dplyr")
use_package("tidyr")

# Create structure
use_r("bes")
use_r("beccs")
use_r("bebcs")
use_r("npv")
use_r("parameters")

message("Package structure initialized in BiocharAG/.")
