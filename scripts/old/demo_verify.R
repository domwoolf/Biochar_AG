# Demo / Verification Script for RPV and Soil Fertility

library(readxl)
library(dplyr)
library(tidyr)

# Source all R files
files <- list.files("R", pattern = "*.R", full.names = TRUE)
sapply(files, source)

# Load Parameters
params <- default_parameters()

# Run Models
bes_res <- calculate_bes(params)
beccs_res <- calculate_beccs(params)
bebcs_res <- calculate_bebcs(params)

# Comparison / RPV
results_list <- list(bes_res, beccs_res, bebcs_res)
rpv_table <- calculate_rpv(results_list)

print("--- Parameter Check ---")
print(paste("Discount Rate:", params$discount_rate))
print(paste("Biochar Price:", params$bc_price))
print(paste("Biochar Ag Value (Annual):", params$bc_ag_value))

print("--- BES Results ---")
print(bes_res$net_value)

print("--- BECCS Results ---")
print(beccs_res$net_value)

print("--- BEBCS Results ---")
print(paste("Net Value:", bebcs_res$net_value))
print(paste("Nbcf (Soil Fertility NPV):", bebcs_res$nbcf))
print(paste("Biochar Yield:", bebcs_res$bc_yield))

print("--- RPV Comparison ---")
print(rpv_table)
