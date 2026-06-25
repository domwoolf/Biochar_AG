
# Demo run for BiocharAG
setwd("/media/dominic/Data/git/Biochar_AG/BiocharAG")

# Load the package functions (simulating load_all)
pkg_dir <- "."
files <- list.files(file.path(pkg_dir, "R"), full.names = TRUE)
sapply(files, source)

# Load default parameters
params <- default_parameters()

# Calculate BES
print("--- BES Results ---")
bes_res <- calculate_bes(params)
print(bes_res)

# Calculate BECCS
print("\n--- BECCS Results ---")
beccs_res <- calculate_beccs(params)
print(beccs_res)

# Calculate BEBCS
print("\n--- BEBCS Results ---")
bebcs_res <- calculate_bebcs(params)
print(bebcs_res)


# Compare NPVs
print("\n--- Comparison ---")
print(paste("BES NPV:", bes_res$net_value))
print(paste("BECCS NPV:", beccs_res$net_value))
print(paste("BEBCS NPV:", bebcs_res$net_value))
