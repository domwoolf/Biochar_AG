heat <- if (file.exists("results/ai_summaries/heat_sensitivity_quantiles.csv")) fread("results/ai_summaries/heat_sensitivity_quantiles.csv") else NULL
hd <- function(r) heat[region == r & c_price == 100, delta_p50]
# Residue on which PyCCS is profitable without a carbon price at the 2% social discount rate (Tg yr-1)
soc0_tg <- ev[region %in% c("China", "India") & dr == "social" & cp == 0 & tech == "PyCCS" & viable == TRUE, sum(biomass_Mg)] / 1e6
