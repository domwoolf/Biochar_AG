library(BiocharAG)

# 1. Base Case (Wood Chips: 1% Ash)
p_low <- default_parameters()
p_low$bm_ash <- 0.01

res_bes_low <- calculate_bes(p_low)
res_beccs_low <- calculate_beccs(p_low)
res_bebcs_low <- calculate_bebcs(p_low)

# 2. High Ash Case (Rice Straw: 10% Ash)
p_high <- default_parameters()
p_high$bm_ash <- 0.10

res_bes_high <- calculate_bes(p_high)
res_beccs_high <- calculate_beccs(p_high)
res_bebcs_high <- calculate_bebcs(p_high)

cat("---------- CAPEX Comparison (Low vs High Ash) ----------\n")
# Need to infer CAPEX from Total Cost or check internal params if we could inspect them
# Since functions return list, we can check Total Cost

cat(sprintf(
    "BES Total Cost: $%.2f (Low) vs $%.2f (High) -> Ratio: %.2f\n",
    res_bes_low$total_cost, res_bes_high$total_cost,
    res_bes_high$total_cost / res_bes_low$total_cost
))

cat(sprintf(
    "BECCS Total Cost: $%.2f (Low) vs $%.2f (High) -> Ratio: %.2f\n",
    res_beccs_low$total_cost, res_beccs_high$total_cost,
    res_beccs_high$total_cost / res_beccs_low$total_cost
))

cat(sprintf(
    "BEBCS Total Cost: $%.2f (Low) vs $%.2f (High) -> Ratio: %.2f\n",
    res_bebcs_low$total_cost, res_bebcs_high$total_cost,
    res_bebcs_high$total_cost / res_bebcs_low$total_cost
))

cat("\n---------- Net Value Comparison ----------\n")
cat(sprintf("BES Net Value: $%.2f (Low) vs $%.2f (High)\n", res_bes_low$net_value, res_bes_high$net_value))
cat(sprintf("BEBCS Net Value: $%.2f (Low) vs $%.2f (High)\n", res_bebcs_low$net_value, res_bebcs_high$net_value))

# Expectation:
# BES High Cost ratio should be > 1.25 (since OPEX is x1.5 and Efficiency drops revenue)
# BEBCS High Cost ratio should be SMALLER (closer to 1.0), because Pyrolysis part isn't penalized.
