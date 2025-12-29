# replicate_figure1.R
library(ggplot2)
library(dplyr)
devtools::load_all(".")

params <- default_parameters()

# Woolf Fig 1 usually shows range 0 to ~150 or 200 $/tCO2e
# We'll run 0 to 300
c_prices <- seq(0, 300, by = 10)

cat("Running simulation for Figure 1 (RPV vs Carbon Price)...\n")
p <- plot_rpv_vs_c_price(params, c_price_range = c_prices, metric = "RPV")

# Add title indicating update
p <- p + labs(subtitle = "Updated with Fperm Optimization & Sophisticated BEBCS Physics")

ggsave("figure1_repro.png", plot = p, width = 8, height = 6)

print("Figure 1 saved to figure1_repro.png")
