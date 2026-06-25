library(ggplot2)
devtools::load_all(".")

params <- default_parameters()

# Generate Plot
p <- plot_rpv_vs_c_price(params, c_price_range = seq(0, 3000, 20), metric = "RPV")

# Save to file
ggsave("figure1_repro.png", plot = p, width = 8, height = 6)

print("Figure 1 saved to figure1_repro.png")
