# scripts/heat_sensitivity.R
# Heat sensitivity scenario (issue #106): BEBCS sells heat (heat-only block) instead of power.
# The scenario is conditional on year-round heat demand at the plant, which the model does not map, so
# results are reported cell by cell (maps and per-cell distributions) and are never aggregated to
# regional totals of biomass, area or abatement.
#
# Heat displaces the cleaner of a fossil boiler (heat_boiler_ci) and a grid heat pump (MEF(P) /
# heat_pump_cop), at the no-carbon heat price heat_price (regional values in parameters.csv).
#
# Outputs: results/heat_sensitivity/ (cell table, figures) and
# results/ai_summaries/heat_sensitivity_quantiles.csv (per-cell quantiles, not totals).

library(dplyr)
library(ggplot2)
library(patchwork)
library(terra)
library(sf)

sf::sf_use_s2(FALSE)
source("scripts/manuscript_figures.R") # load_all, TECH_COLORS, results_dir

OUT_DIR <- paste0(results_dir, "heat_sensitivity/")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
REGIONS <- c("US", "Europe", "China", "India")
C_PRICES <- c(50, 100, 200)
PLANT <- c(BES = 250, BECCS = 250, BEBCS = 250)
TECHS <- c("BES", "BECCS", "BEBCS")
CHINA_NOTE <- "The China panel includes all territory within the World Bank boundary of China, including both the PRC and the ROC."

cells <- list()
admin <- list()
for (r in REGIONS) {
  message("Region: ", r)
  d <- load_region_data(r)
  admin[[r]] <- d$admin0
  for (cp in C_PRICES) {
    run <- function(extra) {
      p <- BiocharAG::set_scenario(c(list(c_price = cp, plant_mw_th = PLANT), extra), region = r)
      run_scenario(d$template, d$layers, p, d$vec)$vec_res
    }
    pw <- run(list())
    ht <- run(list(bebcs_energy_mode = "heat"))
    cells[[length(cells) + 1]] <- data.frame(
      region = r, c_price = cp, x = d$vec$xy[, 1], y = d$vec$xy[, 2], biomass_density = d$vec$layers$biomass_density,
      bebcs_net_power = pw$net[, 3], bebcs_net_heat = ht$net[, 3],
      bebcs_abate_power = pw$abate[, 3], bebcs_abate_heat = ht$abate[, 3],
      opt_power = TECHS[pw$opt], opt_heat = TECHS[ht$opt]
    )
  }
}
cells <- bind_rows(cells) %>%
  mutate(delta_net = bebcs_net_heat - bebcs_net_power,
         outcome = case_when(
           opt_heat == "BEBCS" & opt_power != "BEBCS" ~ "BEBCS only with heat",
           TRUE ~ opt_heat
         ))
write.csv(cells, paste0(OUT_DIR, "heat_sensitivity_cells.csv"), row.names = FALSE)

# Per-cell quantiles of the change in BEBCS net value (not regional totals)
q <- cells %>%
  group_by(region, c_price) %>%
  summarize(delta_p10 = quantile(delta_net, 0.1, na.rm = TRUE), delta_p50 = median(delta_net, na.rm = TRUE),
            delta_p90 = quantile(delta_net, 0.9, na.rm = TRUE),
            abate_change_p50 = median(bebcs_abate_heat - bebcs_abate_power, na.rm = TRUE), .groups = "drop")
write.csv(q, paste0(results_dir, "ai_summaries/heat_sensitivity_quantiles.csv"), row.names = FALSE)
print(q)

# ---- Figures ------------------------------------------------------------------
region_panel <- function(df, r, fill_layer, scale_layer, subtitle = r) {
  p <- ggplot() + fill_layer(df)
  if (!is.null(admin[[r]])) p <- p + geom_sf(data = admin[[r]], fill = NA, color = "grey25", linewidth = 0.3)
  p + coord_sf(crs = 4326) + scale_layer + theme_void(base_size = 10) + labs(subtitle = subtitle) +
    theme(plot.subtitle = element_text(face = "bold", hjust = 0.5))
}
note <- function(p) p + plot_annotation(caption = CHINA_NOTE)

# 1. Change in BEBCS net value at 100 $/t. Sequential (one hue) because the change is positive in
# (almost) every cell; values outside the 2nd-98th percentile range are clamped.
d100 <- cells$delta_net[cells$c_price == 100]
lims <- c(min(0, quantile(d100, 0.02, na.rm = TRUE)), quantile(d100, 0.98, na.rm = TRUE))
delta_scale <- scale_fill_gradient(name = "Gain in BEBCS net value\nfrom heat ($/Mg biomass)", low = "#e0f3f0",
                                   high = "#01665e", limits = lims, oob = scales::squish)
p1 <- wrap_plots(lapply(REGIONS, function(r) region_panel(
  filter(cells, region == r, c_price == 100), r,
  function(d) geom_tile(data = d, aes(x = x, y = y, fill = delta_net)), delta_scale)), ncol = 2) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(paste0(OUT_DIR, "heat_delta_net_map.png"), note(p1), width = 11, height = 8.5, dpi = 300)

# 2. Optimal technology with heat, marking cells where heat makes BEBCS optimal
out_cols <- c(TECH_COLORS, "BEBCS only with heat" = "#98df8a")
out_scale <- scale_fill_manual(name = "Optimal technology\n(where heat demand exists)", values = out_cols,
                               limits = names(out_cols), drop = FALSE)
panels <- list()
for (cp in c(100, 200)) for (r in REGIONS) {
  panels[[paste(cp, r)]] <- region_panel(filter(cells, region == r, c_price == cp), r,
    function(d) geom_tile(data = d, aes(x = x, y = y, fill = outcome), show.legend = TRUE), out_scale,
    subtitle = sprintf("%s, %d $/t", r, cp))
}
p2 <- wrap_plots(panels, ncol = 4) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(paste0(OUT_DIR, "heat_optimal_technology_map.png"), note(p2), width = 16, height = 8, dpi = 300)

# 3. Per-cell distribution of the change in BEBCS net value
p3 <- ggplot(cells, aes(x = factor(c_price), y = delta_net)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.4) +
  geom_boxplot(outlier.shape = NA, width = 0.55, fill = "#c7eae5", color = "grey25", linewidth = 0.4) +
  facet_wrap(~ factor(region, REGIONS), nrow = 1) +
  coord_cartesian(ylim = c(min(0, quantile(cells$delta_net, 0.01, na.rm = TRUE)), quantile(cells$delta_net, 0.99, na.rm = TRUE))) +
  labs(x = "Carbon price ($/t CO2)", y = "Change in BEBCS net value,\nheat minus power ($/Mg biomass)") +
  theme_bw(base_size = 11) + theme(panel.grid.minor = element_blank(), strip.background = element_blank(),
                                   strip.text = element_text(face = "bold"))
ggsave(paste0(OUT_DIR, "heat_delta_net_distribution.png"), p3, width = 10, height = 3.8, dpi = 300)
message("Heat sensitivity complete: ", OUT_DIR)
