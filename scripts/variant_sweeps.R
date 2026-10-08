# scripts/variant_sweeps.R
# Sensitivity of the marginal abatement cost curves to structural assumptions that the Monte Carlo
# analysis holds fixed: plant size (50 and 250 MWth around the 125 MWth default), dedicated pipelines
# instead of shared trunklines (early adoption), and CO2-EOR allowed. Each variant runs the same
# carbon-price sweep as the main MACC (run_price_sweep), at the regional discount rates and regional
# parameters, and aggregates the abatement and biomass of the technology with the highest non-negative
# net value in each grid cell. Replaces the earlier full-factorial run.
#
# Plant size is a design assumption, not an optimised quantity: optimising it per cell tends towards the
# largest size considered, because the model omits the local constraints that limit plant size in
# practice (road traffic and nuisance from continuous truck deliveries, local infrastructure). The size
# variants therefore show the sensitivity to this assumption, not a recommended size.
#
# Outputs: results/variants/variant_macc_data.csv, results/figures/Variants_MACC.png and
# results/ai_summaries/variant_summary.csv.

library(dplyr)
library(ggplot2)

source("scripts/manuscript_figures.R") # load_all, TECH_COLORS, results_dir, out_dir, ai_dir

REGIONS <- c("US", "China", "Europe", "India")
C_PRICES <- seq(0, 250, by = 1)
VARIANTS <- list(
  "Default (125 MW\u209c\u2095)" = list(),
  "50 MW\u209c\u2095 plants" = list(plant_mw_th = 50),
  "250 MW\u209c\u2095 plants" = list(plant_mw_th = 250),
  "Dedicated pipelines" = list(early_adoption = TRUE),
  "CO\u2082-EOR allowed" = list(allow_eor = TRUE)
)
var_dir <- paste0(results_dir, "variants/")
dir.create(var_dir, showWarnings = FALSE, recursive = TRUE)

# Biomass, area and abatement of the technology with the highest non-negative net value, by price
macc_table <- function(sweep, dat) {
  keep <- stats::complete.cases(sweep$n0, sweep$abate[, , 1])
  bm <- dat$vec$layers$biomass_density[keep] * dat$vec$cell_area[keep]
  do.call(rbind, lapply(C_PRICES, function(cp) {
    a <- sweep_abate(sweep, cp)[keep, , drop = FALSE]
    val <- sweep_n0(sweep, cp)[keep, , drop = FALSE] + cp * a
    best <- max.col(val, ties.method = "first")
    adopted <- apply(val, 1, max) >= 0
    data.frame(Price = cp, Technology = c("BES", "BECCS", "BEBCS"),
               Abatement = vapply(1:3, function(k) sum((a[, k] * bm)[adopted & best == k]), numeric(1)) / 1e6,
               Biomass = vapply(1:3, function(k) sum(bm[adopted & best == k]), numeric(1)) / 1e6)
  }))
}

rows <- list()
for (r in REGIONS) {
  dat <- load_region_data(r)
  for (v in names(VARIANTS)) {
    message("Variant sweep: ", r, " / ", v)
    params <- set_scenario(VARIANTS[[v]], region = r)
    params$region <- r
    sweep <- run_price_sweep(dat$template, dat$layers, params, vec = dat$vec)
    rows[[length(rows) + 1]] <- cbind(Region = r, Variant = v, macc_table(sweep, dat))
  }
}
res <- bind_rows(rows)
res$Region <- factor(res$Region, levels = REGIONS)
res$Variant <- factor(res$Variant, levels = names(VARIANTS))
res$Technology <- factor(res$Technology, levels = c("BECCS", "BEBCS", "BES"))
write.csv(res, paste0(var_dir, "variant_macc_data.csv"), row.names = FALSE)

# Supplementary figure: abatement by technology against carbon price, regions x variants
p <- ggplot(res, aes(x = Price, y = Abatement, fill = Technology)) +
  geom_area(alpha = 0.9, color = "black", linewidth = 0.15) +
  scale_fill_manual(values = TECH_COLORS, limits = c("BES", "BECCS", "BEBCS"), labels = tech_label) +
  facet_grid(Region ~ Variant, scales = "free_y") +
  theme_minimal(base_size = 11) +
  labs(x = paste0("Carbon price (", U_CPRICE, ")"), y = paste0("Abatement (", U_ABATE, ")"), fill = "Technology") +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey90", color = NA), plot.title = element_blank())
ggsave(paste0(out_dir, "Variants_MACC.png"), p, width = 13, height = 9, dpi = 300, bg = "white")

# Summary for the text: abatement and biomass by technology at 100, 150 and 200 $/t, relative to default
summ <- res |>
  filter(Price %in% c(100, 150, 200)) |>
  group_by(Region, Variant, Price) |>
  mutate(Total_abatement = sum(Abatement), Total_biomass = sum(Biomass)) |>
  ungroup()
write.csv(summ, paste0(ai_dir, "variant_summary.csv"), row.names = FALSE)
message("Variant sweeps complete: ", var_dir)
