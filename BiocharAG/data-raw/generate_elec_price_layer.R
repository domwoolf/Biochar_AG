# data-raw/generate_elec_price_layer.R
# Rasterizes wholesale electricity prices (2024 USD/MWh) onto the regional templates (issue #95).
#
# The price is what a generator is paid: a wholesale (day-ahead or hub) price, not a retail tariff, so
# retail taxes, network charges and cross-subsidies are excluded. Prices are averaged over 2024-2025,
# excluding the 2022 European gas crisis and its aftermath in 2023 (2023 values are kept below for
# reference). Each year's nominal price is converted at that year's average exchange rate (World Bank,
# PA.NUS.FCRF) and escalated to 2024 USD with US CPI-U (BLS annual averages), then the years are averaged.
#
# Sources:
#   United States: EIA Short-Term Energy Outlook, September 2026, Table 7a, wholesale prices at 11
#     trading hubs (monthly averages; historical data from the ISOs/RTOs). Each state is assigned the
#     hub of the market that serves most of it.
#   Europe: Ember European wholesale electricity price data (day-ahead prices from ENTSO-E, monthly,
#     CC-BY 4.0), data-raw/elec_prices/ember_european_wholesale_monthly.csv. Turkiye: EPIAS day-ahead
#     market clearing price (PTF), annual averages. Ukraine: Market Operator day-ahead BASE index.
#     Moldova, Bosnia and Herzegovina, Kosovo and Albania take a neighbouring market's price. Belarus,
#     which has no wholesale market, takes its 2020 industrial tariff (Climatescope 2021: 74 USD/MWh)
#     times the US ratio of wholesale to industrial price (2024 median across states, 0.42: STEO hub
#     prices / EIA Electric Power Annual Table 2.10). Micro-states and Cyprus take the median of the
#     priced countries. Russia and North Africa are outside the study region (process_borders.R).
#   China: provincial coal-fired benchmark on-grid tariff (NDRC 2019, Fa Gai Jia Ge Gui [2019] No.
#     1658, which fixed the benchmark at each province's prevailing coal tariff). This is the price paid
#     for coal power and the reference for renewable generators, net of any feed-in subsidy. Converted
#     from VAT-inclusive (13%) to VAT-exclusive for consistency with the other regions. Since 2021,
#     market-traded coal power may float from -20% to +20% around the benchmark.
#   India: weighted average day-ahead market price at the Indian Energy Exchange, FY2024-25 (CERC,
#     Report on Short-term Power Market in India, 2024-25; FY2023-24: 5.16 INR/kWh). A single national
#     value is used, as the exchange rarely splits into separately priced regions.
#
# Usage (from BiocharAG/): Rscript data-raw/generate_elec_price_layer.R

library(terra)

gis_proc <- "../GIS/processed"
years <- c("2024", "2025")

# US CPI-U annual averages (BLS CUUR0000SA0) and World Bank official exchange rates (LCU per USD)
cpi <- c("2020" = 258.811, "2023" = 304.702, "2024" = 313.689, "2025" = 321.943)
fx <- list(
  EUR = c("2023" = 0.9248, "2024" = 0.9239, "2025" = 0.8850),
  TRY = c("2023" = 23.7386, "2024" = 32.8059, "2025" = 39.4548),
  UAH = c("2023" = 36.5738, "2024" = 40.1521, "2025" = 41.6891),
  CNY = c("2023" = 7.0840, "2024" = 7.1975, "2025" = 7.1898),
  INR = c("2023" = 82.5993, "2024" = 83.6693, "2025" = 87.1584)
)

# Mean over `years` of nominal prices (named by year, in local currency per MWh) in 2024 USD per MWh
to_usd2024 <- function(price, cur = NULL) {
  y <- years
  price <- price[y]
  rate <- if (is.null(cur)) 1 else fx[[cur]][y]
  mean(price / rate * cpi["2024"] / cpi[y])
}

# ==============================================================================
# 1. Prices by area (2024 USD/MWh)
# ==============================================================================

# --- United States: STEO hub prices (nominal USD/MWh, annual means of monthly values) ---
us_hubs <- rbind(
  ERCOT = c(77.00, 33.64, 37.33), CAISO_SP15 = c(60.17, 29.96, 28.56), ISONE = c(41.36, 46.59, 75.58),
  NYISO_HV = c(37.96, 42.54, 72.10), PJM_West = c(39.34, 40.75, 60.09), MISO_IL = c(34.42, 33.11, 46.82),
  SPP_South = c(34.74, 40.01, 37.91), Into_Southern = c(32.26, 29.64, 41.41), FRCC = c(33.05, 31.49, 44.60),
  Mid_C = c(81.61, 59.68, 45.57), Palo_Verde = c(59.46, 31.50, 31.43)
)
colnames(us_hubs) <- c("2023", "2024", "2025")
us_hub_usd <- apply(us_hubs, 1, to_usd2024)
us_state_hub <- c(
  Texas = "ERCOT", California = "CAISO_SP15",
  Connecticut = "ISONE", Maine = "ISONE", Massachusetts = "ISONE", "New Hampshire" = "ISONE",
  "Rhode Island" = "ISONE", Vermont = "ISONE", "New York" = "NYISO_HV",
  Pennsylvania = "PJM_West", "New Jersey" = "PJM_West", Maryland = "PJM_West", Delaware = "PJM_West",
  "District of Columbia" = "PJM_West", Virginia = "PJM_West", "West Virginia" = "PJM_West",
  Ohio = "PJM_West", Kentucky = "PJM_West",
  Illinois = "MISO_IL", Indiana = "MISO_IL", Michigan = "MISO_IL", Wisconsin = "MISO_IL",
  Minnesota = "MISO_IL", Iowa = "MISO_IL", Missouri = "MISO_IL", "North Dakota" = "MISO_IL",
  Arkansas = "MISO_IL", Louisiana = "MISO_IL", Mississippi = "MISO_IL",
  Oklahoma = "SPP_South", Kansas = "SPP_South", Nebraska = "SPP_South", "South Dakota" = "SPP_South",
  Alabama = "Into_Southern", Georgia = "Into_Southern", "South Carolina" = "Into_Southern",
  "North Carolina" = "Into_Southern", Tennessee = "Into_Southern", Florida = "FRCC",
  Washington = "Mid_C", Oregon = "Mid_C", Idaho = "Mid_C", Montana = "Mid_C",
  Arizona = "Palo_Verde", "New Mexico" = "Palo_Verde", Nevada = "Palo_Verde", Utah = "Palo_Verde",
  Colorado = "Palo_Verde", Wyoming = "Palo_Verde"
)
us_prices <- setNames(us_hub_usd[us_state_hub], names(us_state_hub))

# --- Europe: Ember day-ahead prices (EUR/MWh), by ISO3 ---
ember <- read.csv("data-raw/elec_prices/ember_european_wholesale_monthly.csv", check.names = FALSE)
ember$year <- substr(ember$Date, 1, 4)
ember <- ember[ember$year %in% years & !is.na(ember[["Price (EUR/MWhe)"]]), ]
ann <- tapply(ember[["Price (EUR/MWhe)"]], list(ember[["ISO3 Code"]], ember$year), mean)
ann <- ann[stats::complete.cases(ann[, years, drop = FALSE]), years, drop = FALSE]
eu_prices <- apply(ann, 1, to_usd2024, cur = "EUR")
eu_prices["TUR"] <- to_usd2024(c("2023" = 2190.79, "2024" = 2233.42, "2025" = 2617.83), "TRY")
eu_prices["UKR"] <- to_usd2024(c("2023" = 3373.52, "2024" = 4522.27, "2025" = 5292.62), "UAH")
eu_prices["BLR"] <- 74 * cpi[["2024"]] / cpi[["2020"]] * 0.42
# Neighbouring-market proxies for small unpriced markets
eu_prices["MDA"] <- eu_prices["ROU"]
eu_prices["XKX"] <- eu_prices["SRB"]
eu_prices["BIH"] <- mean(eu_prices[c("SRB", "HRV", "MNE")])
eu_prices["ALB"] <- mean(eu_prices[c("MNE", "GRC")])
eu_default <- stats::median(eu_prices)

# --- China: coal benchmark (CNY/kWh incl. 13% VAT), constant in nominal terms since 2019 ---
cn_benchmark <- c(
  Beijing = 0.3598, Tianjin = 0.3655, Hebei = mean(c(0.3720, 0.3644)), Shanxi = 0.3320,
  "Nei Mongol" = mean(c(0.2829, 0.3035)), Liaoning = 0.3749, Jilin = 0.3731, Heilongjiang = 0.3740,
  Shanghai = 0.4155, Jiangsu = 0.3910, Zhejiang = 0.4153, Anhui = 0.3844, Fujian = 0.3932,
  Jiangxi = 0.4143, Shandong = 0.3949, Henan = 0.3779, Hubei = 0.4161, Hunan = 0.4500,
  Guangdong = 0.4530, Guangxi = 0.4207, Hainan = 0.4298, Chongqing = 0.3964, Sichuan = 0.4012,
  Guizhou = 0.3515, Yunnan = 0.3358, Shaanxi = 0.3545, Gansu = 0.3078, Qinghai = 0.3247,
  Ningxia = 0.2595, Xinjiang = 0.2500
)
cn_benchmark["Xizang"] <- cn_benchmark["Qinghai"] # no coal benchmark in Tibet; neighbouring grid
cn_prices <- sapply(cn_benchmark, function(p) to_usd2024(setNames(rep(p * 1000 / 1.13, length(years)), years), "CNY"))

# --- India: IEX day-ahead weighted average (INR/kWh), FY2024-25 (April 2024 - March 2025) ---
in_price <- 4.26 * 1000 / mean(fx$INR[c("2024", "2025")]) * cpi[["2024"]] / mean(cpi[c("2024", "2025")])

# ==============================================================================
# 2. Rasterize
# ==============================================================================

# Admin1 names carry suffixes ("Hebei Sheng", "Xinjiang Uygur Zizhiqu"); match on the leading words.
match_prices <- function(names_vec, lut) {
  out <- rep(NA_real_, length(names_vec))
  for (k in names(lut)) out[is.na(out) & startsWith(names_vec, paste0(k, " "))] <- lut[[k]]
  out[is.na(out) & names_vec %in% names(lut)] <- lut[names_vec[is.na(out) & names_vec %in% names(lut)]]
  out
}

region_config <- list(
  USA = list(prefix = "us", admin = "admin1",
             price = function(v) unname(us_prices[v$NAM_1]), default = stats::median(us_prices)),
  China = list(prefix = "china", admin = "admin1",
               price = function(v) match_prices(v$NAM_1, cn_prices), default = stats::median(cn_prices)),
  India = list(prefix = "india", admin = "admin1",
               price = function(v) rep(in_price, nrow(v)), default = in_price),
  Europe = list(prefix = "europe", admin = "admin0",
                price = function(v) unname(eu_prices[v$ISO_A3]), default = eu_default)
)

for (r in names(region_config)) {
  reg <- region_config[[r]]
  message("\n== Wholesale electricity price layer: ", r)
  tpl <- terra::rast(file.path(gis_proc, paste0(reg$prefix, "_biomass.tif")))
  v <- terra::vect(file.path(gis_proc, paste0(reg$prefix, "_", reg$admin, ".gpkg")))
  v$price_mwh <- reg$price(v)
  nm <- if (reg$admin == "admin0") v$NAM_0 else v$NAM_1
  unmatched <- is.na(v$price_mwh)
  if (any(unmatched)) message("  Default (", round(reg$default, 1), " USD/MWh) for: ", paste(nm[unmatched], collapse = "; "))
  v$price_mwh[unmatched] <- reg$default
  # Background = regional default, so cells outside the admin polygons never produce NA revenue
  r_elec <- terra::rasterize(v, tpl, field = "price_mwh", background = reg$default)
  names(r_elec) <- "elec_price"
  out_path <- file.path(gis_proc, paste0(reg$prefix, "_elec_price.tif"))
  terra::writeRaster(r_elec, out_path, overwrite = TRUE)
  message("  Range ", paste(round(range(v$price_mwh), 1), collapse = "-"), " USD/MWh -> ", basename(out_path))
}
