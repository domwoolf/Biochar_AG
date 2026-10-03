# data-raw/elec_prices/price_capture.R
# Realised-to-average price ratio ("price capture") of a price-following plant by capacity factor (#107).
#
# Input: Ember hourly day-ahead wholesale prices for European markets (from ENTSO-E, EMR, SEMOpx), not
# stored in the repository (about 40 MB zipped):
#   https://files.ember-energy.org/public-downloads/price/outputs/european_wholesale_electricity_price_data_hourly.zip
# Unzip and set hourly_csv to all_countries.csv.
#
# Method: outages at the DEA (2026) availability of 0.91 are random, so the plant is price-dispatched over
# the available hours, running a share CF / 0.91 of them: the highest-priced hours of each day (a steam
# plant cycling daily). Perfect foresight and no start-up or minimum-load constraints, so the ratios are
# upper bounds. Capture = mean price in the hours run / time-weighted mean price.
# Output: price_capture_ratios.csv (country x year, base CF 0.85 and flexible CF 0.45).

library(data.table)

hourly_csv <- Sys.getenv("EMBER_HOURLY_CSV", "all_countries.csv")
availability <- 0.91
cf <- c(base = 0.85, flex = 0.45)

x <- fread(hourly_csv, select = c(1, 3, 5), col.names = c("country", "t", "p"))
x[, t := as.POSIXct(t, tz = "UTC")][, year := as.integer(format(t, "%Y"))][, day := as.IDate(t)]
x <- x[!is.na(p)]
share <- cf / availability
r <- x[, {
  dd <- .SD[, .(pb = mean(sort(p, TRUE)[seq_len(round(.N * share[["base"]]))]),
                pf = mean(sort(p, TRUE)[seq_len(round(.N * share[["flex"]]))]), pm = mean(p)), by = day]
  list(base = mean(dd$pb) / mean(dd$pm), flex = mean(dd$pf) / mean(dd$pm))
}, by = .(country, year)]
r <- r[is.finite(base) & is.finite(flex)]
fwrite(r, file.path("data-raw", "elec_prices", "price_capture_ratios.csv"))
print(r[, .(n = .N, base = median(base), flex = median(flex), flex_p10 = quantile(flex, 0.1),
            flex_p90 = quantile(flex, 0.9)), by = year][order(year)])
