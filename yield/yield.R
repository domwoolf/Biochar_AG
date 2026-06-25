library(data.table)
library(ranger)
library(mlim)
yield <- fread("yield/BCYield_data2.csv")

# Use mlim to impute missing data
ref <- yield$Ref
yield[, c("Ref", "lat", "lon", "RR", "YieldRatio", "RelativeYieldIncrease") := NULL]

char_cols <- yield[, which(sapply(.SD, is.character))]
if (length(char_cols) > 0) {
    yield[, (char_cols) := lapply(.SD, as.factor), .SDcols = char_cols]
}

# Explicitly initialize H2O. If mlim cannot connect to a Java backend, it fails silently and returns original NAs.
h2o::h2o.init()

# mlim is safer with standard data.frames, and we ensure the output is back to data.table
yield_imp <- mlim::mlim(as.data.frame(yield))
