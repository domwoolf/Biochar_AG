library(terra)
r_min <- rast(ncols=5, nrows=5, vals=NA)
r_sal <- rast(ncols=5, nrows=5, vals=NA)
mask <- is.na(r_sal)

tryCatch({
    r_sal[mask] <- r_min[mask]
    print("worked")
}, error=function(e) print(e))
