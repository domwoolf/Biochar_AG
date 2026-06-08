library(terra)

r <- rast(ncols=5, nrows=5, vals=1)
r2 <- rast(ncols=5, nrows=5, vals=2)

mask_na <- rast(ncols=5, nrows=5, vals=NA)

tryCatch({
    r[mask_na] <- r2[mask_na]
    print("Assigning raster subset with ALL NA mask worked")
}, error=function(e) print(e))

