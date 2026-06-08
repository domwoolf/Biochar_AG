library(terra)

r <- rast(ncols=5, nrows=5, vals=1)
r2 <- rast(ncols=5, nrows=5, vals=1)

mask_winner <- (r == 2) # All FALSE

tryCatch({
    r[mask_winner] <- 5
    print("Assigning scalar worked")
}, error=function(e) print(e))

tryCatch({
    r[mask_winner] <- r2[mask_winner]
    print("Assigning raster subset worked")
}, error=function(e) print(e))

