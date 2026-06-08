library(terra)

r <- rast(ncols=5, nrows=5, vals=1)

tryCatch({
    r_min <- terra::app(r, min, na.rm=TRUE)
    print("app min worked on 1 layer")
}, error=function(e) print(e))

