library(terra)

r1 <- rast(ncols=5, nrows=5, vals=NA)
r2 <- rast(ncols=5, nrows=5, vals=NA)
dist_stack <- c(r1, r2)

tryCatch({
    r_min_dist <- terra::app(dist_stack, min, na.rm=TRUE)
    print("app min worked")
}, error=function(e) print(e))

tryCatch({
    r_winner_idx <- terra::app(dist_stack, which.min)
    print("app which.min worked")
}, error=function(e) print(e))

