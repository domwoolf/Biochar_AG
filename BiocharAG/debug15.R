library(terra)
r_template <- rast(ncols=5, nrows=5, vals=1)

r_offshore_flag <- terra::rast(r_template)
mask_winner <- rast(ncols=5, nrows=5, vals=c(rep(1, 10), rep(0, 15)))

tryCatch({
    r_offshore_flag[mask_winner] <- 1
    print("worked without initialization")
}, error=function(e) print(e))

terra::values(r_offshore_flag) <- NA
tryCatch({
    r_offshore_flag[mask_winner] <- 1
    print("worked WITH initialization")
}, error=function(e) print(e))

