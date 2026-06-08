library(terra)
t0 <- Sys.time()
r_base <- rast(ncols=2950, nrows=1300, ext=ext(-125, -66, 24, 50), vals=1)
r_base[1000, 1000] <- 0
t1 <- Sys.time()
d <- costDist(r_base, target=0)
t2 <- Sys.time()

message("Raster creation: ", t1 - t0)
message("costDist on 3.8M cells: ", t2 - t1)
