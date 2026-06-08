library(terra)
r <- rast(ncols=295, nrows=130, vals=1)
r[1] <- 0
t0 <- Sys.time()
d <- costDist(r, 0)
t1 <- Sys.time()
print(t1-t0)
