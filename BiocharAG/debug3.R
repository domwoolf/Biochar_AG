library(terra)

r <- rast(ncols=5, nrows=5, xmin=0, xmax=5, ymin=0, ymax=5)
values(r) <- 1
# Center of a cell
pts <- matrix(c(2.5, 2.5), ncol=2) 
tryCatch({
    d2 <- costDist(r, target = pts)
    print("costDist with matrix crds worked")
    print(d2)
}, error=function(e) print(e))
