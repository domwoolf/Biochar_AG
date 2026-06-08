library(terra)
packageVersion("terra")

r <- rast(ncols=5, nrows=5, xmin=0, xmax=5, ymin=0, ymax=5)
values(r) <- 1
pts <- vect(matrix(c(2.5, 2.5), ncol=2), crs=crs(r))
tryCatch({
    d1 <- costDist(r, target = pts)
    print("costDist with SpatVector worked")
}, error=function(e) print(e))

tryCatch({
    d2 <- costDist(r, target = crds(pts))
    print("costDist with crds worked")
}, error=function(e) print(e))
