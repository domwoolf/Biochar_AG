library(terra)

r <- rast(ncols=20, nrows=20, vals=1)
r[1:100] <- NA

r_agg <- aggregate(r, fact=10, fun=mean, na.rm=TRUE)
print(r_agg)
print(values(r_agg))
