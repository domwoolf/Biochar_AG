library(terra)

# Load the generated distance sink layer
r <- rast("../GIS/processed/us_dist_sink.tif")
print(r)

# Check some values
vals <- values(r)
# Just print max and min
print(minmax(r))

# Print standard dev or variance to see it has a spread
print(sd(vals, na.rm=TRUE))

