library(wdpar)
library(sf)

print("Fetching MT")
pa <- wdpa_fetch("MT", wait=TRUE)
print("Cleaning MT")
pa_clean <- wdpa_clean(pa)
print(nrow(pa_clean))

