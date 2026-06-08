library(wdpar)
library(sf)

print("Fetching USA")
pa <- wdpa_fetch("USA", wait=TRUE, download_dir = tempdir())
print(nrow(pa))

