library(dplyr)
library(sf)

## We need to download Ireland and great Britain shape files and then
## unite them ;-) 

#Ireland
download.file("https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/ireland-shapefile/at_download/file",
              destfile = file.path(tempdir(), "Ireland_shapefile"), mode = "wb")
unzip(zipfile = file.path(tempdir(), "Ireland_shapefile"), exdir = tempdir())  

Ireland_10grid <- read_sf(file.path(tempdir(), "ie_10km.shp"))

#Great Britain
download.file("https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/great-britain-shapefile/at_download/file",
              destfile = file.path(tempdir(), "Great_Britain_shapefile"), mode = "wb")
unzip(zipfile = file.path(tempdir(), "Great_Britain_shapefile"), exdir = tempdir())  

GB_10grid <- read_sf(file.path(tempdir(), "gb_10km.shp"))

#Combine Ireland and Great Britain
GB_and_IE_grid_10km <- rbind(Ireland_10grid, GB_10grid)

Britain10grid <- distinct(GB_and_IE_grid_10km)

st_write(Britain10grid, "intermediate_data/10kmgrids.shp", append = FALSE)
