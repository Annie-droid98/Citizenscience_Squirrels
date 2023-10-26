library(dplyr)
library(sf)
library(rgdal)
library(raster)


## We need to download Ireland and great Britain shape files and then
## unite them ;-) 

#Ireland
download.file("https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/ireland-shapefile/at_download/file",
              destfile = file.path(tempdir(), "Ireland_shapefile"), mode = "wb")
unzip(zipfile = file.path(tempdir(), "Ireland_shapefile"), exdir = tempdir())  

Ireland_10grid <- shapefile(file.path(tempdir(), "ie_10km.shp"))

#Great Britain
download.file("https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/great-britain-shapefile/at_download/file",
              destfile = file.path(tempdir(), "Great_Britain_shapefile"), mode = "wb")
unzip(zipfile = file.path(tempdir(), "Great_Britain_shapefile"), exdir = tempdir())  

GB_10grid <- shapefile(file.path(tempdir(), "gb_10km.shp"))

GB_and_IE_grid_10km <- bind(Ireland_10grid, GB_10grid )%>%
    st_as_sf()

Britain10grid <- GB_and_IE_grid_10km %>%
    distinct()

saveRDS(Britain10grid, "intermediate_data/10kmgrids.rds")

## #as shapefile
## GB_and_IE_grid_10km_shp <- bind(Ireland_10grid, GB_10grid)
## saveRDS(GB_and_IE_grid_10km_shp,"intermediate_data/Shapefile_British_Islands.rds")

