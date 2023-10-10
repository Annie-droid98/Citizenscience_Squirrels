library(dplyr)
library(sf)
library(rgdal)
library(raster)


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
Grid_ohneduplices <- GB_and_IE_grid_10km%>%
  distinct()

saveRDS(Grid_ohneduplices,"10kmgrids.rds")

#as shapefile
GB_and_IE_grid_10km_shp <- bind(Ireland_10grid, GB_10grid)
saveRDS(GB_and_IE_grid_10km_shp,"Shapefile_British_Islands.rds")

#Europe
download.file("https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/europe-10-km-100-km/at_download/file", destfile = file.path(tempdir(), "Europe.zip"), mode = "wb")
unzip(zipfile = file.path(tempdir(), "Europe.zip"), exdir = tempdir())
Europe10grid <- shapefile(file.path(tempdir(), "europe_10km.shp"))
Europe10grid  <- st_as_sf(Europe10grid)
saveRDS(Europe10grid,"Europe10grid.rds")
