library(exactextractr)
library(raster)
library(fasterize)
library(sf)
library(dplyr)
library(tidyr)

## do we want to repeate the grid and shapefile download
redoGRIDdownload  <- FALSE

if(redoGRIDdownload){
    source("R/1_DownloadGrids.R")
} else {
    Britain10grid <- readRDS("intermediate_data/10kmgrids.rds")
}


### LANDCOVER 

## ## Download the raster at (requires login)
## ## https://land.copernicus.eu/pan-european/corine-land-cover/clc2018?tab=download

## ## Then execute
## clc_2018_landcover <- raster("whereever/you/downloaded")
## saveRDS(clc_2018_landcover, "input_data/Landcover.rds")

## ## read the rds because the tiff is to large for the repository
clc_2018_landcover <- readRDS("input_data/Landcover.rds")

#Squirrels 
rasterOptions(tmpdir=tempdir(), overwrite=TRUE)

## here we crop the land use
clc_2018_landcover <- crop(clc_2018_landcover, extent(Britain10grid))

## "grey urban" 
clc_2018_landcover[clc_2018_landcover <= 9] <- 9

## green urban
clc_2018_landcover[clc_2018_landcover %in% c(10, 11)] <- 11

## Agricultural
clc_2018_landcover[clc_2018_landcover %in%
                             c(12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)] <- 22

## Forrest 23, 24, 25 stay as they are


## 26-39 other seminatural areas
clc_2018_landcover[clc_2018_landcover %in%
                             c(26,27, 28, 29, 30, 31, 32,
                               33, 34, 35, 36, 37, 38, 39)] <- 39

## Waterbodies
clc_2018_landcover[clc_2018_landcover %in%
                             c(40, 41, 42, 43, 44)] <- 44

## save to then read as raster!!!
## writeRaster(clc_2018_landcover, "clc_2018_landcover_categories")
## clc_2018_landcover <- raster("clc_2018_landcover_categories")

## extract
clc_2018_landcover <-
    exactextractr::exact_extract(x = clc_2018_landcover,
                                 y = Britain10grid)

clc_2018_landcover <- lapply(clc_2018_landcover,
                             function(x) x[(names(x) %in% c("value"))])

## empty vectors to store the results
clc_9_s <- vector()
clc_11_s <- vector()
clc_22_s <- vector()
clc_23_s <- vector()
clc_24_s <- vector()
clc_25_s <- vector()
clc_39_s <- vector()
clc_44_s <- vector()
## loop along the list
for (i in 1:length(clc_2018_landcover)){
  tmp_s <- clc_2018_landcover[[i]]
  ## count each of the values
  tmp_9_s <- sum(tmp_s == 9)
  tmp_11_s <- sum(tmp_s == 11)
  tmp_22_s <- sum(tmp_s == 22)
  tmp_23_s <- sum(tmp_s == 23)
  tmp_24_s <- sum(tmp_s == 24)
  tmp_25_s <- sum(tmp_s == 25)
  tmp_39_s <- sum(tmp_s == 39)
  tmp_44_s <- sum(tmp_s == 44)
  # add the counts to a vector following the same order as the grids
  clc_9_s[i] <- tmp_9_s
  clc_11_s[i] <- tmp_11_s
  clc_22_s[i] <- tmp_22_s
  clc_23_s[i] <- tmp_23_s
  clc_24_s[i] <- tmp_24_s
  clc_25_s[i] <- tmp_25_s
  clc_39_s[i] <- tmp_39_s
  clc_44_s[i] <- tmp_44_s
}

## The code above is so ugly I have to concentrate so hard to not
## rewrite it with lapply, but instead put the vectors into a df and
## name them

## Let's use "CountL_" for all the landuse counts
Landuse_10km <-
    cbind.data.frame(CountL_Grey_urban = clc_9_s,
                     CountL_Green_urban = clc_11_s,
                     CountL_Agricultural = clc_22_s,
                     CountL_Broadleafed_Forest = clc_23_s,
                     CountL_Coniferous_Forest = clc_24_s,
                     CountL_Mixed_Forest = clc_25_s,
                     CountL_Other_seminatural = clc_39_s,
                     CountL_Waterbodies = clc_44_s,
                     CELLCODE=Britain10grid$CELLCODE) %>%
    ## get proportions (each cell 10km*10km cell could have 10,000
    ## entries of 100*100m resolved "pixels")
    as_tibble() %>%
    mutate(across(starts_with("CountL_"), ~ .x/10000, .names = "Prop_{.col}"))

saveRDS(Landuse_10km, "intermediate_data/Landuse_10km.rds")
