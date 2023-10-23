library(dplyr)
library(sf)
library(rgdal)
library(raster)
library(INLA)
library(spaMM)
library(tidyr)



redoGRIDdownload  <- FALSE

if(redoGRIDdownload){
    source("R/1_DownloadGrids.R")
} else {
    grids <- readRDS("intermediate_data/10kmgrids.rds")
}


Grid_ohneduplices <- grids %>% 
  st_set_crs(3035)
## Warning message:
## st_crs<- : replacing crs does not reproject data; use st_transform for that 


## csv = download from Gbif, doi:doi.org/10.15468/dl.tu6vjj
temp <- tempfile()
download.file("https://api.gbif.org/v1/occurrence/download/request/0169558-210914110416597.zip", temp)

Mammalia.GB_2021 <-vroom::vroom(temp, quote="",show_col_types = FALSE)

## ## we have some problems
## table(vroom::problems(Mammalia.GB_2021)[, "col"])
## col
##    11    25    26    27    28    29    39 
##  8626 14203    32    32    35    35 24837 
## > (list "" '(("x" . "")) '("x"))
## > dim(Mammalia.GB_2021)
## [1] 1481830      50

## colnames(Mammalia.GB_2021)[c(11, 25, 26, 27, 28, 29, 39)]
## [1] "infraspecificEpithet" "coordinatePrecision"  "elevation"           
## [4] "elevationAccuracy"    "depth"                "depthAccuracy"       
## [7] "catalogNumber"       
## > 

mammalia.GB_selected_21 <- Mammalia.GB_2021 %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode,
                gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                basisOfRecord, institutionCode, taxonKey, class, order, datasetKey)

Mammalia.GB_10km <- mammalia.GB_selected_21 %>% 
  as.data.frame %>%
  filter((! is.na(decimalLatitude)))%>%
  sf::st_as_sf(coords = c(2,3))%>%
  st_set_crs(4326)%>%
  st_transform(st_crs(3035))

Mammalia.GB_10km <- Mammalia.GB_10km%>%
  dplyr::mutate(long = sf::st_coordinates(Mammalia.GB_10km)[,1],
                lat = sf::st_coordinates(Mammalia.GB_10km)[,2])

##Publisher categorisation
Mammalia_observations_GB <- vroom::vroom("input_data/SquirrelPublisherUntil1000obs.csv",
                                         quote="",show_col_types = FALSE)

##Filter observation data from citizen science only
Mammalia_citizenscience <-Mammalia_observations_GB%>%
  filter(Observer == "1" & FocusTaxaTorF == "FALSE" )

Mammalia_citizenscience<- Mammalia.GB_10km %>%
  filter(datasetKey %in% Mammalia_citizenscience$datasetKey)

#Filter observation data from all publishers
Mammalia_GB_mixedpub_10km <- Mammalia_observations_GB%>%
  filter(Observer == "1" & FocusTaxaTorF == "FALSE" | Observer == "2" & FocusTaxaTorF == "FALSE" | Observer == "3"& FocusTaxaTorF == "FALSE")
Mammalia_GB_mixed_pub10km<- Mammalia.GB_10km %>%
  filter(datasetKey %in% Mammalia_GB_mixedpub_10km$datasetKey)

Publisher_categories <- list(Mammalia_citizenscience, Mammalia_GB_mixed_pub10km)

#counting of S.carolinensis, S.vulgaris, M.martes and All mammal observations 
#Mammalia_GB_count_10km <- lapply((Publisher_categories), function(i){

Mammalia_GB_count_10km <- Grid_ohneduplices%>%
  st_join(st_sf(Mammalia_citizenscience)) %>%
  transform(isVulgaris = species%in%"Sciurus vulgaris", isCarolinensis = species%in%"Sciurus carolinensis", isMartes = species%in%"Martes martes")%>%
  group_by(year,CELLCODE) %>%
  count(isVulgaris, isCarolinensis, isMartes, countMammalia = !isVulgaris&!isCarolinensis&!isMartes)%>%
  transform(what = ifelse(isVulgaris,"S.vulgaris", ifelse(isCarolinensis,"S.carolinensis",
                                                          ifelse(isMartes,"M.martes", "countMammalia"))))%>%
  spread(what,n)
#})

#middlepoint of each grid cell here citizen science count
#Mammalia_GB_count_10km[[1]]$Centergrid <-st_centroid(Mammalia_GB_count_10km[[1]]$geometry)
Mammalia_GB_count_10km$Centergrid <-st_centroid(Mammalia_GB_count_10km$geometry)
#Mammalia_GB_count_10km_2 <-Mammalia_GB_count_10km[[1]]%>%
Mammalia_GB_count_10km_2 <-Mammalia_GB_count_10km%>%
  # dplyr::mutate(lon = sf::st_coordinates(Mammalia_GB_count_10km[[1]]$Centergrid)[,1],
  #              lat = sf::st_coordinates(Mammalia_GB_count_10km[[1]]$Centergrid)[,2])
  dplyr::mutate(lon = sf::st_coordinates(Mammalia_GB_count_10km$Centergrid)[,1],
                lat = sf::st_coordinates(Mammalia_GB_count_10km$Centergrid)[,2])

Mammalia_GB_count_10km_2_df <-Mammalia_GB_count_10km_2%>%
  unite('IDYear', CELLCODE:year, remove = FALSE)

Mammalia_GB_count_10km_2_df <- Mammalia_GB_count_10km_2_df%>% 
  arrange(IDYear) %>%
  group_by(IDYear) %>% fill(c(everything()), .direction = "downup") %>% 
  ungroup() %>% 
  distinct(IDYear, .keep_all = T) %>% 
  filter(!is.na(year))


Mammalia_GB_count_10km_2_df[is.na(Mammalia_GB_count_10km_2_df)] <- 0
Mammalia_GB_count_10km_2_df<- transform(Mammalia_GB_count_10km_2_df, 
                                        AllMammalia = countMammalia + S.vulgaris + M.martes + S.carolinensis)
Mammalia_GB_count_10km_2_df<- transform(Mammalia_GB_count_10km_2_df, 
                                        Proportion_carolinensis = S.carolinensis/AllMammalia)

Mammalia_GB_count_10km_2_df <- transform(Mammalia_GB_count_10km_2_df, Proportion_vulgaris = S.vulgaris/AllMammalia)

Mammalia_GB_count_10km_2_df <- transform(Mammalia_GB_count_10km_2_df, Proportion_marten = M.martes/AllMammalia)

saveRDS(Mammalia_GB_count_10km_2_df, "Counts.rds")

