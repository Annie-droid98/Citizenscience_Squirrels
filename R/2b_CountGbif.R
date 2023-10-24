library(dplyr)
library(sf)
library(rgdal)
library(raster)
library(tidyr)

### These are not needed yet, I guess
## library(INLA)
## library(spaMM)

redoGRIDdownload  <- FALSE

if(redoGRIDdownload){
    source("R/1_DownloadGrids.R")
} else {
    Britain10grid <- readRDS("intermediate_data/10kmgrids.rds")
}

Grid_3035 <- st_transform(Britain10grid, 3035)

## csv = download from Gbif, doi:doi.org/10.15468/dl.tu6vjj
temp <- tempfile()
download.file("https://api.gbif.org/v1/occurrence/download/request/0169558-210914110416597.zip", temp)

Mammalia_GB <- vroom::vroom(temp, quote="",show_col_types = FALSE) %>%
    dplyr::select(species, decimalLongitude, decimalLatitude, countryCode,
                  gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                  basisOfRecord, institutionCode, taxonKey, class, order, datasetKey) %>%
    as.data.frame %>%
    filter((! is.na(decimalLatitude))) %>%
    sf::st_as_sf(coords = c(2,3)) %>%
    st_set_crs(4326) %>%
    st_transform(st_crs(3035))  %>% 
    dplyr::mutate(lon = sf::st_coordinates(.)[,"X"],
                  lat = sf::st_coordinates(.)[,"Y"])

## Publisher categorisation
Publishers <- vroom::vroom("input_data/SquirrelPublisherBelow1000obs.csv",
                           show_col_types = FALSE)

### We don't non-categorized publishers here they'll be NA (mege with
### all=TRUE)
Mammalia_GB <- merge(Mammalia_GB, Publishers,
                     by="datasetKey", all=TRUE)

Mammalia_GB_count_10km <- Grid_3035 %>%
    st_join(st_sf(Mammalia_GB)) %>%
    ## First we count Ci 
    transform(isVulgaris = species%in%"Sciurus vulgaris",
              isCarolinensis = species%in%"Sciurus carolinensis",
              isMartes = species%in%"Martes martes")%>%
    ### se count seperately for each category of observers
    group_by(year, CELLCODE, Observer) %>%
    count(isVulgaris, isCarolinensis, isMartes,
          countMammalia = !isVulgaris&!isCarolinensis&!isMartes)%>%
    transform(what = ifelse(isVulgaris,"S.vulgaris",
                     ifelse(isCarolinensis,"S.carolinensis",
                     ifelse(isMartes,"M.martes", "countMammalia"))))%>%
    tidyr::spread(what,n)

## middlepoint of each grid cell as coordinate
Mammalia_GB_count_10km$Centergrid <-st_centroid(Mammalia_GB_count_10km$geometry)
Mammalia_GB_count_10km <- Mammalia_GB_count_10km %>%
    dplyr::mutate(lonC = sf::st_coordinates(Mammalia_GB_count_10km$Centergrid)[,1],
                  latC = sf::st_coordinates(Mammalia_GB_count_10km$Centergrid)[,2])

Mammalia_GB_count_10km <- Mammalia_GB_count_10km %>%
  unite('IDYearObs', CELLCODE:year:Observer, remove = FALSE)

Mammalia_GB_count_10km <- Mammalia_GB_count_10km %>% 
  arrange(IDYearObs) %>%
  group_by(IDYearObs) %>% fill(c(everything()), .direction = "downup") %>% 
  ungroup() %>% 
  distinct(IDYearObs, .keep_all = T) %>% 
  filter(!is.na(year)) %>%
    mutate(
        across(all_of(c("S.vulgaris", "S.carolinensis",
                        "M.martes", "countMammalia")), ~replace_na(.x, 0))
    )

Mammalia_GB_count_10km <- Mammalia_GB_count_10km %>%
    transform(AllMammalia = countMammalia +
                  S.vulgaris + M.martes + S.carolinensis) %>%
    transform(Proportion_carolinensis = S.carolinensis/AllMammalia, 
              Proportion_vulgaris = S.vulgaris/AllMammalia, 
              Proportion_marten = M.martes/AllMammalia)

saveRDS(Mammalia_GB_count_10km, "Counts.rds")



