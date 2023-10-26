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
    ## set the correct coordinate system
    st_set_crs(4326) %>%
    ## and transform it for uniformity with the grid and landuse data
    st_transform(st_crs(3035))  %>% 
    mutate(lon = sf::st_coordinates(.)[,"X"],
           lat = sf::st_coordinates(.)[,"Y"])

## Publisher categorisation
Publishers <- vroom::vroom("input_data/SquirrelPublisherBelow1000obs.csv",
                           show_col_types = FALSE)

### Non-categorized publishers are here NA (mege with all=TRUE)
Mammalia_GB <- merge(Mammalia_GB, Publishers,
                     by="datasetKey", all=TRUE) %>%
    ## keep only records with species
    filter(!is.na(species))

Mammalia_GB_count_10km <- Grid_3035 %>%
### merge the grids and the counts to allow counting in grids
    st_join(st_sf(Mammalia_GB)) %>%
    ## again drop grids with no species counts
    filter(!is.na(species))  %>%
    ## Count seperately by grid, year but also Observer and whether it
    ## was focussed
    group_by(CELLCODE, year, Observer, FocusTaxaTorF, .drop=FALSE) %>%
    ## do the actual counting 
    summarize(CountT_mammalia = n(),
              CountT_vulgaris = sum(species%in%"Sciurus vulgaris"),
              CountT_carolinensis = sum(species%in%"Sciurus carolinensis"),
              CountT_marten = sum(species%in%"Martes martes"),
              geometry = unique(geometry)) %>%
    ungroup() %>%
    ## fill zeros for non-reported values (no count in a cell)
    complete(nesting(CELLCODE, geometry), year, Observer, FocusTaxaTorF, 
             fill = list(CountT_mammalia = 0,
                         CountT_vulgaris = 0,
                         CountT_carolinensis = 0,
                         CountT_marten = 0)) %>%
    mutate(PropT_carolinensis = CountT_carolinensis/CountT_mammalia, 
           PropT_vulgaris = CountT_vulgaris/CountT_mammalia, 
           PropT_marten = CountT_marten/CountT_mammalia,
           ## not scaling for now
           ## across(starts_with("PropT"), scale, .names = "{.col}_z")) %>% 
           CountT_mammalia_log = log(CountT_mammalia),
           year_from_2000 = year - 2000,
           ## middlepoint of each grid cell 
           Centergrid = st_centroid(geometry),
           ## coordinates of these middlepoints, and make them smaller
           ## to avoid problems with the model
           lon = st_coordinates(Centergrid)[,"X"]/1000,
           lat = st_coordinates(Centergrid)[,"Y"]/1000
           ) 


saveRDS(Mammalia_GB_count_10km, "intermediate_data/Counts.rds")



