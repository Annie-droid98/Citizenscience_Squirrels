library(sf)
library(vroom)
library(dplyr)
library(stars)
library(tidyr)

## download the GBIF data
## csv = download from Gbif, doi:doi.org/10.15468/dl.tu6vjj
temp <- tempfile()
download.file("https://api.gbif.org/v1/occurrence/download/request/0169558-210914110416597.zip", temp)

## read in the GBIF data
data_GB <- vroom(temp, quote = "", show_col_types = FALSE)

## check for issues
## note: here columns with issues are not relevant for this analysis
colnames(data_GB)[unique(problems(data_GB)$col)]

## format GBIF data
data_GB |>
  select(species, decimalLongitude, decimalLatitude, year, datasetKey) |>
  filter(!is.na(decimalLatitude)) |>
  st_as_sf(coords = c(2, 3)) |>
  st_set_crs(4326) |> ## set the coordinate system as it is
  ## project into the coordinate system needed (that of the landuse data)
  st_transform(3035) -> Mammalia_GB

rm(data_GB) ## remove object no longer needed

## Publisher categorisation
Publishers <- vroom("input_data/SquirrelPublisherBelow1000obs.csv", show_col_types = FALSE)

### Merge the two datasets
full_join(Mammalia_GB, Publishers, by = "datasetKey", relationship = "many-to-many") |> 
  ## keep only records with species
  filter(!is.na(species)) -> Mammalia_GB_Pub

rm(Mammalia_GB)

Landuse_10k_sfc |> ## used to retain the geometry
  st_join(Mammalia_GB_Pub) |> 
  summarise(CountT_mammalia = n(),
            CountT_vulgaris = sum(species == "Sciurus vulgaris"),
            CountT_carolinensis = sum(species == "Sciurus carolinensis"),
            CountT_marten = sum(species == "Martes martes"),
            .by = c("geometry", "year", "Observer", "FocusTaxaTorF")) |> 
  complete(geometry, year, Observer, FocusTaxaTorF, 
           fill = list(CountT_mammalia = 0,
                       CountT_vulgaris = 0,
                       CountT_carolinensis = 0,
                       CountT_marten = 0)) |> 
  st_as_sf() |> 
  st_join(Landuse_10k_sfc) |> ## to add landcover columns
  mutate(PropT_carolinensis = CountT_carolinensis/CountT_mammalia, 
         PropT_vulgaris = CountT_vulgaris/CountT_mammalia, 
         PropT_marten = CountT_marten/CountT_mammalia,
         ## across(starts_with("PropT"), scale, .names = "{.col}_z")) |> 
         CountT_mammalia_log = log(CountT_mammalia),
         year_from_2000 = year - 2000,
         Centergrid = st_centroid(geometry), ## middlepoint of each grid cell 
         lon = st_coordinates(Centergrid)[,"X"]/1e+05, ## coordinates of these middlepoints, and make them smaller to avoid problems with the model
         lat = st_coordinates(Centergrid)[,"Y"]/1e+05) -> Mammalia_GB_count_10km

saveRDS(Mammalia_GB_count_10km, "intermediate_data/Counts.rds")

rm(Mammalia_GB_Pub)

