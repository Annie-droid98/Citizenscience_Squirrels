library(dplyr)
library(sf)
library(stars)
library(ggplot2)
library(vroom)
library(tidyr)

## do we want to repeat the download from GBIF
new_dl <- FALSE

## do we want to plot things as we got (for checking purposes)
draw_plot <- FALSE

## do we want to remove intermediate objects to save memory or keep
## them for inspection (trouble shooting)?
rm_intermediate <- FALSE

### LANDCOVER 

## ## We downloaded the file from (requires login)
## ## https://land.copernicus.eu/pan-european/corine-land-cover/clc2018?tab=download
## ## selecting:
## ##  - CORINE Land Cover 2018 (vector/raster 100 m), Europe, 6-yearly - Geotiff (NUTS: United Kingdom)
## ##  - CORINE Land Cover 2018 (vector/raster 100 m), Europe, 6-yearly - Geotiff (NUTS: Éire/Ireland) 

## read the tiff
clc_2018_landcover_raw <- read_stars("input_data/U2018_CLC2018_V2020_20u1.tif")

## rename layer
names(clc_2018_landcover_raw) <- "landcover"

## recode landcover values into a single qualitative layer and 
## into one binary layer per category (slowish, but under 1 min)
clc_2018_landcover_raw |> 
  mutate(landcover_cat = case_match(landcover,
                                    c(01:09) ~ "Grey urban",
                                    c(10:11) ~ "Green urban",
                                    c(12:22) ~ "Agricultural",
                                    23       ~ "Broadleaf forest",
                                    24       ~ "Coniferous forest",
                                    25       ~ "Mixed forest",
                                    c(26:39) ~ "Semi-natural areas",
                                    c(40:44) ~ "Waterbodies",
                                    .default = NA
                                    )) |>
  mutate(PropL_Grey_urban = landcover_cat == "Grey urban",
         PropL_Green_urban = landcover_cat == "Green urban",
         PropL_Agricultural = landcover_cat == "Agricultural",
         PropL_Broadleaf_forest = landcover_cat == "Broadleaf forest",
         PropL_Coniferous_forest = landcover_cat == "Coniferous forest",
         PropL_Mixed_forest = landcover_cat == "Mixed forest",
         PropL_Semi_natural_areas = landcover_cat == "Semi-natural areas",
         PropL_Water = landcover_cat == "Waterbodies") -> clc_2018_landcover

if(rm_intermediate) rm(clc_2018_landcover_raw) 

## quick visualization for checking things
if (draw_plot) {
  ggplot() + geom_stars(data = clc_2018_landcover["landcover_cat"], downsample = 20)
  ggplot() + geom_stars(data = clc_2018_landcover["PropL_Grey_urban"], downsample = 20)
}

## demand a recent version of stars
if (packageVersion("stars") < "0.6.4"){
    stop("downsampling with functions is only available from vesion 0.6.4 of stars . On older versions it would silently leave us with binary data for landcover percentages")
}

## gridding landcover information
st_downsample(clc_2018_landcover |>
              select(-landcover, -landcover_cat), n = 99, FUN = mean, na.rm = TRUE) |>
  st_as_sf() |> 
  filter(!is.na(PropL_Grey_urban)) |> 
  st_transform(3035) -> Landuse_10k_sfc

## quick visualization for checking things
if (draw_plot) {
  ggplot() + geom_sf(aes(fill = green), data = Landuse_10k_sfc)
}

if(rm_intermediate) rm(clc_2018_landcover) 



## download the GBIF data FOR ALL VERTEBRATA csv = download from Gbif,
## GBIF.org (21 February 2024) GBIF Occurrence Download
## https://doi.org/10.15468/dl.7h9n3a this needs 32GB in the tmp
## directory... chose wisely (hard code to something on your system
## with the necessary space)!
temp <- "intermediate_data/gh_ignore/GBIF_dl.zip"

if(new_dl) {
    download.file("https://api.gbif.org/v1/occurrence/download/request/0008421-240216155721649.zip", temp)
}

## select columns already during import in vroom
data_GB <- vroom(temp, quote = "",
                 col_select = c(species, class, decimalLongitude,
                                 decimalLatitude, year, datasetKey),
                 show_col_types = FALSE)

probs <- problems(data_GB)
## No problmes in these selected columns!

## format GBIF data
data_GB |>
  filter(!is.na(species)) |>
  st_as_sf(coords = c(3, 4)) |>
  st_set_crs(4326) |> ## set the coordinate system as it is
  ## project into the coordinate system needed (that of the landuse data)
  st_transform(3035) -> Taxa_GB

if(rm_intermediate) rm(data_GB)  

if(draw_plot) {
Taxa_GB %>% filter(species%in%"Sciurus vulgaris") %>%
    ggplot() + geom_sf()
}

Publishers <- vroom("input_data/Focus_categories.csv")

### Merge the two datasets
full_join(Taxa_GB, Publishers, by = "datasetKey", relationship = "many-to-many") |> 
  ## keep only records with species
  filter(!is.na(species)) -> Taxa_GB_Pub

if(rm_intermediate) rm(Taxa_GB) 

Landuse_10k_sfc |> ## used to retain the geometry
  st_join(Taxa_GB_Pub) |> 
  summarise(CountT_vertebrata = n(), 
            CountT_mammalia = sum(class == "Mammalia"),
            CountT_vulgaris = sum(species == "Sciurus vulgaris"),
            CountT_carolinensis = sum(species == "Sciurus carolinensis"),
            CountT_marten = sum(species == "Martes martes"),
            .by = c("geometry", "year", "Observer", "Focus_Vert", "Focus_Mam",)) |>
  filter(!is.na(year)) |>
  complete(geometry, year, Observer, Focus_Vert, Focus_Mam,
           fill = list(CountT_vertebrata = 0,
                       CountT_mammalia = 0,
                       CountT_vulgaris = 0,
                       CountT_carolinensis = 0,
                       CountT_marten = 0)) |> 
  st_as_sf() |> 
  st_join(Landuse_10k_sfc, join=st_equals) |> ## to add landcover columns
  mutate(### Proportions within vertebrata
      PropV_carolinensis = CountT_carolinensis/CountT_vertebrata, 
      PropV_vulgaris = CountT_vulgaris/CountT_vertebrata, 
      PropV_marten = CountT_marten/CountT_vertebrata,
      ### Proportions within Mammalia
      PropM_carolinensis = CountT_carolinensis/CountT_mammalia, 
      PropM_vulgaris = CountT_vulgaris/CountT_mammalia, 
      PropM_marten = CountT_marten/CountT_mammalia,
      CountT_mammalia_log = log(CountT_mammalia),
      CountT_vertebrata_log = log(CountT_vertebrata),
      year_from_2000 = year - 2000,
      Centergrid = st_centroid(geometry), ## middlepoint of each grid cell 
      lon = st_coordinates(Centergrid)[,"X"]/1e+05, ## coordinates of these middlepoints, and make them smaller to avoid problems with the model
         lat = st_coordinates(Centergrid)[,"Y"]/1e+05) |>
  mutate(
      across(starts_with("Prop"), ~replace_na(.x, 0))) ->
      Taxa_GB_count_10km

if(rm_intermediate) rm(Taxa_GB_Pub, Landuse_10k_sfc)

if(draw_plot){
Taxa_GB_count_10km |>
  filter(year==2020 & Observer == "Citizen" & !Focus_Mam) |>
  ggplot() + geom_sf(aes(fill = log(CountT_mammalia+1)))
}

## FIXME? We are losing 22418 observations when joining them on the grid
## see Landuse_10k_sfc |> st_join(Taxa_GB_Pub) ## in l 117
if(!rm_intermediate) sum(Taxa_GB_count_10km$CountT_mammalia) - nrow(Taxa_GB_Pub)

saveRDS(Taxa_GB_count_10km, "intermediate_data/Counts.rds")


