library(dplyr)
library(sf)
library(stars)
library(ggplot2)
library(vroom)
library(tidyr)

## do we want to plot things as we got (for checking purposes)
draw_plot <- TRUE

### LANDCOVER 

## ## We downloaded the file from (requires login)
## ## https://land.copernicus.eu/pan-european/corine-land-cover/clc2018?tab=download
## ## selecting:
## ##  - CORINE Land Cover 2018 (vector/raster 100 m), Europe, 6-yearly - Geotiff (NUTS: United Kingdom)
## ##  - CORINE Land Cover 2018 (vector/raster 100 m), Europe, 6-yearly - Geotiff (NUTS: Éire/Ireland) 

## read the tiff
clc_2018_landcover <- read_stars("input_data/U2018_CLC2018_V2020_20u1.tif")

## rename layer
names(clc_2018_landcover) <- "landcover"

## recode landcover values into a single qualitative layer and 
## into one binary layer per category (slowish, but under 1 min)
clc_2018_landcover |> 
  mutate(landcover_cat = case_match(landcover,
                                    c(01:09) ~ "Grey urban",
                                    c(10:11) ~ "Green urban",
                                    c(12:22) ~ "Aggricultural",
                                    23       ~ "Broad-leaved forest",
                                    24       ~ "Coniferous forest",
                                    25       ~ "Mixed forest",
                                    c(26:39) ~ "Semi natural areas",
                                    c(40:44) ~ "Waterbodies",
                                    .default = NA
                                    )) |>
  mutate(grey = landcover_cat == "Grey urban",
         green = landcover_cat == "Green urban",
         aggri = landcover_cat == "Aggricultural",
         brdlvforest = landcover_cat == "Broad-leaved forest",
         conifforest = landcover_cat == "Coniferous forest",
         mixedforest = landcover_cat == "Mixed forest",
         seminat = landcover_cat == "Semi natural areas",
         water = landcover_cat == "Waterbodies") -> clc_2018_landcover

clc_2018_landcover

## quick visualization for checking things
if (draw_plot) {
  ggplot() + geom_stars(data = clc_2018_landcover["landcover_cat"], downsample = 20)
  ggplot() + geom_stars(data = clc_2018_landcover["grey"], downsample = 20)
}

## gridding landcover information
st_downsample(clc_2018_landcover |>
              select(-landcover, -landcover_cat), n = 99, FUN = mean) |>
  st_as_sf() |> 
  filter(!is.na(grey)) |> 
  st_transform(3035) -> Landuse_10k_sfc

## quick visualization for checking things
if (draw_plot) {
  ggplot() + geom_sf(aes(fill = green), data = Landuse_10k_sfc)
}


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

rm(Mammalia_GB_Pub)

saveRDS(Mammalia_GB_count_10km, "intermediate_data/Counts.rds")



