library(dplyr)
library(sf)
library(stars)

## do we want to repeat the grid download?
redoGRIDdownload  <- FALSE

if (redoGRIDdownload) {
    source("R/1_DownloadGrids.R")
} else {
    Britain10grid <- read_sf("intermediate_data/10kmgrids.shp")
}


## do we want to plot things as we got (for checking purposes)
draw_plot <- TRUE

if (draw_plot) {
  library(ggplot2)
  }

### LANDCOVER 

## ## We downloaded the file from (requires login)
## ## https://land.copernicus.eu/pan-european/corine-land-cover/clc2018?tab=download
## ## selecting:
## ##  - CORINE Land Cover 2018 (vector/raster 100 m), Europe, 6-yearly - Geotiff (NUTS: United Kingdom)
## ##  - CORINE Land Cover 2018 (vector/raster 100 m), Europe, 6-yearly - Geotiff (NUTS: Éire/Ireland) 

## read the tiff
clc_2018_landcover <- read_stars("input_data/U2018_CLC2018_V2020_20u1.tif")

## make CRS the same for the two spatial object that will be combined
clc_2018_landcover <- st_transform(clc_2018_landcover, st_crs(Britain10grid))

## rename layer
names(clc_2018_landcover) <- "landcover"

## recode landcover values into a single qualitative layer and 
## into one binary layer per category (slowish, but under 1 min)
clc_2018_landcover |> 
  mutate(landcover_cat = case_when(landcover %in% c(01:09) ~ "Grey urban",
                                   landcover %in% c(10:11) ~ "Green urban",
                                   landcover %in% c(12:22) ~ "Aggricultural",
                                   landcover == 23         ~ "Broad-leaved forest",
                                   landcover == 24 ~ "Coniferous forest",
                                   landcover == 25 ~ "Mixed forest",
                                   landcover %in% c(26:39) ~ "Semi natural areas",
                                   landcover %in% c(40:44) ~ "Waterbodies",
                                   TRUE ~ NA
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

## compute proportion of coverage for each landcover type within each grid cell polygon defined by `Britain10grid`
## for this we compute, for each binary layer, the mean per grid cell polygon since that directly provide the proportion
## (very slow: takes a 6 hours or so)
clc_2018_landcover |> 
  #st_downsample(1000) |> ## downsampling, for trials only!!!
  select(-landcover, -landcover_cat) |>  ## select all layers but `landcover` and `landcover_cat`
  aggregate(by = Britain10grid, FUN = mean) -> Landuse_10k_stars ## compute mean per grid cell in `Britain10grid`

## turn rasters into simple feature collection with all info from `Britain10grid`
Landuse_10k_stars |>
  st_as_sf() |> 
  filter(!is.na(grey)) |> 
  st_join(Britain10grid) -> Landuse_10k_sfc

## write outcome to the intermediate data
st_write(Landuse_10k_sfc, "intermediate_data/Landuse_10km.shp", append = FALSE)

## quick visualization for checking things
if (draw_plot) {
  Landuse_10k_sfc |> select(grey:water) -> Landuse_10k_sfc_for_plot
  plot(Landuse_10k_sfc_for_plot) ## basic plot of all layers
  ggplot() + geom_sf(aes(fill = green), data = Landuse_10k_sfc_for_plot) ## plot sf object
  ggplot() + geom_stars(data = Landuse_10k_stars["green"]) ## plot stars object
}

