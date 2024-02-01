library(dplyr)
library(sf)
library(stars)
library(ggplot2)


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
st_downsample(clc_2018_landcover |> select(-landcover, -landcover_cat), n = 99, FUN = mean) |>
  st_as_sf() |> 
  filter(!is.na(grey)) |> 
  st_transform(3035) -> Landuse_10k_sfc

## quick visualization for checking things
if (draw_plot) {
  ggplot() + geom_sf(aes(fill = green), data = Landuse_10k_sfc)
}

