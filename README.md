# Citizenscience_Squirrels

Reproducible code for the manuscript "Citizen science allows detection
of effects of invasive grey squirrels on red squirrels".

This readme file describes the preparation of the data and the
subsequent analysis using GLMMs.  For more information on the methods
used, see the manuscript text and the R code. 

For a full reproduction of our analysis, R scripts can be run in
numerical order. Alternatively, all the intermediate data required to
run each script on its own are contained in this repository. We follow
the convention, that your R scripts should run in the root folder of
the repository directly (not in e.g. `\R`, check using `getwd()`).

## 1) Prepare grids across the British isles: R/1_DownloadGrids.R

This downloads 10*10 km grids across the UK and Ireland from the
[European environment agency (EAA)](https://www.eea.europa.eu/en), and
combines them into an sf object. This sf object is stored in
`intermediate_data/10kmgrids.rds`.


## 2a) Summarise landuse in grids: R/2a_PrepLandUse.R
  
This prepares the [Corine landcover (CLC
2018)](https://land.copernicus.eu/en/products/corine-land-cover)
satellite data of the landuse in study area. We have decided to
combine the original 44 categories into eight new categories for the
analysis (see also Table X). We then estimate the percentage that each
landuse category makes up per grid cell. The output of this script is
a `data.frame` object stored in `intermediate_data/Landuse_10km.rds`.

## 2b) Count Gbif observations in grids R/2b_CountGbif.R

This downloads and prepares data for mammals in the British isles from
Gbif. It counts the number of red squirrels, grey squirrels, pine
martens and overall mammal observation in each grid cell for each
year. It uses `input_data/SquirrelPublisherBelow1000obs.csv` to
categorise the observations into "Citizen Science", "mixed" or
"scientific" and assesses whether the respective Publisher/dataset had
a focus taxon within mammalia. The latter datasets later can not be
used, as they can not be used to "normalize" with overall mammal
counts. The output of this is an sf object stored in
`intermediate_data/Counts.rds`.

## 3) Merge counts and landuse, plot and tabulate: R/3_MergePlotPrep.R

This merges the independently reproducible characterization of landues
(2a) and counting of mammal observations (2b) and writes the resulting
sf object to `intermediate_data/CountALL_10km.rds`.

This sf object contains the following columnes:

- "CELLCODE": a unique code for each 10x10km grid cell, `character`
- "geometry": a geocoordinate polygon for each 10x10km grid cell, `sfc_polygon`
- "Centergrid": the geocoordinate of the centerpoint of the grid cell, `sfc_POINT` 
- "lon": longitude, `numeric`
- "lat": latitude, `numeric`
- "year": calendar year (Gregorian calendar), `numeric`
- "year_from_2000": year since 2000 `numeric`
- "Observer": Categroy of observer with levels: "Citizen" "Mixed" and
  "Scientific", contains NA for uncategorzied publishers `character`
- "FocusTaxaTorF": Did the publisher's observations focus on a taxon
  within the mamallia. Observations with a focus can't be normalized
  and are ommited for modelling, `logical`
- "CountT_mammalia" , "CountT_vulgaris" , "CountT_carolinensis" ,
  "CountT_marten": Counts for taxa within respective grids, `numeric`
- "CountT_mammalia_log": log of the counts for mammalia, `numeric`
- "PropT_carolinensis" , "PropT_vulgaris" , "PropT_marten":
  Proportions of counts within mammalia, `numeric`
- "L_Grey_urban", "L_
Green_urban", "L_Agricultural",
  "L_Broadleafed_Forest", "L_Coniferous_Forest", "L_Mixed_Forest",
  "L_Other_seminatural", "L_Waterbodies": Number of pixels for each
  landuse type in sattelite data of each grid `numeric`
- "allLand": Total number of counted pixels in landuse sattelite data
  (should be 10000, in complete 10x10km grids with 100x100m
  resolution), `numeric`
- "PropL_Grey_urban", "PropL_Green_urban", "PropL_Agricultural",
  "PropL_Broadleafed_Forest", "PropL_Coniferous_Forest",
  "PropL_Mixed_Forest", "PropL_Other_seminatural",
  "PropL_Waterbodies": Proportion of landuse category in overall
  landuse in the grid. `numeric`
- "allPropL": sum of all proportions, should be 1, `numeric`

We write this sf object into `intermediate_data/CountALL_10km.rds` it
is the basis for all further analysis. We visualise basics
characteristics of this data (*Figures 1 and 2 and table X of the
manuscript*) with the code in the second part of this script.


## 4) Model the observations of red and grey squirrels: R/4_Model.R in spaMM

This scripts subsets `intermediate_data/CountALL_10km.rds` to obtain
only data collected by citizen ("citizen science") without a taxonomic
focus within mammalia. The latter is important as we use the number of
total observations of mammals within a grid to normalize for
"observation effort".
