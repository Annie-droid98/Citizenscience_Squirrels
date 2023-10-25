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
the repository directly (not in e.g. \code{R}, check using
\code{getwd()}).

## 1) Prepare grids accross the British isles: R/1_DownloadGrids.R

This downloads 10*10 km grids across the UK and Ireland from the
[European environment agency (EAA)](https://www.eea.europa.eu/en), and
combines them into an sf object. This sf object is stored in
\code{intermediate_data/10kmgrids.rds}.


## 2) Count mammals and landuse in these grids: R/2a_PrepLandUse.R and R/2b_CountGbif.R
  
Script \code{R/2a_PrepLandUse.R} prepares the [Corine landcover (CLC
2018)](https://land.copernicus.eu/en/products/corine-land-cover)
sattelite data of the landuse in study area. We have decided to
combine the original 44 categories into eight new categories for the
analysis (see also Table X). We then estimate the percentage that each
lsnduse category makes up per grid cell. The output of this script is
a \code{data.frame} object stored in
\code{intermediate_data/Landuse_10km.rds}.

Script \code{R/2b_CountGbif.R} downloas and prepares data for mammals
in the British isles from Gbif. It counts the number of red squirrels,
grey squirrels, pine martens and overall mammal observation in each
gridcell for each year. It uses
\code{input_data/SquirrelPublisherBelow1000obs.csv} to categorise the
observations into "Citzen Science", "mixed" or "scientific" and
assesses whether the respective Publisher/dataset had a focus taxon
within mammalia. The latter datasets later can not be used, as they
can not be used to "normalize" with overall mammal counts. The output
of this is an sf object stored in \code{intermediate_data/Counts.rds).

## 3) Merge counts and landuse, plot and tabulate: R/3_MergePlotPrep.R
