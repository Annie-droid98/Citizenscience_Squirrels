# Citizenscience_Squirrels
Reproduzierbarer Code for the manuskript "Citizen science allows detection of effects of invasive grey squirrels on red squirrels".

This readme file describes the preparation of the data and the subsequent analysis using GLMMs.
For more information on the methods used, see the manuscript text and the R code.
For a fully reproducible analysis, R scripts must be run in numerical order. Alternatively, All the intermediate data required to run the models only are contained in this repository. 

## 1)

The first script to run is **1.Script_Grids_for_counting.R**.
This script is mainly used to prepare the 10*10 km grids across the UK in which the total number of mammals, red squirrels, grey squirrels and pine martens in each cell will be counted for each year.
It downloads the needed files from the EAA, combines the shapefiles of Great Britain and Ireland and converts them into an sf object. The main output is the RDS object 10kmgrids.rds.
The second output is the same object stored as a shapefile which is needed for figure 2 and the third output is Europe10grid.rds which is needed fort he satelite data preparation in the second script.


## 2)

The next step is to run script **2a.Script_Sattelitedata_preparation.R** and script **2b.counting_Mammalia.R**
  
Script 2a prepares the CLC 2018 sattelite data of the vegetation of the study area.
We have decided to combine the original 44 categories into eight new categories for the analysis (line 23-37). Then the information about the vegetation category was extracted fort he studied area and the percentage that each vegetation category makes up per grid cell is calculated.
The output of this script is Vegetation_europe_squirrels_10km.rds. This is used in the third script to characterise the vegetation of each grid cell where mammals were observed.

Script 2b prepares the data downloaded from Gbif and counts the number of red squirrels, grey squirrels, pine martens and mammalian observation in each gridcell for each year. The output ("Counts.rds") is a data frame containing all counts and the proportion of red squirrels, grey squirrels and pine martens in the total number of Mammmalia observations for each grid cell. To normalise the observation error, the proportion is used instead of the raw counts (see manuscript).
