# Citizenscience_Squirrels
Reproduzierbarer Code for the manuskript "Citizen science allows detection of effects of invasive grey squirrels on red squirrels".

This readme file describes the preparation of the data and the subsequent analysis using GLMMs.
For more information on the methods used, see the manuscript text and the R code.
For a fully reproducible analysis, R scripts must be run in numerical order. Alternatively, All the intermediate data required to run the models only are contained in this repository. 

## 1)

The first script to run is **1.Script_Grids_for_counting.R**.
This script is mainly used to prepare the 10*10 km grids across the UK in which the total number of mammals, red squirrels, grey squirrels and pine martens in each cell will be counted for each year.
It downloads the needed files from the EAA, combines the shapefiles of Great Britain and Ireland and converts them into an sf object. The main output is the RDS object 10kmgrids.rds.
The second output is the same object stored as a shapefile which is needed for figure 2 and the third output is Europe10grid.rds which is needed fort he sattelite data preparation in the second script.