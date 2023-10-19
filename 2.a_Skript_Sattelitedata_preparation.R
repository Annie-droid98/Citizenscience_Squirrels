rm(list=ls(all=TRUE))
install.packages("fasterize")
install.packages("exactextractr")
library(exactextractr)
library(raster)
library(fasterize)
library(sf)
library(dplyr)

Europe10grid <- readRDS("Europe10grid.rds")

#Download the raster: https://land.copernicus.eu/pan-european/corine-land-cover/clc2018?tab=download


#clc_2018_landcover <- raster("u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")
# saveRDS(clc_2018_landcover, "Landcover.rds")

# read the rds because the tiff is to large for the repository
clc_2018_landcover <- readRDS("Landcover.rds")
#clc_2018_landcover <- readAll(clc_2018_landcover)

#Squirrels 
rasterOptions(tmpdir="path")
clc_2018_landcover_squirrels <- clc_2018_landcover
#"grey urban" 

clc_2018_landcover_squirrels[clc_2018_landcover_squirrels <= 9] <- 9

#green urban
clc_2018_landcover_squirrels[clc_2018_landcover_squirrels %in% c(10, 11)] <- 11

#Agra
clc_2018_landcover_squirrels[clc_2018_landcover_squirrels %in% c(12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)] <- 22

#Forrest 23, 24, 25 stay


#26-39 other seminatural areas
clc_2018_landcover_squirrels[clc_2018_landcover_squirrels %in% c(26,27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39)] <- 39

# Waterbodies
clc_2018_landcover_squirrels[clc_2018_landcover_squirrels %in% c(40, 41, 42, 43, 44)] <- 44 # this changes all values at once

#save
writeRaster(clc_2018_landcover_squirrels, "clc_2018_landcover_categories_Squirrels")
clc_2018_landcover_squirrels <- raster("clc_2018_landcover_categories_Squirrels")

#unique(values(clc_2018_landcover_squirrels)) 
#extract
clc_2018_landcover_squirrels <- exact_extract(x = clc_2018_landcover_squirrels, y = Europe10grid)
clc_2018_landcover_squirrels_2 <- lapply(clc_2018_landcover_squirrels, function(x) x[(names(x) %in% c("value"))])

# empty vectors to store the results
clc_9_s <- vector()
clc_11_s <- vector()
clc_22_s <- vector()
clc_23_s <- vector()
clc_24_s <- vector()
clc_25_s <- vector()
clc_39_s <- vector()
clc_44_s <- vector()
# loop along the list
for (i in 1:length(clc_2018_landcover_squirrels_2)){
  tmp_s <- clc_2018_landcover_squirrels_2[[i]]
  # count each of the values
  tmp_9_s <- sum(tmp_s == 9)
  tmp_11_s <- sum(tmp_s == 11)
  tmp_22_s <- sum(tmp_s == 22)
  tmp_23_s <- sum(tmp_s == 23)
  tmp_24_s <- sum(tmp_s == 24)
  tmp_25_s <- sum(tmp_s == 25)
  tmp_39_s <- sum(tmp_s == 39)
  tmp_44_s <- sum(tmp_s == 44)
  # add the counts to a vector following the same order as the grids
  clc_9_s[i] <- tmp_9_s
  clc_11_s[i] <- tmp_11_s
  clc_22_s[i] <- tmp_22_s
  clc_23_s[i] <- tmp_23_s
  clc_24_s[i] <- tmp_24_s
  clc_25_s[i] <- tmp_25_s
  clc_39_s[i] <- tmp_39_s
  clc_44_s[i] <- tmp_44_s
}
rm(clc_2018_landcover_squirrels_2)

# put the vectors into a df
counts_df_squirrels <- cbind.data.frame(clc_9_s, clc_11_s, clc_22_s,clc_23_s, clc_24_s, clc_25_s, clc_39_s, clc_44_s)
counts_df_squirrels$CellCode <- Europe10grid$CellCode



rowSums(counts_df_squirrels[, -6]) # 10000 pixels in each grid cell
head(counts_df)


### get proportions 
prop_table_squirrels <- counts_df_squirrels[,1:8]/10000

Vegetation_europe_squirrels_10km <- cbind(prop_table_squirrels, "CELLCODE"=Europe10grid$CellCode)
saveRDS(Vegetation_europe_squirrels_10km, "Vegetation_europe_squirrels_10km.rds")
