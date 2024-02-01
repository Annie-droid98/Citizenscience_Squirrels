library(ggplot2)
library(ggtext)
library(tidyr)
library(dplyr)

## merge with the prepeared sattelitedata
redoLanduse <- FALSE
redoCount <- FALSE

# if(redoLanduse){
#     source("R/2a_PrepLandUse.R")
# } else {
#     Landuse_10km <- readRDS("intermediate_data/Landuse_10km.rds")
# }

if (redoCount) {
    source("R/2b_CountGbif.R")
} else{
    Mammalia_GB_count_10km <- readRDS("intermediate_data/Counts.rds")
}

###################### Figure 1 and 2

#Figure 1

Mammalia_GB_count_10km |>
  summarize(Sum_Mammalia = sum(CountT_mammalia),
            Sum_S.vulgaris = sum(CountT_vulgaris),
            Sum_S.carolinensis = sum(CountT_carolinensis),
            Sum_M.martes = sum(CountT_marten),
            Cells_Mammalia = sum(CountT_mammalia > 0),
            Cells_S.vulgaris = sum(CountT_vulgaris > 0),
            Cells_S.carolinensis = sum(CountT_carolinensis > 0),
            Cells_M.martes = sum(CountT_marten > 0),
            .by = c("Observer", "FocusTaxaTorF")) |>
  drop_na() |>
  ggplot(aes(fill = FocusTaxaTorF, y = Sum_Mammalia, x = Observer)) + 
  geom_bar(position = "stack", stat = "identity", width = 0.5) +
  scale_fill_viridis(discrete = TRUE) +
  coord_flip() +
  ## scale_x_discrete(
  ##     labels=c("Citizen science", "Mixed", "Scientific")
  ## ) +
  theme_minimal() +
  xlab("") +
  ylab("Number of Observations") +
  guides(fill = guide_legend(title = "Has focus taxon")) +
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 1.48e+05, ymax = 5.8e+05,
           alpha = 0, color = "red") +
  theme(legend.position = c(0.8, 0.8),  #  position within the plot
        legend.background = element_rect(color = "black", fill = "white"),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20)) -> Fig1a

CountALL_10km |>
  as_tibble() |>
  group_by(Observer, FocusTaxaTorF, year) |>
  summarize(Sum_Mammalia = sum(CountT_mammalia),
            Sum_S.vulgaris = sum(CountT_vulgaris),
            Sum_S.carolinensis = sum(CountT_carolinensis),
            Sum_M.martes = sum(CountT_marten),
            ## not using htese for now but might be interesting to
            ## look at
            Cells_Mammalia = sum(CountT_mammalia > 0),
            Cells_S.vulgaris = sum(CountT_vulgaris > 0),
            Cells_S.carolinensis = sum(CountT_carolinensis > 0),
            Cells_M.martes = sum(CountT_marten > 0)) |>
  pivot_longer(cols = Sum_Mammalia:Cells_M.martes, 
               names_to = c("measure", "Taxon"),
               names_sep = "_", 
               values_to = "Observations") |>
  ## I hate this mess for plotting ;-)
  ## the spaces after the \\. are too much for now :-)
  mutate(Taxon = gsub("(M\\.martes|S\\.carolinensis|S\\.vulgaris)", 
                      "italic(\'\\1\')", Taxon)) |>
  ## to drop the few observations with NA, in taxon focus
  drop_na() |>
  ## not the numbr of cells with counts for now but might be
  ## interestin to look at?!
  filter(measure %in% "Sum") |>
  ggplot(aes(x = year, y = Observations, color = Observer,
             linetype = FocusTaxaTorF)) +
  geom_line() +
  ## scale_y_log10(labels = comma_format(big.mark = ".",
  ##                                     decimal.mark = ",")) +
  facet_wrap(~Taxon, scales = "free_y", labeller = label_parsed) + 
  ylab("Number of observations") +
  guides(linetype = guide_legend(title = "Has focus taxon")) +
  theme_minimal() +
  theme(legend.position = c(0.15, 0.3),  #  position within the plot
        legend.background = element_rect(color = "black", fill = "white"),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 18)) -> Fig1b
    

ggpubr::ggarrange(Fig1a, Fig1b,
                  labels = c("A", "B"),
                  ncol = 2, nrow = 1)

ggsave("figures/Fig1.png", width = 18, height = 8)


## ##Figure 2

## writeRaster(clc_2018_landcover_squirrels, "clc_2018_landcover_categories_Squirrels")
## clc_2018_landcover_squirrels <- raster("clc_2018_landcover_categories_Squirrels")

## df_Mammalia_GB_count<- readRDS("CountforFigure2.rds")

## Ausschnitt_GB <-extent(3542199, 3660000, 3140000, 3220000)
## crop_vegetationsquirrel_Ausschnitt <- crop(x = clc_2018_landcover_squirrels, Ausschnitt_GB)

## crop_Grid_Ausschnitt <- st_crop(x = df_Mammalia_GB_count, Ausschnitt_GB)

## crop_Grid_Ausschnitt_2018 <- crop_Grid_Ausschnitt|>
##   filter(year == 2018)

## crop_Grid_Ausschnitt_2018_try <- crop_Grid_Ausschnitt_2018|>
##   filter(lat < 3220000 & lat> 3140000 & lon < 3660000)
## cuts_s=c(1,9,11,22,23,24,25,39,44)
## crop_GB_and_IE_grid_10km_shp <- crop(x = GB_and_IE_grid_10km_shp, Ausschnitt_GB)

## GB_Maße <- raster::extent(2800000,3930000,2880000,4300000)
## r <- raster()
## extent(r) <- GB_Maße

## Squirrels_map_GB_crop <- crop(x = clc_2018_landcover_squirrels, y = GB_Maße)

## Ausschnit_Ed <- extent(3400000,3520000,3680000,3760000)
## crop_vegetationsquirrel_Ausschnitt_Ed <- crop(x = clc_2018_landcover_squirrels, Ausschnit_Ed)

## crop_Grid_Ausschnitt_Ed <- st_crop(x = Mammaliacount_10km, Ausschnit_Ed)
## crop_Grid_Ausschnitt_Ed_2018 <- crop_Grid_Ausschnitt_Ed|>
##   filter(year == 2018)

## crop_Grid_Ausschnitt_2018_try_Ed <- crop_Grid_Ausschnitt_Ed_2018 |>
##   filter(lat < 3765000 & lat> 3675000 & long < 3525000 & long > 3395000)

## crop_GB_and_IE_grid_10km_shp_Ed <- crop(x = GB_and_IE_grid_10km_shp, Ausschnit_Ed)


## #average grid based on citizen science observations
## df_counted_Mammalia <- readRDS("Counts.rds")


## df_counted_Mammalia_try <- df_counted_Mammalia|>
##   dplyr::select(CELLCODE, Grey_urban,green_urban, Agrar,Broadleafed_Forest,Coniferous_Forest, Mixed_Forest,Other_seminatural, Waterbodies)

## df_counted_Mammalia_try_2 <- unique(df_counted_Mammalia_try)
## df_counted_Mammalia_try_2 <- colMeans(df_counted_Mammalia_try_2[2:9])|>
##   as.data.frame()

## df_counted_Mammalia_try_2<-tibble::rownames_to_column(df_counted_Mammalia_try_2, "Vegetationtype")
## colnames(df_counted_Mammalia_try_2)<- c("Landcover","Proportion")
## df_counted_Mammalia_try_2 <- df_counted_Mammalia_try_2|> 
##   mutate(Year = "of total area")
## df_counted_Mammalia_try_2 <- df_counted_Mammalia_try_2|> 
##   mutate_at(vars(Proportion),
##             .funs = funs(. * 100))
## df_counted_Mammalia_try_3 <- df_counted_Mammalia_try_2|>
##   mutate_at(vars(Proportion),
##             funs(round(., 1)))

## Mammalia_in_GB <- Mammalia_citizenscience|>
##   dplyr:: select(species, year, long, lat)|>
##   as.data.frame()
## #Observations in each landcover type
## Mammalia_in_Gb_2 <- SpatialPointsDataFrame(coords = Mammalia_in_GB[,3:4], data =Mammalia_in_GB)
## extract_Mammalia <- raster:: extract(clc_2018_landcover_squirrels ,Mammalia_in_Gb_2 )
## Values_Mammalia <- cbind(Mammalia_in_Gb_2,extract_Mammalia)
## Values_Mammalia_df <- as.data.frame(Values_Mammalia)
## Values_Mammalia_df$Vegetation <-Values_Mammalia_df$c.39..44..11..9..22..9..22..9..22..9..9..9..39..9..22..9..11.. 
## Vegetation_of_each_obs <- Values_Mammalia_df|>
##   count(Vegetation,  sort = F)            
## Percentage_obs_inVeg <-transform(Vegetation_of_each_obs, Percentage_Mamm = Vegetation_of_each_obs[2]/colSums(Vegetation_of_each_obs[2]))
## Percentage_obs_inVeg<- Percentage_obs_inVeg[-c(9),]|>
##   mutate(Year = "of observations")|>
##   mutate_at(vars(n.1),
##             .funs = funs(. * 100))|>
##   mutate_at(vars(n.1),
##             funs(round(., 1)))
## Percentage_obs_inVeg$Landcover <- c("Grey_urban", "green_urban", "Agrar", "Broadleafed_Forest", "Coniferous_Forest", "Mixed_Forest","Other_seminatural","Waterbodies")

## Percentage_obs_inVeg_try<-Percentage_obs_inVeg|>
##   dplyr::select(Landcover,n.1, Year)
## Percentage_obs_inVeg_try<- Percentage_obs_inVeg_try|>
##   rename(Proportion = n.1)
## perc_try <-rbind(df_counted_Mammalia_try_3,Percentage_obs_inVeg_try)
## perc_try_3 <-perc_try
## perc_try_3$Year <-factor(perc_try_3$Year, levels = c("of total area", "of observations"))
## perc_try_4 <- transform(perc_try_3, Year_num = ifelse(Year == "of observations",
##                                                       as.numeric(factor(Year)) - .25,
##                                                       as.numeric(factor(Year)) + .25) )
## pdf("EineMap_Inseln.pdf", width= 11, height=14)
## plot(crop_vegetationsquirrel_Ausschnitt,  legend = FALSE, breaks=cuts_s, col= c("#737373","#addd8e","#fec44f","#005a32","#8c2d04","#88419d","#dd3497","#0c2c84"),xaxt = "n", yaxt = "n")
## #axis(1, at = c(3540000, 3570000, 3600000, 3630000, 3660000))
## #axis(2, at = c(3125000, 3150000, 31750000, 3200000, 3225000))
## text(x = crop_Grid_Ausschnitt_2018_try$lon,
##      y = crop_Grid_Ausschnitt_2018_try$lat,
##      labels = crop_Grid_Ausschnitt_2018_try$AllMammalia, col = "black", font = 2, cex=2.5)
## plot(crop_GB_and_IE_grid_10km_shp , add=T)

## plot(Squirrels_map_GB_crop,  legend = FALSE, breaks=cuts_s, col= c("#737373","#addd8e","#fec44f","#005a32","#8c2d04","#88419d","#dd3497","#0c2c84"),xaxt = "n", yaxt = "n")
## #xlim=c(2900000,4500000), ylim=c(3000000,4300000)
## plot(crop_vegetationsquirrel_Ausschnitt_Ed,  legend = FALSE, breaks=cuts_s, col= c("#737373","#addd8e","#fec44f","#005a32","#8c2d04","#88419d","#dd3497","#0c2c84"), xaxt = "n", yaxt = "n") # ann
## #xlim=c(3380000,3520000),ylim=c(3680000,3760000)
## text(x = crop_Grid_Ausschnitt_2018_try_Ed$lon,
##      y = crop_Grid_Ausschnitt_2018_try_Ed$lat,
##      labels = crop_Grid_Ausschnitt_2018_try_Ed$AllMammalia, col = "black", font = 2, cex=2.5)
## plot(crop_GB_and_IE_grid_10km_shp_Ed, add=T)

## dev.off()

## pdf("Barplot_Inseln.pdf", width= 7, height=5.5)
## ggplot(perc_try_4, aes(x = Year, y = Proportion, fill = Landcover)) + 
##   geom_bar(width = 0.5,stat = "identity") +
##   # scale_x_discrete(limits = c("of total area", "of observations")) +
##   geom_line( aes(x = Year_num), 
##              position = position_stack())+
##   geom_text(aes(label = paste0(Proportion, "%")),
##             position = position_stack(vjust = 0.5), size = 7) +
##   scale_fill_manual(values = c("#fec44f","#005a32","#8c2d04","#addd8e","#737373","#88419d","#dd3497","#0c2c84"),
##                     labels = c("Agricultural", "Broadleafed forest","Coniferous forest","Green urban",
##                                "Grey urban","Mixed forest","Other seminatural", "Waterbodies"))+
##   # scale_fill_brewer(palette = "Set1") +
##   theme_minimal(base_size = 16) +
##   ylab("Percentage") +
##   xlab(NULL)+
##   theme(plot.title = element_text(size = rel(2.4),face ='bold'))

## dev.off()

