library(dplyr)
library(sf)
library(ggmap)


## We need the original count data this is in the repository or you
## could count again!
reCount <- FALSE

if(reCount) {
    source("R/2b_CountGbif.R")
} else {
    CountALL_10km <- readRDS("intermediate_data/CountALL_10km.rds")
}


## We need the models! These are too large for github storage
## Do you want to rerun them?
reRunModels <- FALSE

## If you want to read them from files, that's where they shoudl be:
modelFile <- "intermediate_data/gh_ignore/Citizenscience_modelle.rds"
modelFileCaro <- "intermediate_data/gh_ignore/Citizensciencemodelle_caro.rds"

if(!reRunModels) {
    if(exists("result") & exists("result_caro")) {
        message("Using models in your current environment")
    } else {
        if(file.exists(modelFile) &
           file.exists(modelFileCaro)){
            message("Reading models from local files")
            results <- readRDS(modelFile)
            results_caro <- readRDS()
        } else {
            stop("Download the models from Zenodo ",
                 "using this link: https://zenodo.org/records/10141957  ",
                 "to intermediate_data/gh_ignore/ and re-run this code")
        }
    }
} else {
    message("Re-running models, this will take some time")
    message("They'll be saved in ", modelFile, " and ", modelFileCaro)
    source("R/4_Model.R")
}

Predictiondf <- data.frame(
    PropT_carolinensis = rep(seq(0, 1, 0.1), each = 4)) %>%
    mutate(PropL_Grey_urban = rep(c(0.0, 0.3, 0.6, 0.9), times=11),
           PropL_Agricultural = rep(c(1, 0.7, 0.4, 0.1), times=11),
           year_from_2000 = 18, 
           PropL_Green_urban = 0.00, 
           PropL_Broadleafed_Forest = 0.00, 
           PropL_Coniferous_Forest = 0.00, 
           PropL_Mixed_Forest = 0.00, 
           PropL_Other_seminatural = 0.00, 
           PropL_Waterbodies = 0.00, ## error in initial code?
           PropT_marten = 0.00, 
           lat = 3615,
           lon = 3185,
           CountT_mammalia_log = log(100)) %>%
    mutate(Grey_urban_plot = factor(PropL_Grey_urban, 
                                    labels = c("0% Grey urban", "30% Grey urban", 
                                               "60% Grey urban","90% Grey urban")),
           ## We predict using the first element in the results list
           ## of models! This is the main model (the following models
           ## are for/from lrs testing
           predictions = as.vector(predict(result[[1]],
                                           newdata=. ,
                                           type = "response")))


Predictiondf %>%
    ggplot(aes(PropT_carolinensis, predictions,
               colour = Grey_urban_plot,
               group = Grey_urban_plot)) +
    geom_line() +
    scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
    ## ## we wanted confidence inervals at some point, but dropped the idea?!
    ## geom_ribbon(aes(ymin=respVar_int.respVar_0.025,
    ##                 ymax=respVar_int.respVar_0.975),
    ##                 alpha=0.3)+
    ## scale_x_continuous(breaks = c(0.00 ,0.20, 0.40, 0.60,0.80,1.00),
    ## labels = c("0", "20", "40", "60","80","100"))+
    theme_bw()+
    xlab("Number of S.carolinensis")+
    ylab("Predicted number of S.vulgaris")+
    labs(x=expression(paste("Proportion of ",italic("S.carolinensis"))))+
    labs(y=expression(paste("Predicted number of ",italic("S.vulgaris"))))+
    labs(colour="Percentage of grey urban area")+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),legend.key.size = unit(2, 'cm'))
## plotpseudonew +  facet_wrap(. ~ Grey_urban_z2,ncol=2)

ggsave("figures/Predictiondifferentcarolinensisgreyurban.pdf", width=9, height=5)
dev.off()


### Prediction map

d_2018 <- CountALL_10km %>% filter(Observer%in%"Citizen"&
                                     !FocusTaxaTorF)%>%
  filter(year == 2018)%>%
  mutate(AllMammalia = 4.60517)
d_2018[is.na(d_2018)] <- 0

Predictions_citizenscience <- d_2018 %>%
  transform(predictions_vulgaris =predict(fit_vulgaris_2_try, newdata= d_2018, type = "response"))

Predictions_citizenscience <-Predictions_citizensciencelog() %>%
  transform(predictions_caro=predict(fits_carolinensis, newdata= df_10kmzeros_2, type = "response"))

#transform the coordinates back to plot the map
Predictions_retransformed  <- Predictions_citizenscience %>%
  mutate(lon = lon*100000, lat = lat*100000)%>%
  sf::st_as_sf(coords = c(16,17))%>%
  st_set_crs(3035)%>%
  st_transform(4326)

Predictions_retransformed <- Predictions_retransformed %>%
  dplyr::mutate(long = sf::st_coordinates(Predictions_retransformed)[,1],
                lat = sf::st_coordinates(Predictions_retransformed)[,2])  

png(filename = "Predictionmap.png",units="in", width=8, height=8, res=300)

#ggmap(myMap_B_toner_2) +
ggplot()+
  geom_point(data=Predictions_retransformed, aes(x=long, y=lat, color = predictions_vulgaris))+
  labs(color = "Predicted S.vulgaris observations")
# scale_colour_gradientn(
#  limits  = range(log(Predictions_retransformed$predictions_vulgaris)),
# colours = colours[c(1, seq_along(colours), length(colours))],
#values  = c(0, scales::rescale(colour_breaks, from = range(Predictions_retransformed$predictions_vulgaris)), 1),
#  )+
# scale_colour_continuous(trans = "log10", type = "viridis")+
theme(legend.key.size = unit(1.8, 'cm'),text = element_text(size = 12))
#scale_fill_gradient(name = "count", trans = 'log10',
#         breaks = c(0,15,30,45,60,75,90,105,120,135,150), labels = c(0,15,30,45,60,75,90,105,120,135,150))

dev.off()


png(filename = "Predictionmap_caro.png",units="in", width=8, height=8, res=300)

#ggmap(myMap_B_toner_2) +
ggplot()+
  geom_point(data=Predictions_retransformed, aes(x=long, y=lat, color = predictions_carolinensis))+
  labs(color = "Predicted S.carolinensis observations")+
  # scale_colour_continuous(trans = "log10", type = "viridis")+
  theme(legend.key.size = unit(1.8, 'cm'),text = element_text(size = 12))

dev.off()
