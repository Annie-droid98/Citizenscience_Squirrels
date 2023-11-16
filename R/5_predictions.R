library(dplyr)
library(sf)
library(ggmap)

# Figure 4

Values_predictions <- list(0.0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,1)
Values_for_caro <-lapply((Values_predictions), function(i){
  rep(c(i),times=4)
})

Values_for_caro_2 <- as.data.frame(unlist(Values_for_caro))

Values_for_Greyurban <- as.data.frame(rep(c(0.0, 0.3, 0.6, 0.9),times=11))
Values_for_Agrar <- as.data.frame(rep(rev(c(0.1, 0.4, 0.7,1)),times=11))
Predictiondf_1 <- cbind(Values_for_caro_2, Values_for_Greyurban, Values_for_Agrar)
names(Predictiondf_1) <- c("PropT_carolinensis", "PropL_Grey_urban", "PropL_Agricultural")
#vulgaris Gebiet
Predictiondf_2 <- Predictiondf_1%>%
  mutate(year_from_2000 = 18)%>%
  mutate(PropL_Green_urban = 0.00)%>%
  mutate(PropL_Broadleafed_Forest = 0.00)%>%
  mutate(PropL_Coniferous_Forest = 0.00)%>%
  mutate(PropL_Mixed_Forest = 0.00)%>%
  mutate(PropL_Other_seminatural = 0.00)%>%
  mutate(Waterbodies_z = 0.00)%>%
  mutate(PropT_marten = 0.00)%>%
  mutate(lat = 3615)%>%
  mutate(lon = 3185) %>%
  mutate(CountT_mammalia_log = log(100))

Pseudodata_new <- Predictiondf_2                                         # Replicate data
Pseudodata_new$Grey_urban_z2 <- factor(Pseudodata_new$PropL_Grey_urban, 
                                       labels = c("0% Grey urban", "30% Grey urban", 
                                                  "60% Grey urban","90% Grey urban"))

#prediction werden noch mit einem einzelnen fit gemacht, da er ein Objekt in der Liste von fits nicht nehmen wollte
predictionsgrey <- Pseudodata_new %>%
  transform(predictions=predict(fit_vulgaris_2_try, newdata=Pseudodata_new, type = "response"))
png(filename= "Predictiondifferentcarolinensisgreyurban.png")

predictionsgrey %>%
  # transform(predictions=predict(result[[1]], newdata=Pseudodata_new, type = "response")) %>% 
  ggplot(aes(PropT_carolinensis, predictions, colour =Grey_urban_z2, group = Grey_urban_z2)) +
  geom_line()+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
  # geom_ribbon(aes(ymin=respVar_int.respVar_0.025,ymax=respVar_int.respVar_0.975),alpha=0.3)+
  # scale_x_continuous(breaks = c(0.00 ,0.20, 0.40, 0.60,0.80,1.00),
  #labels = c("0", "20", "40", "60","80","100"))+
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


#plotpseudonew +  facet_wrap(. ~ Grey_urban_z2,ncol=2)
dev.off()


### Prediction map

CountALL_10km <- readRDS("CountALL_10km.rds")
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