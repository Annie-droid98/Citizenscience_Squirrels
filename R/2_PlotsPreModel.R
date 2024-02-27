library(ggplot2)
library(ggtext)
library(tidyr)
library(dplyr)
library(viridis)
library(cowplot)
library(patchwork)
library(ggalluvial)
library(RColorBrewer)


## prepare the data from scratch
redoDataPrep <- TRUE

if (redoDataPrep) {
    source("R/1_data_prep.R")
} else{
    Taxa_GB_count_10km <- readRDS("intermediate_data/Counts.rds")
}

###################### Figure 1 and 2


#Figure 1

Taxa_GB_count_10km |>
  as_tibble() |>
  ## to drop the few observations with NA, in taxon focus
  summarize(Sum_Mammalia = sum(CountT_mammalia),
            Sum_S.vulgaris = sum(CountT_vulgaris),
            Sum_S.carolinensis = sum(CountT_carolinensis),
            Sum_M.martes = sum(CountT_marten),
            Cells_Mammalia = sum(CountT_mammalia > 0),
            Cells_S.vulgaris = sum(CountT_vulgaris > 0),
            Cells_S.carolinensis = sum(CountT_carolinensis > 0),
            Cells_M.martes = sum(CountT_marten > 0),
            .by = c("Observer", "FocusTaxaTorF")) |>
  drop_na()|> 
  ggplot(aes(fill = FocusTaxaTorF, y = Sum_Mammalia, x = Observer)) + 
  geom_bar(position = "stack", stat = "identity", width = 0.5) +
  scale_fill_viridis(discrete = TRUE,
                     labels= c("W/O focus taxon", "With focus taxon")) +
  coord_flip() +
  theme_minimal() +
  xlab("") +
  ylab("Number of Observations") +
  guides(fill = guide_legend(title = "")) +
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 1.48e+05, ymax = 5.8e+05,
           alpha = 0, color = "red") +
  annotate("text", x = 1.54, y = 2.1e+05, color = "black",
            label = "Used in this study", size=5) +
  theme(legend.position = c(0.8, 0.8),  #  position within the plot
        legend.background = element_rect(color = NA, fill = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 14)) -> Fig1a

Taxa_GB_count_10km |>
  as_tibble() |>
  group_by(Observer, FocusTaxaTorF, year) |>
  summarize(Sum_Mammalia = sum(CountT_mammalia),
            Sum_S.vulgaris = sum(CountT_vulgaris),
            Sum_S.carolinensis = sum(CountT_carolinensis),
            Sum_M.martes = sum(CountT_marten),
            ## not using these for now but might be interesting to
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
  ## not the number of cells with counts for now but might be
  ## interestin to look at?!
  filter(measure %in% "Sum") |>
  drop_na()|> 
  ggplot(aes(x = year, y = Observations, color = Observer,
             linetype = FocusTaxaTorF)) +
  geom_line() +
  ## scale_y_log10(labels = comma_format(big.mark = ".",
  ##                                     decimal.mark = ",")) +
  facet_wrap(~Taxon, scales = "free_y", labeller = label_parsed) + 
  ylab("Number of observations") +
  guides(linetype = guide_legend(title = "")) +
  scale_linetype_discrete(labels = c("W/O focus taxon", "With focus taxon")) +
  theme_minimal() +
  theme(legend.position = c(0.15, 0.3),  #  position within the plot
        legend.background = element_rect(color = NA, fill = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 14)) -> Fig1b
    

Fig1 <- ggpubr::ggarrange(Fig1a, Fig1b,
                  labels = c("a", "b"),
                  ncol = 2, nrow = 1)

ggsave("figures/Fig1.png", Fig1, width = 22, height = 11, bg="white")


## ##Figure 2

## Create a palette for our landscape categories
reds <- brewer.pal(n = 9, name = "YlOrRd")[c(6, 8)]
greens <- brewer.pal(n=9, name="Greens")[c(5, 7, 9)]
browns <- brewer.pal(n = 9, name = "BrBG")[c(2, 4)]
blue <- brewer.pal(n = 9, name = "Blues")[c(6, 3)]


mypal <- c(reds, greens, browns, blue)
mypal <- mypal[c(2, 1, 7, 6, 3, 4, 5, 8, 9)]

clc_2018_landcover$landcover_cat[is.na(clc_2018_landcover$landcover_cat)] <- "Ocean"

clc_2018_landcover$landcover_cat <-
    factor(clc_2018_landcover$landcover_cat,
           levels = c("Grey urban", "Green urban",
                      "Agricultural", "Semi-natural areas",
                      "Broadleaf forest", "Mixed forest", "Coniferous forest",
                      "Waterbodies", "Ocean"))

## Mammal counts for each grid overall for years
Ma_counts <- Taxa_GB_count_10km |>
  filter(Observer == "Citizen" & FocusTaxaTorF) |>
  summarise(All_mammalia = sum(CountT_mammalia),
            .by = "geometry") |>
  st_transform(crs=st_crs(clc_2018_landcover)) 

## manually setting the box around Edinburgh and Glasgow to match the
## grid in this area
bb_scot  <-  st_bbox(c(xmin = 3355000,
                       ymin = 3679800, 
                       xmax = 3524000,
                       ymax = 3759200),
                     crs = st_crs(clc_2018_landcover))

clc_scot <- st_crop(clc_2018_landcover, bb_scot, as_points=FALSE)
Ma_counts_scot <- st_crop(Ma_counts, bb_scot)

map_scot <- ggplot() +
    geom_stars(data = clc_scot["landcover_cat"], downsample = 0) +
    scale_fill_manual(values = mypal,
                      guide = "none"
                      ) +
    scale_y_continuous("") +
    scale_x_continuous("") + 
    theme_minimal() +
    theme(legend.position = "none") + 
    geom_sf(data = Ma_counts_scot, alpha=0) + ## for the grid
    geom_sf_label(data = Ma_counts_scot, aes(label = All_mammalia)) +
    ggspatial::annotation_scale(
                   location = 'br', text_family = "Open Sans", text_cex = 1.2,
                   pad_x = unit(1.05, "cm"),
                   pad_y = unit(0.07, "cm")
               ) +
    ggspatial::annotation_north_arrow(location = "bl",
                                      rotation = 348) 

map_scot_tmp <- ggplot() +
    geom_stars(data = clc_scot["landcover_cat"], downsample = 0) +
    scale_fill_manual(values = mypal,
                      name = "Landuse:"
                      ) +
    theme(legend.background = element_rect(color = NA))

fill_legend <- cowplot::get_legend(map_scot_tmp)

## manually setting the box around London to match the
## grid in this area
bb_lond  <-  st_bbox(c(xmin = 3534800,
                       ymin = 3159800, 
                       xmax = 3704000,
                       ymax = 3239200),
                     crs = st_crs(clc_2018_landcover))

clc_lond <- st_crop(clc_2018_landcover, bb_lond, as_points=FALSE)
Ma_counts_lond <- st_crop(Ma_counts, bb_lond)


map_lond <- ggplot() +
    geom_stars(data = clc_lond["landcover_cat"], downsample = 0) +
    scale_fill_manual(values = mypal,
                      guide = "none"
                      ) +
    scale_y_continuous("") +
    scale_x_continuous("") + 
    theme_minimal() +
    theme(legend.position = "none") + 
    geom_sf(data=Ma_counts_lond, alpha=0) + ## for the grid
    geom_sf_label(data = Ma_counts_lond, aes(label=All_mammalia)) +
    ggspatial::annotation_scale(
                   location = 'br', text_family = "Open Sans", text_cex = 1.2,
                   pad_x = unit(1.05, "cm"),
                   pad_y = unit(0.07, "cm")
               ) +
    ggspatial::annotation_north_arrow(location = "bl",
                                      rotation = 348) 

All_UK <- ggplot() +
    geom_stars(data = clc_2018_landcover["landcover_cat"], downsample = 0) +
    scale_fill_manual(values = mypal,
                      guide = "none"
                      ) +
    scale_y_continuous("") +
    scale_x_continuous("") +
    geom_sf(data = st_as_sfc(bb_scot), aes(fill = NULL),
            color = "black", alpha = 0, linewidth = 1, show.legend = FALSE) +
    geom_text(aes(x = bb_scot["xmin"],y = bb_scot["ymin"],
                  label = "c) Scotland (part)"),
              vjust = "outward", hjust = "outward",
              nudge_x = 2000, nudge_y = 5000) + 
    geom_sf(data = st_as_sfc(bb_lond), aes(fill = NULL),
            color = "black", alpha = 0, linewidth = 1, show.legend = FALSE) +
    geom_text(aes(x = bb_lond["xmin"],y = bb_lond["ymin"],
                  label = "d) London"),
              vjust = "inward", hjust = "outward",
              nudge_x = 5000, nudge_y = 5000) + 
    ggspatial::annotation_scale(
                   location = 'br', text_family = "Open Sans", text_cex = 1.2
               ) +
    ggspatial::annotation_north_arrow(location = "bl",
                                      rotation = 348) +
    theme_minimal() 

## overview map
sf_world <- 
  st_as_sf(rworldmap::getMap(resolution = "low")) %>% 
    st_transform(crs = st_crs(clc_2018_landcover)) %>% 
    st_buffer(dist = 0) %>% 
    dplyr::select(ISO_A2, SOVEREIGNT, LON, continent) %>% 
  mutate(area = st_area(.))

## seems convoluted... easiere way?
xmin_UK <- min(st_get_dimension_values(clc_2018_landcover, "x", where = "start"))
xmax_UK <- max(st_get_dimension_values(clc_2018_landcover, "x", where = "end"))
ymin_UK <- min(st_get_dimension_values(clc_2018_landcover, "y", where = "start"))
ymax_UK <- max(st_get_dimension_values(clc_2018_landcover, "y", where = "end"))

map_europe <- 
  ggplot(sf_world) +
  geom_sf(fill = "grey80", color = "grey96", lwd = .1) +
  geom_rect(
      xmin = xmin_UK, xmax = xmax_UK, ymin = ymin_UK, ymax = ymax_UK,
      color = "#212121", linewidth = .7, fill = NA,
  ) +
  geom_sf_text(
      data = filter(sf_world, ISO_A2 %in% c("FR", "ES", "GB", "PT", "NL", "IE", 
                                            "NL", "BE", "LU"
    )),
    aes(label = ISO_A2),
    family = "Open Sans", color = "grey40", fontface = "bold", size = 4.5,
    nudge_x = 20000, nudge_y = -10000
  ) +
  ggspatial::annotation_scale(
    location = 'br', text_family = "Open Sans", text_cex = 1.2
  ) +
  coord_sf(xlim = c(2650000, 4150000), ylim = c(1750000, 4400000)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-10, 30, by = 10)) +
  labs(x = NULL, y = NULL) +
  theme_map() +
  theme(panel.ontop = FALSE,
        panel.grid.major = element_line(color = "grey75", linetype = "15", linewidth = .3))

map_globe <- d6berlin::globe(col_earth = "grey80", col_water = "grey96",
                             bg = TRUE, center = c(0.0, 52))

map_UK <- All_UK +
    inset_element(map_globe,  .0, .8, .42, .95, align_to = "plot") + 
    inset_element(map_europe, .01, .55, .42, .82, align_to = "plot")


## The proportions of area and counts

Proportions_lu <- Taxa_GB_count_10km |>
  as_tibble()|> ## to get rid of the geometry
  filter(Observer == "Citizen" & FocusTaxaTorF) |>
  summarise(across(starts_with("PropL_"), ~ mean(.x, na.rm = TRUE)))

Proportions_co <- Taxa_GB_count_10km |>
  as_tibble()|> ## to get rid of the geometry
  filter(Observer == "Citizen" & FocusTaxaTorF) |>
  summarise(across(starts_with("PropL_"),
                   ~ weighted.mean(.x, CountT_mammalia, na.rm = TRUE)))

Proportions <- data.frame("Area" = t(Proportions_lu),
                          "Counts" = t(Proportions_co)) %>%
    as_tibble(rownames="Landuse") %>%
    mutate(Landuse = case_match(Landuse, 
                                "PropL_Grey_urban" ~ "Grey urban", 
                                "PropL_Green_urban" ~ "Green urban",
                                "PropL_Agricultural" ~ "Agricultural",
                                "PropL_Broadleaf_forest" ~ "Broadleaf forest", 
                                "PropL_Coniferous_forest" ~ "Coniferous forest", 
                                "PropL_Mixed_forest" ~ "Mixed forest", 
                                "PropL_Semi_natural_areas" ~ "Semi-natural areas", 
                                "PropL_Water" ~ "Waterbodies")) %>%
    mutate(Landuse =
               factor(Landuse, 
                      levels = c("Grey urban", "Green urban",
                                 "Agricultural", "Semi-natural areas",
                                 "Broadleaf forest", "Mixed forest",
                                 "Coniferous forest", "Waterbodies"))) %>%
    pivot_longer(!Landuse, names_to = "Class", values_to = "Value") 


Proportions_plot <-
    Proportions %>%
    ggplot(aes(x = Class, y = Value, fill = Landuse)) +
    geom_flow(aes(alluvium = Landuse), alpha= .5, color = "white",
              curve_type = "linear",
              width = .5) +
    geom_col(width = .5, color = "white") +
    scale_fill_manual(values = mypal[-length(mypal)],
                      guide = "none") +
    scale_y_continuous("Percent (%)", labels = c(0, 25, 50, 75, 100)) +
    scale_x_discrete("", labels = c("of the area", "of counts\n(in area)")) + 
    theme(legend.position = "none") + 
    theme_minimal()


### Putting it togehter
inserts <- cowplot::plot_grid(map_scot, map_lond,
                              labels = c("c", "d"),
                              nrow = 2)

legend_bars <- cowplot::plot_grid(fill_legend, Proportions_plot,
                                  labels = c("", "b"),
                                  nrow = 2)
                                  

complex_map <- cowplot::plot_grid(map_UK, legend_bars, inserts,
                                  labels = c("a", "", "", "", ""),
                                  ncol = 3, rel_widths = c(1, 0.3, 1),
                                  align = "h", axis = "t")

ggsave("figures/Fig2.png", complex_map, width = 600,
       height = 300, unit = "mm")


## Instead of proportions counting directly in which kind of landcover
## an observation was made

## Mammalia_relevant <- Taxa_GB_Pub |>
##     filter(Observer == "Citizen" & FocusTaxaTorF)

## landcover_relevant <- clc_2018_landcover %>%
##     st_as_sf() %>%
##     filter(!is.na(landcover_cat)) %>%
##     st_transform(3035)


## counts_landcover <- st_join(landcover_relevant, Mammalia_relevant)

## ## what
##     summarize(Value = mean()) %>%
    
    


