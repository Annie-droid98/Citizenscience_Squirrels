library(spaMM)
library(INLA)
## library(sf)
library(tidyr)
library(dplyr)
library(ggcorrplot)
library(patchwork)
library(gt)

redoDataPrep <- FALSE

## read in the data you need to reproduce the models
if (redoDataPrep) {
    source("R/1_data_prep.R")
} else{
    Taxa_GB_count_10km <- readRDS("intermediate_data/Counts.rds")
}


## how many grids per year with non-zero data
tapply(Taxa_GB_count_10km$CountT_mammalia,
       list(Taxa_GB_count_10km$year, Taxa_GB_count_10km$Observer, Taxa_GB_count_10km$Focus),
       function(x)length(x[x>0]))

## how counts per year
tapply(Taxa_GB_count_10km$CountT_mammalia,
       list(Taxa_GB_count_10km$year, Taxa_GB_count_10km$Observer, Taxa_GB_count_10km$Focus),
       function(x) sum(x))

tapply(Taxa_GB_count_10km$CountT_mammalia,
       list(Taxa_GB_count_10km$year, Taxa_GB_count_10km$Observer, Taxa_GB_count_10km$Focus),
       function(x)length(x))

## subset it to only citizen-science data without focus taxon within
## mammalia
d <- as_tibble(Taxa_GB_count_10km) |>
   filter(Observer%in%"Citizen" &
          Focus == "without" &
          CountT_mammalia_log >= 0 &
          !is.na(year) &
          year_from_2000 < 22)

apply(d, 2, function (x) any(is.infinite(unlist(x))))

apply(d, 2, function (x) any(is.na(unlist(x))))


mesh <- INLA::inla.mesh.2d(loc = d[, c("lon", "lat")], max.n = 100, max.edge = c(3, 20))

full_formula <- formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
                        PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Semi_natural_areas + 
                        PropM_marten+
                        PropM_carolinensis +
                        PropL_Mixed_forest + PropL_Broadleaf_forest + PropL_Coniferous_forest +
                        PropM_carolinensis:PropM_marten +
                        PropM_carolinensis:PropL_Grey_urban + PropM_carolinensis:PropL_Green_urban +
                        PropM_carolinensis:PropL_Mixed_forest + PropM_carolinensis:PropL_Broadleaf_forest +
                        PropM_carolinensis:PropL_Coniferous_forest)


formulas_vulgaris <-
    list(full = full_formula,
         no_year = update(full_formula, . ~ . - year_from_2000),
         no_grey_urban = update(full_formula,
                                . ~ . - PropL_Grey_urban -
                                    PropL_Grey_urban:PropM_carolinensis),
         no_green_urban = update(full_formula, 
                                 . ~ . - PropL_Green_urban - 
                                     PropL_Green_urban:PropM_carolinensis),
         no_agri = update(full_formula, 
                          . ~ . - PropL_Agricultural),
         no_semi = update(full_formula,
                          . ~ . - PropL_Semi_natural_areas),
         no_marten = update(full_formula, 
                            . ~ . - PropM_marten -
                                PropM_carolinensis:PropM_marten),
         no_carolinensis = update(full_formula,
                                  . ~ . - PropM_carolinensis -
                                      PropM_carolinensis:PropM_marten -
                                      PropM_carolinensis:PropL_Grey_urban -
                                      PropM_carolinensis:PropL_Green_urban -
                                      PropM_carolinensis:PropL_Mixed_forest -
                                      PropM_carolinensis:PropL_Broadleaf_forest -
                                      PropM_carolinensis:PropL_Coniferous_forest),
         no_mixed = update(full_formula,
                           . ~ . -  PropL_Mixed_forest -
                               PropM_carolinensis:PropL_Mixed_forest),
         no_brlf = update(full_formula,
                          . ~ . - PropL_Broadleaf_forest -
                              PropM_carolinensis:PropL_Broadleaf_forest),
         no_conif = update(full_formula,
                           . ~ . - PropL_Coniferous_forest -
                               PropM_carolinensis:PropL_Coniferous_forest),
         no_caro_marten = update(full_formula,
                                 . ~ . - PropM_carolinensis:PropM_marten),
         no_caro_grey = update(full_formula,
                                . ~ . - PropM_carolinensis:PropL_Grey_urban),
         no_caro_green = update(full_formula,
                                . ~ . - PropM_carolinensis:PropL_Green_urban),
         no_caro_mixed = update(full_formula,
                                . ~ . - PropM_carolinensis:PropL_Mixed_forest), 
         no_caro_brlf = update(full_formula,
                               . ~ . - PropM_carolinensis:PropL_Broadleaf_forest),
         no_caro_conif = update(full_formula,
                                . ~ . - PropM_carolinensis:PropL_Coniferous_forest)
         )

##

formulas_fixed_vulgaris <- lapply(formulas_vulgaris, function (x) {
    update(x,
           . ~ . + MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25)))
})

formulas_init_vulgaris <- lapply(formulas_vulgaris, function (x) {
    update(x, 
    . ~ . +  MaternIMRFa(1|lon+lat, mesh=mesh))
})

get_init_and_fit <-function(x, y, data_df) {
    fits_1.25_1 <- fitme(x,
                         family = negbin(link = "log"),
                         init=list(corrPars=list("1"=c(kappa=0.26)),NB_shape=2.9,
                                   lambda=10),
                         ## control.HLfit=list(LevenbergM=TRUE), # maybe
                         verbose = c(TRACE = TRUE), method="PQL/L", 
                         data = data_df)
    inits_1.25 <- get_inits_from_fit(fits_1.25_1)$init
    adapted_fit <-
        fitme(y,family = negbin(link = "log"),
              init=list(corrPars=list("1"=c(alpha=1.25,
                                            kappa=inits_1.25$corrPars[[1]][["kappa"]])),
                        NB_shape=inits_1.25$NB_shape, lambda=inits_1.25$lambda),
              verbose = c(TRACE = TRUE), method="PQL/L",
              control.HLfit=list(LevenbergM=TRUE),
              data = data_df)
    adapted_fit
}

## ## try the first fit in case of problmes
## foo_fit <- fitme(formulas_fixed_vulgaris[[1]],
##                  family = negbin(link = "log"),
##                  init=list(corrPars=list("1"=c(kappa=0.26)),NB_shape=2.9,
##                            lambda=10),
##                  ## control.HLfit=list(LevenbergM=TRUE), # maybe
##                  verbose = c(TRACE = TRUE), method="PQL/L", 
##                  data = d)

## ## try the first fit optimization in case of problems bar_fit <-
## get_init_and_fit(formulas_fixed_vulgaris[[1]],
##  formulas_init_vulgaris[[1]], data_df = d)

## result_vulgaris <- mapply(get_init_and_fit,
##                           x = formulas_fixed_vulgaris,
##                           y = formulas_init_vulgaris,
##                           MoreArgs = list(data_df = d), 
##                           SIMPLIFY = FALSE )

One_result_vulgaris <- get_init_and_fit(
    x = formulas_fixed_vulgaris[[1]],
    y = formulas_init_vulgaris[[1]],
    data_df = d)


## ## our local save (not reproducible but saving work in re-computation)

## ## Not saving (overwriting for no, just comaring models)
## saveRDS(result_vulgaris, "intermediate_data/gh_ignore/Modelle_vulgaris.rds")

############################################ same models for S. carolinensis 
replace_formula <- function(from, to, my_formula){
    f <- deparse(my_formula)
    s <- gsub(from, to, f)
    formula(paste(s, collapse = " ")) 
}

formulas_fixed_carolinensis <- lapply(formulas_fixed_vulgaris, function(x) {
    pred <-   replace_formula("carolinensis", "vulgaris", x)
    replace_formula("vulgaris ~", "carolinensis ~", pred)
})
names(formulas_fixed_carolinensis) <- sub("caro", "vulgaris",
                                          names(formulas_fixed_carolinensis))

formulas_init_carolinensis <- lapply(formulas_init_vulgaris, function(x) {
    pred <- replace_formula("carolinensis", "vulgaris", x)
    replace_formula("vulgaris ~", "carolinensis ~", pred)
})
names(formulas_init_carolinensis) <- sub("caro", "vulgaris",
                                          names(formulas_init_carolinensis))

## result_carolinensis <- mapply(get_init_and_fit,
##                               x = formulas_fixed_carolinensis,
##                               y = formulas_init_carolinensis,
##                               MoreArgs = list(data_df = d), 
##                               SIMPLIFY = FALSE )

## our local save (not reproducible but saving work in re-computation)

## ## Not saving (overwriting for no, just comaring models)
## saveRDS(result_carolinensis, "intermediate_data/gh_ignore/Modelle_carolinensis.rds")

One_result_carolinensis <- get_init_and_fit(
    x = formulas_fixed_carolinensis[[1]],
    y = formulas_init_carolinensis[[1]],
    data_df = d)



## ###### Corrplot and Likelyhoodratio testing
get_cor_nice_plot <- function(model.fit) { 
    model_corr <- vcov(model.fit)
    corr_tested <- cov2cor(model_corr)

    rownames(corr_tested) <- gsub("Prop[M|V]_c", "S. c",
                                  rownames(corr_tested))
    rownames(corr_tested) <- gsub("Prop[M|V]_v", "S. v",
                                  rownames(corr_tested))
    rownames(corr_tested) <- gsub("Prop[M|V]_marten", "M. martes",
                                         rownames(corr_tested))
    rownames(corr_tested) <- gsub("PropL_", "",
                                         rownames(corr_tested))
    colnames(corr_tested) <- gsub("Prop[M|V]_c", "S. c",
                                         colnames(corr_tested))
    colnames(corr_tested) <- gsub("Prop[M|V]_marten", "M. martes",
                                         colnames(corr_tested))
    colnames(corr_tested) <- gsub("PropL_", "",
                                         colnames(corr_tested))
    ggcorrplot(corr_tested, hc.order = TRUE,
               lab = TRUE, lab_size = 3.5)
}


## fixCorPlot_vulgaris <- get_cor_nice_plot(result_carolinensis[[1]])

fixCorPlot_vulgaris <- get_cor_nice_plot(One_result_carolinensis)

## fixCorPlot_carolinensis <- get_cor_nice_plot(result_carolinensis[[1]])
fixCorPlot_carolinensis <- get_cor_nice_plot(One_result_carolinensis)

fixed_effects_corr_plot <- wrap_plots(fixCorPlot_vulgaris,
                                         fixCorPlot_carolinensis, 
                                         nrow=2,
                                         guides = "collect") +
    plot_annotation(tag_levels = 'a',
                    theme = theme(legend.title = element_text(hjust = .5)))

ggsave("figures/FixedEffectCorrsBoth.pdf", fixed_effects_corr_plot,
       width = 10, height = 20, device = cairo_pdf)

## lapply((2:17), function(i){
##     anova(result_vulgaris[[1]], result_vulgaris[[i]])[["basicLRT"]]
## }) %>% 
##     do.call(rbind, .) %>%
##     add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1) %>%
##     cbind(as.data.frame(summary(result_vulgaris[[1]])$beta_table), .) %>%
##     round(digits = 3) %>%
##     ## mutate(p_val_scientific = format(p_value,
##     ##                                  scientific = FALSE, big.mark = ","))
##     tibble::rownames_to_column("Predictor") -> pval_table_vulgaris

## lapply((2:17), function(i){
##     anova(result_carolinensis[[1]], result_carolinensis[[i]])[["basicLRT"]]
## }) %>% 
##     do.call(rbind, .) %>%
##     add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1) %>%
##     cbind(as.data.frame(summary(result_carolinensis[[1]])$beta_table), .) %>%
##     round(digits = 3) %>%
##     ## mutate(p_val_scientific = format(p_value,
##     ##                                  scientific = FALSE, big.mark = ","))
##     tibble::rownames_to_column("Predictor") -> pval_table_carolinensis

## cbind(pval_table_vulgaris, pval_table_carolinensis) |>
##     as_tibble(.name_repair="universal")|>
##     gt()  |>
##     tab_spanner(label = md("<br><em>S. vulgaris</em>"),
##                 columns = c("Estimate...2",
##                             "Cond..SE...3","t.value...4", "chi2_LR...5",
##                             "df...6","p_value...7"))|>
##     tab_spanner(label = md("<br><em>S. carolinensis</em>"),
##                 columns = c("Estimate...9","Cond..SE...10","t.value...11",
##                             "chi2_LR...12", "df...13","p_value...14")) |>
##     tab_style(
##         style = list(
##             cell_text(weight = "bold")
##         ),
##         locations = cells_body(
##             columns = `p_value...7`,
##             rows = `p_value...7`<= 0.05
##     ))|>
##     tab_style(
##         style = list(
##             cell_text(weight = "bold")
##         ),
##         locations = cells_body(
##             columns = `p_value...14`,
##             rows = `p_value...14`<= 0.05
##         )) |>
##     cols_label(
##               Predictor...1 = "Predictor",
##               Estimate...2 = "Estimate",
##               Cond..SE...3 =  "Cond SE",
##               t.value...4 = "t value",
##               chi2_LR...5 = "chi^2 LR",
##               df...6 = "DF",
##               p_value...7 = "p value",
##               Predictor...8 = "Predictor",
##               Estimate...9 = "Estimate",
##               Cond..SE...10 = "Cond SE",
##               t.value...11 = "t value",
##               chi2_LR...12 = "chi^2 LR",
##               df...13 = "DF",
##               p_value...14 = "p value"
##     ) |>
##     text_replace(
##         locations = cells_body(columns = c(Predictor...1, Predictor...8)),
##         pattern = "PropM_(v\\w*|c\\w*)",
##         replacement = "<br><em>S. \\1</em>") |>
##     text_replace(
##         locations = cells_body(columns = c(Predictor...1, Predictor...8)),
##         pattern = "PropM_marten",
##         replacement = "<br><em>M. martes\\1</em>") |>
##     text_replace(
##         locations = cells_body(columns = c(Predictor...1, Predictor...8)),
##         pattern = "PropL_",
##         replacement = "") |>
##     text_replace(
##         locations = cells_body(columns = c(Predictor...1, Predictor...8)),
##         pattern = "_",
##         replacement = " ") |>
##     sub_zero(zero_text="<0.001") -> Nice_table

## gtsave(Nice_table, "tables/Table_ModelsLRT.html")
## #


## Alternative Vertebrata models

formulas_init_vulgaris_Vert <- lapply(formulas_init_vulgaris, function(x){ 
    pred <- replace_formula("PropM_",
                            "PropV_", x)
    replace_formula("mammalia",
                    "vertebrata", pred)
})


formulas_fixed_vulgaris_Vert <- lapply(formulas_fixed_vulgaris, function(x){ 
    pred <- replace_formula("PropM_",
                            "PropV_", x)
    replace_formula("mammalia",
                    "vertebrata", pred)
})


formulas_init_carolinensis_Vert <- lapply(formulas_init_carolinensis, function(x){ 
    pred <- replace_formula("PropM_",
                            "PropV_", x)
    replace_formula("mammalia",
                    "vertebrata", pred)
})


formulas_fixed_carolinensis_Vert <- lapply(formulas_fixed_carolinensis, function(x){ 
    pred <- replace_formula("PropM_",
                            "PropV_", x)
    replace_formula("mammalia",
                    "vertebrata", pred)
})

## The data is slightly different as focus taxa are different
## (esp. when mammalia are a foucs within vertebrates; mention this in
## the manuscipt!) and additional normalisation counts are available
## for some grids in some years (list in the manuscript how many).

v <- as_tibble(Taxa_GB_count_10km) |>
   filter(Observer%in%"Citizen" &
          Focus == "without" &
          ##  remove grid cells which didn't have vertebrata counted
          CountT_vertebrata_log >= 0 &
          !is.na(year) &
          year_from_2000 < 22)

## result_vulgaris_Vert <- mapply(get_init_and_fit,
##                                x = formulas_fixed_vulgaris_Vert,
##                                y = formulas_init_vulgaris_Vert,
##                                MoreArgs = list(data_df = v), 
##                                SIMPLIFY = FALSE )

One_result_vulgaris_Vert <- get_init_and_fit(
    x = formulas_fixed_vulgaris_Vert[[1]],
    y = formulas_init_vulgaris_Vert[[1]],
    data_df = v)

## ## Not saving (overwriting for no, just comaring models)
## ## our local save (not reproducible but saving work in re-computation)
## saveRDS(result_vulgaris_Vert, "intermediate_data/gh_ignore/Modelle_vulgaris_Vert.rds")


## result_carolinensis_Vert <- mapply(get_init_and_fit,
##                                    x = formulas_fixed_carolinensis_Vert,
##                                    y = formulas_init_carolinensis_Vert,
##                                    MoreArgs = list(data_df = v), 
##                                    SIMPLIFY = FALSE)


One_result_carolinensis_Vert <- get_init_and_fit(
    x = formulas_fixed_carolinensis_Vert[[1]],
    y = formulas_init_carolinensis_Vert[[1]],
    data_df = v)

## ## Not saving (overwriting for no, just comaring models)
## ## our local save (not reproducible but saving work in re-computation)
## saveRDS(result_carolinensis_Vert, "intermediate_data/gh_ignore/Modelle_carolinensis_Vert.rds")

## Corr plots and LRT for VERTEBRATA normalisation 

## fixCorPlot_vulgaris_Vert <- get_cor_nice_plot(result_vulgaris_Vert[[1]])
fixCorPlot_vulgaris_Vert <- get_cor_nice_plot(One_result_vulgaris_Vert)


## fixCorPlot_carolinensis_Vert <- get_cor_nice_plot(result_carolinensis_Vert[[1]])
fixCorPlot_carolinensis_Vert <- get_cor_nice_plot(One_result_carolinensis_Vert)


fixed_effects_corr_plot_Vert <- wrap_plots(fixCorPlot_vulgaris_Vert,
                                         fixCorPlot_carolinensis_Vert, 
                                         nrow=2,
                                         guides = "collect") +
    plot_annotation(tag_levels = 'a',
                    theme = theme(legend.title = element_text(hjust = .5)))

ggsave("figures/FixedEffectCorrsBoth_Vert.pdf", fixed_effects_corr_plot_Vert,
       width = 10, height = 20, device = cairo_pdf)

## lapply((2:17), function(i){
##     anova(result_vulgaris_Vert[[1]], result_vulgaris_Vert[[i]])[["basicLRT"]]
## }) %>% 
##     do.call(rbind, .) %>%
##     add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1) %>%
##     cbind(as.data.frame(summary(result_vulgaris_Vert[[1]])$beta_table), .) %>%
##     round(digits = 3) %>%
##     ## mutate(p_val_scientific = format(p_value,
##     ##                                  scientific = FALSE, big.mark = ","))
##     tibble::rownames_to_column("Predictor") -> pval_table_vulgaris_Vert

## lapply((2:17), function(i){
##     anova(result_carolinensis_Vert[[1]], result_carolinensis_Vert[[i]])[["basicLRT"]]
## }) %>% 
##     do.call(rbind, .) %>%
##     add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1) %>%
##     cbind(as.data.frame(summary(result_carolinensis_Vert[[1]])$beta_table), .) %>%
##     round(digits = 3) %>%
##     ## mutate(p_val_scientific = format(p_value,
##     ##                                  scientific = FALSE, big.mark = ","))
##     tibble::rownames_to_column("Predictor") -> pval_table_carolinensis_Vert

## cbind(pval_table_vulgaris_Vert, pval_table_carolinensis_Vert) |>
##     as_tibble(.name_repair="universal")|>
##     gt()  |>
##     tab_spanner(label = md("<br><em>S. vulgaris</em>"),
##                 columns = c("Estimate...2",
##                             "Cond..SE...3","t.value...4", "chi2_LR...5",
##                             "df...6","p_value...7"))|>
##     tab_spanner(label = md("<br><em>S. carolinensis</em>"),
##                 columns = c("Estimate...9","Cond..SE...10","t.value...11",
##                             "chi2_LR...12", "df...13","p_value...14")) |>
##     tab_style(
##         style = list(
##             cell_text(weight = "bold")
##         ),
##         locations = cells_body(
##             columns = `p_value...7`,
##             rows = `p_value...7`<= 0.05
##     ))|>
##     tab_style(
##         style = list(
##             cell_text(weight = "bold")
##         ),
##         locations = cells_body(
##             columns = `p_value...14`,
##             rows = `p_value...14`<= 0.05
##         )) |>
##     cols_label(
##               Predictor...1 = "Predictor",
##               Estimate...2 = "Estimate",
##               Cond..SE...3 =  "Cond SE",
##               t.value...4 = "t value",
##               chi2_LR...5 = "chi^2 LR",
##               df...6 = "DF",
##               p_value...7 = "p value",
##               Predictor...8 = "Predictor",
##               Estimate...9 = "Estimate",
##               Cond..SE...10 = "Cond SE",
##               t.value...11 = "t value",
##               chi2_LR...12 = "chi^2 LR",
##               df...13 = "DF",
##               p_value...14 = "p value"
##     ) |>
##     text_replace(
##         locations = cells_body(columns = c(Predictor...1, Predictor...8)),
##         pattern = "PropM_(v\\w*|c\\w*)",
##         replacement = "<br><em>S. \\1</em>") |>
##     text_replace(
##         locations = cells_body(columns = c(Predictor...1, Predictor...8)),
##         pattern = "PropM_marten",
##         replacement = "<br><em>M. martes\\1</em>") |>
##     text_replace(
##         locations = cells_body(columns = c(Predictor...1, Predictor...8)),
##         pattern = "PropL_",
##         replacement = "") |>
##     text_replace(
##         locations = cells_body(columns = c(Predictor...1, Predictor...8)),
##         pattern = "_",
##         replacement = " ") |>
##     sub_zero(zero_text="<0.001") -> Nice_table_Vert

## gtsave(Nice_table_Vert, "tables/Table_ModelsLRT_Vert.html")
## #
