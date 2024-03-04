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

## subset it to only citizen-science data without focus taxon within
## mammalia
d <- as_tibble(Taxa_GB_count_10km) |>
   filter(Observer%in%"Citizen" &
          !FocusTaxaTorF &
          ## also need to remove grid cells which didn't have mammalia
          ## counted
          CountT_mammalia_log > 0
          )

## We now have uneven numbers of grid cells per year. But it has to be
## like that...  Otherwise we'd not be able to normalize by log
## (higher level taxa) numbers.  

## if(!all(table(d$year)==3787)){ ## previously this was ##
## !all(table(d$year)==4409) stop("Each year should have 3787 grid
## cells accessed") }

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

get_init_and_fit <-function(x, y) {
    fits_1.25_1 <- fitme(x,
                         family = negbin(link = "log"),
                         init=list(corrPars=list("1"=c(kappa=0.26)),NB_shape=2.9,
                                   lambda=10),
                         ## control.HLfit=list(LevenbergM=TRUE), # maybe
                         verbose = c(TRACE = TRUE), method="PQL/L", 
                         data = d)
    inits_1.25 <- get_inits_from_fit(fits_1.25_1)$init
    adapted_fit <-
        fitme(y,family = negbin(link = "log"),
              init=list(corrPars=list("1"=c(alpha=1.25,
                                            kappa=inits_1.25$corrPars[[1]][["kappa"]])),
                        NB_shape=inits_1.25$NB_shape, lambda=inits_1.25$lambda),
              verbose = c(TRACE = TRUE), method="PQL/L",
              control.HLfit=list(LevenbergM=TRUE),
              data = d)
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
## formulas_init_vulgaris[[1]])

result_vulgaris <- mapply(get_init_and_fit,
                          x = formulas_fixed_vulgaris,
                          y = formulas_init_vulgaris, SIMPLIFY = FALSE )

## our local save (not reproducible but saving work in re-computation)
saveRDS(result_vulgaris, "intermediate_data/gh_ignore/Modelle_vulgaris.rds")

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

result_carolinensis <- mapply(get_init_and_fit,
                              x = formulas_fixed_carolinensis,
                              y = formulas_init_carolinensis, SIMPLIFY = FALSE )

## our local save (not reproducible but saving work in re-computation)
saveRDS(result_carolinensis, "intermediate_data/gh_ignore/Modelle_carolinensis.rds")


## ###### Corrplot and Likelyhoodratio testing
## S.vulgaris
corr_vulgaris <- vcov(result_vulgaris[[1]])
corr_test_vulgaris <- cov2cor(corr_vulgaris)

rownames(corr_test_vulgaris) <- gsub("PropM_c", "S. c", rownames(corr_test_vulgaris))
rownames(corr_test_vulgaris) <- gsub("PropM_marten", "M. martes", rownames(corr_test_vulgaris))
rownames(corr_test_vulgaris) <- gsub("PropL_", "", rownames(corr_test_vulgaris))

colnames(corr_test_vulgaris) <- gsub("PropM_c", "S. c", colnames(corr_test_vulgaris))
colnames(corr_test_vulgaris) <- gsub("PropM_marten", "M. martes", colnames(corr_test_vulgaris))
colnames(corr_test_vulgaris) <- gsub("PropL_", "", colnames(corr_test_vulgaris))

fixCorPlot_vulgaris <-  ggcorrplot(corr_test_vulgaris, hc.order = TRUE,
                                   lab = TRUE, lab_size = 3.5)

## Same for the carolinensis model
corr_caro <- vcov(result_carolinensis[[1]])
corr_test_caro <- cov2cor(corr_caro)

rownames(corr_test_caro) <- gsub("PropM_v", "S. v", rownames(corr_test_caro))
rownames(corr_test_caro) <- gsub("PropM_marten", "M. martes", rownames(corr_test_caro))
rownames(corr_test_caro) <- gsub("PropL_", "", rownames(corr_test_caro))

colnames(corr_test_caro) <- gsub("PropM_v", "S. v", colnames(corr_test_caro))
colnames(corr_test_caro) <- gsub("PropM_marten", "M. martes", colnames(corr_test_caro))
colnames(corr_test_caro) <- gsub("PropL_", "", colnames(corr_test_caro))

fixCorPlot_caro <- ggcorrplot(corr_test_caro, hc.order = TRUE,
                              lab = TRUE, lab_size = 3.5)

fixed_effects_corr_plot <- wrap_plots(fixCorPlot_vulgaris,
                                         fixCorPlot_caro, 
                                         nrow=2,
                                         guides = "collect") +
    plot_annotation(tag_levels = 'a',
                    theme = theme(legend.title = element_text(hjust = .5)))

ggsave("figures/FixedEffectCorrsBoth.pdf", fixed_effects_corr_plot,
       width = 10, height = 20, device = cairo_pdf)

lapply((2:17), function(i){
    anova(result_vulgaris[[1]], result_vulgaris[[i]])[["basicLRT"]]
}) %>% 
    do.call(rbind, .) %>%
    add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1) %>%
    cbind(as.data.frame(summary(result_vulgaris[[1]])$beta_table), .) %>%
    round(digits = 3) %>%
    ## mutate(p_val_scientific = format(p_value,
    ##                                  scientific = FALSE, big.mark = ","))
    tibble::rownames_to_column("Predictor") -> pval_table_vulgaris

lapply((2:17), function(i){
    anova(result_carolinensis[[1]], result_carolinensis[[i]])[["basicLRT"]]
}) %>% 
    do.call(rbind, .) %>%
    add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1) %>%
    cbind(as.data.frame(summary(result_carolinensis[[1]])$beta_table), .) %>%
    round(digits = 3) %>%
    ## mutate(p_val_scientific = format(p_value,
    ##                                  scientific = FALSE, big.mark = ","))
    tibble::rownames_to_column("Predictor") -> pval_table_carolinensis

cbind(pval_table_vulgaris, pval_table_carolinensis) |>
    as_tibble(.name_repair="universal")|>
    gt()  |>
    tab_spanner(label = md("<br><em>S. vulgaris</em>"),
                columns = c("Estimate...2",
                            "Cond..SE...3","t.value...4", "chi2_LR...5",
                            "df...6","p_value...7"))|>
    tab_spanner(label = md("<br><em>S. carolinensis</em>"),
                columns = c("Estimate...9","Cond..SE...10","t.value...11",
                            "chi2_LR...12", "df...13","p_value...14")) |>
    tab_style(
        style = list(
            cell_text(weight = "bold")
        ),
        locations = cells_body(
            columns = `p_value...7`,
            rows = `p_value...7`<= 0.05
    ))|>
    tab_style(
        style = list(
            cell_text(weight = "bold")
        ),
        locations = cells_body(
            columns = `p_value...14`,
            rows = `p_value...14`<= 0.05
        )) |>
    cols_label(
              Predictor...1 = "Predictor",
              Estimate...2 = "Estimate",
              Cond..SE...3 =  "Cond SE",
              t.value...4 = "t value",
              chi2_LR...5 = "chi^2 LR",
              df...6 = "DF",
              p_value...7 = "p value",
              Predictor...8 = "Predictor",
              Estimate...9 = "Estimate",
              Cond..SE...10 = "Cond SE",
              t.value...11 = "t value",
              chi2_LR...12 = "chi^2 LR",
              df...13 = "DF",
              p_value...14 = "p value"
    ) |>
    text_replace(
        locations = cells_body(columns = c(Predictor...1, Predictor...8)),
        pattern = "PropM_(v\\w*|c\\w*)",
        replacement = "<br><em>S. \\1</em>") |>
    text_replace(
        locations = cells_body(columns = c(Predictor...1, Predictor...8)),
        pattern = "PropM_marten",
        replacement = "<br><em>M. martes\\1</em>") |>
    text_replace(
        locations = cells_body(columns = c(Predictor...1, Predictor...8)),
        pattern = "PropL_",
        replacement = "") |>
    text_replace(
        locations = cells_body(columns = c(Predictor...1, Predictor...8)),
        pattern = "_",
        replacement = " ") |>
    sub_zero(zero_text="<0.001") -> out

gtsave(out, "tables/Table_ModelsLRT.html")
#
