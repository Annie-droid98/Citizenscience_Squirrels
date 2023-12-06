library(spaMM)
library(INLA)
library(sf)
library(tidyr)
library(dplyr)
library(ggcorrplot)
library(patchwork)
library(gt)

## read in the data you need to reproduce the models
redoMerge <- FALSE

if(redoMerge){
    source("R/3_MergePlotPrep.R")
} else {
    CountALL_10km <- readRDS("intermediate_data/CountALL_10km.rds")
}

## subset it to only citizen-science data without focus taxon within
## mammalia
d <- CountALL_10km |> filter(Observer%in%"Citizen"&
                              !FocusTaxaTorF)

## test whether we're alright
if(!all(table(d$year)==4409)){
    stop("Each year should have 4409 grid cells accessed")
}

mesh <- INLA::inla.mesh.2d(loc = d[, c("lon", "lat")], max.n = 100, max.edge = c(3, 20))

#list with all the formulas for the likelyhood ratio testing
diff_glmm_formula_1.25 <- list(
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + 
            PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest +
            PropT_carolinensis:PropL_Coniferous_Forest +            
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + 
            PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest +
            PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest + 
            PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest +
            PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest + 
            PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest +
            PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + 
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest +
            PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Broadleafed_Forest + PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest  + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + 
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest + 
            PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest +
            PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest + 
            PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Broadleafed_Forest + PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))))

diff_glmm_formula_init <- list(
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + 
            PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest +
            PropT_carolinensis:PropL_Coniferous_Forest +            
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + 
            PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest +
            PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest + 
            PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest +
            PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest + 
            PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest +
            PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + 
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest +
            PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Broadleafed_Forest + PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest  + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + 
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest + 
            PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest +
            PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest + 
            PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Broadleafed_Forest + PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_vulgaris ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_carolinensis +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_carolinensis:PropT_marten +
            PropT_carolinensis:PropL_Grey_urban + PropT_carolinensis:PropL_Green_urban +
            PropT_carolinensis:PropL_Mixed_Forest + PropT_carolinensis:PropL_Broadleafed_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)))

myfunction<-function(x, y) {
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
    ## summary(adapted_fit)
}


result <- mapply(myfunction,
                 x=diff_glmm_formula_1.25,
                  y=diff_glmm_formula_init, SIMPLIFY = FALSE )

## our local save (not reproducible but saving work in re-computation)
saveRDS(result, "intermediate_data/gh_ignore/Citizenscience_modelle.rds")

############################################ Carolinensis models
diff_glmm_formula_1.25_caro <- list(
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + 
            PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest +
            PropT_vulgaris:PropL_Coniferous_Forest +            
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + 
            PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest +
            PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest + 
            PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest +
            PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest + 
            PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest +
            PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + 
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest +
            PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Broadleafed_Forest + PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest  + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + 
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest + 
            PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest +
            PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest + 
            PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Broadleafed_Forest + PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))))

diff_glmm_formula_init_caro <- list(
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + 
            PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest +
            PropT_vulgaris:PropL_Coniferous_Forest +            
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + 
            PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest +
            PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest + 
            PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest +
            PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest + 
            PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest +
            PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + 
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest +
            PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Broadleafed_Forest + PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest  + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + 
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest + 
            PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest +
            PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest + 
            PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Broadleafed_Forest + PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Coniferous_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(CountT_carolinensis ~ offset(CountT_mammalia_log) + year_from_2000 +
            PropL_Grey_urban + PropL_Green_urban + PropL_Agricultural + PropL_Other_seminatural + PropT_marten+
            PropT_vulgaris +
            PropL_Mixed_Forest + PropL_Broadleafed_Forest + PropL_Coniferous_Forest +
            PropT_vulgaris:PropT_marten +
            PropT_vulgaris:PropL_Grey_urban + PropT_vulgaris:PropL_Green_urban +
            PropT_vulgaris:PropL_Mixed_Forest + PropT_vulgaris:PropL_Broadleafed_Forest +
            MaternIMRFa(1|lon+lat, mesh=mesh)))

result_caro <- mapply(myfunction,
                      x=diff_glmm_formula_1.25_caro,
                      y=diff_glmm_formula_init_caro, SIMPLIFY = FALSE )

## our local save (not reproducible but saving work in re-computation)
saveRDS(result_caro, "intermediate_data/gh_ignore/Citizensciencemodelle_caro.rds")


## ###### Corrplot and Likelyhoodratio testing

## S.vulgaris
corr <- vcov(result[[1]])
cor_test <- cov2cor(corr)

rownames(cor_test) <- gsub("PropT_c", "S. c", rownames(cor_test))
rownames(cor_test) <- gsub("PropT_marten", "M. martes", rownames(cor_test))
rownames(cor_test) <- gsub("PropL_", "", rownames(cor_test))

colnames(cor_test) <- gsub("PropT_c", "S. c", colnames(cor_test))
colnames(cor_test) <- gsub("PropT_marten", "M. martes", colnames(cor_test))
colnames(cor_test) <- gsub("PropL_", "", colnames(cor_test))

fixCorPlot <-  ggcorrplot(cor_test, hc.order = TRUE,
                          lab = TRUE, lab_size = 3.5)

## Same for the carolinensis model
corr_caro <- vcov(result_caro[[1]])
cor_test_caro <- cov2cor(corr_caro)

rownames(cor_test_caro) <- gsub("PropT_v", "S. v", rownames(cor_test_caro))
rownames(cor_test_caro) <- gsub("PropT_marten", "M. martes", rownames(cor_test_caro))
rownames(cor_test_caro) <- gsub("PropL_", "", rownames(cor_test_caro))

colnames(cor_test_caro) <- gsub("PropT_v", "S. v", colnames(cor_test_caro))
colnames(cor_test_caro) <- gsub("PropT_marten", "M. martes", colnames(cor_test_caro))
colnames(cor_test_caro) <- gsub("PropL_", "", colnames(cor_test_caro))

fixCorPlot_Caro <- ggcorrplot(cor_test_caro, hc.order = TRUE,
                              lab = TRUE, lab_size = 3.5)

wrap_plots(fixCorPlot,
           fixCorPlot_Caro, 
           nrow=2,
           guides = "collect") +
    plot_annotation(tag_levels = 'a',
                    theme = theme(legend.title = element_text(hjust = .5)))

ggsave("figures/FixedEffectCorrsBoth.pdf",
       width = 10, height = 20, device = cairo_pdf)

lapply((2:17), function(i){
    anova(result[[1]], result[[i]])[["basicLRT"]]
}) |> 
    do.call(rbind, _) |>
    add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1) |>
    cbind(as.data.frame(summary(result[[1]])$beta_table), .) |>
    round(digits = 3) |>
    ## mutate(p_val_scientific = format(p_value,
    ##                                  scientific = FALSE, big.mark = ","))
    tibble::rownames_to_column("Predictor") -> foo

lapply((2:17), function(i){
    anova(result_caro[[1]], result_caro[[i]])[["basicLRT"]]
}) |> 
    do.call(rbind, _) |>
    add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1) |>
    cbind(as.data.frame(summary(result_caro[[1]])$beta_table), _) |>
    round(digits = 3) |>
    ## mutate(p_val_scientific = format(p_value,
    ##                                  scientific = FALSE, big.mark = ","))
    tibble::rownames_to_column("Predictor") -> bar

cbind(foo, bar) |>
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
        pattern = "PropT_(v\\w*|c\\w*)",
        replacement = "<br><em>S. \\1</em>") |>
    text_replace(
        locations = cells_body(columns = c(Predictor...1, Predictor...8)),
        pattern = "PropT_marten",
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
