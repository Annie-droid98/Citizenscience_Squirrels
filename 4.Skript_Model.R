library(spaMM)
library(INLA)

#read in the data you need to reproduce the models
d <- readRDS("citdata.rds")
setwd("/localstorage/annie5/Citizenscience_Squirrels/")
mesh <- INLA::inla.mesh.2d(loc = d[, c("lon", "lat")], max.n = 100, max.edge = c(3, 20))

#list with all the formulas for the likelyhood ratio testing
diff_glmm_formula_1.25 <- list(
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +            
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z  + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))))

diff_glmm_formula_init <- list(
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +            
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.vulgaris ~ offset(AllMammalia_log) +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z  + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z + 
            Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Broadleafed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.vulgaris ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_carolinensis_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_carolinensis_z:Proportion_marten_z +
            Proportion_carolinensis_z:Grey_urban_z + Proportion_carolinensis_z:green_urban_z +
            Proportion_carolinensis_z:Mixed_Forest_z + Proportion_carolinensis_z:Broadleafed_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)))

myfunction<-function(x,y) 
{ fits_1.25_1 <- fitme(x,
                       family = negbin(link = "log"),
                       init=list(corrPars=list("1"=c(kappa=0.26)),NB_shape=2.9, lambda=10),
                       # control.HLfit=list(LevenbergM=TRUE), # maybe
                       verbose = c(TRACE = TRUE), method="PQL/L", 
                       data = d)
inits_1.25 <- get_inits_from_fit(fits_1.25_1)$init

angepasster_fit <- fitme(y,family = negbin(link = "log"),
                         init=list(corrPars=list("1"=c(alpha=1.25,kappa=inits_1.25$corrPars[[1]][["kappa"]])),NB_shape=inits_1.25$NB_shape, lambda=inits_1.25$lambda),
                         verbose = c(TRACE = TRUE), method="PQL/L",
                         control.HLfit=list(LevenbergM=TRUE),
                         data = d)

#summary(angepasster_fit)
}
result <- mapply(myfunction,x=diff_glmm_formula_1.25,y=diff_glmm_formula_init,SIMPLIFY = FALSE )

saveRDS(result, "Citizenscience_modelle.rds")

############################################ Carolinensismodelle
#modelle carolinensis
diff_glmm_formula_1.25_caro <- list(
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +            
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z  + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh, fixed=c(alpha=1.25))))

diff_glmm_formula_init_caro <- list(
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +            
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.carolinensis ~ offset(AllMammalia_log) +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + 
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z  + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z + 
            Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Broadleafed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Coniferous_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)),
  formula(S.carolinensis ~ offset(AllMammalia_log) + year_from_2000 +
            Grey_urban_z + green_urban_z + Agrar_z + Other_seminatural_z + Proportion_marten_z+
            Proportion_vulgaris_z +
            Mixed_Forest_z + Broadleafed_Forest_z + Coniferous_Forest_z +
            Proportion_vulgaris_z:Proportion_marten_z +
            Proportion_vulgaris_z:Grey_urban_z + Proportion_vulgaris_z:green_urban_z +
            Proportion_vulgaris_z:Mixed_Forest_z + Proportion_vulgaris_z:Broadleafed_Forest_z +
            MaternIMRFa(1|lon+lat, mesh=mesh)))

result_caro <- mapply(myfunction,x=diff_glmm_formula_1.25_caro,y=diff_glmm_formula_init_caro,  SIMPLIFY = FALSE )

saveRDS(result_caro, "Citizensciencemodelle_caro.rds")

###### Corrplot and Likelyhoodratio testing

#S.vulgaris
results_citizen <- readRDS("Citizenscience_modelle.rds")
corr<- vcov(scientific[[1]])
cor_test <- cov2cor(corr)
ggcorrplot(cor_test, hc.order = TRUE,
           lab = TRUE, lab_size = 2)

pValues_vulgaris <- lapply((2:17), function(i){
  anova(results_citizen[[1]], results_citizen[[i]])
})
p_values_for_vulgaris_2 <- lapply(pValues_vulgaris, "[[", "basicLRT")%>%
  do.call(rbind, .)
Vulgaris_table <- as.data.frame(summary(results_citizen[[1]])$beta_table)

p_values_for_vulgaris<- p_values_for_vulgaris_2 %>% 
  add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1)
Model_table_vulgaris <- cbind(Vulgaris_table, p_values_for_vulgaris)
Model_table_vulgaris_1.1 <- round(Model_table_vulgaris, digits = 3)
Model_table_vulgaris_2 <-Model_table_vulgaris_1.1%>%  
  mutate(translation = format(Model_table_vulgaris_1.1$p_value, scientific = FALSE, big.mark = ","))
colnames(Model_table_vulgaris_2) <-c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p_valuescientific","p-value")
Model_table_vulgaris_2<- tibble::rownames_to_column(Model_table_vulgaris_2, "Predictor")
Model_table_vulgaris_3 <- Model_table_vulgaris_2%>%
  dplyr::select(-c(p_valuescientific))

#S.carolinensis
results_caro <- readRDS("Citizensciencemodelle_caro.rds")

corr_caro<- vcov(result_caro[[1]])
cor_test_caro <- cov2cor(corr_caro)
ggcorrplot(cor_test_caro, hc.order = TRUE,
           lab = TRUE, lab_size = 2)

pValues_carolinensis <- lapply((2:17), function(i){
  anova(results_caro[[1]], results_caro[[i]])
})
p_values_for_carolinensis_2 <- lapply(pValues_carolinensis, "[[", "basicLRT")%>%
  do.call(rbind, .)
Carolinensis_table <- as.data.frame(summary(results_caro[[1]])$beta_table)

p_values_for_carolinensis <- p_values_for_carolinensis_2 %>% 
  add_row(chi2_LR = NA, df =NA , p_value =NA, .before = 1)
Model_table_carolinensis <- cbind(Carolinensis_table, p_values_for_carolinensis)
Model_table_carolinensis_1.1 <- round(Model_table_carolinensis, digits = 3)
Model_table_carolinensis_2 <-Model_table_carolinensis_1.1%>%  
  mutate(translation = format(Model_table_carolinensis_1.1$p_value, scientific = FALSE, big.mark = ","))
colnames(Model_table_carolinensis_2) <-c("Estimate", "Cond.SE","t-value","Chi2_LR", "df","p_valuescientific","p-value")
Model_table_carolinensis_2<- tibble::rownames_to_column(Model_table_carolinensis_2, "Predictor")
Model_table_carolinensis_3 <- Model_table_carolinensis_2%>%
  dplyr::select(-c(p_valuescientific))

################################ make Tab.1
install.packages("gtsummary")
install.packages("gapminder")
install.packages("gt")
library(psych)
library(gt)
library(gapminder)
library(tidyverse)
library(gtsummary)


Model_table_vulgaris_3
Model_table_carolinensis_3

Model_table_carolinensis_3.1 <- Model_table_carolinensis_3[,-(1),drop=FALSE] 
Model_table_carolinensis_3.1$`p-value`[Model_table_carolinensis_3.1$`p-value`=="0.000"] <-"0.001"


Model_table_vulgaris_3$Predictor <- c("Intercept", "Year","Grey urban","Green urban", "Agrar",
                                      "Other natural", "Proportion marten", "Proportion other squirrel",
                                      "Mixed Forest","Broadleafed forest", "Coniferous forest","Proportion marten:Proportion other squirrel", "Grey urban:Proportion other squirrel",
                                      "Green urban:Proportion other squirrel", "Mixed forest:Proportion other squirrel","Broadleafed forest:Proportion other squirrel", "Coniferous forest:Proportion other squirrel")
Model_table_vulgaris_3$`p-value`[Model_table_vulgaris_3$`p-value`=="0.000"] <-"0.001"

Tabellecaroundvul_2 <- cbind(Model_table_vulgaris_3,Model_table_carolinensis_3.1)
colnames(Tabellecaroundvul_2) <- c("Predictor","Estimate","Cond.SE","t-value","Chi2_LR", "df","p-value","Estimate*","Cond.SE*","t-value*","Chi2_LR*", "df*","p-value*")

Tabelle_fürbeide <-Tabellecaroundvul_2%>%
  gt()%>%
  tab_spanner(label = md("*S.vulgaris*"), columns = c("Estimate","Cond.SE","t-value","Chi2_LR", "df","p-value"))%>%
  tab_spanner(label = md("*S.carolinensis*"), columns = c("Estimate*","Cond.SE*","t-value*","Chi2_LR*", "df*","p-value*"))%>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = `p-value`,
      rows = `p-value`<= 0.05
    ))%>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = `p-value*`,
      rows = `p-value*`<= 0.05
    ))



