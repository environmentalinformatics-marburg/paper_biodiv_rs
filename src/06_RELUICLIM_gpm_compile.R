
source("C:/Users/uselig/Documents/GitHub/project_biodiv_rs/src/00_paths_libraries.R")

# Compile grassland diversity and remote sensing -------------------------------

# read veg.data
vegrel15 <- readRDS(file = paste0(path_rdata_pre, "vegrel15.rds"))

# read predictors
re_df<- readRDS(paste0(path_rdata_pre, "re2015_AprilMAIN_MarDIFF_predictors.rds"))

veg_re_g <- merge(vegrel15, re_df, by.x = "EPID",
                  by.y = "EPID")
max(colSums(is.na(veg_re_g)))

###############################
#################################################################################

### compile GPM object for RE (precitors werden hier schon ausgefiltert, dass LUI nicht drin ist)
belc <- c("AEG", "HEG", "SEG")
veg_re_g_gpm_indv <- lapply(belc, function(b){
  act_veg_re_g <- veg_re_g[veg_re_g$EP == b, ] # navigates to only character:"AEG", "HEG, "SEG"
  
  col_selector <- which(names(act_veg_re_g) == "EPID")
  
  col_diversity <- c(grep("specrich|shannon|eveness", 
                          tolower(names(act_veg_re_g))),
                         grep("biomass_g",
                              names(act_veg_re_g)))
  
  col_precitors <- seq(grep("G_std_glb", names(act_veg_re_g)), # erster predictor in Tabelle
                       grep("RaosQ_Haralick_Correlation", names(act_veg_re_g))) #letzter predictor mit meta (ohne: pca_Long_Run_Low_Grey.Level_Emphasis.b1r50o1_var)
  
  col_precitors <- col_precitors[-grep("EP", names(act_veg_re_g[, col_precitors]))] #entferne "EP" aus Prediktoren

  col_meta <- seq(length(act_veg_re_g))[-c(col_selector, col_diversity, col_precitors)]
  
  meta <- createGPMMeta(act_veg_re_g, type = "input",
                        selector = col_selector,
                        response = col_diversity,
                        predictor = col_precitors,
                        meta = col_meta)
  veg_re_g_gpm <- gpm(act_veg_re_g, meta, scale = FALSE)
  
  # Compute resamples following a leave location out approach, LOOC5 für 9fache CV
  veg_re_g_gpm <- splitMultRespLSO(x = veg_re_g_gpm, nbr = 10)
})
names(veg_re_g_gpm_indv) <- belc
######################################################################################
#################################################
############################
# SPECIFY the model
# the FINAL response remain the same for approaches 2-5 and do not need to change (it has been taken care of in GPM-Compile)
# Model 2

mod2<-veg_re_g_gpm_indv

## choose FINAL predictors for LUI only model (Model2)- all predictors are chosen for all BE
for(be in names(mod2)){
  mod2[[be]]@meta$input$PREDICTOR_FINAL<- mod2[[be]]@meta$input$PREDICTOR_FINAL[1:8]
}

# save the model
saveRDS(mod2, paste0(path_rdata, "gpm_models_paper/LUI_Model_2.rds"))
#________________________________________________________________________________________________
#__________________________________________

## MODEL 3a: depending on the result of Model_1a and Model_1b, we choose specific RE predictors or the same + LUI
mod3<- veg_re_g_gpm_indv
#only use predictors appearing in all 3 exploratories 
predictors_common <- RE_model1b[[1]]@meta$input$PREDICTOR_FINAL[RE_model1b[[1]]@meta$input$PREDICTOR_FINAL %in% RE_model1b[[2]]@meta$input$PREDICTOR_FINAL]
predictors_common <- predictors_common[predictors_common %in% RE_model1b[[3]]@meta$input$PREDICTOR_FINAL]
# remove LUI predictor
predictors_common <-predictors_common[-1]
# adding some more predictors to get a more summed up/complete list of preditcors
predictors_common <- c(predictors_common, "Near", 
                       "Red")
#predictors_common <- c(predictors_common, paste0("re04_PC", c(1,3:5), "_mean"))

predictors_common <- predictors_common[order(predictors_common)]
#save the common predictors in the final variable
RE_model1b[[1]]@meta$input$PREDICTOR_FINAL <- predictors_common
RE_model1b[[2]]@meta$input$PREDICTOR_FINAL <- predictors_common
RE_model1b[[3]]@meta$input$PREDICTOR_FINAL <- predictors_common

# save the model
saveRDS(RE_model1b, paste0(path_rdata, "gpm_models_paper/RE_Model_1b.rds"))

#________________________________________________________________________________________________
#__________________________________________


