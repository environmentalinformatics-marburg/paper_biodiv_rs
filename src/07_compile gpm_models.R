
source("C:/Users/uselig/Documents/GitHub/project_biodiv_rs/src/00_paths_libraries.R")
#read base gpm object
veg_re_g_gpm_indv<-readRDS(paste0(path_rdata, "gpm_models_paper/GPM-base-object.rds"))

#________________________________________________________________________________________________
############################
# SPECIFY the model RESPONSE
############################__________________________________________
###### NOTE: RESPONSE-FINAL BRINGT NICHTS! WIRD IN GPM TRAINING ANSCHEINEND NICHT BERÜCKSICHTIGT (Stand januar 2019),
####### geht dann nur im trainModel Aufruf mit z.B. response_nbr= c(1:3,5,12,14)

# in manchen plots sind in der response NA Werte, die Modelle können dann nicht rechnen, deswegen nehmen wir sie erst jetzt an dieser Stelle raus
# und nicht bevor wir die TrainTest Selektion festlegen, da sonst die ganze Zeile herausgelöscht werden muss,wo alle anderen responses auch drin sind

# remove HEG09 and HEG44 only in response traintest biomass
for (train in seq(length(veg_re_g_gpm_indv$HEG@meta$input$TRAIN_TEST[[14]]))){
  veg_re_g_gpm_indv$HEG@meta$input$TRAIN_TEST[[14]][[train]]$training$SAMPLES<- veg_re_g_gpm_indv$HEG@meta$input$TRAIN_TEST[[14]][[train]]$training$SAMPLES[
    veg_re_g_gpm_indv$HEG@meta$input$TRAIN_TEST[[14]][[train]]$training$SAMPLES != 9] #lösche HEG09 in biomass (14)
  veg_re_g_gpm_indv$HEG@meta$input$TRAIN_TEST[[14]][[train]]$training$SAMPLES<- veg_re_g_gpm_indv$HEG@meta$input$TRAIN_TEST[[14]][[train]]$training$SAMPLES[
    veg_re_g_gpm_indv$HEG@meta$input$TRAIN_TEST[[14]][[train]]$training$SAMPLES != 44] #lösche HEG44 in biomass (14)
  
}

# remove AEG09 only from response bare soil
for (train in seq(length(veg_re_g_gpm_indv$AEG@meta$input$TRAIN_TEST[[12]]))){
  veg_re_g_gpm_indv$AEG@meta$input$TRAIN_TEST[[12]][[train]]$training$SAMPLES<- veg_re_g_gpm_indv$AEG@meta$input$TRAIN_TEST[[12]][[train]]$training$SAMPLES[
    veg_re_g_gpm_indv$AEG@meta$input$TRAIN_TEST[[12]][[train]]$training$SAMPLES != 9] #lösche AEG09 in baresoil (12)
  
}
#________________________________________________________________________________________________
############################
# SPECIFY the model PREDICTORS
############################__________________________________________

## MODEL 1: RE predictors "model 1": predictors are the same in all BE
  mod1<- veg_re_g_gpm_indv
  
  #only use predictors appearing in all 3 exploratories 
  predictors_common <- mod1[[1]]@meta$input$PREDICTOR_FINAL[mod1[[1]]@meta$input$PREDICTOR_FINAL %in% mod1[[2]]@meta$input$PREDICTOR_FINAL]
  predictors_common <- predictors_common[predictors_common %in% mod1[[3]]@meta$input$PREDICTOR_FINAL]
  # remove CLIMA predictors
  predictors_common <-predictors_common[-c(13:14)]
    # adding some more predictors to get a more summed up/complete list of preditcors
  predictors_common <- c(predictors_common, "RE_NDVI_diff", "Blue")
  
  predictors_common <- predictors_common[order(predictors_common)]
  #save the common predictors in the final variable
  for(be in names(mod1)){
    mod1[[be]]@meta$input$PREDICTOR_FINAL<- predictors_common
  }
# save the model
saveRDS(mod1, paste0(path_rdata, "gpm_models_paper/RE_Model_1.rds"))
#________________________________________________________________________________________________
#__________________________________________

# SPECIFY the model
# Model 0- use ONLY RE predictors ohne 2.RE Szene
mod0<-veg_re_g_gpm_indv

pred_common<-predictors_common[-c(4,9)]
for(be in names(mod0)){
  mod0[[be]]@meta$input$PREDICTOR_FINAL<- mod1[[be]]@meta$input$PREDICTOR_FINAL[-c(4,9)]
}
# save the model
saveRDS(mod0, paste0(path_rdata, "gpm_models_paper/RE_Model_0.rds"))

#________________________________________________________________________________________________
#__________________________________________

# SPECIFY the model
# Model 2- use ONLY LUI components as final predictors, but we only use regional components ("LUI_reg" etc.)
mod2<-veg_re_g_gpm_indv
## choose FINAL predictors for LUI only model (Model2)- all predictors are chosen for all BE
for(be in names(mod2)){
  mod2[[be]]@meta$input$PREDICTOR_FINAL<- mod2[[be]]@meta$input$PREDICTOR[5:8]
}

# save the model
saveRDS(mod2, paste0(path_rdata, "gpm_models_paper/LUI_Model_2.rds"))

#________________________________________________________________________________________________
#__________________________________________

# SPECIFY the model
# Model 3- use all LUI components from mod2 AND final predictors from mod1 as final predictors
mod3<-veg_re_g_gpm_indv

for(be in names(mod3)){
  mod3[[be]]@meta$input$PREDICTOR_FINAL<- c(mod1[[be]]@meta$input$PREDICTOR_FINAL,
                                            mod2[[be]]@meta$input$PREDICTOR_FINAL) 
}

# save the model
saveRDS(mod3, paste0(path_rdata, "gpm_models_paper/RELUI_Model_3.rds"))

#________________________________________________________________________________________________
#__________________________________________

# SPECIFY the model
# Model 4- use all LUI components from mod2 AND CLIMA VARIBLES as final predictors
mod4<-veg_re_g_gpm_indv

for(be in names(mod4)){
  mod4[[be]]@meta$input$PREDICTOR_FINAL<- c(mod2[[be]]@meta$input$PREDICTOR_FINAL,
                                            mod2[[be]]@meta$input$PREDICTOR[c(65,71:76)]) #some climate predictors are removed because of NA values
}

# save the model
saveRDS(mod4, paste0(path_rdata, "gpm_models_paper/CLIMALUI_Model_4.rds"))

#________________________________________________________________________________________________
#__________________________________________

# SPECIFY the model
# Model 5- use all LUI AND CLIMA components from mod4 AND RE_Final predictors as final predictors
mod5<-veg_re_g_gpm_indv

for(be in names(mod5)){
  mod5[[be]]@meta$input$PREDICTOR_FINAL<- c(mod4[[be]]@meta$input$PREDICTOR_FINAL,
                                            mod1[[be]]@meta$input$PREDICTOR_FINAL) 
}

# save the model
saveRDS(mod5, paste0(path_rdata, "gpm_models_paper/RECLIMALUI_Model_5.rds"))


