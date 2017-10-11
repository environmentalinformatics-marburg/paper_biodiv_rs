# Start with model without split mult resp:
veg_re_g_gpm = readRDS( paste0(path_rdata,"initial_model.rds"))
belc = c("AEG", "HEG", "SEG")
veg_re_g_gpm_indv = lapply(belc, function(be){
  return(splitMultRespLSO(x = veg_re_g_gpm_indv[[be]], nbr = 5))
  
})
names(veg_re_g_gpm_indv) = belc

#only use predictors appearing in all 3 exploratories (leaves us eventually with 24 predictors)
predictors_common <- veg_re_g_gpm_indv[[1]]@meta$input$PREDICTOR_FINAL[veg_re_g_gpm_indv[[1]]@meta$input$PREDICTOR_FINAL %in% veg_re_g_gpm_indv[[2]]@meta$input$PREDICTOR_FINAL]
predictors_common <- predictors_common[predictors_common %in% veg_re_g_gpm_indv[[3]]@meta$input$PREDICTOR_FINAL]
# adding some more predictors to get a more summed up/complete list of preditcors
predictors_common <- c(predictors_common, "PC1_sd", "PC1_var", "PC2_sd", paste0("PC", c(1,3:5), "_median"))
predictors_common <- predictors_common[order(predictors_common)]
#save the common predictors in the final variable
veg_re_g_gpm_indv[[1]]@meta$input$PREDICTOR_FINAL <- predictors_common
veg_re_g_gpm_indv[[2]]@meta$input$PREDICTOR_FINAL <- predictors_common
veg_re_g_gpm_indv[[3]]@meta$input$PREDICTOR_FINAL <- predictors_common

# Test: just use some of them...
for(i in seq(3)){
  veg_re_g_gpm_indv[[i]]@meta$input$PREDICTOR_FINAL = c("PC1_sd","PC2_sd", "PC3_median","PC4_median","PC4_sd", "PC5_median", "PC5_sd",
                                                        "pca_Grey.Level_Nonuniformity.b1r7o1_median") #this is for Ev und Shan
}

saveRDS(veg_re_g_gpm_indv, paste0(path_rdata,"5_split_model.rds"))
readRDS(paste0(path_rdata,"5_split_model.rds"))

cl <- makeCluster(detectCores())
registerDoParallel(cl)

for(be in names(veg_re_g_gpm_indv)){
  act_gpm_selected <- veg_re_g_gpm_indv[[be]]
  act_gpm_selected <- trainModel(x = act_gpm_selected,
                                 n_var = NULL,             
                                 mthd = "pls", #rf, gam, glmboost
                                 mode = "ffs",
                                 seed_nbr = 11, 
                                 cv_nbr = 9,
                                 var_selection = "indv",
                                 response_nbr = c(1:3),
                                 filepath_tmp = path_temp)
  veg_re_g_gpm_indv[[be]] <- act_gpm_selected
}

saveRDS(veg_re_g_gpm_indv, "9CV_pls_model_7pred.rds")
