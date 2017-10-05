# GLM -------------------------------------------------------
source("D:/UNI/Master/MA/exploratorien/scripts/00_set_environment.R")
source("C:/Users/tnauss/permanent/plygrnd/exploratorien/project_biodiv_rs/src/tnauss/00_set_environment.R")
pls_rfe_24_10 <- readRDS(file = paste0(path_rdata, "test_model_7preds.rds"))
pls_rfe_24_10

be = "SEG"

for(be in names(pls_rfe_24_10)){
  
  act <- pls_rfe_24_10[[be]]
  act@meta$input$PREDICTOR_FINAL = c("PC3_median", "PC3_sd", "PC1_sd",
                                     "PC1_median", "PC2_sd", "PC5_median")
  
  act <- trainModel(x = act,
                    n_var = NULL, 
                    mthd = "pls",
                    mode = "ffs",
                    seed_nbr = 11, 
                    cv_nbr = 5,
                    var_selection = "indv",
                    response_nbr = 1,
                    filepath_tmp = path_temp,
                    runParallel=T)
  
  
  # act_01 <- act
  
  act@model$pls_ffs[[1]][[1]]$model$finalModel$xNames
  act_01@model$pls_ffs[[1]][[1]]$model$finalModel$xNames
  
  act@model$pls_ffs[[1]][[1]]$model
  act_01@model$pls_ffs[[1]][[1]]$model
  
  act@model$pls_ffs[[1]][[1]]$testing$PREDICTED
  act_01@model$pls_ffs[[1]][[1]]$testing$PREDICTED
  
  vars <- lapply(act_01@model$pls_ffs[[1]], function(r){
    r$model$finalModel$xNames
  })
  unique(unlist(vars))
  
  
  
  eval <- lapply(seq(length(act@model$pls_ffs[[1]])), function(i){
    data.frame(ACT = act@model$pls_ffs[[1]][[i]]$testing$PREDICTED,
               ACT1 = act_01@model$pls_ffs[[1]][[i]]$testing$PREDICTED)
  })
  eval <- do.call("rbind", eval)
  eval$diff = eval$pred-eval$pred.1
  sum(eval$diff == 0)
  max(eval$diff)
  
  
  saveRDS(act, file = paste0(path_results, "LUI_4models_", be, ".rds"))
  
  pls_rfe_24_10[[be]] <- act
}
saveRDS(pls_rfe_24_10, file = paste0(path_results, "LUI_4models.rds")) #check if its all in one df and delete the previous output


pls_rfe_24_10 <- readRDS(file = paste0(path_rdata, "LUImodel.rds"))

for(be in names(pls_rfe_24_10)){
  
  act <- pls_rfe_24_10[[be]]
  
  act <- trainModel(x = act,
                    n_var = NULL, 
                    mthd = "glmboost",
                    mode = "ffs",
                    seed_nbr = 11, 
                    cv_nbr = 5,
                    var_selection = "indv",
                    response_nbr = 18,
                    filepath_tmp = path_temp,
                    runParallel=T)
  saveRDS(act, file = paste0(path_results, "pls_rfe_ffs_24_10_", be, ".rds"))
  
  pls_rfe_24_10[[be]] <- act
}
saveRDS(pls_rfe_24_10, file = paste0(path_results, "glm_ffs_LUI.rds")) #check if its all in one df and delete the previous output

