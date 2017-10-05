# GLM -------------------------------------------------------
source("D:/UNI/Master/MA/exploratorien/scripts/00_set_environment.R")
pls_rfe_24_10 <- readRDS(file = paste0(path_results, "LUI_pls_rf_gam.rds"))


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

