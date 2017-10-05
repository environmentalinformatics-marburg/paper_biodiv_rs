
# Set path ---------------------------------------------------------------------
source("F:/Ulli/exploratorien/scripts/00_set_environment.R")

#--------train dataset with PLS_ rfe on 10 responses with 100 predictors
if{
  veg_re_g_gpm_indv <- readRDS(file = paste0(path_rdata, "veg_rs_100.rds"))
  for(be in names(veg_re_g_gpm_indv)){
    cl <- makeCluster(detectCores())
    registerDoParallel(cl)
    
    act_gpm_selected <- veg_re_g_gpm_indv[[be]]
    
    act_gpm_selected <- trainModel(x = act_gpm_selected,
                                   n_var = NULL, 
                                   mthd = "pls",
                                   mode = "rfe",
                                   seed_nbr = 11, 
                                   cv_nbr = 5,
                                   var_selection = "indv",
                                   response_nbr = c(1:3,5:6,14:18),
                                   filepath_tmp = path_temp)
    saveRDS(act_gpm_selected, file = paste0(path_results, "pls_rfe_100_10_", be, ".rds"))
    
    stopCluster(cl) 
    
    veg_re_g_gpm_indv[[be]] <- act_gpm_selected
  }
  saveRDS(veg_re_g_gpm_indv, file = paste0(path_results, "pls_rfe_100_10.rds"))
} else {
 
#######--- calculate pls_ffs_100
   pls_rfe_100_10<- readRDS(file = paste0(path_results, "pls_rfe_100_10.rds"))
  
  for(be in names(pls_rfe_100_10)){
    
    acting <- pls_rfe_100_10[[be]]
    
    acting <- trainModel(x = acting,
                                   n_var = NULL, 
                                   mthd = "pls",
                                   mode = "ffs",
                                   seed_nbr = 11, 
                                   cv_nbr = 5,
                                   var_selection = "indv",
                                   response_nbr = c(1:3,5:6,14:18),
                                   filepath_tmp = path_temp)
    saveRDS(act_gpm_selected, file = paste0(path_results, "pls_rfe_ffs_100_10_", be, ".rds"))
    
    
    pls_rfe_100_10[[be]] <- acting
  }
  saveRDS(veg_re_g_gpm_indv, file = paste0(path_results, "pls_rfe_ffs_100_10.rds")) # make sure thats saved correctly and delete the prev.
}


# ----------train dataset with RF_ ffs on 7 responses with 100 predictors (cluster is implemented in the function)--------------------------------------------------------------

#######??????? maybe i can use the df already created!!!!!!!!
if{
  pls_rfe_ffs_100_10 <- readRDS(file = paste0(path_rdata, "pls_rfe_ffs_100_10.rds"))
  
  for(be in names(pls_rfe_ffs_100_10)){
    
    act_gpm_selected <- pls_rfe_ffs_100_10[[be]]
    
    act_gpm_selected <- trainModel(x = act_gpm_selected,
                          n_var = NULL, 
                          mthd = "rf",
                          mode = "ffs",
                          seed_nbr = 11, 
                          cv_nbr = 5,
                          var_selection = "indv",
                          response_nbr = c(1:3,5:6,14:18),
                          filepath_tmp = path_temp)
    #saveRDS(act_gpm_selected, file = paste0(path_rdata, "veg_re_g_gpm_100_", be, ".rds"))
    pls_rfe_ffs_100_10[[be]] <- act_gpm_selected
  }
  saveRDS(pls_rfe_ffs_rf_ffs_100_10, file = paste0(path_results, "pls_rfe_ffs_rf_ffs_100_10.rds"))
} else 
####--------train dataset with RF_ rfe on 10 responses with 100 predictors (cluster needs to be set)
  {
  pls_rfe_ffs_rf_ffs_100_10 <- readRDS(file = paste0(path_results, "pls_rfe_ffs_rf_ffs_100_10.rds"))
  for(be in names(pls_rfe_ffs_rf_ffs_100_10)){
    cl <- makeCluster(detectCores())
    registerDoParallel(cl)
    
    act_gpm_selected <- pls_rfe_ffs_rf_ffs_100_10[[be]]
    
    act_gpm_selected <- trainModel(x = act_gpm_selected,
                                   n_var = NULL, 
                                   mthd = "rf",
                                   mode = "rfe",
                                   seed_nbr = 11, 
                                   cv_nbr = 5,
                                   var_selection = "indv",
                                   response_nbr = c(1:3,5:6,14:18),
                                   filepath_tmp = path_temp)
    #saveRDS(act_gpm_selected, file = paste0(path_rdata, "veg_re_g_gpm_100_", be, ".rds"))
    
    stopCluster(cl)    
    pls_rfe_ffs_rf_ffs_100_10[[be]] <- act_gpm_selected
  }
  saveRDS(pls_rfe_ffs_rf_ffs_100_10, file = paste0(path_rdata, "complete_training_100_10.rds"))
  
}