#--------train dataset with PLS_ rfe on 7 responses with 24 predictors
if{
  veg_re_g_gpm_indv <- readRDS(file = paste0(path_rdata, "veg_re_g_gpm_indv.rds"))
  
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
                                   response_nbr = c(1:3,6,15:18),
                                   filepath_tmp = path_temp)
    saveRDS(act_gpm_selected, file = paste0(path_rdata, "veg_re_my_gpm_model_100_", be, ".rds"))
      
    stopCluster(cl) 
    
    veg_re_g_gpm_indv[[be]] <- act_gpm_selected
  }
  saveRDS(veg_re_g_gpm_indv, file = paste0(path_rdata, "veg_re_my_gpm_model_100.rds"))
} else {
  AEG_veg_re_my_gpm_model <- readRDS(file = paste0(path_rdata, "veg_re_my_gpm_model_AEG.rds"))
}