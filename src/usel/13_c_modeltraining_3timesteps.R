source("D:/UNI/Master/MA/exploratorien/scripts/project_biodiv_rs/src/usel/00_b_set_environment_start_modeltrain.R")

x<-readRDS(paste0(path_rdata,"specmodel_3timesteps.rds"))

for(be in names(x)){
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  
  act_gpm_selected <- x[[be]]
  
  act_gpm_selected <- trainModel(x = act_gpm_selected,
                                 n_var = NULL,             
                                 mthd = "pls", #rf, gam, glmboost
                                 mode = "ffs",
                                 seed_nbr = 11, 
                                 cv_nbr = 9,
                                 var_selection = "indv",
                                 response_nbr = 1,
                                 filepath_tmp = path_temp
  )
  x[[be]] <- act_gpm_selected
  stopCluster(cl) 
}

saveRDS(x, file = paste0(path_results, "3timesteps_pls_ffs_9CV_AEG.rds"))
