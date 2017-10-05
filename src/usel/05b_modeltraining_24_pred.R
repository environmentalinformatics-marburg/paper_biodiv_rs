
source("D:/UNI/Master/MA/exploratorien/scripts/00_set_environment.R")

#--------train dataset with PLS_ rfe on 10 responses with 24 predictors
if{
  veg_re_g_gpm_indv <- readRDS(file = paste0(path_rdata, "gpm_obj_24pred.rds"))
  
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
    saveRDS(act_gpm_selected, file = paste0(path_results, "pls_rfe_24_10_", be, ".rds"))
    
    stopCluster(cl) 
    
    veg_re_g_gpm_indv[[be]] <- act_gpm_selected
  }
  saveRDS(veg_re_g_gpm_indv, file = paste0(path_results, "pls_rfe_24_10.rds"))
} 
if { 
  ####--------------------make pls with ffs on 24 predictors in the new dataframe
  
  pls_rfe_24_10 <- readRDS(file = paste0(path_results, "pls_rfe_24_10.rds"))
  
  for(be in names(pls_rfe_24_10)){
    
    act <- pls_rfe_24_10[[be]]
    
    act <- trainModel(x = act,
                      n_var = NULL, 
                      mthd = "pls",
                      mode = "ffs",
                      seed_nbr = 11, 
                      cv_nbr = 5,
                      var_selection = "indv",
                      response_nbr = c(1:3,5:6,13:18),
                      filepath_tmp = path_temp)
    saveRDS(act, file = paste0(path_results, "pls_rfe_ffs_24_10_", be, ".rds"))
    
    pls_rfe_24_10[[be]] <- act
  }
  saveRDS(pls_rfe_24_10, file = paste0(path_results, "pls_rfe_ffs_24_10.rds")) #check if its all in one df and delete the previous output
} 
else {
# train dataset with RF_ ffs on 11 responses with 24 predictors (cluster needs to be set) ----------
  pls_rfe_ffs_24_10<-readRDS(paste0(path_results,"pls_rfe_ffs_24_10.rds"))
    
    for(be in names(pls_rfe_ffs_24_10)){
      
      act_gpm_selected <- pls_rfe_ffs_24_10[[be]]
      
      act_gpm_selected <- trainModel(x = act_gpm_selected,
                                     n_var = NULL, 
                                     mthd = "rf",
                                     mode = "ffs",
                                     seed_nbr = 11, 
                                     cv_nbr = 5,
                                     var_selection = "indv",
                                     response_nbr = c(1:3,5:6,13:18),
                                     filepath_tmp = path_temp)
      saveRDS(act_gpm_selected, file = paste0(path_results, "pls_rfe_ffs_rf_ffs_24_10_", be, ".rds"))
      pls_rfe_ffs_24_10[[be]] <- act_gpm_selected
    }
    saveRDS(pls_rfe_ffs_rf_ffs_24_10, file = paste0(path_results, "pls_rfe_ffs_rf_ffs_24_10.rds"))
  } else 
    ####--------train dataset with RF_ rfe on 11 responses with 24 predictors (cluster needs to be set)
  {
    pls_rfe_ffs_rf_ffs_24_10 <- readRDS(file = paste0(path_results, "pls_rfe_ffs_rf_ffs_24_10.rds"))
    for(be in names(pls_rfe_ffs_rf_ffs_24_10)){
      cl <- makeCluster(detectCores())
      registerDoParallel(cl)
      
      act_gpm_selected <- pls_rfe_ffs_rf_ffs_24_10[[be]]
      
      act_gpm_selected <- trainModel(x = act_gpm_selected,
                                     n_var = NULL, 
                                     mthd = "rf",
                                     mode = "rfe",
                                     seed_nbr = 11, 
                                     cv_nbr = 5,
                                     var_selection = "indv",
                                     response_nbr = c(1:3,5:6,13:18),
                                     filepath_tmp = path_temp)
      saveRDS(act_gpm_selected, file = paste0(path_results, "pls_rfe_ffs_rf_ffs_rfe_24_10_", be, ".rds"))
      
      stopCluster(cl)    
      pls_rfe_ffs_rf_ffs_24_10[[be]] <- act_gpm_selected
    }
    saveRDS(pls_rfe_ffs_rf_ffs_24_10, file = paste0(path_results, "complete_training_24_10.rds"))
    
  }

# attach the nomode models to the complete training model
x<-readRDS(paste0(path_results,"complete_24_10.rds"))
rf<-readRDS(paste0(path_results,"no_mode_rf_24_11.rds")) #note: both rf and pls are in this file


#now i need to attach the rf_rfe df to the prev. df
x$AEG@model$rf_nomode<-rf$AEG@model$rf_rfe
x$HEG@model$rf_nomode<-rf$HEG@model$rf_rfe
x$SEG@model$rf_nomode<-rf$SEG@model$rf_rfe

x$AEG@model$pls_nomode<-rf$AEG@model$pls_rfe
x$HEG@model$pls_nomode<-rf$HEG@model$pls_rfe
x$SEG@model$pls_nomode<-rf$SEG@model$pls_rfe

saveRDS(x, file = paste0(path_results, "complete_24_10.rds"))



  #####------------------make rf with rfe (its already been done)----------{
'  #take a new df in case you destroy everything
  pls_rfe_ffs_24_10<-readRDS(paste0(path_results,"pls_rfe_ffs_24_10.rds")) 
  #read in the 3 rf_rfe dataframes (already calculated)
  Arf_rfe24<-readRDS(file = paste0(path_results, "backup/rf_rfe_24_AEG.rds")) 
  Hrf_rfe24<-readRDS(file = paste0(path_results, "backup/rf_rfe_24_HEG.rds"))
  Srf_rfe24<-readRDS(file = paste0(path_results, "backup/rf_rfe_24_SEG.rds"))
  #list it together
  rf_rfe_list<-list(Arf_rfe24,Hrf_rfe24,Srf_rfe24)
  rm(Arf_rfe24,Hrf_rfe24,Srf_rfe24)
  names(rf_rfe_list)<-names( pls_rfe__ffs_24_10)

