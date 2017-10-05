# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  source("D:/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
} else {
  source("/media/permanent/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
}

compute <- TRUE

# Predict dataset --------------------------------------------------------------
if(compute){
  adf_clim_g_gpm <- readRDS(file = paste0(path_rdata, "gls_adf_clim_g_gpm_traintest.rds"))
  
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  
  adf_clim_g_gpm <- trainModel(x = adf_clim_g_gpm,
                           n_var = NULL, 
                           mthd = "pls",
                           mode = "rfe",
                           seed_nbr = 11, 
                           cv_nbr = 5,
                           var_selection = "indv",
                           response_nbr = c(1, 2, 3),
                           resample_nbr = c(1, 2),
                           filepath_tmp = path_temp)
  saveRDS(adf_rs_gpm, file = paste0(path_rdata, "adf_rs_gpm_trainModel.rds"))
} else {
  adf_rs_gpm <- readRDS(file = paste0(path_results, "adf_rs_gpm_trainModel.rds"))
}


var_imp <- compVarImp(adf_clim_g_gpm@model$pls_rfe, scale = FALSE)

var_imp_scale <- compVarImp(adf_clim_g_gpm@model$pls_rfe, scale = TRUE)

var_imp_plot <- plotVarImp(var_imp)

var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")

tstat <- compRegrTests(adf_clim_g_gpm@model$pls_rfe)

aggregate(tstat$r_squared, by = list(tstat$model_response), mean)

plotModelCV(adf_clim_g_gpm@model$pls_rfe[[1]][[2]]$model)
