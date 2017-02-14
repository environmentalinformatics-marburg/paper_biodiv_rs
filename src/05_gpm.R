# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  source("D:/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
} else {
  source("/media/permanent/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
}

compute <- TRUE

# Predict dataset --------------------------------------------------------------
if(compute){
  adf_rs_gpm <- readRDS(file = paste0(path_rdata, "gls_adf_rs_gpm_traintest.rds"))
  
  # cl <- makeCluster(detectCores())
  # registerDoParallel(cl)
  # stopCluster(cl)
  pred_final_org <- adf_rs_gpm@meta$input$PREDICTOR_FINAL
  adf_rs_gpm@meta$input$PREDICTOR_FINAL <- pred_final_org[1:3]
  
  adf_rs_gpm <- trainModel(x = adf_rs_gpm,
                           n_var = NULL, 
                           mthd = "pls",
                           mode = "rfe",
                           seed_nbr = 11, 
                           cv_nbr = 5,
                           var_selection = "indv", 
                           filepath_tmp = NULL)
  saveRDS(adf_rs_gpm, file = paste0(path_rdata, "adf_rs_gpm_trainModel.rds"))
} else {
  adf_rs_gpm <- readRDS(file = paste0(path_results, "adf_rs_gpm_trainModel.rds"))
}


var_imp <- compVarImp(adf_rs_gpm@model[[1]], scale = FALSE)

var_imp_scale <- compVarImp(adf_rs_gpm@model, scale = TRUE)

var_imp_plot <- plotVarImp(var_imp)

var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")

tstat <- compRegrTests(adf_rs_gpm@model[[1]])

tstat_mean <- merge(tstat[[1]], adf_rs_gpm[[prj]]@meta$input$MIN_OCCURENCE, 
                    by.x = "Response", by.y="names")

tstat_mean[order(tstat_mean$Kappa_mean, decreasing = TRUE),]

ggplot(data = t