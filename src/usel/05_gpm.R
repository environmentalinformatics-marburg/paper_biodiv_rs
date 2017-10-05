# Set path ---------------------------------------------------------------------

compute <- TRUE

# Predict dataset --------------------------------------------------------------
if(compute){
  adf_rs_gpm <- readRDS(file = paste0(path_rdata, "gls_adf_rs_gpm_traintest.rds"))
  
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  
  adf_rs_gpm <- trainModel(x = adf_rs_gpm,
                           n_var = NULL, 
                           mthd = "pls",
                           mode = "rfe",
                           seed_nbr = 11, 
                           cv_nbr = 5,
                           var_selection = "indv", 
                           filepath_tmp = path_temp)
  saveRDS(adf_rs_gpm, file = paste0(path_rdata, "adf_rs_gpm_trainModel.rds"))
} else {
  adf_rs_gpm <- readRDS(file = paste0(path_results, "adf_rs_gpm_trainModel.rds"))
}


var_imp <- compVarImp(adf_rs_gpm@model[[1]], scale = FALSE)

var_imp_scale <- compVarImp(adf_rs_gpm@model[[1]], scale = TRUE)

var_imp_plot <- plotVarImp(var_imp)

var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")

tstat <- compRegrTests(adf_rs_gpm@model[[1]])

tstat_mean <- merge(tstat[[1]], adf_rs_gpm@model[[1]]@meta$input$MIN_OCCURENCE, 
                    by.x = "Response", by.y="names")
aggregate(tstat$r_squared, by = list(tstat$model_response), mean)
tstat_mean[order(tstat_mean$Kappa_mean, decreasing = TRUE),]

ggplot(data = t