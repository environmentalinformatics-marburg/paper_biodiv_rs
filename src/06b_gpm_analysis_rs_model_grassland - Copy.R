# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  source("D:/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
} else {
  source("/media/permanent/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
}

compute <- TRUE

# Analyse grassland remote sensing model ---------------------------------------
veg_re_g <- readRDS(paste0(path_rdata, "veg_re_g_gpm_indv_model_rf.rds"))  
veg_re_g <- readRDS(paste0(path_rdata, "veg_re_g_gpm_indv_model_pls.rds"))  

veg_re_g[[1]]@model$rf_rfe[[1]][[1]]$response


veg_re_g_stat <- lapply(veg_re_g, function(be){
  var_imp <- compVarImp(be@model[[1]], scale = FALSE)
  var_imp_scale <- compVarImp(be@model[[1]], scale = TRUE)
  var_imp_plot <- plotVarImp(var_imp)
  var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")
  tstat <- compRegrTests(be@model[[1]])
  return(list(var_imp = var_imp, var_imp_scale = var_imp_scale, 
              var_imp_plot = var_imp_plot, var_imp_heat = var_imp_heat,
              tstat = tstat))
})


# veg_re_g_stat_rf <- veg_re_g_stat
tstat_AEG <- veg_re_g_stat[[1]]$tstat
summary(lm(testing_predicted ~ testing_response, data = tstat_AEG[tstat_AEG$model_response == "SPECRICH", ]))

tstat_HEG <- veg_re_g_stat[[2]]$tstat
summary(lm(testing_predicted ~ testing_response, data = tstat_HEG[tstat_HEG$model_response == "SPECRICH", ]))

tstat_SEG <- veg_re_g_stat[[3]]$tstat
summary(lm(testing_predicted ~ testing_response, data = tstat_SEG[tstat_SEG$model_response == "SPECRICH", ]))


ggplot(data = tstat[tstat$model_response == "SHANNON", ], aes(x = testing_response, y = testing_predicted)) + 
  geom_point()
