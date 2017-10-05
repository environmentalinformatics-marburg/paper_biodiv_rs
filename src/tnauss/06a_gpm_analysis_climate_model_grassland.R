# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  source("D:/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
} else {
  source("/media/permanent/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
}

compute <- TRUE

# Analyse grassland remote sensing model ---------------------------------------
veg_meta_g <- readRDS(paste0(path_rdata, "veg_meta_g_gpm_indv_model_rf.rds"))  




var_imp <- compVarImp(veg_meta_g@model[[1]], scale = FALSE)
var_imp_scale <- compVarImp(veg_meta_g@model[[1]], scale = TRUE)
var_imp_plot <- plotVarImp(var_imp)
var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")
tstat <- compRegrTests(veg_meta_g@model[[1]])
veg_meta_g_stat <- list(var_imp = var_imp, var_imp_scale = var_imp_scale, 
                        var_imp_plot = var_imp_plot, var_imp_heat = var_imp_heat,
                        tstat = tstat)



stats <- lapply(unique(veg_meta_g_stat$tstat$model_response), function(mr){
  be_stats <- lapply(unique(substr(veg_meta_g_stat$tstat$model_selector, 1, 3)), function(be){
    rs <- summary(lm(testing_predicted ~ testing_response, 
                     data = veg_meta_g_stat$tstat[veg_meta_g_stat$tstat$model_response == mr &
                                                    substr(veg_meta_g_stat$tstat$model_selector, 1, 3) == be, ]))$r.squared
    data.frame(be = be,
               model_response = mr,
               r_squared = rs)
  })
  be_stats <- do.call("rbind", be_stats)
})
stats <- do.call("rbind", stats)
rownames(stats) <- NULL
saveRDS(stats, file = paste0(path_rdata, "veg_meta_g_gpm_indv_model_rf_stats.rds"))

ggplot(data = stats, aes(x = model_response, y = r_squared, color = be)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

veg_meta_g_stat$tstat$be <- substr(veg_meta_g_stat$tstat$model_selector, 1, 3)

ggplot(data = veg_meta_g_stat$tstat, aes(x = model_response, y = r_squared, fill = be)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


stats_mod <- lapply(veg_meta_g@model[[1]], function(r){
  mod_s <- lapply(r, function(s){
    if(class(s$model) == "try-error"){
      df <- NULL
    } else {
      df <- data.frame(be = substr(s$training$SELECTOR[1], 1, 3), 
                       response = s$response,
                       r_squared = max(s$model$results$Rsquared),
                       r_squared_sd = max(s$model$results$RsquaredSD),
                       rmse = max(s$model$results$RMSE),
                       rmse_sd = max(s$model$results$RMSESD))
    }
    return(df)
  })
  return(do.call("rbind", mod_s))
})
stats_mod <- do.call("rbind", stats_mod)
rownames(stats_mod) <- NULL
saveRDS(stats_mod, file = paste0(path_rdata, "veg_meta_g_gpm_indv_model_rf_stats_mod.rds"))

mr <- c("SPECRICH")
cbPalette <- c('#d53e4f','#fc8d59','#fee08b','#e6f598','#99d594','#3288bd')
ggplot(data = stats_mod[stats_mod$response %in% mr, ], aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() + 
  geom_point(data = stats[stats$model_response %in% mr, ], aes(x = model_response, y = r_squared, color = be)) +
  labs(title = "LUI & meteorology", x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE)


