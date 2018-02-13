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

veg_re_g[[1]]@model[[1]][[1]][[1]]$response


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

stats <- lapply(veg_re_g_stat, function(be){
  be_stats <- lapply(unique(be$tstat$model_response), function(mr){
    rs <- summary(lm(testing_predicted ~ testing_response, 
                     data = be$tstat[be$tstat$model_response == mr, ]))$r.squared
    data.frame(be = substr(be$tstat$model_selector[[1]], 1, 3),
               model_response = mr,
               r_squared = rs)
  })
  do.call("rbind", be_stats)
})
stats <- do.call("rbind", stats)
rownames(stats) <- NULL
saveRDS(stats, file = paste0(path_rdata, "veg_re_g_gpm_indv_model_rf_stats.rds"))

mr_all <- c("SPECRICH", "SHANNON", "EVENESS", "cover_shrubs_pc", "cover_herbs_pc", "biomass_g")
mr_two <- c("LUI_glb", "LUI_reg", "cover_lichens_pc", "biomass_g", "vegetation_height_mean_cm", "cover_bryophytes_pc", "cover_litter_pc")
ggplot(data = stats[stats$model_response %in% mr_all,], aes(x = model_response, y = r_squared, color = be)) + 
  geom_point()

ggplot(data = stats[stats$model_response %in% mr_two,], aes(x = model_response, y = r_squared, color = be)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


mod_stats <- lapply(veg_re_g, function(be){
  mod_r <- lapply(seq(length(be@model[[1]])), function(r){
    mod_s <- lapply(seq(length(be@model[[1]][[r]])), function(s){
      if(class(be@model[[1]][[r]][[s]]$model) == "try-error"){
        df <- NULL
      } else {
        df <- data.frame(be = substr(be@model[[1]][[r]][[s]]$training$SELECTOR[1], 1, 3), 
                         response = be@model[[1]][[r]][[s]]$response,
                         r_squared = max(be@model[[1]][[r]][[s]]$model$results$Rsquared),
                         r_squared_sd = max(be@model[[1]][[r]][[s]]$model$results$RsquaredSD),
                         rmse = max(be@model[[1]][[r]][[s]]$model$results$RMSE),
                         rmse_sd = max(be@model[[1]][[r]][[s]]$model$results$RMSESD))
      }
      return(df)
    })
    return(do.call("rbind", mod_s))
  })
  return(do.call("rbind", mod_r))
})
mod_stats <- do.call("rbind", mod_stats)
rownames(mod_stats) <- NULL

saveRDS(mod_stats, file = paste0(path_rdata, "veg_re_g_gpm_indv_model_rf_mod_stats.rds"))

mr_all <- c("SPECRICH", "SHANNON", "EVENESS", "cover_shrubs_pc", "cover_herbs_pc", "biomass_g")
mr_two <- c("LUI_glb", "LUI_reg", "cover_lichens_pc", "biomass_g", "vegetation_height_mean_cm", "cover_bryophytes_pc", "cover_litter_pc")
ggplot(data = mod_stats[mod_stats$response %in% mr_all,], aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot()

ggplot(data = mod_stats[mod_stats$response %in% mr_two,], aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))