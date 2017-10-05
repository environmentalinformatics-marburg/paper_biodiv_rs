# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  source("D:/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
} else {
  source("/media/permanent/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
}

compute <- TRUE

# Analyse grassland remote sensing model ---------------------------------------
veg_re_f <- readRDS(paste0(path_rdata, "veg_re_f_gpm_indv_model_rf.rds"))  
veg_re_f <- readRDS(paste0(path_rdata, "veg_re_f_gpm_indv_model_pls.rds"))  

veg_re_f[[1]]@model[[1]][[1]][[1]]$response


veg_re_f_stat <- lapply(veg_re_f, function(be){
  var_imp <- compVarImp(be@model[[1]], scale = FALSE)
  var_imp_scale <- compVarImp(be@model[[1]], scale = TRUE)
  var_imp_plot <- plotVarImp(var_imp)
  var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")
  tstat <- compRegrTests(be@model[[1]])
  return(list(var_imp = var_imp, var_imp_scale = var_imp_scale, 
              var_imp_plot = var_imp_plot, var_imp_heat = var_imp_heat,
              tstat = tstat))
})

stats <- lapply(veg_re_f_stat, function(be){
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
saveRDS(stats, file = paste0(path_rdata, "veg_re_f_gpm_indv_model_rf_stats.rds"))

stats[order(stats$r_squared),]

mr_all <- c("SMId", "SMIr", "SMI")
ggplot(data = stats[stats$model_response %in% mr_all,], aes(x = model_response, y = r_squared, color = be)) + 
  geom_point()

ggplot(data = stats, aes(x = model_response, y = r_squared, color = be)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


mod_stats <- lapply(veg_re_f, function(be){
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
saveRDS(mod_stats, file = paste0(path_rdata, "veg_re_f_gpm_indv_model_rf_mod_stats.rds"))

mr_all <- c("SMId", "SMIr", "SMI", "dc_2D")
ggplot(data = mod_stats[mod_stats$response %in% mr_all,], aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot()

ggplot(data = mod_stats, aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

mr_all <- c("SMId", "SMIr", "SMI", "dc_2D")
ggplot(data = mod_stats[mod_stats$response %in% mr_all,], aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = stats[stats$model_response %in% mr_all,], aes(x = model_response, y = r_squared, color = be)) +
  labs(title = "LUI & meteorology", x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE)

