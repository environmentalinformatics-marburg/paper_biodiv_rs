# Set path ---------------------------------------------------------------------
source("F:/exploratorien/scripts/00_set_environment.R")

# Analyse grassland remote sensing model ---------------------------------------
x<-readRDS(paste0(path_results,"complete_24_10.rds"))

####random forest rfe method (model im 3.Listeneintrag)
  #varibale importance
rf_rfe_24 <- lapply(x, function(be){
 
  var_imp <- compVarImp(be@model[[3]], scale = FALSE)
  var_imp_scale <- compVarImp(be@model[[3]], scale = TRUE)
  var_imp_plot <- plotVarImp(var_imp)
  var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")
  tstat <- compRegrTests(be@model[[3]])
  return(list(var_imp = var_imp, var_imp_scale = var_imp_scale, 
              var_imp_plot = var_imp_plot, var_imp_heat = var_imp_heat,
              tstat = tstat))})
  #R? and response statistics for actual test-data
stats_rf_rfe24 <- lapply(rf_rfe_24, function(be){
  be_stats <- lapply(unique(be$tstat$model_response), function(mr){
    rs <- summary(lm(testing_predicted ~ testing_response, 
                     data = be$tstat[be$tstat$model_response == mr, ]))$r.squared
    data.frame(be = substr(be$tstat$model_selector[[1]], 1, 3),
               model_response = mr,
               r_squared = rs)
  })
  do.call("rbind", be_stats)
})
stats_rf_rfe24 <- do.call("rbind", stats_rf_rfe24) #bind and set rownames to Null because ggplot (later) wont work
rownames(stats_rf_rfe24) <- NULL
saveRDS(stats_rf_rfe24, file = paste0(path_results, "rf_rfe_24_stats.rds"))

#########pls rfe method (model im 1.Listeneintrag)
pls_rfe_24 <- lapply(x, function(be){
  #varibale importance
  var_imp <- compVarImp(be@model[[1]], scale = FALSE)
  var_imp_scale <- compVarImp(be@model[[1]], scale = TRUE)
  var_imp_plot <- plotVarImp(var_imp)
  var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")
  tstat <- compRegrTests(be@model[[1]])
  return(list(var_imp = var_imp, var_imp_scale = var_imp_scale, 
              var_imp_plot = var_imp_plot, var_imp_heat = var_imp_heat,
              tstat = tstat))})
  #R? and response statistics
stats_pls_rfe24 <- lapply(pls_rfe_24, function(be){
  be_stats <- lapply(unique(be$tstat$model_response), function(mr){
    rs <- summary(lm(testing_predicted ~ testing_response, 
                     data = be$tstat[be$tstat$model_response == mr, ]))$r.squared
    data.frame(be = substr(be$tstat$model_selector[[1]], 1, 3),
               model_response = mr,
               r_squared = rs)
  })
  do.call("rbind", be_stats)
})
stats_pls_rfe24 <- do.call("rbind", stats_pls_rfe24) #bind and set rownames to Null because ggplot (later) wont work
rownames(stats_pls_rfe24) <- NULL
saveRDS(stats_pls_rfe24, file = paste0(path_results, "pls_rfe_24_stats.rds"))

# statistic for Model training and testing -----
mod_stats_rf_rfe <- lapply(x, function(be){
  mod_r <- lapply(seq(length(be@model[[3]])), function(r){
    mod_s <- lapply(seq(length(be@model[[3]][[r]])), function(s){
      if(class(be@model[[3]][[r]][[s]]$model) == "try-error"){
        df <- NULL
      } else {
        df <- data.frame(be = substr(be@model[[3]][[r]][[s]]$training$SELECTOR[1], 1, 3), 
                         response = be@model[[3]][[r]][[s]]$response,
                         r_squared = max(be@model[[3]][[r]][[s]]$model$results$Rsquared),
                         r_squared_sd = max(be@model[[3]][[r]][[s]]$model$results$RsquaredSD),
                         rmse = max(be@model[[3]][[r]][[s]]$model$results$RMSE),
                         rmse_sd = max(be@model[[3]][[r]][[s]]$model$results$RMSESD))
      }
      return(df)
    })
    #return(do.call("rbind", mod_s))
  })
  #return(do.call("rbind", mod_r))
})
mod_stats_rf_rfe <- do.call("rbind", mod_stats_rf_rfe)
rownames(mod_stats_rf_rfe) <- NULL

saveRDS(mod_stats_rf_rfe, file = paste0(path_results, "tmp_mod_stats_rf_rfe.rds"))


# statics for ffs method ---------------

plsffs<-readRDS(paste0(filepath_base,"update_v_PC/win/results/pls_rfe_ffs_24_10_AEG.rds"))

x<-plsffs

stats_pls_ffs24 <- tstat <- compRegrTests(x@model[[2]])

stats_pls_ffs24 <- lapply(stats_pls_ffs24, function(be){
  be_stats <- lapply(unique(tstat$model_response), function(mr){
    rs <- summary(lm(testing_predicted ~ testing_response, 
                     data = tstat[tstat$model_response == mr, ]))$r.squared
    data.frame(be = substr(tstat$model_selector[[1]], 1, 3),
               model_response = mr,
               r_squared = rs)
  })
  do.call("rbind", be_stats)
})
stats_pls_ffs24 <- do.call("rbind", stats_pls_ffs24) #bind and set rownames to Null because ggplot (later) wont work
rownames(stats_pls_ffs24) <- NULL
saveRDS(stats_pls_ffs24, file = paste0(path_results, "pls_ffs_24_stats.rds"))

# statistic for Model training and testing -----

#mod_stats_pls_ffs <- lapply(x, function(be){
mod_stats_pls_ffs<- 
    mod_r <- lapply(seq(length(x@model[[2]])), function(r){
    mod_s <- lapply(seq(length(x@model[[2]][[r]])), function(s){
      if(class(x@model[[2]][[r]][[s]]$model) == "try-error"){
        df <- NULL
      } else {
        df <- data.frame(be = substr(x@model[[2]][[r]][[s]]$training$SELECTOR[1], 1, 3), 
                         response = x@model[[2]][[r]][[s]]$response,
                         r_squared = max(x@model[[2]][[r]][[s]]$model$results$Rsquared),
                         r_squared_sd = max(x@model[[2]][[r]][[s]]$model$results$RsquaredSD),
                         rmse = max(x@model[[2]][[r]][[s]]$model$results$RMSE),
                         rmse_sd = max(x@model[[2]][[r]][[s]]$model$results$RMSESD))
      }
      return(df)
    })
    return(do.call("rbind", mod_s))
  return(do.call("rbind", mod_r))
})
mod_stats_pls_ffs <- do.call("rbind", mod_stats_pls_ffs)
rownames(mod_stats_pls_ffs) <- NULL

saveRDS(mod_stats_pls_ffs, file = paste0(path_results, "mod_stats_pls_ffs.rds"))
