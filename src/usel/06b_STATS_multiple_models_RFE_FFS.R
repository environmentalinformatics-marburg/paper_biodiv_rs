# Set path ---------------------------------------------------------------------
source("F:/Ulli/exploratorien/scripts/00_set_environment.R")

# Analyse grassland remote sensing model ---------------------------------------
x<-readRDS(paste0(path_results,"complete_24_11.rds"))
sp<-readRDS(paste0(path_results,"pls_ffs__9CV_SPECRICH.rds"))
rest<-readRDS(paste0(path_results,"pls_ffs__9CV_Sh_Ev_LUI.rds"))

# RF -----------------------------------------------------------------------------------------------------------------------------
######### RF RFE method (model im 3.Listeneintrag) ----------------------------------
#varibale importance
rf_rfe_24 <- lapply(x, function(be){
  var_imp <- compVarImp(be@model[[1]], scale = FALSE)
  var_imp_scale <- compVarImp(be@model[[1]], scale = TRUE)
  var_imp_plot <- plotVarImp(var_imp)
  var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")
  tstat <- compRegrTests(be@model[[1]])
  return(list(var_imp = var_imp, var_imp_scale = var_imp_scale, 
              var_imp_plot = var_imp_plot, var_imp_heat = var_imp_heat, 
              tstat = tstat))
})
#R-sq and response statistics for actual test-data
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
saveRDS(stats_rf_rfe24, file = paste0(path_stats, "test_rf_rfe_24_stats.rds"))

# statistic RF for Model training im Trainingsdatensatz -----
mod_stats_rf_rfe <- lapply(x, function(be){
  mod_r <- lapply(seq(length(be@model[[2]])), function(r){
    mod_s <- lapply(seq(length(be@model[[2]][[r]])), function(s){
      if(class(be@model[[2]][[r]][[s]]$model) == "try-error"){
        df <- NULL
      } else {
        df <- data.frame(be = substr(be@model[[2]][[r]][[s]]$training$SELECTOR[1], 1, 3), 
                         response = be@model[[2]][[r]][[s]]$response,
                         r_squared = max(be@model[[2]][[r]][[s]]$model$results$Rsquared),
                         r_squared_sd = max(be@model[[2]][[r]][[s]]$model$results$RsquaredSD),
                         rmse = min(be@model[[2]][[r]][[s]]$model$results$RMSE),
                         rmse_sd = min(be@model[[2]][[r]][[s]]$model$results$RMSESD))
      }
      return(df)
    })
    return(do.call("rbind", mod_s))
  })
  return(do.call("rbind", mod_r))
})
mod_stats_rf_rfe <- do.call("rbind", mod_stats_rf_rfe)
rownames(mod_stats_rf_rfe) <- NULL
saveRDS(mod_stats_rf_rfe, file = paste0(path_stats, "train_rf_rfe_24_stats.rds"))

#########rf NOMODE method (model im 5.Listeneintrag)
#####
##
rf_rfe_24 <- lapply(x, function(be){
  #varibalen importance
  var_imp <- compVarImp(be@model[[5]], scale = FALSE)
  var_imp_scale <- compVarImp(be@model[[5]], scale = TRUE)
  var_imp_plot <- plotVarImp(var_imp)
  var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")
  tstat <- compRegrTests(be@model[[5]])
  return(list(var_imp = var_imp, var_imp_scale = var_imp_scale, 
              var_imp_plot = var_imp_plot, var_imp_heat = var_imp_heat,
              tstat = tstat))
})

#R-sq and response statistics for actual test-data
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
saveRDS(stats_rf_rfe24, file = paste0(path_stats, "test_rf_NOMODE_24_stats.rds"))

# statistic rf for Model training im Trainingsdatensatz ----- -----
mod_stats_rf_rfe <- lapply(x, function(be){
  mod_r <- lapply(seq(length(be@model[[5]])), function(r){
    mod_s <- lapply(seq(length(be@model[[5]][[r]])), function(s){
      if(class(be@model[[5]][[r]][[s]]$model) == "try-error"){
        df <- NULL
      } else {
        df <- data.frame(be = substr(be@model[[5]][[r]][[s]]$training$SELECTOR[1], 1, 3), 
                         response = be@model[[5]][[r]][[s]]$response,
                         r_squared = max(be@model[[5]][[r]][[s]]$model$results$Rsquared),
                         r_squared_sd = max(be@model[[5]][[r]][[s]]$model$results$RsquaredSD),
                         rmse = min(be@model[[5]][[r]][[s]]$model$results$RMSE),
                         rmse_sd = min(be@model[[5]][[r]][[s]]$model$results$RMSESD))
      }
      return(df)
    })
    return(do.call("rbind", mod_s))
  })
  return(do.call("rbind", mod_r))
})
mod_stats_rf_rfe <- do.call("rbind", mod_stats_rf_rfe)
rownames(mod_stats_rf_rfe) <- NULL
saveRDS(mod_stats_rf_rfe, file = paste0(path_stats, "train_rf_NOMODE_24_stats.rds"))


# PLS ------------------------------------------------------------------------------------------------------------------------------------
#########PLS RFE method (model im 1.Listeneintrag)
#####
##
pls_rfe_24 <- lapply(x, function(be){
  #varibalen importance
  var_imp <- compVarImp(be@model[[1]], scale = FALSE)
  var_imp_scale <- compVarImp(be@model[[1]], scale = TRUE)
  var_imp_plot <- plotVarImp(var_imp)
  var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")
  tstat <- compRegrTests(be@model[[1]])
  return(list(var_imp = var_imp, var_imp_scale = var_imp_scale, 
              var_imp_plot = var_imp_plot, var_imp_heat = var_imp_heat,
              tstat = tstat))})

#R-sq and response statistics for actual test-data
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
saveRDS(stats_pls_rfe24, file = paste0(path_stats, "test_pls_rfe_24_stats.rds"))

# statistic PLS for Model training im Trainingsdatensatz ----- -----
mod_stats_pls_rfe <- lapply(x, function(be){
  mod_r <- lapply(seq(length(be@model[[1]])), function(r){
    mod_s <- lapply(seq(length(be@model[[1]][[r]])), function(s){
      if(class(be@model[[3]][[r]][[s]]$model) == "try-error"){
        df <- NULL
      } else {
        df <- data.frame(be = substr(be@model[[1]][[r]][[s]]$training$SELECTOR[1], 1, 3), 
                         response = be@model[[1]][[r]][[s]]$response,
                         r_squared = max(be@model[[1]][[r]][[s]]$model$results$Rsquared),
                         r_squared_sd = max(be@model[[1]][[r]][[s]]$model$results$RsquaredSD),
                         rmse = min(be@model[[1]][[r]][[s]]$model$results$RMSE),
                         rmse_sd = min(be@model[[1]][[r]][[s]]$model$results$RMSESD))
      }
      return(df)
    })
    return(do.call("rbind", mod_s))
  })
  return(do.call("rbind", mod_r))
})
mod_stats_pls_rfe <- do.call("rbind", mod_stats_pls_rfe)
rownames(mod_stats_pls_rfe) <- NULL
saveRDS(mod_stats_pls_rfe, file = paste0(path_stats, "train_pls_rfe_24_stats.rds"))

#########pls NOMODE method (model im 6.Listeneintrag)
#####
##
pls_rfe_24 <- lapply(x, function(be){
  #varibalen importance
  var_imp <- compVarImp(be@model[[6]], scale = FALSE)
  var_imp_scale <- compVarImp(be@model[[6]], scale = TRUE)
  var_imp_plot <- plotVarImp(var_imp)
  var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")
  tstat <- compRegrTests(be@model[[6]])
  return(list(var_imp = var_imp, var_imp_scale = var_imp_scale, 
              var_imp_plot = var_imp_plot, var_imp_heat = var_imp_heat,
              tstat = tstat))})

#R-sq and response statistics for actual test-data
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
saveRDS(stats_pls_rfe24, file = paste0(path_stats, "test_pls_NOMODE_24_stats.rds"))

# statistic PLS for Model training im Trainingsdatensatz ----- -----
mod_stats_pls_rfe <- lapply(x, function(be){
  mod_r <- lapply(seq(length(be@model[[6]])), function(r){
    mod_s <- lapply(seq(length(be@model[[6]][[r]])), function(s){
      if(class(be@model[[6]][[r]][[s]]$model) == "try-error"){
        df <- NULL
      } else {
        df <- data.frame(be = substr(be@model[[6]][[r]][[s]]$training$SELECTOR[1], 1, 3), 
                         response = be@model[[6]][[r]][[s]]$response,
                         r_squared = max(be@model[[6]][[r]][[s]]$model$results$Rsquared),
                         r_squared_sd = max(be@model[[6]][[r]][[s]]$model$results$RsquaredSD),
                         rmse = min(be@model[[6]][[r]][[s]]$model$results$RMSE),
                         rmse_sd = min(be@model[[6]][[r]][[s]]$model$results$RMSESD))
      }
      return(df)
    })
    return(do.call("rbind", mod_s))
  })
  return(do.call("rbind", mod_r))
})
mod_stats_pls_rfe <- do.call("rbind", mod_stats_pls_rfe)
rownames(mod_stats_pls_rfe) <- NULL
saveRDS(mod_stats_pls_rfe, file = paste0(path_stats, "train_pls_NOMODE_24_stats.rds"))


# look at RF best tuning paramters
  mod_r <- lapply(seq(length(spec$AEG@model[[1]][[1]])), function(r){
    mod_s <- spec$AEG@model[[1]][[1]][[r]]$model$bestTune
  })
                    return(mod_s)

# PLS FFS statistik 
  x<-SPECRICH
  pls_ffs_24 <- lapply(x, function(be){
    #regressionstest for pls ffs (2)
    tstat <- compRegrTests(be@model$pls_ffs)
    return(list(tstat = tstat))
  })
  #R?, rmse and residuals and response statistics
  stats_pls_ffs24 <- lapply(pls_ffs_24, function(be){
    be_stats <- lapply(unique(be$tstat$model_response), function(mr){
      rs <- summary(lm(testing_predicted ~ testing_response,
                       data = be$tstat[be$tstat$model_response == mr, ]))$r.squared
      # res <- summary(lm(testing_predicted ~ testing_response,
      #                  data = be$tstat[be$tstat$model_response == mr, ]))$residuals
      rmse <- sqrt(mean((be$tstat[be$tstat$model_response == mr, ]$testing_predicted - be$tstat[be$tstat$model_response == mr, ]$testing_response)**2))
      data.frame(be = substr(be$tstat$model_selector[1], 1, 3), model_response = mr, r_squared =rs , rmse = rmse)#,residuals=res)
    })
    do.call("rbind", be_stats)
  })
  stats_pls_ffs24 <- do.call("rbind", stats_pls_ffs24) #bind and set rownames to Null because ggplot (later) wont work
  rownames(stats_pls_ffs24) <- NULL
  saveRDS(stats_pls_ffs24, file = paste0(path_stats, "test_CV9.rds"))
  
  # for training data statistic PLS_FFS for Model training im Trainingsdatensatz ----- -----
  mod_stats_pls_rfe <- lapply(x, function(be){
    mod_r <- lapply(seq(length(be@model[[2]])), function(r){
      mod_s <- lapply(seq(length(be@model[[2]][[r]])), function(s){
        if(class(be@model[[2]][[r]][[s]]$model) == "try-error"){
          df <- NULL
        } else {
          df <- data.frame(be = substr(be@model[[2]][[r]][[s]]$training$SELECTOR[1], 1, 3), 
                           response = be@model[[2]][[r]][[s]]$response,
                           r_squared = max(be@model[[2]][[r]][[s]]$model$results$Rsquared),
                           r_squared_sd = max(be@model[[2]][[r]][[s]]$model$results$RsquaredSD),
                           rmse = min(be@model[[2]][[r]][[s]]$model$results$RMSE),
                           rmse_sd = min(be@model[[2]][[r]][[s]]$model$results$RMSESD))
        }
        return(df)
      })
      return(do.call("rbind", mod_s))
    })
    return(do.call("rbind", mod_r))
  })
  mod_stats_pls_rfe <- do.call("rbind", mod_stats_pls_rfe)
  rownames(mod_stats_pls_rfe) <- NULL
  saveRDS(mod_stats_pls_rfe, file = paste0(path_stats, "train_CV9.rds"))
  
  
  