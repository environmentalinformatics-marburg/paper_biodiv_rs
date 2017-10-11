AE142<-readRDS(paste0(path_results,"142_AEG_HEG.rds"))
AE142<-AH142
# write dataframe for tested-data of pls_rfe 142predictors
pls_100 <- lapply(AE142, function(be){
  var_imp <- compVarImp(be@model$pls_rfe, scale = FALSE)
  tstat<-compRegrTests(be@model$pls_rfe)
  return(list(var_imp = var_imp,tstat=tstat))
})
#R-sq and response statistics for actual test-data
stats <- lapply(pls_100, function(be){
  be_stats <- lapply(unique(be$tstat$model_response), function(mr){
    rs <- summary(lm(testing_predicted ~ testing_response, 
                     data = be$tstat[be$tstat$model_response == mr, ]))$r.squared
    data.frame(be = substr(be$tstat$model_selector[[1]], 1, 3),
               model_response = mr,
               r_squared = rs)
  })
  do.call("rbind", be_stats)
})
stats <- do.call("rbind", stats) #bind and set rownames to Null because ggplot (later) wont work
rownames(stats) <- NULL
saveRDS(stats, file = paste0(path_stats, "test_pls_rfe_142_7v_stats.rds"))

# statistic PLS_RFE_142 for Model training im Trainingsdatensatz -----
mod_stats_pls_rfe <- lapply(AE142, function(be){
  mod_r <- lapply(seq(length(be@model$pls_rfe)), function(r){
    mod_s <- lapply(seq(length(be@model$pls_rfe[[r]])), function(s){
      if(class(be@model$pls_rfe[[r]][[s]]$model) == "try-error"){
        df <- NULL
      } else {
        df <- data.frame(be = substr(be@model$pls_rfe[[r]][[s]]$training$SELECTOR[1], 1, 3), 
                         response = be@model$pls_rfe[[r]][[s]]$response,
                         r_squared = max(be@model$pls_rfe[[r]][[s]]$model$results$Rsquared),
                         r_squared_sd = max(be@model$pls_rfe[[r]][[s]]$model$results$RsquaredSD),
                         rmse = min(be@model$pls_rfe[[r]][[s]]$model$results$RMSE),
                         rmse_sd = min(be@model$pls_rfe[[r]][[s]]$model$results$RMSESD))
      }
      return(df)
    })
    return(do.call("rbind", mod_s))
  })
  return(do.call("rbind", mod_r))
})
mod_stats_pls_rfe <- do.call("rbind", mod_stats_pls_rfe)
rownames(mod_stats_pls_rfe) <- NULL
saveRDS(mod_stats_pls_rfe, file = paste0(path_stats, "train_pls_rfe_142_7v_stats.rds"))


