# Set path ---------------------------------------------------------------------
source("F:/exploratorien/scripts/00_set_environment.R")

# Analyse grassland remote sensing model ---------------------------------------
x<-readRDS(paste0(path_results,"LUI_4models.rds"))
x<-bio

#pls_ffs_24<-readRDS(paste0(path_stats,"24er/test_pls_ffs_24_stats_complete.rds"))
# statics for ffs method ---------------
pls_ffs_24 <- lapply(x, function(be){
  #regressionstest for pls ffs (2)
  tstat <- compRegrTests(be@model[[1]])
  return(list(tstat = tstat))})
#Residuen and response statistics
stats_pls_ffs24 <- lapply(pls_ffs_24, function(be){
  be_stats <- lapply(unique(be$tstat$model_response), function(mr){
    rs <- summary(lm(testing_predicted ~ testing_response,
                     data = be$tstat[be$tstat$model_response == mr, ]))$r.squared
    res<-summary(lm(testing_predicted ~ testing_response,
                    data = be$tstat[be$tstat$model_response == mr, ]))$residuals
    rmse <- sqrt(mean((be$tstat[be$tstat$model_response == mr, ]$testing_predicted - be$tstat[be$tstat$model_response == mr, ]$testing_response)**2))
    data.frame(be = substr(be$tstat$model_selector[1], 1, 3), model_response = mr, r_squared =rs , rmse = rmse, residuals=res)
  })
  do.call("rbind", be_stats)
})
stats_pls_ffs24 <- do.call("rbind", stats_pls_ffs24) #bind and set rownames to Null because ggplot (later) wont work
rownames(stats_pls_ffs24) <- NULL
saveRDS(stats_pls_ffs24, file = paste0(path_stats, "test_gam_ffs_bio_stats.rds"))

#statistical stuff
qqplot(  pls_ffs_24$AEG$tstat$testing_predicted,stats_pls_ffs24$residuals %in% stats_pls_ffs24$be=="AEG")
qqnorm(stats_pls_ffs24$residuals)
# oder standardisiert
mod<-lm(pls_ffs_24$AEG$tstat$testing_predicted ~ pls_ffs_24$AEG$tstat$testing_response)
standard<-rstandard(mod)
plot(pls_ffs_24$AEG$tstat$testing_predicted, standard, 
     ylab="Standardized Residuals", 
     xlab="predicted values") 

#residuen der trainingsdaten?
resid<-lapply(seq(length(x$AEG@model[[1]][[1]], function(s){
  residuen<-data.frame( resid(x$AEG@model[[1]][[1]][[s]]$model$results$resid))))
}

# statistic PLS_FFS for Model training im Trainingsdatensatz ----- -----
mod_stats_pls_rfe <- lapply(x, function(be){
  mod_r <- lapply(seq(length(be@model[[1]])), function(r){
    mod_s <- lapply(seq(length(be@model[[1]][[r]])), function(s){
      if(class(be@model[[1]][[r]][[s]]$model) == "try-error"){
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
saveRDS(mod_stats_pls_rfe, file = paste0(path_stats, "train_gam_ffs_bio_stats.rds"))
