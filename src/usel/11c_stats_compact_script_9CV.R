source("D:/UNI/Master/MA/exploratorien/scripts/project_biodiv_rs/src/usel/00_set_environment.R")
spec<-readRDS(paste0(path_results,"test_model_spec_7pred.rds"))

#tstat<-tstat$AEG
#tstat<-tstat$tstat[tstat$tstat$response=="SPECRICH",]

# regressiontests
tstat<-lapply(spec,function(be){
  tstat <- compRegrTests(models = be@model$pls_ffs, per_model = TRUE, 
                         per_selector = FALSE, sub_selectors = NULL, 
                         details = TRUE)
  return(list(tstat=tstat))
})

######## TESTING/VALIDATION DATA------------------------------------------------------------
# we want the Rqu/RMSE from each RESAMPLE(10 iterations) set (eventually 10 values per response and Explo.)
tstat_tstat_lm_smpl_r2 <-lapply(tstat, function(be){
  
  mr<-lapply(unique(be$tstat$model_response), function(mr){
    dt_r<-be$tstat[be$tstat$model_response == mr,]
    be_stats<- lapply(unique(be$tstat$sample), function(s){
      dt <- be$tstat[be$tstat$sample == s & be$tstat$model_response == mr, ]
      tstat_lm_smpl <- lm(testing_predicted ~ testing_response, 
                          data = dt)
      #rmse<- sqrt(mean((dt$testing_predicted- dt$testing_response)**2))
      #rmse<-rmse(dt$testing_predicted, dt$testing_response)
      rmse<-RMSE(dt$testing_predicted, dt$testing_response) # doesnt matter which fun
      data.frame(be = substr(be$tstat$model_selector[1], 1, 3), 
                 response = dt$model_response[3],
                 r_squared = summary(tstat_lm_smpl)$r.squared, #smry cause we have 5samples held out and we want 1 value per RESAMPLE (but we dont actually calculate it, the value is already there)
                 rmse= rmse,
                 smpl = dt$sample[2])
    })
    do.call("rbind",be_stats)  
  })
  do.call("rbind",mr)
})
tstat_lm_smpl_r2 = do.call("rbind", tstat_tstat_lm_smpl_r2)
rownames(tstat_lm_smpl_r2) <- NULL

# we cal. Rsq/RMSE as "usual" (one R-value per response) "all" or "Rsum"
sum_r <- lapply(tstat, function(be){
  be_stats <- lapply(unique(be$tstat$model_response), function(mr){
    rs <- summary(lm(testing_predicted ~ testing_response,
                     data = be$tstat[be$tstat$model_response == mr, ]))$r.squared
    rmse <- sqrt(mean((be$tstat[be$tstat$model_response == mr, ]$testing_predicted -
                         be$tstat[be$tstat$model_response == mr, ]$testing_response)**2))
    data.frame(be = substr(be$tstat$model_selector[1], 1, 3),
               response = mr, 
               smpl = "Rsum",
               r_squared =rs ,
               rmse = rmse)
  })
  do.call("rbind", be_stats)
})
sum_r <- do.call("rbind", sum_r) 
rownames(sum_r) <- NULL

tstat_errors = rbind(tstat_lm_smpl_r2, sum_r)

# calc. RMEAN for the RESAMPLES (1 Rmean for each of the 10 resamples)
rmean <-lapply(tstat, function(be){
  
  mr<-lapply(unique(be$tstat$model_response), function(mr){
    #dt_r<-be$tstat[be$tstat$model_response == mr,]
    dt <- be$tstat[be$tstat$model_response == mr, ]
    rmean <- mean(dt$r_squared)
    rmse <- sqrt(mean(dt$testing_predicted- dt$testing_response)^2)
    data.frame(be = substr(be$tstat$model_selector[1], 1, 3), 
               response = dt$model_response[3],
               r_squared= rmean,
               rmse= rmse,
               smpl="Rmean")
  })
  do.call("rbind",mr)  
})
rmean = do.call("rbind", rmean)
rownames(rmean)<-NULL

tstat_errors = rbind(tstat_errors, rmean)

# melt the values all in one column
tstat_errors<-melt(tstat_errors, id.var=c("response","be","smpl"))
tstat_errors$stat = "test"

####### TRAINING data--------------------------------------------

mstat <- lapply(spec, function(be){
  mod_r <- lapply(seq(length(be@model[[1]])), function(r){
    mod_s <- lapply(seq(length(be@model[[1]][[r]])), function(s){
      if(class(be@model[[1]][[r]][[s]]$model) == "try-error"){
        df <- NULL
      } else {
        df <- data.frame(be = substr(be@model[[1]][[r]][[s]]$training$SELECTOR[1], 1, 3), 
                         response = be@model[[1]][[r]][[s]]$response,
                         r_squared = be@model[[1]][[r]][[s]]$model$results$Rsquared,
                         #r_squared_sd = be@model[[1]][[r]][[s]]$model$results$RsquaredSD,
                         rmse = be@model[[1]][[r]][[s]]$model$results$RMSE)
                         #rmse_sd = be@model[[1]][[r]][[s]]$model$results$RMSESD)
      }
      return(df)
    })
    return(do.call("rbind", mod_s))
  })
  return(do.call("rbind", mod_r))
})
# calculate rmean and rmse
zu<- lapply(mstat, function(be){
data.frame(
            response=unique(be$response),
            r_squared=mean(unique(be$r_squared)),
           #r_squared_sd="r_squared_sd",
           rmse=mean(unique(be$rmse)),
           smpl="Rmean")
           #rmse_sd= "rmse_sd")
  })
rmean<-do.call("rbind", zu)
rmean$be<-c("AEG","HEG","SEG")
rownames(rmean) <- NULL
#bind mstat
mstat<-do.call("rbind",mstat)
rownames(mstat) <- NULL
#for each resampling a ID
mstat$smpl = seq(10)
#combine the mean and normal Rsq
mstat = rbind(mstat, rmean)

# make all values in a column
mstat = melt(mstat, id.var = c("response", "be", "smpl"))
mstat$stat = "train"

# join train and test data together
errors = rbind(tstat_errors, mstat)
unique(errors$variable) #check if all calcs are there there

saveRDS(errors,paste0(path_stats,"allnewstats_9CV_SPEC"))
