source("C:/Users/uselig/Documents/GitHub/project_biodiv_rs/src/00_paths_libraries.R")
mods<-list.files(path_results, full.names=TRUE,pattern=glob2rx("*rds"))


# we call this function which we need in the loop afterwards for the relevant statistics in mstat
library(data.table)

mstat_fun<-function(mstat){
  std_dev<- setDT(mstat)[, lapply(.SD, sd), by = .(be, response)]
  std_dev<-std_dev[,c(1:2,4)]
  colnames(std_dev)[3]<-"rmse_sd"
  
  mean<- setDT(mstat)[, lapply(.SD, mean), by = .(be, response)]
  mean<-mean[,c(1:2,4)]
  colnames(mean)[3]<-"rmse_mean"
  
  mstat_sum<-merge(std_dev,mean)
  mstat_sum$model<-substr(basename(mods[x]), 1, nchar(basename(mods[x])) - 12)
  return(mstat_sum)
}

vstat_fun<-function(vstat){
  std_dev<- setDT(vstat)[, lapply(.SD, sd), by = .(be, response)]
  std_dev<-std_dev[,c(1:5)]
  colnames(std_dev)[3]<-"testing_response_sd"
  colnames(std_dev)[4]<-"testing_predicted_sd"
  colnames(std_dev)[5]<-"rmse_sd"
  
  mean<- setDT(vstat)[, lapply(.SD, mean), by = .(be, response)]
  mean<-mean[,c(1:5)]
  colnames(mean)[3]<-"testing_response_mean"
  colnames(mean)[4]<-"testing_predicted_mean"
  colnames(mean)[5]<-"rmse_mean"
  
  cor<- setDT(vstat)[, list(correlation= cor.test(testing_predicted, testing_response)$estimate), by= .(be, response)]
  
  vstat_sum<-merge(std_dev,mean)
  vstat_sum<-merge(vstat_sum,cor)
  vstat_sum$model<-substr(basename(mods[x]), 1, nchar(basename(mods[x])) - 12)
  return(vstat_sum)
}

# create an empty vector which we need for the loop
mstat_all=NULL
mod_sum=NULL
val_sum=NULL
vstat_all=NULL

# trying a loop  
for (x in seq(mods)){
  print(mods[x])
  mod<-readRDS(mods[x])
  ##
  # exract statistic from training results
  mstat <- lapply(mod, function(be){
    mod_r <- lapply(seq(length(be@model$pls_ffs)), function(r){
      mod_s <- lapply(seq(length(be@model$pls_ffs[[r]])), function(s){
        if(class(be@model$pls_ffs[[r]][[s]]$model) == "try-error"){
          df <- NULL
        } else {
          df <- data.frame(be = substr(be@model$pls_ffs[[r]][[s]]$training$SELECTOR[1], 1, 3), 
                           response = be@model$pls_ffs[[r]][[s]]$response,
                           ncomp = be@model$pls_ffs[[r]][[s]]$model$results$ncomp[be@model$pls_ffs[[r]][[s]]$model$bestTune$ncomp],
                           rmse = be@model$pls_ffs[[r]][[s]]$model$results$RMSE[be@model$pls_ffs[[r]][[s]]$model$bestTune$ncomp],
                           r_squared = be@model$pls_ffs[[r]][[s]]$model$results$Rsquared[be@model$pls_ffs[[r]][[s]]$model$bestTune$ncomp]
          )
        }
        return(df)
      })
      return(do.call("rbind", mod_s))
    })
    return(do.call("rbind", mod_r))
  })
  #bind mstat
  mstat<-do.call("rbind",mstat)
  rownames(mstat) <- NULL
  #for each resampling (5 fold CV) a ID
  mstat$smpl = seq(5)
  mstat$model = substr(basename(mods[x]), 1, nchar(basename(mods[x])) - 12)
  # wenn man nur eine oder ausgewählte response herausziehen will
    #mstat<-mstat[mstat$response=="SPECRICH",] # for more add: |mstat$response=="EVENESS"|mstat$response=="SHANNON"
  mstat_all<- rbind(mstat, mstat_all) # keep this df for "normal model statistics
  
  # different kind of statics with sd and mean for all values, using the function above
  act<-mstat_all
  act$model=NULL # we remove it here as setDT wont work but we recreate it in the function call
  mstat_mod<-mstat_fun(act)
  mod_sum = rbind(mod_sum, data.frame(mstat_mod))
  
  #### und für validation set
  vstat <- lapply(mod, function(be){
    mod_r <- lapply(seq(length(be@model$pls_ffs)), function(r){
      mod_s <- lapply(seq(length(be@model$pls_ffs[[r]])), function(s){
        if(class(be@model$pls_ffs[[r]][[s]]$model) == "try-error"){
          df <- NULL
        } else {
          df <- data.frame(EPID = be@model$pls_ffs[[r]][[s]]$testing$SELECTOR, 
                           be = substr(be@model$pls_ffs[[r]][[s]]$testing$SELECTOR[1], 1, 3), 
                           response = be@model$pls_ffs[[r]][[s]]$response,
                           #ncomp = be@model$pls_ffs[[r]][[s]]$model$results$ncomp[be@model$pls_ffs[[r]][[s]]$model$bestTune$ncomp],
                           testing_response = be@model$pls_ffs[[r]][[s]]$testing$RESPONSE,
                           testing_predicted = be@model$pls_ffs[[r]][[s]]$testing$PREDICTED$pred,
                           rmse = RMSE(be@model$pls_ffs[[r]][[s]]$testing$PREDICTED$pred, be@model$pls_ffs[[r]][[s]]$testing$RESPONSE),
                           rmseSD= 0,
                           cor = cor.test(be@model$pls_ffs[[r]][[s]]$testing$PREDICTED$pred, be@model$pls_ffs[[r]][[s]]$testing$RESPONSE)$estimate
          )
          
        }
        return(df)
      })
      return(do.call("rbind", mod_s))
    })
    # calculate nromalized RMSE
    for (i in seq(length(mod_r))){
      mod_r[[i]][,7]<- mod_r[[i]][,6]/ sd(mod_r[[i]]$testing_response)
    }
    #print(mod_r)
    return(do.call("rbind", mod_r))
  })
  
  #bind vstat
  vstat<-do.call("rbind",vstat)
  rownames(vstat) <- NULL
  # wenn man nur eine oder ausgewählte response herausziehen will
    #vstat<-vstat[vstat$response=="SPECRICH",]
  
  vstat$smpl = rep(1:5, each=10) #for each resampling (5 fold CV) a ID of the predicted values (always 10 predicted values per fold)
  vstat$model = substr(basename(mods[x]), 1, nchar(basename(mods[x])) - 12)
  vstat_all<- rbind(vstat, vstat_all) # keep this normal df for statistics and graphics
  
  #differnet statics with sd and mean values, using the function above
  vstat$model=NULL # we remove it here as setDT wont work but we recreate it in the function call
  vstat$EPID=NULL # need to remove this factor, otherwise setDT don't work
  vstat_mod<-vstat_fun(vstat)
  val_sum = rbind(val_sum, data.frame(vstat_mod))
  
  rm(mstat_mod, vstat_mod,mstat,vstat)
}

write.csv2(val_sum, paste0(path_stats, "overview_models_validation_results.csv"))
write.csv2(mod_sum, paste0(path_stats, "overview_models_training_results.csv"))


