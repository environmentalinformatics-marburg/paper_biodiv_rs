source("C:/Users/uselig/Documents/GitHub/project_biodiv_rs/src/00_paths_libraries.R")
mods<-list.files(path_results, full.names=TRUE,pattern=glob2rx("*rds"))

# create an empty vector which we need for the loop
mstat_all=NULL
mod_sum=NULL
val_sum=NULL
vstat_all=NULL

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

mstat$model=NULL # we remove it here as setDT wont work but we recreate it in the function call

mstat_mod<-mstat_fun(mstat)
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
                         rmse = RMSE(be@model$pls_ffs[[r]][[s]]$testing$PREDICTED$pred, be@model$pls_ffs[[r]][[s]]$testing$RESPONSE)
        )
      }
      return(df)
    })
    return(do.call("rbind", mod_s))
  })
  return(do.call("rbind", mod_r))
})
#bind vstat
vstat<-do.call("rbind",vstat)
rownames(vstat) <- NULL
# wenn man nur eine oder ausgewählte response herausziehen will
#vstat<-vstat[vstat$response=="SPECRICH",]

#for each resampling (5 fold CV) a ID of the predicted values (always 10 predicted values per fold)
vstat$smpl = rep(1:5, each=10)
vstat$model = substr(basename(mods[x]), 1, nchar(basename(mods[x])) - 12)
vstat_all<- rbind(vstat, vstat_all) # keep this normal df for statistics and graphics

vstat$model=NULL # we remove it here as setDT wont work but we recreate it in the function call
vstat$EPID=NULL # need to remove this factor, otherwise setDT don't work
vstat_mod<-vstat_fun(vstat)
val_sum = rbind(val_sum, data.frame(vstat_mod))
rm(mstat_mod, vstat_mod,mstat,vstat)
}

write.csv2(val_sum, paste0(path_stats, "overview_models_validation_results.csv"))
write.csv2(mod_sum, paste0(path_stats, "overview_models_training_results.csv"))

#######################################################################################################
# VISUALISATION
  
library(wesanderson)
models=c("Model_5_RECLIMALUI","Model_4_CLIMALUI","Model_3_RELUI","Model_2_LUI","Model_1_RE","Model_0_RE")


## plot CV folds of model training (boxplots von CV-folds of the models)
m= list() # create empty list for all models

for (x in seq(models)){
m[[x]]<-ggplot(data= mstat_all[mstat_all$model %in% models[x],], # füge hinzu wenn nur reine response güwnscht [mstat$response %in% "SPECRICH",]
                 aes( x= smpl ,y=rmse,
                      fill=be))+
  geom_bar(position="dodge", colour="black", stat="identity")+
  #scale_fill_brewer(palette="Pastel1")+
  scale_fill_manual(values=c("#660066","#FFCC33","#CC6600"), 
                    name=substr(basename(mods[x]), 1, nchar(basename(mods[x])) - 12))+
  #coord_cartesian(xlim = NULL, ylim = c(0:15),expand = FALSE)+
  #scale_x_discrete(breaks=c(20,30,40,50,60), limits=15:60)+
  #scale_y_discrete(breaks=c(4,5,6,7,8,9), limits=3:15)+
  facet_wrap(~response, scales = "free")+
  labs(title = models[x],
                 x = "Fold number", y = "RMSE")
}
do.call(grid.arrange, m)
# save image via export: 

#####################################
# plot the correlation of validation

    # #make a title if you have on single model as df
    # tit<-paste("Predictions using the final model of each fold", 
    #            substr(basename(mods[x]), 1, nchar(basename(mods[x])) - 12))

# plot
v= list() # create empty list for all models
for (x in seq(models)){
v[[x]]<-ggplot(data= vstat_all[vstat_all$model %in% models[x],], #füge hinzu wenn nur eine response geünscht [ vstat$response %in% "SPECRICH",]
       aes( x= testing_response ,y=testing_predicted,
            colour=be, fill=be))+
  geom_point(position="jitter",shape=21,color = "black", size=1)+
  geom_smooth(method='lm', span= 0.9)+geom_abline(show.legend=F)+
  scale_color_manual(values=c("#660066","#FFCC33","#CC6600"),
                     name="",labels=c("Alb","Hainich","Schorfheide"))+
  scale_fill_manual(values=c("#660066","#FFCC33","#CC6600"),
                    name="",
                    labels=c("Alb","Hainich","Schorfheide"))+
  facet_wrap(~response, scales = "free")+
  labs(title = models[x] ,
       x = "observed values", y = "predicted values")
#print(v) # only if you want to see each model on single page
}
do.call(grid.arrange, v)
# save image via export: validation_AllModels_AllResponses 1300x700

#############################################################################################################
## SPIELWIESE
##############

    #sd berechnen, dazu muss der einzelne rmse wert gezogen werden
      # vstat_var<-vstat[!duplicated(vstat$rmse),]
      # aggregate( rmse ~ be, vstat_var, sd)
    # Rsq and Correlation
      # summary(lm(testing_predicted~testing_response, data=vstat[vstat$be=="AEG"& vstat$response=="SPECRICH",]))$r.squared 
      # cor.test(vstat$testing_predicted[vstat$be=="AEG"& vstat$response=="SPECRICH"],
      #          vstat$testing_response[vstat$be=="AEG"& vstat$response=="SPECRICH"])$estimate



