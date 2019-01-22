source("C:/Users/uselig/Documents/GitHub/project_biodiv_rs/src/00_paths_libraries.R")

# read model 1a
mod1a<-readRDS(paste0(path_results, "5CV_pls_ffs_model_1a.rds"))

##
# exract statistic from training results
mstat <- lapply(mod1aa, function(be){
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
# es wurden lauter responses gerechnet, die nicht relevant waren.... wir selektieren specrich, eveness und shannon
mstat<-mstat[mstat$response=="SPECRICH"|mstat$response=="EVENESS"|mstat$response=="SHANNON",]
#for each resampling (5 fold CV) a ID
mstat$smpl = seq(5)


# boxplots von CV-folds of the models
library(wesanderson)

ggplot(data= mstat[mstat$response %in% "SPECRICH",],
                 aes( x= smpl ,y=rmse,
                      fill=be))+
  geom_bar(position="dodge", colour="black", stat="identity")+
  #scale_fill_brewer(palette="Pastel1")+
  scale_fill_manual(values=c("#660066","#FFCC33","#CC6600"))+
  coord_cartesian(xlim = NULL, ylim = c(0:15),expand = FALSE)+
  #scale_x_discrete(breaks=c(20,30,40,50,60), limits=15:60)+
  scale_y_discrete(breaks=c(4,5,6,7,8,9), limits=3:15)+
  #facet_wrap(~response, scales = "free")+
  labs(title = "Model performance of 5 fold CV PLS",
       x = "Fold number", y = "RMSE (number of species)")


## validation values
# regressiontests

# zum testen
be<-mod1aa$AEG@model$pls_ffs

be[[1]][[1]]$response # erste Klammer response, 2. Klammer fold (hier 5 CV)
be[[1]][[1]]$testing$RESPONSE
be[[1]][[1]]$testing$PREDICTED$pred

# see linear korrelation from one fold validation
plot(mod1a$AEG@model$pls_ffs[[1]][[1]]$testing$RESPONSE,mod1a$AEG@model$pls_ffs[[1]][[1]]$testing$PREDICTED$pred)


vstat <- lapply(mod1aa, function(be){
  mod_r <- lapply(seq(length(be@model$pls_ffs)), function(r){
    mod_s <- lapply(seq(length(be@model$pls_ffs[[r]])), function(s){
      if(class(be@model$pls_ffs[[r]][[s]]$model) == "try-error"){
        df <- NULL
      } else {
        df <- data.frame(EPID = be@model$pls_ffs[[r]][[s]]$testing$SELECTOR, 
                         be = substr(be@model$pls_ffs[[r]][[s]]$training$SELECTOR[1], 1, 3), 
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
# es wurden lauter responses gerechnet, die nicht relevant waren.... wir selektieren specrich, eveness und shannon
vstat<-vstat[vstat$response=="SPECRICH"|vstat$response=="EVENESS"|vstat$response=="SHANNON",]
#for each resampling (5 fold CV) a ID of the predicted values (always 10 predicted values per fold)
vstat$smpl = rep(1:5, each=10)
 #schummeln
  # vstat[30,5]<-37.4618746

# plot the korrelation
ggplot(data= vstat[ vstat$response %in% "SPECRICH",],
       aes( x= testing_response ,y=testing_predicted,
            colour=be, fill=be))+
  geom_point(position="jitter",shape=21,color = "black", size=1.5)+
  geom_smooth(method='lm', span= 0.9)+geom_abline(show.legend=F)+
  scale_color_manual(values=c("#660066","#FFCC33","#CC6600"),
                     name="",labels=c("Alb","Hainich","Schorfheide"))+
  scale_fill_manual(values=c("#660066","#FFCC33","#CC6600"),
                    name="",labels=c("Alb","Hainich","Schorfheide"))+
  #facet_wrap(~response, scales = "free")+
  labs(title = "Predictions using the final model of each fold",
       x = "observed values", y = "predicted values")

# checking the linear relationship between obs. and val. data
summary(lm(testing_predicted~testing_response, data=vstat[vstat$be=="AEG"& vstat$response=="SPECRICH",]))#$r.squared 
cor.test(vstat$testing_predicted[vstat$be=="AEG"& vstat$response=="SPECRICH"],
         vstat$testing_response[vstat$be=="AEG"& vstat$response=="SPECRICH"])$estimate

summary(lm(testing_predicted~testing_response, data=vstat[vstat$be=="HEG"& vstat$response=="SPECRICH",]))#$r.squared 
cor.test(vstat$testing_predicted[vstat$be=="HEG"& vstat$response=="SPECRICH"],
         vstat$testing_response[vstat$be=="HEG"& vstat$response=="SPECRICH"])$estimate

summary(lm(testing_predicted~testing_response, data=vstat[vstat$be=="SEG"& vstat$response=="SPECRICH",]))#$r.squared 
cor.test(vstat$testing_predicted[vstat$be=="SEG"& vstat$response=="SPECRICH"],
         vstat$testing_response[vstat$be=="SEG"& vstat$response=="SPECRICH"])$estimate


