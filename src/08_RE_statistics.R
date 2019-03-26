source("C:/Users/uselig/Documents/GitHub/project_biodiv_rs/src/00_paths_libraries.R")
mods<-list.files(path_results, full.names=TRUE,pattern=glob2rx("*rds"))

# create an empty vector which we need for the loop
mstat_all=NULL
vstat_all=NULL

# loop over all 6 model approaches, BEs and responses  
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
                         #responsevalue = be@model$pls_ffs[[r]][[s]]$training$RESPONSE,
                         ncomp = be@model$pls_ffs[[r]][[s]]$model$results$ncomp[be@model$pls_ffs[[r]][[s]]$model$bestTune$ncomp],
                         rmse = be@model$pls_ffs[[r]][[s]]$model$results$RMSE[be@model$pls_ffs[[r]][[s]]$model$bestTune$ncomp],
                         r_squared = be@model$pls_ffs[[r]][[s]]$model$results$Rsquared[be@model$pls_ffs[[r]][[s]]$model$bestTune$ncomp]
                         #rmseSD = 0
        )
      }
      #print(df)
      return(df)
    })
    return(do.call("rbind", mod_s))
  })
  # calculate normalized RMSE
    # for (i in seq(length(mod_r))){
    #   mod_r[[i]][,6]<- mod_r[[i]][,5]/ sd(mod_r[[i]]$responsevalue)
    # }
  #print(mod_r)
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

###############################################################################################################
#############################################
#### und für validation set
#############################################
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
                         rmse = RMSE(be@model$pls_ffs[[r]][[s]]$testing$PREDICTED$pred, be@model$pls_ffs[[r]][[s]]$testing$RESPONSE, na.rm = TRUE),
                         rmseSD= 0, # empty now, will be filled in line 112-114
                         cor = cor.test(be@model$pls_ffs[[r]][[s]]$testing$PREDICTED$pred, be@model$pls_ffs[[r]][[s]]$testing$RESPONSE)$estimate
                         )
      }
      return(df)
    })
    return(do.call("rbind", mod_s))
  })
  # calculate normalized RMSE
  for (i in seq(length(mod_r))){
    mod_r[[i]][,7]<- mod_r[[i]][,6]/ sd(mod_r[[i]]$testing_response, na.rm=TRUE)
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

rm(mstat,vstat) #remove temporary looping dfs
}

# get RMSE_SD also
{aeg <- mod$AEG@data$input[, c("EPID","SPECRICH", "SHANNON", "EVENESS", "cover_shrubs_pc", "cover_bare_soil_pc", "biomass_g")]
aeg$EPID<-"AEG"
heg <- mod$HEG@data$input[, c("EPID","SPECRICH", "SHANNON", "EVENESS", "cover_shrubs_pc", "cover_bare_soil_pc", "biomass_g")]
heg$EPID<-"HEG"
seg <- mod$SEG@data$input[, c("EPID","SPECRICH", "SHANNON", "EVENESS", "cover_shrubs_pc", "cover_bare_soil_pc", "biomass_g")]
seg$EPID<-"SEG"

inda<- rbind(aeg,heg,seg)
input_melt <- melt(inda)
sdobs<-aggregate( .~EPID+variable, data=input_melt, sd, na.rm=TRUE)

mstat_all<- merge(mstat_all, sdobs, by.x=c("be","response"), by.y=c("EPID","variable"))
mstat_all$rmse_sd<-mstat_all$rmse/mstat_all$value 
mstat_all$value<-NULL
rm(aeg,heg,seg,inda,input_melt,sdobs)}
####################

write.csv2(vstat_all, paste0(path_stats, "overview_models_validation_results.csv"))
saveRDS(vstat_all, paste0(path_stats, "overview_models_validation_results.rds"))
write.csv2(mstat_all, paste0(path_stats, "overview_models_training_results.csv"))
saveRDS(mstat_all, paste0(path_stats, "overview_models_training_results.rds"))


#############################################################################################################
## SPIELWIESE
##############

# get mean RMSE for each model
v_cor<-vstat_all %>%                
  group_by(model, response, be) %>%    # Group by these variables
  summarise( 
    age.mean = mean(cor) #get mean of correlation coeffients
  )

mstat_mean<-mstat_all %>%                
  group_by(model, response, be) %>%    # Group by these variables
  summarise( 
    rmse.mean = mean(rmse) #get mean of correlation coeffients
  )

mr<-c("Species richness","Eveness") #only for specrich and eveness
mstat_mean<-mstat_mean[mstat_mean$response %in% mr,]
levels(mstat_mean$be)<-c("Alb","Hainich", "Schorfheide")
#mstat_mean<-mstat_mean[order(mstat_mean$be),]
#mstat_mean<-mstat_mean[order(mstat_mean$model),]
write.csv2(mstat_mean, "C:/exploratorien/Latex/publication/mstat_mean.csv")
