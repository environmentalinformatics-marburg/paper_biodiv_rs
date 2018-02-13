

#trying new statistical ways for variance attmpt
source("D:/UNI/Master/MA/exploratorien/scripts/project_biodiv_rs/src/usel/00_b_set_environment_start_modeltrain.R")
x<-readRDS(paste0(path_results,"9CV_5models_13pred.rds"))

# regressiontests
tstat<-lapply(x,function(be){
  tstat <- compRegrTests( be@model$rf_ffs)
  return(list(tstat=tstat))
})
saveRDS(tstat, paste0(path_stats,"tstat_rf.rds"))
saveRDS(tstat, paste0(path_stats,"tstat_pls.rds"))

tstat<-readRDS(paste0(path_stats,"tstat_rf.rds"))
######## linear model output VALIDATION-Data
#------------------------------------------------------------
# we want the Rqu/RMSE from each RESAMPLE(10 iterations) set 
#  calculating/extracting variance and correlation values
linmod <-lapply(tstat, function(be){
  
  mr<-lapply(unique(be$tstat$model_response), function(mr){
    dt_r<-be$tstat[be$tstat$model_response == mr,]
    be_stats<- lapply(unique(be$tstat$sample), function(s){
      dt <- be$tstat[be$tstat$sample == s & be$tstat$model_response == mr, ]
      tstat_lm_smpl <- lm(testing_predicted ~ testing_response, 
                          data = dt)
      #rmse<- sqrt(mean((dt$testing_predicted- dt$testing_response)**2))
      rmse<-RMSE(dt$testing_predicted, dt$testing_response) # doesnt matter which fun
      data.frame(be = substr(be$tstat$model_selector[1], 1, 3), 
                 response = dt$model_response[1], #auf 1 bei mehreren responses
                 std_error=summary(tstat_lm_smpl)$coefficients,
                 #variance_anov = anova(tstat_lm_smpl),
                 rmse= rmse,
                 var=var(be$tstat[be$tstat$model_response == mr, ]$testing_response, 
                         be$tstat[be$tstat$model_response == mr, ]$testing_predicted),
                 correl=cor(be$tstat[be$tstat$model_response == mr, ]$testing_response, 
                            be$tstat[be$tstat$model_response == mr, ]$testing_predicted),
                 smpl = dt$sample[2]) #auf 1 bei mehreren responses
    })
    do.call("rbind",be_stats)  
  })
  do.call("rbind",mr)
})
linmod = do.call("rbind", linmod)
rownames(linmod) <- NULL
saveRDS(linmod,paste0(path_stats,"var_corel_std_err_rf_ffs.rds"))

tstat<-readRDS(paste0(path_stats,"tstat_pls.rds"))
###### acessing predicted and tested values
value_pred_test <-lapply(tstat, function(be){
  
  mr<-lapply(unique(be$tstat$model_response), function(mr){
    dt_r<-be$tstat[be$tstat$model_response == mr,]
    dt <- be$tstat[be$tstat$model_response == mr, ]
    tstat_lm_smpl <- lm(testing_predicted ~ testing_response, 
                        data = dt)
   data.frame(be = substr(be$tstat$model_selector[1], 1, 3), 
               response = dt$model_response[1], #auf 1 bei mehreren responses
               testing_predicted=be$tstat[be$tstat$model_response ==mr,]$testing_predicted,
               testing_response=be$tstat[be$tstat$model_response ==mr,]$testing_response)
     })
  do.call("rbind",mr)  
})

value_pred_test = do.call("rbind", value_pred_test)
rownames(value_pred_test) <- NULL



# rearrange data.frame for easier visualisation in ggplot
pls_value_pred_test = melt(value_pred_test, id.var = c("response", "be"))
rf_value_pred_test=melt(value_pred_test, id.var = c("response", "be")) #(respectively if you did tstat with the esacte model)

#merge pls und rf together (better/compact visualisation)
levels(pls_value_pred_test$variable)<-c("pls_testing_predicted","testing_response")
levels(rf_value_pred_test$variable)<-c("rf_testing_predicted","testing_response")
test_pred<-rbind(rf_value_pred_test, pls_value_pred_test)

saveRDS(value_pred_test,paste0(path_stats,"obs_pred_pls_ffs.rds"))


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
               rmse = rmse,
               var=var(be$tstat[be$tstat$model_response == mr, ]$testing_response, 
                       be$tstat[be$tstat$model_response == mr, ]$testing_predicted)
              )
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
    variance<-var(dt$testing_predicted, dt$testing_response)
    #rmse <- sqrt(mean(dt$testing_predicted- dt$testing_response)^2)
    rmse<-RMSE(dt$testing_predicted, dt$testing_response)
    data.frame(be = substr(be$tstat$model_selector[1], 1, 3), 
               response = dt$model_response[3],
               rmse= rmse,
               var=variance,
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

saveRDS(tstat_errors,paste0(path_stats,"variance_PLS_9CV_allRESP.rds"))

#train and test data 
## some statistics interpreatations
rf<-readRDS(paste0(path_stats, "allnewstats_RF_FFS_9CV_allRESP.rds"))
pls<-readRDS(paste0(path_stats, "allnewstats_PLS_FFS_9CV_allRESP.rds"))
rf[which(rf$response=="SPECRICH"& rf$variable=="rmse" & rf$smpl=="Rmean"),] #get Rmean values
pls[which(pls$response=="SPECRICH"& pls$variable=="rmse" & pls$smpl=="Rmean"),]


# prediction and validation data
## calculate mean of predicted and vaidated values
test_pred<-readRDS(paste0(path_stats,"obs_pred_PLS_RF.rds"))
meanval<-aggregate(test_pred$value, list(test_pred$response, test_pred$be, test_pred$variable), median)
colnames(meanval)<-c("response","be","variable","value")
meanval[which(meanval$response=="Species richness"& meanval$variable=="pls_testing_predicted"),]

#get min and max data from predicted models
min(rf$value[which(rf$response=="Species richness"& rf$variable=="rmse"& rf$stat=="train" & rf$be=="HEG")])
max(rf$value[which(rf$response=="Species richness"& rf$variable=="rmse"& rf$stat=="train" & rf$be=="HEG")])

min(pls$value[which(pls$response=="Species richness"& pls$variable=="rmse"& pls$stat=="train" & pls$be=="HEG")])
max(pls$value[which(pls$response=="Species richness"& pls$variable=="rmse"& pls$stat=="train" & pls$be=="HEG")])

min(pls$value[which(pls$response=="Species richness"& pls$variable=="rmse"& pls$stat=="test" & pls$be=="AEG")])
max(pls$value[which(pls$response=="Species richness"& pls$variable=="rmse"& pls$stat=="test" & pls$be=="AEG")])

min(rf$value[which(rf$response=="Species richness"& rf$variable=="rmse"& rf$stat=="test" & rf$be=="AEG")])
max(rf$value[which(rf$response=="Species richness"& rf$variable=="rmse"& rf$stat=="test" & rf$be=="AEG")])


