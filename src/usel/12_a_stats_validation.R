

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

spec<-value_pred_test[value_pred_test$response=="SPECRICH",]
linmodel_aeg<-glm(testing_predicted~testing_response, data=spec[spec$be=="AEG",])
summary(linmodel_aeg)
# PLS p-value: 0.0007482 ist <0,05 --> ich interpretiere den p-Wert aus Tabelle: 9.7e-14 ***
# RF  p-value: 0.0001613 p-Wert: 7.75e-11 ***
linmodel_heg<-glm(testing_predicted~testing_response, data=spec[spec$be=="HEG",])
summary(linmodel_heg)
#PLS p-value: 0.1755 das Modell ist nichts wert
# RF p-value: 0.069 p-Wert: 2.18e-13 ***
linmodel_seg<-glm(testing_predicted~testing_response, data=spec[spec$be=="SEG",])
summary(linmodel_seg)
# PLS p-value: 0.09332 ich interpretiere p-Wert aus Tabelle: <2e-16 ***
# RF p-value: 0.9099 das mOdell ist nichts wert

#aber die R² sind immer rottig?!
# was ist mit übrprüfungstest: Multikoll. und Autokorrel.? 


# rearrange data.frame for easier visualisation in ggplot
pls_value_pred_test = melt(value_pred_test, id.var = c("response", "be"))
rf_value_pred_test=melt(value_pred_test, id.var = c("response", "be")) #(respectively if you did tstat with the esacte model)

#merge pls und rf together (better/compact visualisation)
levels(pls_value_pred_test$variable)<-c("pls_testing_predicted","testing_response")
levels(rf_value_pred_test$variable)<-c("rf_testing_predicted","testing_response")
test_pred<-rbind(rf_value_pred_test, pls_value_pred_test)

saveRDS(test_pred,paste0(path_stats,"obs_pred_PLS_RF.rds"))

# linearmodel<-lm(seg$testing_predicted~aeg$testing_response)
# plot(linearmodel)
# coef<-coef(linearmodel)
# predict(linearmodel, interval="confidence") # according to the model to species
# #richness is, on average, between21 and 24 on a 95 percent confidence interval around the mean prediction
# predict(linearmodel,interval="prediction") # 95%of species richness are between 19 and 27 (R dummie p.298)

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
rf[which(rf$response=="SPECRICH"& rf$variable=="rmse" & rf$smpl=="Rmean"),]
pls[which(pls$response=="SPECRICH"& pls$variable=="rmse" & pls$smpl=="Rmean"),]


# prediction and validation data
## calculate mean of predicted and vaidated values
test_pred<-readRDS(paste0(path_stats,"obs_pred_PLS_RF.rds"))
meanval<-aggregate(test_pred$value, list(test_pred$response, test_pred$be, test_pred$variable), median)
colnames(meanval)<-c("response","be","variable","value")
meanval[which(meanval$response=="Species richness"& meanval$variable=="pls_testing_predicted"),]

#normalize RMSE to get percentage of results
library(hydroGOF)
test<-test_pred[which(test_pred$response=="Species richness"& test_pred$be=="SEG"),]
#test<-test[101:150,]
sim<-test[which(test$variable=="pls_testing_predicted"),]
obs<-test[which(test$variable=="testing_response"),]
obs<-obs[1:50,]
nrmse(sim$value,obs$value, norm="maxmin")
