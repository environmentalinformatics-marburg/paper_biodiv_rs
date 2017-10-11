veg_re_g_gpm_indv@model[[1]][[1]][[3]]$model #statistik
veg_re_g_gpm_indv$AEG@model[[1]][[1]][[1]]$model #statistik rf


#look at the files from training
first<-readRDS(file = paste0(path_rdata, "veg_re_my_gpm_model.rds")) #rf rfe model output with 24 predictors, 8 responses
firstsingleexplo<-readRDS(file = paste0(path_rdata, "veg_re_my_gpm_model_AEG.rds")) #pls rfe output with 24 predictors, 8 responses (only through this file accessable)

second<-readRDS(file = paste0(path_rdata, "veg_re_my_gpm_model_100.rds")) #pls rfe 100 predictors,8 responses
secondsingleexplo<-readRDS(file=paste0(path_rdata,"veg_re_my_gpm_model_100_AEG.rds")) #pls rfe with all responses?

third<-readRDS(file = paste0(path_rdata, "veg_re_g_gpm_indv_model.rds")) #rf ffs

veg_re_g_gpm_indv<-first
###########-------einige Abfragen nach dem training
saveRDS(veg_re_g_gpm_indv, file = paste0(path_rdata, "veg_re_g_gpm_indv_model.rds"))
}
var_imp <- compVarImp(veg_re_g_gpm_indv@model$pls_rfe, scale = FALSE)
be
be = names(veg_re_g_gpm_indv)[[1]]
be
var_imp <- compVarImp(veg_re_g_gpm_indv$AEG@model$pls_rfe, scale = FALSE)
var_imp_scale <- compVarImp(veg_re_g_gpm_indv$AEG@model$pls_rfe, scale = TRUE)
var_imp_plot <- plotVarImp(var_imp)
var_imp_plot
veg_re_g_gpm_indv$AEG@model
veg_re_g_gpm_indv$AEG@model[1]
var_imp <- compVarImp(veg_re_g_gpm_indv$AEG@model[1], scale = FALSE)
veg_re_g_gpm_indv$AEG@model$rf_rfe[[1]][[1]]
veg_re_g_gpm_indv$AEG@model$rf_rfe[[1]][[1]]$model
veg_re_g_gpm_indv$AEG@model[1][[1]][[1]]$model
veg_re_g_gpm_indv$AEG@model[[1]][[1]][[1]]$model
var_imp <- compVarImp(veg_re_g_gpm_indv$AEG@model[[1]], scale = FALSE)
veg_re_g_gpm_indv$AEG@model[1][[1]][[1]]$model
veg_re_g_gpm_indv$AEG@model[[1]][[1]][[1]]$model
veg_re_g_gpm_indv$AEG@model[[1]][[1]][[1]]$model$finalModel
caret::varImp(veg_re_g_gpm_indv$AEG@model[[1]][[1]][[1]]$model)
caret::varImp(veg_re_g_gpm_indv$AEG@model[[1]][[1]][[1]]$model$finalModel)
caret::varImp(veg_re_g_gpm_indv$AEG@model[[1]][[1]][[1]]$model$pred)
veg_re_g_gpm_indv$AEG@model[[1]][[1]][[1]]$model$pred
veg_re_g_gpm_indv$AEG@model[[1]][[1]][[1]]$model
veg_re_g_gpm_indv$AEG@model[[1]][[1]][[1]]$model$finalModel
veg_re_g_gpm_indv$AEG@model[[1]][[1]][[1]]$model$finalModel$call
veg_re_g_gpm_indv$AEG@model[[1]][[1]][[1]]$model$finalModel$predicted
veg_re_g_gpm_indv$AEG@model[[1]][[1]][[1]]$model$finalModel$importance
veg_re_g_gpm_indv$AEG@model[[1]][[1]][[1]]$model$finalModel

tstat <- compRegrTests(veg_re_g_gpm_indv$AEG@model[[1]])
tstat
aggregate(tstat$r_squared, by = list(tstat$model_response), mean)
tstat
plot(tstat$testing_response, tstat$testing_predicted)
summary(lm(tstat$testing_response~tstat$testing_predicted))
tstat <- compRegrTests(veg_re_g_gpm_indv$AEG@model)
tstat <- compRegrTests(veg_re_g_gpm_indv$AEG@model[[1]])
plot(tstat$testing_response, tstat$testing_predicted)
summary(lm(tstat$testing_response~tstat$testing_predicted))
boxplot(tstat$testing_response - tstat$testing_predicted)
boxplot(tstat$testing_predicted/tstat$testing_response)
veg_re_g_gpm_indv$AEG@model[[1]][[1]][[1]]$model
veg_re_g_gpm_indv[[1]]@model[[1]][[1]][[1]]$model$finalModel$importance

veg_re_g_gpm_indv$AEG@model[[1]][[1]][[1]]$model$finalModel$importance
veg_re_g_gpm_indv$AEG@model[[1]][[1]][[2]]$model$finalModel$importance
veg_re_g_gpm_indv$AEG@model[[1]][[1]][[3]]$model$finalModel$importance

veg_re_g_gpm_indv$AEG@model[[1]][[1]][[3]]$model


str(veg_re_g_gpm_indv$AEG@model[[1]][[1]][[3]]$model$finalModel)
str(veg_re_g_gpm_indv$AEG@model[[1]][[1]][[3]]$model$finalModel$importance)
veg_re_g_gpm_indv$AEG@model[[1]]
veg_re_g_gpm_indv$AEG@model[[2]]
veg_re_g_gpm_indv$AEG@model[[1]]

veg_re_g_gpm_indv$AEG@model$rf_ffs[[1]][[1]]$model
veg_re_g_gpm_indv$AEG@model$rf_ffs[[1]][[1]]$model$finalModel
veg_re_g_gpm_indv$AEG@model$rf_ffs[[1]][[1]]$model$finalModel$importance
veg_re_g_gpm_indv$AEG@model$rf_ffs[[1]][[2]]$model$finalModel$importance
veg_re_g_gpm_indv$AEG@model$rf_ffs[[1]][[3]]$model$finalModel$importance
second$AEG@model$pls_rfe[[1]][[1]]$model
second$AEG@model$pls_rfe[[1]][[1]]$model$fit
second$AEG@model$pls_rfe[[1]][[1]]$model$variables

veg_re_g_gpm_indv$AEG@model$rf_ffs[[1]][[1]]$model
veg_re_g_gpm_indv$AEG@model$rf_ffs[[1]][[1]]$model$finalModel
veg_re_g_gpm_indv$AEG@model$rf_ffs[[1]][[1]]$model$finalModel$importance
veg_re_g_gpm_indv$AEG@model$rf_ffs[[1]][[2]]$model$finalModel$importance
veg_re_g_gpm_indv$AEG@model$rf_ffs[[1]][[3]]$model$finalModel$importance
second$AEG@model$pls_rfe[[1]][[1]]$model
second$AEG@model$pls_rfe[[1]][[1]]$model$fit
second$AEG@model$pls_rfe[[1]][[1]]$model$variables
                