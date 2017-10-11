# Some testing
# Set path ---------------------------------------------------------------------
source("F:/exploratorien/scripts/00_set_environment.R")
source("C:/Users/tnauss/permanent/plygrnd/exploratorien/project_biodiv_rs/src/tnauss/00_set_environment.R")
library(doParallel)
library(ggplot2)
library(foreach)
library(reshape2)
library(gpm)


  # Start with model without split mult resp:
veg_re_g_gpm = readRDS(paste0(path_rdata, "uselig_initial_model.rds"))
belc = c("AEG", "HEG", "SEG")
veg_re_g_gpm_indv = lapply(belc, function(be){
  return(splitMultRespLSO(x = veg_re_g_gpm[[be]], nbr = 5))
  
})
names(veg_re_g_gpm_indv) = belc
veg_re_g_gpm_indv[["AEG"]]@meta$input$TRAIN_TEST

#only use predictors appearing in all 3 exploratories (leaves us eventually with 24 predictors)
predictors_common <- veg_re_g_gpm_indv[[1]]@meta$input$PREDICTOR_FINAL[veg_re_g_gpm_indv[[1]]@meta$input$PREDICTOR_FINAL %in% veg_re_g_gpm_indv[[2]]@meta$input$PREDICTOR_FINAL]
predictors_common <- predictors_common[predictors_common %in% veg_re_g_gpm_indv[[3]]@meta$input$PREDICTOR_FINAL]
# adding some more predictors to get a more summed up/complete list of preditcors
predictors_common <- c(predictors_common, "PC1_sd", "PC1_var", "PC2_sd", paste0("PC", c(1,3:5), "_median"))
predictors_common <- predictors_common[order(predictors_common)]
#save the common predictors in the final variable
veg_re_g_gpm_indv[[1]]@meta$input$PREDICTOR_FINAL <- predictors_common
veg_re_g_gpm_indv[[2]]@meta$input$PREDICTOR_FINAL <- predictors_common
veg_re_g_gpm_indv[[3]]@meta$input$PREDICTOR_FINAL <- predictors_common

# Test: just use some of them...
for(i in seq(3)){
  veg_re_g_gpm_indv[[i]]@meta$input$PREDICTOR_FINAL = c("PC3_median", "PC3_sd", "PC1_sd",
                                                        "PC1_median", "PC2_sd", "PC5_median")
}

#c("PC1_sd","PC2_sd", "PC3_median","PC4_median","PC4_sd", "PC5_median", "PC5_sd","PC1_median", "PC2_sd", "PC5_median")
#"pca_Grey.Level_Nonuniformity.b1r7o1_median") #this is for Ev und Shan

cl <- makeCluster(detectCores())
registerDoParallel(cl)
test_model <- lapply(belc, function(be){
  return(trainModel(x = veg_re_g_gpm_indv[[be]],
                           n_var = NULL,             
                           mthd = "pls", #rf, gam, glmboost
                           mode = "ffs",
                           seed_nbr = 11, 
                           cv_nbr = 9,
                           var_selection = "indv",
                           response_nbr = 2,
                           filepath_tmp = path_temp))
})
names(test_model) = belc
# saveRDS(test_model, file = paste0(path_rdata, "test_model_2017-10-05.rds"))
# test_model = readRDS(file = paste0(path_rdata, "test_model_2017-10-05.rds"))

eval = lapply(belc, function(be){
  tstat = compRegrTests(models = test_model[[be]]@model[[1]], per_model = TRUE, 
                        per_selector = FALSE, sub_selectors = NULL, 
                        details = TRUE)
  
  tstat_lm_smpl_r2 <- lapply(unique(tstat$sample), function(s){
    dt = tstat[tstat$sample == s, ]
    tstat_lm_smpl = lm(testing_predicted ~ testing_response, data = dt)
    data.frame(response = dt$model_response[1],
               tstat_lm_smpl_r2 = summary(tstat_lm_smpl)$r.squared,
               smpl = dt$sample[1])
  })
  tstat_lm_smpl_r2 = do.call("rbind", tstat_lm_smpl_r2)
  tstat_errors = melt(tstat_lm_smpl_r2,  id.vars = c("response", "smpl"))
  tstat_errors$variable = "r_squared"
  
  tstat_lm_all = lm(testing_predicted ~ testing_response, data = tstat)
  tstat_lm_all_r2 = data.frame(response = tstat$model_response[1],
                               smpl = "all",
                               variable = "r_squared",
                               value = summary(tstat_lm_all)$r.squared)
  tstat_errors = rbind(tstat_errors, tstat_lm_all_r2)
  
  tstat_lm_set_r2 = data.frame(response = tstat$model_response[1],
                               smpl = "set", 
                               variable = "r_squared",
                               value = mean(tstat$r_squared))
  tstat_errors = rbind(tstat_errors, tstat_lm_set_r2)
  
  tstat_smpl_rmse <- lapply(unique(tstat$sample), function(s){
    dt = tstat[tstat$sample == s, ]
    data.frame(response = tstat$model_response[1],
               tstat_smpl_rmse = sqrt(mean((dt$testing_predicted - dt$testing_response)**2)),
               smpl = dt$sample[1])
  })
  tstat_smpl_rmse = do.call("rbind", tstat_smpl_rmse)
  tstat_smpl_rmse = melt(tstat_smpl_rmse,  id.vars = c("response", "smpl"))
  tstat_smpl_rmse$variable = "rmse"
  tstat_errors = rbind(tstat_errors, tstat_smpl_rmse)
  
  tstat_rmse_all = data.frame(response = tstat$model_response[1],
                              smpl = "all",
                              variable = "rmse",
                              value = sqrt(mean((tstat$testing_predicted - tstat$testing_response)**2)))
  tstat_errors = rbind(tstat_errors, tstat_rmse_all)
  tstat_errors$stat = "test"
  tstat_errors$be = substr(tstat$model_selector, 1, 3)[1]
  
  
  mstat = lapply(test_model[[be]]@model$pls_ffs[[1]], function(s){
    data.frame(be = substr(s$training$SELECTOR[1], 1, 3),
               response = s$response,
               r_squared = max(s$model$results$Rsquared),
               r_squared_sd = max(s$model$results$RsquaredSD),
               rmse = max(s$model$results$RMSE),
               rmse_sd = max(s$model$results$RMSESD))
  })
  mstat = do.call("rbind", mstat)
  mstat$smpl = seq(10)
  mstat = melt(mstat, id.var = c("response", "be", "smpl"))
  mstat$stat = "train"
  
  
  errors = rbind(tstat_errors, mstat)
  unique(errors$variable)
  
  plot_rmse = ggplot(data = errors[errors$smpl %in% seq(10) & errors$variable == "rmse",], 
                     aes(x = response, y = value, color = stat)) + 
    geom_boxplot() + 
    geom_point(data = errors[errors$smpl == "all" & errors$variable == "rmse",],
               aes(x = response, y = value, color="RMSE all")) + 
    theme_bw()
  
  
  plot_r2 = ggplot(data = errors[errors$smpl %in% seq(10) & errors$variable == "r_squared",], 
                   aes(x = response, y = value, color = stat)) + 
    geom_boxplot() + 
    geom_point(data = errors[errors$smpl == "set" & errors$variable == "r_squared",],
               aes(x = response, y = value, color = "Mean R2 from sets")) + 
    geom_point(data = errors[errors$smpl == "all" & errors$variable == "r_squared",],
               aes(x = response, y = value, color = "R2 over all")) + 
    theme_bw()
  return(list(plot_rmse, plot_r2))
})
names(eval) = belc
eval[["AEG"]][[1]]
eval[["AEG"]][[2]]
eval[["HEG"]][[1]]
eval[["HEG"]][[2]]
eval[["SEG"]][[1]]
eval[["SEG"]][[2]]
