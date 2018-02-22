library(corrplot)
library(gpm)
library(reshape2)
library(parallel)
library(doParallel)

#### Take Ulis' data and merge it with Peter's data
m = readRDS("C:/Users/tnauss/permanent/plygrnd/exploratorien/project_biodiv_rs/files_presentation/NDVI_pls_ffs_9CV.rds")

traits = read.table("C:/Users/tnauss/permanent/plygrnd/exploratorien/data/manning/cwm_exploratories_1.csv",
                    header = TRUE, sep = ",", dec = ".")
traits = traits[traits$Year == 2015, -c(1, 3)]
traits$EP_PlotID = as.character(traits$EP_PlotID)
traits$EP_PlotID[nchar(traits$EP_PlotID) == 4] = paste0(substr(traits$EP_PlotID[nchar(traits$EP_PlotID) == 4], 1, 3), "0", substr(traits$EP_PlotID[nchar(traits$EP_PlotID) == 4], 4, 4))

m_df = list()
m_df[[1]] = m[[1]]@data$input
m_df[[2]] = m[[2]]@data$input
m_df[[3]] = m[[3]]@data$input

m_df[[1]] = merge(m_df[[1]], traits, by.x = "EPID", by.y = "EP_PlotID")
m_df[[2]] = merge(m_df[[2]], traits, by.x = "EPID", by.y = "EP_PlotID")
m_df[[3]] = merge(m_df[[3]], traits, by.x = "EPID", by.y = "EP_PlotID")
belc = c("AEG", "HEG", "SEG")
names(m_df) = belc



mgpm = lapply(seq(length(belc)), function(i){
  act_m_df = m_df[[i]]
  act_m_df = act_m_df[-grep("\\.1", colnames(act_m_df))]
  act_m_df = act_m_df[-grep("\\.2", colnames(act_m_df))]
  act_m_df = act_m_df[-grep("\\.3", colnames(act_m_df))]
  
  col_selector = which(names(act_m_df) == "EPID")
  
  col_diversity = c(grep("specrich|shannon|eveness",
                         tolower(names(act_m_df))),
                    seq(grep("vegetation_height_mean_cm",
                             names(act_m_df)),
                        grep("biomass_g",
                             names(act_m_df))),
                    seq(grep("speciesrichness",
                             names(act_m_df)),
                        grep("SSD",
                             names(act_m_df))))
  
  col_precitors = seq(grep("RE201504_1_mean", names(act_m_df)),
                      grep("NDVI_Long_Run_High_Grey.Level_Emphasis.b1r5o1_sd", names(act_m_df)))
  
  # col_precitors = col_precitors[-grep("ep|id|type", tolower(names(act_m_df[, col_precitors])))]
  
  col_meta = seq(ncol(act_m_df))[-c(col_selector, col_diversity, col_precitors)]
  
  meta = createGPMMeta(act_m_df, type = "input",
                       selector = col_selector,
                       response = col_diversity,
                       predictor = col_precitors,
                       meta = col_meta)
  mgpm = gpm(act_m_df, meta, scale = TRUE)
  
  # Clean predictor variables
  mgpm = cleanPredictors(x = mgpm, nzv = TRUE,
                         highcor = TRUE, cutoff = 0.80)
  
  # Compute resamples following a leave location out approach
  mgpm = splitMultRespLSO(x = mgpm, nbr = 5)
})
names(mgpm) = belc
pred_fin = unique(c(mgpm[[1]]@meta$input$PREDICTOR_FINAL, mgpm[[2]]@meta$input$PREDICTOR_FINAL,
                    mgpm[[3]]@meta$input$PREDICTOR_FINAL))
mgpm[[1]]@meta$input$PREDICTOR_FINAL = pred_fin
mgpm[[2]]@meta$input$PREDICTOR_FINAL = pred_fin
mgpm[[3]]@meta$input$PREDICTOR_FINAL = pred_fin

saveRDS(mgpm, "C:/Users/tnauss/permanent/plygrnd/exploratorien/data/manning/cwm_mgpm.rds")




#### Compile model
div_fin = c("speciesrichness", "SLA", "leaf_P", "leaf_N", "leaf_drymass", "height", "leaf_area", "seedmass", "SSD")

for(be in names(mgpm)){
  cl = makeCluster(detectCores())
  registerDoParallel(cl)
  
  mgpm[[be]] = trainModel(x = mgpm[[be]],
                          n_var = c(2:20),
                          mthd = "pls",
                          mode = "rfe",
                          seed_nbr = 11,
                          cv_nbr = 5,
                          var_selection = "indv",
                          response_nbr = which(mgpm[[1]]@meta$input$RESPONSE_FINAL %in% div_fin),
                          filepath_tmp = NULL)
  saveRDS(mgpm[[be]], file = paste0("C:/Users/tnauss/permanent/plygrnd/exploratorien/data/manning/", be, ".rds"))
  stopCluster(cl)
}
# saveRDS(mgpm, file = paste0("C:/Users/tnauss/permanent/plygrnd/exploratorien/data/manning/cmw_mgpm_model.rds"))
mgpm = readRDS(file = paste0("C:/Users/tnauss/permanent/plygrnd/exploratorien/data/manning/cmw_mgpm_model.rds"))




#### Some stats
# Model CV
mgpm_model_stats = lapply(names(mgpm), function(be){
  rspns = lapply(seq(length(mgpm[[be]]@model$pls_rfe)), function(r){
    smpls = lapply(seq(length(r)), function(s){
      data.frame(belc = be, 
                 model_response = mgpm[[be]]@model$pls_rfe[[r]][[s]]$response,
                 mgpm[[be]]@model$pls_rfe[[r]][[s]]$model$fit$results[mgpm[[be]]@model$pls_rfe[[r]][[s]]$model$fit$bestTune[,1],],
                 model_rmse_norm = mgpm[[be]]@model$pls_rfe[[r]][[s]]$model$fit$results$RMSE[mgpm[[be]]@model$pls_rfe[[r]][[s]]$model$fit$bestTune[,1]] /
                   mean(mgpm[[be]]@model$pls_rfe[[r]][[s]]$training$RESPONSE))
      
    })
    do.call("rbind", smpls)
  })
  do.call("rbind", rspns)
})
mgpm_model_stats = do.call("rbind", mgpm_model_stats)
names(mgpm_model_stats)[4:7] = paste0("model_", names(mgpm_model_stats)[4:7])

# Independent prediction
mgpm_stats = lapply(names(mgpm), function(be){
  var_imp = compVarImp(mgpm[[be]]@model$pls_rfe, scale = FALSE)
  var_imp_scale = compVarImp(mgpm[[be]]@model$pls_rfe, scale = TRUE)
  var_imp_plot = plotVarImp(var_imp)
  var_imp_heat = plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")
  tstat = compRegrTests(mgpm[[be]]@model$pls_rfe, details = TRUE)
  tstat$belc = substr(tstat$model_selector, 1, 3)
  lmod = lm(testing_predicted ~ testing_response, data = tstat)
  rsqrd = summary(lmod)$r.squared
  tstat_cmpl = list(var_imp = var_imp, var_imp_scale = var_imp_scale, 
                    var_imp_plot = var_imp_plot, var_imp_heat = var_imp_heat,
                    tstat = tstat, lmod = lmod, rsqrd = rsqrd)
  return(tstat_cmpl)
})
names(mgpm_stats) = names(mgpm)

mgpm_stats_tstat = do.call("rbind", lapply(seq(3), function(i){mgpm_stats[[i]]$tstat}))
mgpm_stats_tstat = merge(mgpm_stats_tstat, mgpm_model_stats, by = c("belc", "model_response"))

mgpm_stats_tstat_rmse = data.frame(belc = mgpm_stats_tstat$belc,
                                   model_response = mgpm_stats_tstat$model_response,
                                   # indp_rmse = mgpm_stats_tstat$rmse, 
                                   # model_rmse = mgpm_stats_tstat$model_RMSE,
                                   indp_rmse_norm = mgpm_stats_tstat$rmse_norm,
                                   model_rmse_norm = mgpm_stats_tstat$model_rmse_norm)
mgpm_stats_tstat_rmse = melt(mgpm_stats_tstat_rmse, idvar = c("belc", "model_response"))





#### Some plots
mresps = unique(mgpm_stats_tstat$model_response)
q75 = lapply(mresps, function(r){
  data.frame(response = r,
             quant_075 = quantile(mgpm_stats_tstat$rmse_norm[mgpm_stats_tstat$model_response == r])[4])
})
q75 = do.call("rbind", q75)

ok = q75$response[q75$quant_075 <= 0.5]

# Model vs. independent rmse_norm (ok)
png(filename = paste0("C:/Users/tnauss/permanent/plygrnd/exploratorien/data/manning/training_vs_test_error.png"),
    width = 1064, height = 693)
ggplot(data = mgpm_stats_tstat_rmse[mgpm_stats_tstat_rmse$model_response %in% ok,], aes(x = model_response, y = value, fill = variable)) + 
  geom_boxplot() + 
  theme_bw() +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=20))
dev.off()

# Model vs. independent rmse_norm (not ok)
png(filename = paste0("C:/Users/tnauss/permanent/plygrnd/exploratorien/data/manning/training_vs_test_error_not_ok.png"),
    width = 1064, height = 693)
ok_wla = c(as.character(ok), "leaf_area")
ggplot(data = mgpm_stats_tstat_rmse[!mgpm_stats_tstat_rmse$model_response %in% ok_wla,], aes(x = model_response, y = value, fill = variable)) + 
  geom_boxplot() + 
  theme_bw() +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=20))
dev.off()

png(filename = paste0("C:/Users/tnauss/permanent/plygrnd/exploratorien/data/manning/training_vs_test_error_completely_not_ok.png"),
    width = 1064, height = 693)
ggplot(data = mgpm_stats_tstat_rmse[mgpm_stats_tstat_rmse$model_response %in% "leaf_area",], aes(x = model_response, y = value, fill = variable)) + 
  geom_boxplot() + 
  theme_bw() +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=20))
dev.off()


# Independent rmse_norm for each exploratory
png(filename = paste0("C:/Users/tnauss/permanent/plygrnd/exploratorien/data/manning/test_error_belc.png"),
    width = 1064, height = 693)
ggplot(data = mgpm_stats_tstat[mgpm_stats_tstat$model_response %in% ok,], aes(x = model_response, y = rmse_norm, fill = belc)) + 
  geom_boxplot() + 
  theme_bw() +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=20))
dev.off()

scatterplots = lapply(mresps, function(r){
  ggplot(data = mgpm_stats_tstat[mgpm_stats_tstat$model_response == r,], 
         aes(x = testing_response, y = testing_predicted, color = belc)) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    theme_bw() +
    scale_color_manual(values =  c("#1f78b4", "#33a02c", "#e31a1c")) +
    labs(title = r) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=20))
})
names(scatterplots) = mresps

png(filename = paste0("C:/Users/tnauss/permanent/plygrnd/exploratorien/data/manning/train_test_scatter_height.png"),
    width = 1064, height = 693)
scatterplots$height
dev.off()

png(filename = paste0("C:/Users/tnauss/permanent/plygrnd/exploratorien/data/manning/train_test_scatter_seedmass.png"),
    width = 1064, height = 693)
scatterplots$seedmass
dev.off()

png(filename = paste0("C:/Users/tnauss/permanent/plygrnd/exploratorien/data/manning/train_test_scatter_speciesrichness.png"),
    width = 1064, height = 693)
scatterplots$speciesrichness
dev.off()

png(filename = paste0("C:/Users/tnauss/permanent/plygrnd/exploratorien/data/manning/train_test_scatter_SSD.png"),
    width = 1064, height = 693)
scatterplots$SSD
dev.off()


lmod_stats = lapply(ok, function(r){
  lmod_r = lapply(names(mgpm), function(b){
    summary(lm(testing_response ~ testing_predicted, 
               data = mgpm_stats_tstat[mgpm_stats_tstat$model_response == r & 
                                         mgpm_stats_tstat$belc == b,]))
  })
  names(lmod_r) = names(mgpm)
  lmod_r
})
names(lmod_stats) = ok
lmod_stats$height
lmod_stats$seedmass
lmod_stats$speciesrichness
lmod_stats$SSD


trait_correlation = cor(traits[, 3:11])

png(filename = paste0("C:/Users/tnauss/permanent/plygrnd/exploratorien/data/manning/trait_correlation.png"),
    width = 1064, height = 693)
corrplot(trait_correlation)
dev.off()
