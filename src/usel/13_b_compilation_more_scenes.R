
# Set path ---------------------------------------------------------------------
source("D:/UNI/Master/MA/exploratorien/scripts/project_biodiv_rs/src/usel/00_a_set_environment_until_gpm_compile.R")

# Compile grassland diversity and remote sensing -------------------------------
  vegrel15 <- readRDS(file = paste0(path_rdata, "preprocessing/vegrel15.rds"))
  
  meta <- readRDS(paste0(path_rdata, "preprocessing/meta.rds"))
  
  re_df<- readRDS(paste0(path_rdata, "preprocessing/re03_predictors.rds"))
  
  veg_re_g <- merge(vegrel15, re_df, by.x = c("EPID", "Year"),
                    by.y = c("re03_EP", "Year"))
  max(colSums(is.na(veg_re_g)))
  
  veg_re_g <- merge(veg_re_g, meta, by.x = c("EPID", "Year"),
                    by.y = c("plotID", "datetime"))
  
  #saveRDS(veg_re_g, paste0(path_rdata_pre, "03veg_re_g.rds"))

# Prepare gpm data set used for remote sensing prediction study ----------------
  #veg_re_g <- readRDS(paste0(path_rdata, "preprocessing/veg_re_g.rds"))
  
belc <- c("AEG", "HEG", "SEG")
veg_re_g_gpm_indv <- lapply(belc, function(b){
  act_veg_re_g <- veg_re_g[veg_re_g$g_belc == b, ]
  
  col_selector <- which(names(act_veg_re_g) == "EPID")
  
  col_diversity <- c(grep("specrich|shannon|eveness", 
                          tolower(names(act_veg_re_g))),
                     seq(grep("vegetation_height_mean_cm",
                              names(act_veg_re_g)),
                         grep("biomass_g",
                              names(act_veg_re_g))),
                     seq(grep("G_std_glb",
                              names(act_veg_re_g)),
                         grep("LUI_reg",
                              names(act_veg_re_g))))
  
  col_precitors <- seq(grep("GLI_mean", names(act_veg_re_g)), 
                       grep("Ts_50", names(act_veg_re_g))) #letzter predictor mit meta (ohne: pca_Long_Run_Low_Grey.Level_Emphasis.b1r50o1_var)
  
  col_precitors <- col_precitors[-grep("ep|id|type", tolower(names(act_veg_re_g[, col_precitors])))]
  
  col_meta <- seq(length(act_veg_re_g))[-c(col_selector, col_diversity, col_precitors)]
  
  meta <- createGPMMeta(act_veg_re_g, type = "input",
                        selector = col_selector,
                        response = col_diversity,
                        predictor = col_precitors,
                        meta = col_meta)
  veg_re_g_gpm <- gpm(act_veg_re_g, meta, scale = FALSE)
  
  # Clean predictor variables 
  veg_re_g_gpm <- cleanPredictors(x = veg_re_g_gpm, nzv = TRUE,
                                  highcor = TRUE, cutoff = 0.80)
  
  # Compute resamples following a leave location out approach, LOOC5 für 9fache CV
  veg_re_g_gpm <- splitMultRespLSO(x = veg_re_g_gpm, nbr = 5)
})
names(veg_re_g_gpm_indv) <- belc

#only use predictors appearing in all 3 exploratories (leaves us eventually with 24 predictors)
predictors_common <- veg_re_g_gpm_indv[[1]]@meta$input$PREDICTOR_FINAL[veg_re_g_gpm_indv[[1]]@meta$input$PREDICTOR_FINAL %in% veg_re_g_gpm_indv[[2]]@meta$input$PREDICTOR_FINAL]
predictors_common <- predictors_common[predictors_common %in% veg_re_g_gpm_indv[[3]]@meta$input$PREDICTOR_FINAL]
# adding some more predictors to get a more summed up/complete list of preditcors
  # for re03
      predictors_common <- c(predictors_common, "re03_GLI_sd","re03_TGI_var", 
                            "re03_PC1_sd" ,"re03_PC4_sd","re03_PC5_var" ,"re03_mahal_var","re03_mahal_sd",
                            "Ta_10","Ts_5", "Ts_10", "Ts_20", "Ts_50","rH_200_max","rH_200_min")
  # for re04
      predictors_common <- c(predictors_common, "re04_NRGDI_sd", "re04_PC1_sd" , paste0("re04_PC", c(1,3:5), "_mean")) #brauchen meta hier nicht nochmal zu adden da es immer gleich ist

predictors_common <- predictors_common[order(predictors_common)]
#save the common predictors in the final variable
veg_re_g_gpm_indv[[1]]@meta$input$PREDICTOR_FINAL <- predictors_common
veg_re_g_gpm_indv[[2]]@meta$input$PREDICTOR_FINAL <- predictors_common
veg_re_g_gpm_indv[[3]]@meta$input$PREDICTOR_FINAL <- predictors_common

# now we check the freuquency of predictors to save computation time (use skript 08a)
  # we only look for species richness
gpm03<-veg_re_g_gpm_indv
pred<-gpm03[[1]]@meta$input$PREDICTOR_FINAL #schriebe vektor und extrahiere die n?tigen Prediktoren

# do the different responses
# check the entire list and get te digits from predictor positions
gpm03[[1]]@meta$input$PREDICTOR_FINAL

  #for 04
specpred<-pred[-c(1,2,5,8,11,14,17,18:22,30:32)]
  # for 03
specpred<-pred[-c(2:6,8,10,12,14,16,19,22,23,32)]

for(be in names(gpm03)){
  gpm03[[be]]@meta$input$PREDICTOR_FINAL<-specpred
}
saveRDS(gpm03,paste0(path_rdata,"03SPECmodel.rds"))

# now lets bring all predictors in the gpm object
gpm04<-readRDS(paste0(path_rdata,"04SPECmodel.rds"))
gpm03<-readRDS(paste0(path_rdata,"03SPECmodel.rds"))
gpmini<-readRDS(paste0(path_data,"results/9CV_5models_13pred.rds"))

# make a vector for the final predictors in both 04 and 03
specpred04<-gpm04[[1]]@meta$input$PREDICTOR_FINAL
# for 03
specpred03<-gpm03[[1]]@meta$input$PREDICTOR_FINAL
# for the original modeling
specpred<-gpmini[[1]]@meta$input$PREDICTOR_FINAL

predictors_common <- c(specpred03,specpred04, specpred)

# we just overwrite gpm03- here we write the data predictors (here are the values)
for (be in names(gpm03)){
  gpm03[[be]]@data$input<-cbind(gpm04[[be]]@data$input,gpm03[[be]]@data$input, gpmini[[be]]@data$input)
}

for (be in names(gpm03)){
  gpm03[[be]]@meta$input$PREDICTOR_FINAL<-predictors_common
}

saveRDS(gpm03, paste0(path_rdata, "specmodel__clima_RE_3timesteps.rds"))

