# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  source("D:/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
} else {
  source("/media/permanent/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
}

compute = TRUE

# Compile grassland diversity and remote sensing -------------------------------
if(compute){
  sdiv <- readRDS(file = paste0(path_rdata, "sdiv.rds"))
  nas <- colSums(is.na(sdiv))
  sdiv <- sdiv[, which(nas < 1)]

  meta <- readRDS(paste0(path_rdata, "meta.rds"))
  
  re <- readRDS(paste0(path_rdata, "re_predictors.rds"))
  re_df <- lapply(re, function(e){
    as.data.frame(e)
  })
  re_df <- do.call("rbind", re_df)
  re_df$Year <- 2015
  nas <- colSums(is.na(re_df))
  re_df <- re_df[, which(nas < 1)]
  
  veg_re_f <- merge(sdiv, re_df, by.x = "EPID", by.y = "EP")
  max(colSums(is.na(veg_re_f)))
  
  veg_re_f <- merge(veg_re_f, meta, by.x = c("EPID", "Year"),
                    by.y = c("plotID", "datetime"))
  
  saveRDS(veg_re_f, paste0(path_rdata, "veg_re_f.rds"))
} else {
  veg_re_f <- readRDS(paste0(path_rdata, "veg_re_f.rds"))
}


# Prepare gpm data set used for remote sensing prediction study ----------------

belc <- c("AEW", "HEW", "SEW")
veg_re_f_gpm_indv <- lapply(belc, function(b){
  act_veg_re_f <- veg_re_f[veg_re_f$g_belc == b, ]
  
  col_selector <- which(names(act_veg_re_f) == "EPID")

  col_diversity <- seq(grep("SMId", names(act_veg_re_f)), 
                       grep("dc_sp_2D", names(act_veg_re_f)))
  
  col_precitors <- seq(grep("GLI_mean", names(act_veg_re_f)), 
                       grep("pca_Long_Run_Low_Grey.Level_Emphasis.b1r50o1_var", names(act_veg_re_f)))
  
  col_precitors <- col_precitors[-grep("ep|id|type", tolower(names(act_veg_re_f[, col_precitors])))]
  
  col_meta <- seq(length(act_veg_re_f))[-c(col_selector, col_diversity, col_precitors)]
  
  meta <- createGPMMeta(act_veg_re_f, type = "input",
                        selector = col_selector,
                        response = col_diversity,
                        predictor = col_precitors,
                        meta = col_meta)
  veg_re_f_gpm <- gpm(act_veg_re_f, meta, scale = FALSE)
  
  # Clean predictor variables 
  veg_re_f_gpm <- cleanPredictors(x = veg_re_f_gpm, nzv = TRUE,
                                  highcor = TRUE, cutoff = 0.90)
  
  # Compute resamples following a leave location out approach
  veg_re_f_gpm <- splitMultRespLSO(x = veg_re_f_gpm, nbr = 1)
})
names(veg_re_f_gpm_indv) <- belc

aew <- veg_re_f_gpm_indv[[1]]
aew <- aew@data$input[, c("EPID", aew@meta$input$PREDICTOR)]
max(colSums(is.na(aew)))

hew <- veg_re_f_gpm_indv[[2]]
hew <- hew@data$input[, c("EPID", hew@meta$input$PREDICTOR)]
max(colSums(is.na(hew)))

sew <- veg_re_f_gpm_indv[[3]]
sew <- sew@data$input[, c("EPID", sew@meta$input$PREDICTOR)]
max(colSums(is.na(sew)))

bew <- rbind(aew, hew)
bew <- rbind(bew, sew)
max(colSums(is.na(bew)))

meta <- createGPMMeta(bew, type = "input",
                      selector = 1,
                      response = 1,
                      predictor = c(2:986),
                      meta = 1)
bew_gpm <- gpm(bew, meta, scale = TRUE)
bew_gpm <- cleanPredictors(x = bew_gpm, nzv = TRUE,
                           highcor = TRUE, cutoff = 0.90)
bew_gpm@meta$input$PREDICTOR_FINAL
bew_final <- bew_gpm@data$input[, c("EPID", bew_gpm@meta$input$PREDICTOR_FINAL)]
max(colSums(is.na(bew_final)))
nrow(bew_final)
saveRDS(bew_final, file = paste0(path_rdata, "bew_final_syn17.rds"))

bew_final$EXPL <- substr(bew_final$EPID, 1, 3)
ggplot(data = bew_final, aes(x = TGI_sd, y = NDVI_median, color = EXPL)) + 
  geom_point()


