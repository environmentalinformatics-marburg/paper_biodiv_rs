# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  source("D:/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
} else {
  source("/media/permanent/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
}

compute = TRUE

# Compile analysis dataset -----------------------------------------------------
if(compute){
  vegrel0815_div <- readRDS(file = paste0(path_rdata, "vegrel0815_div.rds"))
  vegrel14 <- readRDS(file = paste0(path_rdata, "vegrel14.rds"))
  vegrel15 <- readRDS(file = paste0(path_rdata, "vegrel15.rds"))
  
  meta <- readRDS(paste0(path_rdata, "meta.rds"))
  
  ref <- readRDS(paste0(path_rdata, "re_predictors.rds"))
  ref_df <- lapply(ref, function(e){
    as.data.frame(e)
  })
  ref_df <- do.call("rbind", ref_df)
  ref_df$Year <- 2015
  
  adf_clim <- merge(vegrel0815_div, meta, by.x = c("EPID", "Year"), by.y = c("plotID", "g_a"))
  adf_rs <- merge(adf_clim, ref_df, by.x = c("EPID", "Year"), by.y = c("EP", "Year"))
  
  adf_clim <- adf_clim[!is.na(adf_clim$SHANNON),]
  
  nas <- unlist(lapply(seq(ncol(adf_rs)), function(i){sum(is.na(adf_rs[, i]))}))
  adf_rs <- adf_rs[, which(nas == 0 )]
  
  saveRDS(adf_clim, paste0(path_rdata, "adf_clim.rds"))
  saveRDS(adf_rs, paste0(path_rdata, "adf_rs.rds"))
} else {
  adf_clim <- readRDS(paste0(path_rdata, "adf_clim.rds"))
  adf_rs <- readRDS(paste0(path_rdata, "adf_rs.rds"))
}

# Prepare gpm data set used for remote sensing prediction study ----------------
if(compute){
  col_selector <- which(names(adf_rs) == "EPID")
  
  col_meta <- c(grep("id|year|ep|type|g_belc|g_pa", tolower(names(adf_rs))))
  col_meta <- col_meta[-col_selector]

  col_diversity <- c(grep("specrich|shannon|eveness", tolower(names(adf_rs))))
  
  col_precitors <- seq(length(adf_rs))[-c(col_selector, col_meta, col_diversity)]
  
  meta <- createGPMMeta(adf_rs, type = "input",
                        selector = col_selector, 
                        response = col_diversity, 
                        predictor = col_precitors, 
                        meta = col_meta)
  adf_rs_gpm <- gpm(adf_rs, meta, scale = TRUE)
  saveRDS(adf_rs_gpm, paste0(path_rdata, "adf_rs_gpm.rds"))
} else {
  adf_rs_gpm <- readRDS(paste0(path_rdata, "adf_rs_gpm.rds"))
}


# Clean predictor variables ----------------------------------------------------
if(compute){
  adf_rs_gpm <- cleanPredictors(x = adf_rs_gpm, nzv = TRUE, 
                              highcor = TRUE, cutoff = 0.90)
  saveRDS(adf_rs_gpm, file = paste0(path_rdata, "gls_adf_rs_gpm_cleanPredictors.rds"))
} else {
  adf_rs_gpm <- readRDS(file = paste0(path_rdata, "gls_adf_rs_gpm_cleanPredictors.rds"))
}


# Compile model training and evaluation dataset --------------------------------
if(compute){
  # Compute resamples
  adf_rs_gpm <- resamplingsByVariable(x = adf_rs_gpm,
                                      use_selector = TRUE,
                                      grabs = 1,
                                      resample = 10)
  
  # Split resamples into training and testing samples
  adf_rs_gpm <- splitMultResp(x = adf_rs_gpm, 
                              p = 0.80, 
                              use_selector = FALSE)
  
  saveRDS(adf_rs_gpm, file = paste0(path_rdata, "gls_adf_rs_gpm_traintest.rds"))
} else {
  adf_rs_gpm <- readRDS(file = paste0(path_rdata, "gls_adf_rs_gpm_traintest.rds"))
}