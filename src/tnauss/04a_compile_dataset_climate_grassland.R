# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  source("D:/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
} else {
  source("/media/permanent/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
}

compute = TRUE

# Grassland diversity and climate --------------------------------------------
vegrel0815_div <- readRDS(file = paste0(path_rdata, "vegrel0815_div.rds"))
meta <- readRDS(paste0(path_rdata, "meta.rds"))


veg_meta_g <- merge(vegrel0815_div, meta, by.x = c("EPID", "Year"),
                    by.y = c("plotID", "g_a"))
veg_meta_g <- veg_meta_g[!is.na(veg_meta_g$LUI_reg), ]
veg_meta_g <- veg_meta_g[, -grep("EP|Ta_200_max|Ta_200_min|rH_200|datetime", 
                                 colnames(veg_meta_g))[-1]]
veg_meta_g <- veg_meta_g[complete.cases(veg_meta_g), ]
saveRDS(veg_meta_g, paste0(path_rdata, "veg_meta_g.rds"))


# Prepare gpm data set used for remote sensing prediction study ----------------
col_selector <- which(names(veg_meta_g) == "EPID")
col_meta <- c(grep("year|g_belc|g_pa", tolower(names(veg_meta_g))))
col_diversity <- c(grep("specrich|shannon|eveness", tolower(names(veg_meta_g))))
col_precitors <- seq(length(veg_meta_g))[-c(col_selector, col_meta, col_diversity)]

meta <- createGPMMeta(veg_meta_g, type = "input",
                      selector = col_selector,
                      response = col_diversity,
                      predictor = col_precitors,
                      meta = col_meta)
veg_meta_g_gpm <- gpm(veg_meta_g, meta, scale = FALSE)


# Clean predictor variables ----------------------------------------------------
veg_meta_g_gpm <- cleanPredictors(x = veg_meta_g_gpm, nzv = TRUE,
                                  highcor = TRUE, cutoff = 0.90)
  
# Compile model training and evaluation dataset --------------------------------
# Compute resamples following a leave location out approach
veg_meta_g_gpm <- splitMultRespLSO(x = veg_meta_g_gpm, nbr = 1)

saveRDS(veg_meta_g_gpm, file = paste0(path_rdata, "veg_meta_g_gpm.rds"))