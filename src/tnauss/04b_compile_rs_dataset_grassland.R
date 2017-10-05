# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  source("C:/Users/tnauss/permanent/plygrnd/exploratorien/project_biodiv_rs/src/00_set_environment.R")
} else {
  source("/media/permanent/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
}

compute = TRUE

# Compile grassland diversity and remote sensing -------------------------------
if(compute){
  vegrel14 <- readRDS(file = paste0(path_rdata, "vegrel14.rds"))
  
  vegrel15 <- readRDS(file = paste0(path_rdata, "vegrel15.rds"))
  
  meta <- readRDS(paste0(path_rdata, "meta.rds"))
  
  re <- readRDS(paste0(path_rdata, "re_predictors.rds"))
  re_df <- lapply(re, function(e){
    as.data.frame(e)
  })
  re_df <- do.call("rbind", re_df)
  re_df$Year <- 2015
  nas <- colSums(is.na(re_df))
  re_df <- re_df[, which(nas < 1)]
  
  veg_re_g <- merge(vegrel15, re_df, by.x = c("EPID", "Year"),
                    by.y = c("EP", "Year"))
  max(colSums(is.na(veg_re_g)))
  
  veg_re_g <- merge(veg_re_g, meta, by.x = c("EPID", "Year"),
                    by.y = c("plotID", "datetime"))
  
  
  saveRDS(veg_re_g, paste0(path_rdata, "veg_re_g.rds"))
} else {
  veg_re_g <- readRDS(paste0(path_rdata, "veg_re_g.rds"))
}


# Group plots into areas -------------------------------------------------------
poly <- readRDS(file = paste0(path_rdata, "poly.rds"))

for(i in seq(length(poly))){
  grld <- coordinates(poly[[i]][poly[[i]]@data$type=="Grld",])
  grld_clust <- hclust(dist(grld))
  grld_cuts <- cutree(grld_clust, 4)
  poly[[i]]@data$SpatialCluster <- NA
  poly[[i]]@data$SpatialCluster[poly[[i]]@data$type=="Grld"] <- grld_cuts
}
table(poly[[i]]@data$SpatialCluster)

grld <- poly[[i]][poly[[i]]@data$type=="Grld",]
plot(grld)

plot(spsample(grld, type = "naligned", n = 10))
plot(spsample(extent(grld), 10, type = "regular"))


grd <- raster(grld, nrows = 3, ncols = 3)
grd_poly <- rasterToPolygons(grd)
plot(grd_poly)
mapview(grd_poly) + grld



# Prepare gpm data set used for remote sensing prediction study ----------------
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
                       grep("pca_Long_Run_Low_Grey.Level_Emphasis.b1r50o1_var", names(act_veg_re_g)))
  
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
  
  # Compute resamples following a leave location out approach
  veg_re_g_gpm <- splitMultRespLSO(x = veg_re_g_gpm, nbr = 1)
})
names(veg_re_g_gpm_indv) <- belc
saveRDS(veg_re_g_gpm_indv, file = paste0(path_rdata, "veg_re_g_gpm_indv.rds"))
