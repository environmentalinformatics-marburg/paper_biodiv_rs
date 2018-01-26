source("D:/UNI/Master/MA/exploratorien/scripts/project_biodiv_rs/src/usel/00_a_set_environment_until_gpm_compile.R")

#read RE Scenes from late april and the polygons
re<-readRDS(paste0(path_rdata_pre,"RE_24_24_20.rds"))
poly <- readRDS(file = paste0(path_rdata_pre, "poly.rds"))

# make a rasterclip
re_snip <- lapply(seq(length(re)), function(i){
  re_snip <- snipRaster(raster=re[[i]], 
                        spatial=poly[[i]], selector="EP", #changed EP into general-differnet datasource...?!
                        buffer=500, byid = TRUE)
})
saveRDS(re_snip, file = paste0(path_rdata_pre, "re_end04_plots.rds"))

#calculate indices
re_idx <- lapply(re_snip, function(e){
  lapply(e, function(s){
    re_rgb <- rgbIndices(stack(s[[3]], s[[2]], s[[1]]), 
                         rgbi = c("GLI", "NGRDI", "TGI", "VVI"))
    re_ndvi <- (s[[5]]-s[[3]])/(s[[5]]+s[[3]]) #red and NIR band, is it?
    names(re_ndvi) <- "NDVI"
    re_rendvi <- (s[[4]]-s[[3]])/(s[[4]]+s[[3]]) 
    names(re_rendvi) <- "RE_NDVI"
    re_idx <- stack(re_rgb, re_ndvi, re_rendvi)
  })
})
saveRDS(re_idx, file = paste0(path_rdata_pre, "re_end04_idx.rds"))

#make some space in environment
rm(poly,re_snip,re)

#calculate Haralcik für NDVI
windows <- 5
for(i in seq(length(re_idx))){
  print(paste("Listennummer: ",i))
  minv <- min(unlist(lapply(re_idx[[i]], function(s){minValue(s$NDVI)})))
  maxv <- max(unlist(lapply(re_idx[[i]], function(s){maxValue(s$NDVI)})))
  re_pca_ht <- lapply(re_idx[[i]], function(s){
    print(paste("Punktnummer: ", i))
    lapply(windows, function(w){
      oth <- otbTexturesHaralick(x=s$NDVI, path_output = path_temp, 
                                 return_raster = TRUE, 
                                 parameters.xyrad=list(c(w,w)),
                                 parameters.xyoff=list(c(1,1)),
                                 parameters.minmax=c(minv, maxv),
                                 parameters.nbbin = 8,
                                 texture="all",
                                 channel = 1)
      names(oth) <- paste0("NDVI_", names(oth))
      return(oth)
    })
  })
  outfile <- paste0("re_NDVI_h_", substr(names(re_idx[[i]][1]), 1, 3), ".rds")
  }
saveRDS(re_pca_ht, file = paste0(path_rdata_pre, outfile))
}
  
  
# Extract raster data based on polygon -----------------------------------------
    poly<-readRDS(file = paste0(path_rdata_pre, "poly.rds"))
    re_snip <- readRDS(file = paste0(path_rdata_pre, "re_end04_plots.rds"))
    re_snip_extr <- lapply(seq(3), function(i){
      extractFromRasterSnips(raster = re_snip[[i]], spatial = poly[[i]], buffer = 0, 
                             mahal = TRUE)
    })
    
    # For some reason, some medians are characters
    for(i in grep("median", colnames(re_snip_extr[[2]]@data))){
      re_snip_extr[[2]]@data[, i] <- as.numeric(re_snip_extr[[2]]@data[, i])
    }
    saveRDS(re_snip_extr, paste0(path_rdata, "re_snip_extr.rds"))
    
#extraction for indices  
    re_idx_extr <- lapply(seq(3), function(i){
      re_idx <- readRDS(file = paste0(path_rdata, "re_idx.rds"))
      extractFromRasterSnips(raster = re_idx[[i]], spatial = poly[[i]], buffer = 0, 
                             mahal = FALSE)
    })
    saveRDS(re_idx_extr, paste0(path_rdata, "re_idx_extr.rds"))
    
    