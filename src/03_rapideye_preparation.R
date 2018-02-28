# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  source("D:/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
} else {
  source("/media/permanent/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
}

compute = TRUE

# Read RapidEye data -----------------------------------------------------------
if(compute){
  fns = list.files(path_re, pattern = glob2rx("*.tif"), full.names = TRUE)
  lns = paste0(substr(fns, nchar(fns)-6, nchar(fns)-4), "_", substr(basename(fns), 10, 17))
  re <- lapply(seq(length(fns)), function(i){
    print(i)
    re <- stack(fns[i])
    names(re) <- paste0(lns[i], "_bnd", seq(5))
    return(re)
  })
  names(re) <- lns
}
saveRDS(re, paste0(path_rdata, "/01_re.rds"))










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
#windows <- c(seq(1,5,4))
#.libPaths(c("F:/exploratorien/Documents/R", .libPaths()))
#cl <- makePSOCKcluster(length(re_idx))
#drs <- .libPaths(); clusterExport(cl, "drs"); jnk <- clusterEvalQ(cl, .libPaths(drs))
#clusterExport(cl, "re_idx"); jnk <- clusterEvalQ(cl, library(satelliteTools))

  for(i in seq(length(re_idx))){
  print(paste("Listennummer: ",i))
  minv <- min(unlist(lapply(re_idx[[i]], function(s){minValue(s$NDVI)})))
  maxv <- max(unlist(lapply(re_idx[[i]], function(s){maxValue(s$NDVI)})))
  
  re_pca_ht <-  lapply(re_idx[[i]], function(s){
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
    saveRDS(re_snip_extr, paste0(path_rdata, "re_end04_snip_extr.rds"))
    
#extraction for indices  
    re_idx_extr <- lapply(seq(3), function(i){
   #   re_idx <- readRDS(file = paste0(path_rdata, "re_idx.rds"))
      extractFromRasterSnips(raster = re_idx[[i]], spatial = poly[[i]], buffer = 0, 
                             mahal = FALSE)
    })
    saveRDS(re_idx_extr, paste0(path_rdata_pre, "re_end04_idx_extr.rds"))
    
#extraction for haralick textures  
    re_pca_ht_files <- list.files(path_rdata_pre, pattern = glob2rx("re_NDVI_h_*.rds"), 
                                  full.names = TRUE)
    for(e in seq(3)){
      re_pca_ht <- readRDS(re_pca_ht_files[e])
      re_pca_ht_ext <- extractFromRasterSnips(raster = re_pca_ht, 
                                              spatial = poly[[e]], buffer = 0,
                                              mahal = FALSE)
      rm(re_pca_ht)
      n <- names(re_pca_ht_ext)
      pos_m <- gregexpr('m\\.', names(re_pca_ht_ext))
      pos_us <- regexpr("\\_[^\\_]*$", names(re_pca_ht_ext))
      for(i in seq(length(pos_m))){
        if(pos_m[[i]][1] != -1){
          n[i] <- paste0(substr(n[i], 1, pos_m[[i]]-1), 
                         substr(n[i], pos_us[[i]], nchar(n[i])))
          #rm(pos_m,pos_us)
        }
      }
      names(re_pca_ht_ext) <- n
      
      nc <- nchar(re_pca_ht_files[e])
      outfile <- paste0("re_NDVI_ht_extr_", substr(re_pca_ht_files[e], nc-6, nc-4), ".rds")
      saveRDS(re_pca_ht_ext, paste0(path_rdata_pre, outfile))
    }
   
# ---------------------------------------------------------------------------------------
# wiederhole das für die Anfangsszene (nur Indices werden benötigt)

#read RE Scenes from late april and the polygons
      re<-readRDS(paste0(path_rdata_pre,"RE_09_09_10.rds"))
      poly <- readRDS(file = paste0(path_rdata_pre, "poly.rds"))
      
      # make a rasterclip
      re_snip <- lapply(seq(length(re)), function(i){
        re_snip <- snipRaster(raster=re[[i]], 
                              spatial=poly[[i]], selector="EP", #changed EP into general-differnet datasource...?!
                              buffer=500, byid = TRUE)
      })
      
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
     
 # Extract raster data based on polygon -----------------------------------------

      #extraction for indices  
      re_idx_extr <- lapply(seq(3), function(i){
        #   re_idx <- readRDS(file = paste0(path_rdata, "re_idx.rds"))
        extractFromRasterSnips(raster = re_idx[[i]], spatial = poly[[i]], buffer = 0, 
                               mahal = FALSE)
      })
      saveRDS(re_idx_extr, paste0(path_rdata_pre, "re_beg04_idx_extr.rds"))

# calculate difference between indices from beginning of april and end of april for SD,MED,MEAN
      #NDVI_mean_Ende - NDVI_mean_Anfang / 
      #Tagedifferenz (Tage Ende April- Tage Anfang April)
beg_idx<-readRDS(paste0(path_rdata_pre,"re_beg04_idx_extr.rds"))
end_idx<-readRDS(paste0(path_rdata_pre,"re_end04_idx_extr.rds"))
new<-beg_idx
# we make a loop for each explo (diff. between days are different)
 # AEG
new[[1]]@data[,-1:-3]<-( end_idx[[1]]@data[,-1:-3] - beg_idx[[1]]@data[,-1:-3])/ (24-9)

 # HEG
new[[2]]@data[,-1:-3]<-( end_idx[[2]]@data[,-1:-3] - beg_idx[[2]]@data[,-1:-3])/ (24-9)

 #SEG
new[[3]]@data[,-1:-3]<-( end_idx[[3]]@data[,-1:-3] - beg_idx[[3]]@data[,-1:-3])/ (20-10)
gel<-c("AEG","HEG","SEG")
names(new)<-gel   
#rename the difference of veg.indices
for (be in names(new)){
  names(new[[be]])<- gsub(x=names(new[[be]]), pattern= "mean" ,replacement="mean_diff")
}      
for (be in names(new)){
  names(new[[be]])<- gsub(x=names(new[[be]]), pattern= "median" ,replacement="median_diff")
} 
for (be in names(new)){
  names(new[[be]])<- gsub(x=names(new[[be]]), pattern= "sd" ,replacement="sd_diff")
}

# write the difference df in beg_idx
  diff_idx<-new
  rm(new,beg_idx)
#---------------------------------------------------------
  #read haralick textures of NDVI
      ref <- list.files(path_rdata_pre, pattern = glob2rx("re_NDVI_ht_extr_*.rds"), 
                           full.names = TRUE) #liste der haralick texturen pro exploratory
      ref_01 <- readRDS(ref[1])
      ref_02 <- readRDS(ref[2])
      ref_03 <- readRDS(ref[3])
      ref_snip<-readRDS(paste0(path_rdata_pre,"re_end04_snip_extr.rds"))
  # bind haralcik with all other predictors
      ref_AE <- cbind(ref_snip[[1]], end_idx[[1]], diff_idx[[1]], ref_01)
      ref_HE <- cbind(ref_snip[[2]], end_idx[[2]], diff_idx[[2]], ref_02)
      ref_SE <- cbind(ref_snip[[3]], end_idx[[3]], diff_idx[[3]], ref_03)
      
      ref <- list(AE=ref_AE, HE=ref_HE, SE=ref_SE)
     # remove all variance variables 
      for (be in names(ref)){
        ref[[be]]<-ref[[be]][, -grep("var",names(ref[[be]]))]
      }
      saveRDS(ref, paste0(path_rdata_pre, "re_basic_predictors.rds"))
    
    