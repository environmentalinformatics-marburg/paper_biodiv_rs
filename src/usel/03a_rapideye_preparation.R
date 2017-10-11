# Set path ---------------------------------------------------------------------
rm(list=ls()) 
  source("F:/exploratorien/scripts/00_set_environment.R")
  source("D:/UNI/Master/MA/exploratorien/scripts/00_set_environment.R")
setwd<-"D:/UNI/Master/MA/exploratorien/data/"
setwd<-"F:/exploratorien/data/"
compute = T


#after topographic correction we use the according new datasets of HEG,AEG,SEG
AEG_topo<-readRDS(file=paste0(path_rdata,"AEG_topo_corr.rds"))
HEG_topo<-readRDS(file=paste0(path_rdata,"HEG_topo_corr.rds"))
SEG_topo<-readRDS(file=paste0(path_rdata,"SEG_topo_corr.rds"))
re<-list(AEG_topo,HEG_topo,SEG_topo)
rm(AEG_topo,HEG_topo,SEG_topo)
#change layer names
names(re) <- c("AEG", "HEG", "SEG") 
names(re[[1]])<-c("RE201504_1","RE201504_2","RE201504_3","RE201504_4","RE201504_5")
names(re[[2]])<-c("RE201504_1","RE201504_2","RE201504_3","RE201504_4","RE201504_5")
names(re[[3]])<-c("RE201504_1","RE201504_2","RE201504_3","RE201504_4","RE201504_5")


# Read plot polygon data -------------------------------------------------------
if(compute){
  fns <- c("polyAlbEp.shp", "polyHaiEp.shp", "polySchEp.shp")
  poly <- lapply(seq(length(fns)), function(i){
    poly <- readBExpPoly(shp = paste0(path_plots, fns[i]), crs = crs(re[[i]]))
    # poly[grepl("EG", poly@data$EP),]
  })
  names(poly) <- c("Alb", "Hai", "Sch")
  saveRDS(poly, file = paste0(path_rdata, "poly.rds"))
} else {
  poly <- readRDS(file = paste0(path_rdata, "poly.rds"))
}

#mit benni checkings
{#extent(re[[2]])
  #extent(poly[[2]])
  #crs(poly[[1]])
  #crs(re$AEG[[1]])
  #plot(re[[2]][[1]]) #testweise f?r eine zeile [3]
  #plot(poly[[2]], add=T) #plotte vektordaten in raster wenn dieses zuerst eingeladen wurde
}

# Create one raster tile for each observation plot (NOTE: re are atmoscorr. tifs following data will be overwritten) -----------------------------
if(compute){
  re_snip <- lapply(seq(length(re)), function(i){
    re_snip <- snipRaster(raster=re[[i]], 
                          spatial=poly[[i]], selector="EP", #changed EP into general-differnet datasource...?!
                          buffer=500, byid = TRUE)
  })
  saveRDS(re_snip, file = paste0(path_rdata, "re_plots.rds"))
} else {
  re_snip <- readRDS(file = paste0(path_rdata, "re_plots.rds"))
}
#check plots im 2.Raster(SEG) das 4.Polygon plot(re_snip[[2]][[4]][[5]]) 
#checkcorrection<-plotRGB(re_snip[[1]][[1]], r=3, g=2, b=1)
#rm(checkcorrection)
# Compute pca for all plots ----------------------------------------------------
if(compute){
  re_pca <- lapply(re_snip, function(e){
    lapply(e, function(s){
      re_pca <- pca(stack(s), center = TRUE, scale = TRUE, return_raster = TRUE)
    })
  })
  saveRDS(re_pca, file = paste0(path_rdata, "re_pca.rds"))
} else {
  re_pca <- readRDS(file = paste0(path_rdata, "re_pca.rds"))
}

plot(re_pca[[2]][[49]][[2]])
#summary(re_pca[[2]][[1]])

# Compute indices for all plots ------------------------------------------------
if(compute){
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
  saveRDS(re_idx, file = paste0(path_rdata, "re_idx.rds"))
} else {
  re_idx <- readRDS(file = paste0(path_rdata, "re_idx.rds"))
}


rm(re_idx, poly,re_snip,re)

# Compute Haralick textures for all plots based on PCA -------------------------
#ACHTUNG Rechenprocedere mehrere Stunden
windows <- c(seq(1, 15, 2), 50)
if(compute){
  for(i in seq(length(re_pca))){
    print(paste("Listennummer: ",i))
    minv <- min(unlist(lapply(re_pca[[i]], function(s){minValue(s$PC1)})))
    maxv <- max(unlist(lapply(re_pca[[i]], function(s){maxValue(s$PC1)})))
    re_pca_ht <- lapply(re_pca[[i]], function(s){
      print(paste("Punktnummer: ", i))
      lapply(windows, function(w){
        oth <- otbTexturesHaralick(x=s$PC1, path_output = path_temp, 
                                   return_raster = TRUE, 
                                   parameters.xyrad=list(c(w,w)),
                                   parameters.xyoff=list(c(1,1)),
                                   parameters.minmax=c(minv, maxv),
                                   parameters.nbbin = 8,
                                   texture="all",
                                   channel = 1)
        names(oth) <- paste0("pca_", names(oth))
        return(oth)
      })
    })
    outfile <- paste0("re_pca_h_", substr(names(re_pca[[i]][1]), 1, 3), ".rds")
    saveRDS(re_pca_ht, file = paste0(path_rdata, outfile))
  }
}


# Extract raster data based on polygon -----------------------------------------
if(compute){
  poly<-readRDS(file = paste0(path_rdata, "poly.rds"))
  re_snip <- readRDS(file = paste0(path_rdata, "re_plots.rds"))
  re_snip_extr <- lapply(seq(3), function(i){
    extractFromRasterSnips(raster = re_snip[[i]], spatial = poly[[i]], buffer = 0, 
                           mahal = TRUE)
  })
  
  # For some reason, some medians are characters
  for(i in grep("median", colnames(re_snip_extr[[2]]@data))){
    re_snip_extr[[2]]@data[, i] <- as.numeric(re_snip_extr[[2]]@data[, i])
  }
  saveRDS(re_snip_extr, paste0(path_rdata, "re_snip_extr.rds"))

#extraction for pca  
  re_pca_extr <- lapply(seq(3), function(i){
    extractFromRasterSnips(raster = re_pca[[i]], spatial = poly[[i]], buffer = 0, 
                           mahal = FALSE)
  })
  saveRDS(re_pca_extr, paste0(path_rdata, "re_pca_extr.rds"))

#extraction for indices  
  re_idx_extr <- lapply(seq(3), function(i){
    re_idx <- readRDS(file = paste0(path_rdata, "re_idx.rds"))
    extractFromRasterSnips(raster = re_idx[[i]], spatial = poly[[i]], buffer = 0, 
                           mahal = FALSE)
  })
  saveRDS(re_idx_extr, paste0(path_rdata, "re_idx_extr.rds"))
  
#extraction for haralick textures  
  re_pca_ht_files <- list.files(path_rdata, pattern = glob2rx("re_pca_h_*.rds"), 
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
    outfile <- paste0("re_pca_ht_extr_", substr(re_pca_ht_files[e], nc-6, nc-4), ".rds")
    saveRDS(re_pca_ht_ext, paste0(path_rdata, outfile))
  }
} else {
  ref_01 <- list.files(path_rdata, pattern = glob2rx("*_extr.rds"), 
                       full.names = TRUE)
  ref_02 <- list.files(path_rdata, pattern = glob2rx("*_extr_*.rds"), 
                       full.names = TRUE)
  ref <- c(ref_01, ref_02)
  
  ref_01 <- readRDS(ref[1])
  ref_02 <- readRDS(ref[2])
  ref_03 <- readRDS(ref[3])
  ref_04 <- readRDS(ref[4])
  ref_05 <- readRDS(ref[5])
  ref_06 <- readRDS(ref[6])
  
  ref_AE <- cbind(ref_01[[1]], ref_02[[1]], ref_03[[1]], ref_04)
  ref_HE <- cbind(ref_01[[2]], ref_02[[2]], ref_03[[2]], ref_05)
  ref_SE <- cbind(ref_01[[3]], ref_02[[3]], ref_03[[3]], ref_06)
  
  ref <- list(AE=ref_AE, HE=ref_HE, SE=ref_SE)
  saveRDS(ref, paste0(path_rdata, "re_predictors.rds"))
}
predictors<- readRDS(paste0(path_rdata, "re_predictors.rds"))
