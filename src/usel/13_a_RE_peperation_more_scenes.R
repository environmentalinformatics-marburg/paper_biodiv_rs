# Set path ---------------------------------------------------------------------
rm(list=ls()) 
source("D:/UNI/Master/MA/exploratorien/scripts/project_biodiv_rs/src/usel/00_a_set_environment_until_gpm_compile.R")

compute = T

# MARCH all scenes -----------------------
#after topographic correction we use the according new datasets of HEG,AEG,SEG
re03 <- list.files(paste0(path_re_cor), pattern = glob2rx("RapidEye_201503*.tif"), 
                    full.names = TRUE)

rel03 <- lapply(re03, function(f){
  rel03 <- stack(f)
  return(rel03)
})
#change layer names for march aquisition

names(rel03[[1]]) <- paste0("RE20150308_", seq(5)) #SEG
names(rel03[[2]]) <- paste0("RE20150318_", seq(5)) #SEG
names(rel03[[3]]) <- paste0("RE20150319_", seq(5)) #AEG
names(rel03[[4]]) <- paste0("RE20150319_", seq(5)) #HEG
#in den names stehen die 5 b?nder mit RE...1,RE...2 usw.seq. zur Wdh. f?r alle b?nder

names(rel03)<-c("SEG0308","SEG0318","AEG0319","HEG0319")
rel03<-rel03[order(names(rel03))]
rel03_SEG<-rel03[[3]] #extra df für die "Außenseiter-Szene", einfacher weil Datenstruktur beibehalten
rel03[[3]]<-NULL
saveRDS(rel03_SEG, file = paste0(path_rdata, "preprocessing/re20150308_SEG_raster.rds"))
names(rel03)<-c("AEG","HEG","SEG")
#-------------------------------------------------------------------------

# APRIL all scenes (ohne die orginal prozessierten Szenen) -----------------------
#after topographic correction we use the according new datasets of HEG,AEG,SEG
re04 <- list.files(paste0(path_re_cor), pattern = glob2rx("RapidEye_201504*.tif"), 
                   full.names = TRUE)

rel04 <- lapply(re04, function(f){
  rel04 <- stack(f)
  return(rel04)
})
#change layer names for march aquisition
names(rel04[[1]]) <- paste0("RE20150409_", seq(5)) #AEG
names(rel04[[2]]) <- paste0("RE20150409_", seq(5)) #HEG
names(rel04[[3]]) <- paste0("RE20150415_", seq(5)) #SEG
names(rel04[[4]]) <- paste0("RE20150420_", seq(5)) #SEG
#in den names stehen die 5 b?nder mit RE...1,RE...2 usw.seq. zur Wdh. f?r alle b?nder
rel0420_SEG<-rel04[[4]] #extra df für die "Außenseiter-Szene", einfacher weil Datenstruktur beibehalten
rel04[[4]]<-NULL
names(rel04)<-c("AEG","HEG","SEG")
#names(rel04)<-c("AEG0409","HEG0409","SEG0415","SEG0420")
saveRDS(rel0420_SEG, file = paste0(path_rdata, "preprocessing/re20150420_SEG_raster.rds"))
#-----------------------------------------------------------------------------

# MAY all scenes (its only one) -----------------------
#after topographic correction we use the according new datasets of HEG,AEG,SEG
re05 <- list.files(paste0(path_re_cor), pattern = glob2rx("RapidEye_201505*.tif"), 
                   full.names = TRUE)

rel05 <- lapply(re05, function(f){
  rel05 <- stack(f)
  names(rel05)<-paste0("RE20150515_",seq(5))
  return(rel05)
})

names(rel05)<-"HEG0515"

# Read plot polygon data -------------------------------------------------------
fns <- c("plots/polyAlbEp.shp", "plots/polyHaiEp.shp", "plots/polySchEp.shp")
#notiz: die "plots"shapes im 1."plots" ordner sind nur Punktdaten!
poly <- lapply(seq(length(fns)), function(i){
  poly <- readBExpPoly(shp = paste0(path_plots, fns[i]), crs=crs(rel03[[i]]))
 #poly[grepl("EG", poly@data$plot),]
})
#extrahiere gleich nur Grasland da der Extent mit Wald nciht mehr stimmt (ggf. wegen der beschädigten files, wo
# die Ränder überall fehlen)
poly_g<- lapply(seq(length(poly)), function(y){
  poly_g<- subset(poly[[y]][1:50,])
})
names(poly_g) <- c("AEG", "HEG", "SEG")

saveRDS(poly_g, file = paste0(path_rdata, "preprocessing/poly_grassland.rds"))
------------------------------------------------------------
poly_g <- readRDS(file = paste0(path_rdata, "preprocessing/poly_grassland.rds"))

#mit benni checkings
{#extent(re[[2]])
  #extent(poly[[2]])
  #crs(poly[[1]])
  #crs(re$AEG[[1]])
  #plot(re[[2]][[1]]) #testweise f?r eine zeile [3]
  #plot(poly[[2]], add=T) #plotte vektordaten in raster wenn dieses zuerst eingeladen wurde
}

re<-rel04
  re_snip <- lapply(seq(length(re)), function(i){
    re_snip <- snipRaster(raster=re[[i]], 
                          spatial=poly_g[[i]], selector="EP", #changed EP into general-differnet datasource...?!
                          buffer=200, byid = TRUE) #we change buffer from 500 to 200
  })
  saveRDS(re_snip, file = paste0(path_rdata, "preprocessing/re04_plots.rds"))

#check plots im 2.Raster(HEG) das 4.Polygon plot(re_snip[[2]][[4]][[5]]) 

  # Compute pca for all plots ----------------------------------------------------
  re_pca <- lapply(re_snip, function(e){
    lapply(e, function(s){
      re_pca <- pca(stack(s), center = TRUE, scale = TRUE, return_raster = TRUE)
    })
  })
  saveRDS(re_pca, file = paste0(path_rdata, "preprocessing/re04_pca.rds"))

plot(re_pca[[2]][[49]][[2]])
#summary(re_pca[[2]][[1]])

# Compute indices for all plots ------------------------------------------------
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
  saveRDS(re_idx, file = paste0(path_rdata, "preprocessing/re04_idx.rds"))

# Compute Haralick textures for all plots based on PCA -------------------------
#ACHTUNG Rechenprocedere ca.4 Stunden

#re_pca <- readRDS(file = paste0(path_rdata, "preprocessing/re03_pca.rds"))
 
  #möglichst alles unwichtige löschen
rm(re_idx, poly_g,re_snip,re,rel04)
re_pca<-readRDS(paste0(path_rdata, "preprocessing/re03_pca.rds"))
  windows <- c(seq(1, 15, 2), 50)
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
        names(oth) <- paste0("03pca_", names(oth))
        return(oth)
      })
    })
    outfile <- paste0("03re_pca_h_", substr(names(re_pca[[i]][1]), 1, 3), ".rds")
    saveRDS(re_pca_ht, file = paste0(path_rdata, outfile))
  }


# Extract raster data based on polygon -----------------------------------------
if(compute){
  poly<-readRDS(file = paste0(path_rdata, "preprocessing/poly_grassland.rds"))
  re_snip <- readRDS(file = paste0(path_rdata, "preprocessing/re04_plots.rds"))
  re_snip_extr <- lapply(seq(3), function(i){
    extractFromRasterSnips(raster = re_snip[[i]], spatial = poly[[i]], buffer = 0, 
                           mahal = TRUE)
  })
  
  # For some reason, some medians are characters
  for(i in grep("median", colnames(re_snip_extr[[2]]@data))){
    re_snip_extr[[2]]@data[, i] <- as.numeric(re_snip_extr[[2]]@data[, i])
  }
  saveRDS(re_snip_extr, paste0(path_rdata, "preprocessing/re04_snip_extr.rds"))
  
  #extraction for pca  
  re_pca_extr <- lapply(seq(3), function(i){
    extractFromRasterSnips(raster = re_pca[[i]], spatial = poly[[i]], buffer = 0, 
                           mahal = FALSE)
  })
  saveRDS(re_pca_extr, paste0(path_rdata, "preprocessing/re04_pca_extr.rds"))
  
  #extraction for indices  
  re_idx_extr <- lapply(seq(3), function(i){
    re_idx <- readRDS(file = paste0(path_rdata, "preprocessing/re04_idx.rds"))
    extractFromRasterSnips(raster = re_idx[[i]], spatial = poly[[i]], buffer = 0, 
                           mahal = FALSE)
  })
  saveRDS(re_idx_extr, paste0(path_rdata, "preprocessing/re04_idx_extr.rds"))
  
  #extraction for haralick textures  
  re_pca_ht_files <- list.files(path_rdata, pattern = glob2rx("04re_pca_h_*.rds"), 
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
    outfile <- paste0("preprocessing/re04_pca_ht_extr_", substr(re_pca_ht_files[e], nc-6, nc-4), ".rds")
    saveRDS(re_pca_ht_ext, paste0(path_rdata, outfile))
  }
  
  ref_01 <- list.files(path_rdata_pre, pattern = glob2rx("re04_*extr.rds"), 
                       full.names = TRUE)
  ref_02 <- list.files(path_rdata_pre, pattern = glob2rx("re04*_extr_*.rds"), 
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
  #rename all predictors to identify month
  re_df <- lapply(ref, function(e){
    as.data.frame(e)
  })
  for (be in names(re_df)){
   names(re_df[[be]])<- paste("re04", colnames(re_df[[be]]),sep="_")
  }
  
  # if we want bind, we need identical predictornames in all Explo., so change the 
  # bandnames (we need to access the exact date at another point) 
      #check first if really the replacemtns are in RE-columns
  names(re_df[[1]][,grep("0409", names(re_df[[1]]))])
  
  for (be in names(re_df)){
    names(re_df[[be]])<- gsub(x=names(re_df[[be]]), pattern= "0409|0415" ,replacement="04")
  }
  #now prve if cols are identical
  identical(names(re_df[[3]]), names(re_df[[1]]))
  
  re_df <- do.call("rbind", re_df)
  re_df$Year <- 2015
  nas <- colSums(is.na(re_df))
  re_df <- re_df[, which(nas < 1)]
  
  saveRDS(re_df, paste0(path_rdata_pre, "re04_predictors.rds"))

predictors<- readRDS(paste0(path_rdata, "re03_predictors.rds"))
