# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  source("D:/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
} else {
  source("/media/permanent/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
}

compute = TRUE

# Read RapidEye data -----------------------------------------------------------
if(compute){
  fns <- c("2015-04-24T110941_RE1_1B-NAC_20835999_303429_ortho_alb.tif",
           "2015-04-24T110857_RE1_1B-NAC_20835994_303428_ortho_hai.tif",
           "2015-04-10T111339_RE1_1B-NAC_20835979_303426_ortho_sch.tif")
  re <- lapply(fns, function(f){
    re <- stack(paste0(path_re, f))
    names(re) <- paste0("RE201504_", seq(5))
    return(re)
  })
  names(re) <- c("Alb", "Hai", "Sch")
}


# Read plot polygon data -------------------------------------------------------
if(compute){
  fns <- c("polyAlbEp.shp", "polyHaiEp.shp", "polySchEp.shp")
  poly <- lapply(seq(length(fns)), function(i){
    poly <- readBExpPoly(shp = paste0(path_plots, fns[i]), crs = crs(re[[i]]))
    poly[grepl("EG", poly@data$EP),]
  })
  names(poly) <- c("Alb", "Hai", "Sch")
  saveRDS(poly, file = paste0(path_rdata, "poly.rds"))
} else {
  poly <- readRDS(file = paste0(path_rdata, "poly.rds"))
}


# Create one raster tile for each observation plot -----------------------------
if(compute){
  re_snip <- lapply(seq(length(re)), function(i){
    re_snip <- snipRaster(raster=re[[i]], 
                          spatial=poly[[names(re[i])]], selector = "EP",
                          buffer=500, byid = TRUE)
  })
  saveRDS(re_snip, file = paste0(path_rdata, "re_plots.rds"))
} else {
  re_snip <- readRDS(file = paste0(path_rdata, "re_plots.rds"))
}


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


# Compute indices for all plots ------------------------------------------------
if(compute){
  re_idx <- lapply(re_snip, function(e){
    lapply(e, function(s){
      re_rgb <- rgbIndices(stack(s[[3]], s[[2]], s[[3]]), 
                           rgbi = c("GLI", "NGRDI", "TGI", "VVI"))
      re_ndvi <- (s[[5]]-s[[3]])/(s[[5]]+s[[3]])
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



# Compute Haralick textures for all plots based on PCA -------------------------
windows <- c(seq(1, 15, 2), 50)
if(compute){
  for(i in seq(length(re_pca))){
    minv <- min(unlist(lapply(re_pca[[i]], function(s){minValue(s$PC1)})))
    maxv <- max(unlist(lapply(re_pca[[i]], function(s){maxValue(s$PC1)})))
    re_pca_ht <- lapply(re_pca[[i]], function(s){
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
  re_snip_extr <- lapply(seq(3), function(i){
    extractFromRasterSnips(raster = re_snip[[i]], spatial = poly[[i]], buffer = 0, 
                           mahal = TRUE)
  })
  # For some reason, some medians are characters
  for(i in grep("median", colnames(re_snip_extr[[2]]@data))){
    re_snip_extr[[2]]@data[, i] <- as.numeric(re_snip_extr[[2]]@data[, i])
  }
  saveRDS(re_snip_extr, paste0(path_rdata, "re_snip_extr.rds"))
  
  re_pca_extr <- lapply(seq(3), function(i){
    extractFromRasterSnips(raster = re_pca[[i]], spatial = poly[[i]], buffer = 0, 
                           mahal = FALSE)
  })
  saveRDS(re_pca_extr, paste0(path_rdata, "re_pca_extr.rds"))
  
  re_idx_extr <- lapply(seq(3), function(i){
    extractFromRasterSnips(raster = re_idx[[i]], spatial = poly[[i]], buffer = 0, 
                           mahal = FALSE)
  })
  saveRDS(re_idx_extr, paste0(path_rdata, "re_idx_extr.rds"))
  
  re_pca_ht_files <- list.files(path_rdata, pattern = glob2rx("re_pca_h_*.rds"), 
                                full.names = TRUE)
  for(e in seq(3)){
    re_pca_ht <- readRDS(re_pca_ht_files[e])
    re_pca_ht_ext <- extractFromRasterSnips(raster = re_pca_ht, 
                                            spatial = poly[[e]], buffer = 0,
                                            mahal = FALSE)
    n <- names(re_pca_ht_ext)
    pos_m <- gregexpr('m\\.', names(re_pca_ht_ext))
    pos_us <- regexpr("\\_[^\\_]*$", names(re_pca_ht_ext))
    for(i in seq(length(pos_m))){
      if(pos_m[[i]][1] != -1){
        n[i] <- paste0(substr(n[i], 1, pos_m[[i]]-1), 
                       substr(n[i], pos_us[[i]], nchar(n[i])))
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
  
  ref_AEG <- cbind(ref_01[[1]], ref_02[[1]], ref_03[[1]], ref_04)
  ref_HEG <- cbind(ref_01[[2]], ref_02[[2]], ref_03[[2]], ref_05)
  ref_SEG <- cbind(ref_01[[3]], ref_02[[3]], ref_03[[3]], ref_06)
  
  ref <- list(AEG=ref_AEG, HEG=ref_HEG, SEG=ref_SEG)
  saveRDS(ref, paste0(path_rdata, "re_predictors.rds"))
}


