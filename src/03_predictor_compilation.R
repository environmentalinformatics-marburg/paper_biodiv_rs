source("C:/Users/uselig/Documents/GitHub/project_biodiv_rs/src/00_paths_libraries.R")
# read the RE Scene(s)

load("C:/exploratorien/exploratorien/data/rdata/preprocessing/RE_2015_April_A24_H24_S20.RData")

# calculate NDVI and Rao's Q

# the package to calc. RaosQ is loaded directly from source and can not be .oaded or installed vioa RStudio and libraries
source("C:/Users/uselig/Documents/GitHub/spectralrao-master/spectralrao.r")

re_idxe <- lapply(april, function(e){
    lapply(e, function(s){
      # calculate NDVI with rededge band
      re_ndvi <- (s[[5]]-s[[3]])/(s[[5]]+s[[3]]) 
      names(re_ndvi) <- "NDVI"
      #calculate ndvi with near infrared band
      re_rendvi <- (s[[4]]-s[[3]])/(s[[4]]+s[[3]]) 
      names(re_rendvi) <- "RE_NDVI"
      # calculate Rao's Q on original bands
      raomatrix <- spectralrao(as.list(s), #alle baender werden einzeln gelistet
                               mode="multidimension", 
                               distance_m="euclidean", 
                               window= 3,
                               shannon=FALSE, 
                               debugging=TRUE, 
                               simplify=3)
      rao = setValues(s[[1]], raomatrix[[1]])
      names(rao)="RaosQ"
      re_idxe <- stack(re_ndvi, re_rendvi, rao)
    })
  })

save(re_idxe, file = paste0(path_rdata_pre, "RE_2015_April__NDVI_RaosQ.RData"))

#________________________________________________________________________________

# calculate single ratio with all possible combinations of a stack

sr <- function(RasterStack){
  # contingency table with all band combinations
  cont <- combn(nlayers(RasterStack), 2)
  # calculate all simple ratios
  ratios <- lapply(seq(ncol(cont)), function(i){
    r <- RasterStack[[cont[1,i]]]/RasterStack[[cont[2,i]]]
    # name the result accoringly
    names(r) <- paste0("sr_", cont[1,i], "_", cont[2,i])
    return(r)
  })
  return(stack(ratios))
}

# call function
re_sr<- lapply(april, function(r){
  lapply(r, function(t){
      simpleratio<-sr(t)
      })
  })
save(re_sr, file = paste0(path_rdata_pre, "RE_2015_April__SimpleRatio.RData"))

#________________________________________________________________________________

#calculate Haralick für NDVI
windows <- 3

for(i in seq(length(re_idxe))){
  print(paste("Listennummer: ",i))
  minv <- min(unlist(lapply(re_idxe[[i]], function(s){minValue(s$RE_NDVI)})))
  maxv <- max(unlist(lapply(re_idxe[[i]], function(s){maxValue(s$RE_NDVI)})))
  
  re_RE_NDVI_ht <-  lapply(re_idxe[[i]], function(s){
    print(paste("Punktnummer: ", i))
    lapply(windows, function(w){
      oth <- otbTexturesHaralick(x=s$RE_NDVI, path_output = path_temp, 
                                 return_raster = TRUE, 
                                 parameters.xyrad=list(c(w,w)),
                                 parameters.xyoff=list(c(1,1)),
                                 parameters.minmax=c(minv, maxv),
                                 parameters.nbbin = 32,
                                 texture="simple",
                                 channel = 1)
      names(oth) <- paste0("RE_NDVI_", names(oth))
      return(oth)
    })
  })
  outfile <- paste0("RE_NDVI_h_simple_", substr(names(re_idxe[[i]][1]), 1, 3), ".rds")
  saveRDS(re_RE_NDVI_ht, file = paste0(path_rdata_pre, outfile))
}

# Haralcik für RaosQ advanced
for(i in seq(length(re_idxe))){
  print(paste("Listennummer: ",i))
  minv <- min(unlist(lapply(re_idxe[[i]], function(s){minValue(s$RaosQ)})))
  maxv <- max(unlist(lapply(re_idxe[[i]], function(s){maxValue(s$RaosQ)})))
  
  re_RaosQ_ht <-  lapply(re_idxe[[i]], function(s){
    print(paste("Punktnummer: ", i))
    lapply(windows, function(w){
      oth <- otbTexturesHaralick(x=s$RaosQ, path_output = path_temp, 
                                 return_raster = TRUE, 
                                 parameters.xyrad=list(c(w,w)),
                                 parameters.xyoff=list(c(1,1)),
                                 parameters.minmax=c(minv, maxv),
                                 parameters.nbbin = 32,
                                 texture="simple", 
                                 channel = 1)
      names(oth) <- paste0("RaosQ_", names(oth))
      return(oth)
    })
  })
  outfile <- paste0("RaosQ_h_simple_", substr(names(re_idxe[[i]][1]), 1, 3), ".rds")
  saveRDS(re_RaosQ_ht, file = paste0(path_rdata_pre, outfile))
}
