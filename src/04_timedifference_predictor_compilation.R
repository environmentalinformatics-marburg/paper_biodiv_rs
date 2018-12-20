source("C:/Users/uselig/Documents/GitHub/project_biodiv_rs/src/00_paths_libraries.R")

load("C:/exploratorien/exploratorien/data/rdata/preprocessing/RE_2015_March_A19_H19_S18.RData")

# calculate NDVI and Rao's Q

# the package to calc. RaosQ is loaded directly from source and can not be .oaded or installed vioa RStudio and libraries
source("C:/Users/uselig/Documents/GitHub/spectralrao-master/spectralrao.r")

re_idxe_mar <- lapply(march, function(e){
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
    re_idxe_mar <- stack(re_ndvi, re_rendvi, rao)
  })
})

# we dont save this file because we only want the difference between those indices from March and April

# read april Indices
re_idxe_apr<-get(load("C:/exploratorien/exploratorien/data/rdata/preprocessing/RE_2015_April__NDVI_RaosQ.RData"))

diff <- lapply(seq(length(re_idxe_apr)), function(k){ # they have the same structure and dimension so we can base the loop on one of the list
  t <- lapply(seq(length(re_idxe_apr[[k]])), function(i){
     diff<-re_idxe_apr[[k]][[i]] - re_idxe_mar[[k]][[i]]
  })
  names(t) <- names(re_idxe_apr[[k]])
  return(t)
})
names(diff)<-c("AEG", "HEG", "SEG")
  backup<-diff
#rename the difference of veg.indices
for (be in names(diff)){
  for (loc in names(diff[[be]])){
  names(diff[[be]][[loc]])<- gsub(x=names(diff[[be]][[loc]]), pattern= "NDVI" ,replacement="NDVI_diff")
  }  
}
for (be in names(diff)){
  for (loc in names(diff[[be]])){
  names(diff[[be]][[loc]])<- gsub(x=names(diff[[be]][[loc]]), pattern= "RaosQ" ,replacement="RaosQ_diff")
  }
}

save(diff, file = paste0(path_rdata_pre, "RE_2015_April_March_diff_NDVI_RaosQ.RData"))
  

