
source("C:/Users/uselig/Documents/GitHub/project_biodiv_rs/src/00_paths_libraries.R")
  
# all<-readOGR("C:/exploratorien/exploratorien/data/plots/plots.shp")
  # 
  # # make a new projection
  # newprojAH<- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # for AEG and HEG
  # p_aeg<-all[1:50,]
  # p_aeg<-spTransform(p_aeg, newprojAH)
  # 
  # p_heg<-all[51:100,]
  # p_heg<-spTransform(p_heg, newprojAH)
  # 
  # newprojS<- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # for AEG and HEG
  # p_seg<-all[101:150,]
  # p_seg<-spTransform(p_seg, newprojS)
  # 
  # points<-list(p_aeg, p_heg, p_seg)
  # names(points) <- c("AEG", "HEG", "SEG")
  # 
  # saveRDS(points, file = paste0(path_rdata_pre, "points_utm.rds"))

#_________________________________________________________________________________________
points <- readRDS(file = paste0(path_rdata_pre, "points_utm.rds"))

#_________________________________________________________________________________________
### extract original bands

# read the RE Scene
re_bands<-get(load(paste0(path_rdata_pre,"RE_2015_April_A24_H24_S20.RData")))

re_extr <- lapply(seq(length(re_bands)), function(k){
  t <- lapply(seq(length(re_bands[[k]])), function(i){
    ex <- as.data.frame(extract(re_bands[[k]][[i]], extent(points[[k]][i,]@coords[1]-17.5,
                                                           points[[k]][i,]@coords[1]+17.5,
                                                           points[[k]][i,]@coords[2]-17.5,
                                                           points[[k]][i,]@coords[2]+17.5)))
  })
  names(t) <- names(re_bands[[k]])
  return(t)
  
})
names(re_extr)<-c("AEG", "HEG", "SEG")
rm(re_bands,april)

#_______________________________________________
# extract NDVI, RE_NDVI and RaosQ
re_idxe<-get(load(paste0(path_rdata_pre,"RE_2015_April__NDVI_RaosQ.RData")))

re_idxe_extr <- lapply(seq(length(re_idxe)), function(k){
  t <- lapply(seq(length(re_idxe[[k]])), function(i){
    ex <- as.data.frame(extract(re_idxe[[k]][[i]], extent(points[[k]][i,]@coords[1]-17.5,
                                                           points[[k]][i,]@coords[1]+17.5,
                                                           points[[k]][i,]@coords[2]-17.5,
                                                           points[[k]][i,]@coords[2]+17.5)))
  })
  names(t) <- names(re_idxe[[k]])
  
  return(t)
})
names(re_idxe_extr)<-c("AEG", "HEG", "SEG")
rm(re_idxe)

#_______________________________________________
# extract Difference to March NDVI, RE_NDVI and RaosQ
re_diff<-get(load(paste0(path_rdata_pre,"RE_2015_April_March_diff_NDVI_RaosQ.RData")))

re_diff_extr <- lapply(seq(length(re_diff)), function(k){
  t <- lapply(seq(length(re_diff[[k]])), function(i){
    ex <- as.data.frame(extract(re_diff[[k]][[i]], extent(points[[k]][i,]@coords[1]-17.5,
                                                          points[[k]][i,]@coords[1]+17.5,
                                                          points[[k]][i,]@coords[2]-17.5,
                                                          points[[k]][i,]@coords[2]+17.5)))
  })
  names(t) <- names(re_diff[[k]])
  return(t)
})
names(re_diff_extr)<-c("AEG", "HEG", "SEG")
rm(re_diff, diff)

#_______________________________________________
# extract Simple ratio
re_sr<-get(load(paste0(path_rdata_pre,"RE_2015_April__SimpleRatio.RData")))
re_sr_extr <- lapply(seq(length(re_sr)), function(k){
  t <- lapply(seq(length(re_sr[[k]])), function(i){
    ex <- as.data.frame(extract(re_sr[[k]][[i]], extent(points[[k]][i,]@coords[1]-17.5,
                                                          points[[k]][i,]@coords[1]+17.5,
                                                          points[[k]][i,]@coords[2]-17.5,
                                                          points[[k]][i,]@coords[2]+17.5)))
  })
  names(t) <- names(re_sr[[k]])
  return(t)
})
names(re_sr_extr)<-c("AEG", "HEG", "SEG")
rm(re_sr)

#_______________________________________________
# extract haralicks NDVI ADVANCED

ht_files <- list.files(path_rdata_pre, pattern = glob2rx("RE_*h_advanced*.rds"), 
                              full.names = TRUE)
# merge advanced haralicks
ht_aeg<- readRDS(ht_files[1])
ht_heg<- readRDS(ht_files[2])
ht_seg<- readRDS(ht_files[3])
ht_advanced<-list(ht_aeg, ht_heg, ht_seg)
names(ht_advanced)<-c("AEG", "HEG", "SEG")

ht_advanced_extr <- lapply(seq(length(ht_advanced)), function(k){
  t <- lapply(seq(length(ht_advanced[[k]])), function(i){
    ex <- as.data.frame(extract(ht_advanced[[k]][[i]][[1]], extent(points[[k]][i,]@coords[1]-17.5,
                                                        points[[k]][i,]@coords[1]+17.5,
                                                        points[[k]][i,]@coords[2]-17.5,
                                                        points[[k]][i,]@coords[2]+17.5)))
    
  })
  names(t) <- names(ht_advanced[[k]])
  return(t)

})
# extract haralicks Rao ADVANCED

ht_files <- list.files(path_rdata_pre, pattern = glob2rx("RaosQ_h_advanced*.rds"), 
                       full.names = TRUE)
# merge advanced haralicks
ht_aeg<- readRDS(ht_files[1])
ht_heg<- readRDS(ht_files[2])
ht_seg<- readRDS(ht_files[3])
ht_advanced<-list(ht_aeg, ht_heg, ht_seg)
names(ht_advanced)<-c("AEG", "HEG", "SEG")

ht_Rao_advanced_extr <- lapply(seq(length(ht_advanced)), function(k){
  t <- lapply(seq(length(ht_advanced[[k]])), function(i){
    ex <- as.data.frame(extract(ht_advanced[[k]][[i]][[1]], extent(points[[k]][i,]@coords[1]-17.5,
                                                                   points[[k]][i,]@coords[1]+17.5,
                                                                   points[[k]][i,]@coords[2]-17.5,
                                                                   points[[k]][i,]@coords[2]+17.5)))
    
  })
  names(t) <- names(ht_advanced[[k]])
  return(t)
  
})
names(ht_Rao_advanced_extr)<-c("AEG", "HEG", "SEG")

#_______________________________________________
# extract haralicks NDVI SIMPLE

ht_files <- list.files(path_rdata_pre, pattern = glob2rx("RE_*h_simple*.rds"), 
                       full.names = TRUE)
# merge advanced haralicks
ht_aeg<- readRDS(ht_files[1])
ht_heg<- readRDS(ht_files[2])
ht_seg<- readRDS(ht_files[3])

ht_simple<-list(ht_aeg, ht_heg, ht_seg)
names(ht_simple)<-c("AEG", "HEG", "SEG")

ht_simple_extr <- lapply(seq(length(ht_simple)), function(k){
  t <- lapply(seq(length(ht_simple[[k]])), function(i){
    ex <- as.data.frame(extract(ht_simple[[k]][[i]][[1]], extent(points[[k]][i,]@coords[1]-17.5,
                                                                   points[[k]][i,]@coords[1]+17.5,
                                                                   points[[k]][i,]@coords[2]-17.5,
                                                                   points[[k]][i,]@coords[2]+17.5)))
    
  })
  names(t) <- names(ht_simple[[k]])
  return(t)
  
})
names(ht_simple_extr)<-c("AEG", "HEG", "SEG")

# extract haralicks Rao SIMPLE

ht_files <- list.files(path_rdata_pre, pattern = glob2rx("RaosQ_h_simple*.rds"), 
                       full.names = TRUE)
# merge advanced haralicks
ht_aeg<- readRDS(ht_files[1])
ht_heg<- readRDS(ht_files[2])
ht_seg<- readRDS(ht_files[3])

ht_simple<-list(ht_aeg, ht_heg, ht_seg)
names(ht_simple)<-c("AEG", "HEG", "SEG")

ht_Rao_simple_extr <- lapply(seq(length(ht_simple)), function(k){
  t <- lapply(seq(length(ht_simple[[k]])), function(i){
    ex <- as.data.frame(extract(ht_simple[[k]][[i]][[1]], extent(points[[k]][i,]@coords[1]-17.5,
                                                                 points[[k]][i,]@coords[1]+17.5,
                                                                 points[[k]][i,]@coords[2]-17.5,
                                                                 points[[k]][i,]@coords[2]+17.5)))
    
  })
  names(t) <- names(ht_simple[[k]])
  return(t)
  
})
names(ht_Rao_simple_extr)<-c("AEG", "HEG", "SEG")

#________________________________________________________________________________
rm(ht_advanced,ht_aeg, ht_heg, ht_seg, ht_simple)

# bind haralcik with all other predictors
ref_AE_mean <- lapply(seq(50), function(k){
  ref_AE_mean<- as.data.frame.list(colMeans(cbind(re_extr[[1]][[k]], re_idxe_extr[[1]][[k]], re_diff_extr[[1]][[k]], 
                re_sr_extr[[1]][[k]],ht_advanced_extr[[1]][[k]],ht_Rao_advanced_extr[[1]][[k]],
                ht_simple_extr[[1]][[k]],ht_Rao_simple_extr[[1]][[k]]))
  )
  
})

for (k in seq(50)){
  ref_AE_mean[[k]][58]<-names(re_extr$AEG[k])
}
ref_AE_mean<-bind(ref_AE_mean)
names(ref_AE_mean)<-gsub("\\..*","",names(ref_AE_mean))


ref_HE_mean <- lapply(seq(50), function(k){
  ref_HE_mean<-  as.data.frame.list(colMeans(cbind(re_extr[[2]][[k]], re_idxe_extr[[2]][[k]], re_diff_extr[[2]][[k]], 
                 re_sr_extr[[2]][[k]],ht_advanced_extr[[2]][[k]],ht_Rao_advanced_extr[[2]][[k]],
                 ht_simple_extr[[2]][[k]],ht_Rao_simple_extr[[2]][[k]]))  
                 )
  
  })
for (k in seq(50)){
  ref_HE_mean[[k]][58]<-names(re_extr$HEG[k])
}
ref_HE_mean<-bind(ref_HE_mean)
names(ref_HE_mean)<-gsub("\\..*","",names(ref_HE_mean))
                                          
ref_SE_mean <- lapply(seq(50), function(k){
  ref_SE_mean<-  as.data.frame.list(colMeans(cbind(re_extr[[3]][[k]], re_idxe_extr[[3]][[k]], re_diff_extr[[3]][[k]], 
                 re_sr_extr[[3]][[k]],ht_advanced_extr[[3]][[k]],ht_Rao_advanced_extr[[3]][[k]],
                 ht_simple_extr[[3]][[k]],ht_Rao_simple_extr[[3]][[k]]))
  )
  
})
for (k in seq(50)){
  ref_SE_mean[[k]][58]<-names(re_extr$SEG[k])
}
ref_SE_mean<-bind(ref_SE_mean)
names(ref_SE_mean)<-gsub("\\..*","",names(ref_SE_mean))
                                          
##########
ref<-bind_rows(ref_AE_mean, ref_HE_mean, ref_SE_mean)
colnames(ref)[57]<-"EPID"
# save predictors
saveRDS(ref, paste0(path_rdata_pre, "re2015_AprilMAIN_MarDIFF_predictors.rds"))
#saveRDS(ref, paste0(path_rdata_pre, "re2015_AprilMAIN_MarDIFF_7x7px_predictors.rds"))

