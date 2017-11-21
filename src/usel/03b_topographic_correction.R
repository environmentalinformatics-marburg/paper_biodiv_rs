# Set path ---------------------------------------------------------------------
rm(list=ls()) 
source("F:/exploratorien/scripts/00_set_environment.R")
source("D:/UNI/Master/MA/exploratorien/scripts/project_biodiv_rs/src/usel/00_set_environment.R")
#setwd<-"D:/UNI/Master/MA/exploratorien/data/"
setwd<-"F:/exploratorien/data/"
compute = T

####topographic correction------------------------------------------------------
{#no function! this should work for all exploratories with the replaced name-files!
scene<-stack(re[[2]]) #call the requiered listnumber from your dataset
HEG_DEM<-raster(paste0(path_re,"data/RE2015/2015-04-24T110857_RE1_1B-NAC_20835994_303428_dem_hai.tif")) #read the DEM 
dem<-HEG_DEM
if(compute) {
  RE_DEM<- projectRaster(dem,scene,method="bilinear") #probably not necassary anymore after new atm.corr.
  
  # get slope and aspect through R with terrain (since slope&aspekt dont have same dim as DEM)
  slasp<-terrain(RE_DEM, opt=c('slope','aspect'),unit='radians')
  
  # get hillshade because we need this for topo.correction
  hillshade<- hillShade(slope=slasp[[1]],aspect=slasp[[2]], angle=51.85199, direction=177.336, normalize=F) #customize angle and direction
  # remove files no longer needed and save hillshade, since the procedure takes a while
  rm(list = "dem", "RE_DEM","slasp")
  saveRDS(hillshade, file=paste0(path_rdata,"HEG_hillshade.rds")) #give the requiered name in front, so you dont overwrite the rds in the next round!
} else {
  HEG_hillshade<-readRDS(file=paste0(path_rdata,"HEG_hillshade.rds")) #requiered name in front of the df so you can work with it in R
}
# topographic correction and save the output, since the procedure takes a while
HEG_topo<- calcTopoCorr(scene,hillsh=HEG_hillshade, cloudmask= NULL)
saveRDS(HEG_topo, file = paste0(path_rdata, "HEG_final_topo_corr.rds"))

####info for values for the differnet exploratories-----------------------------
# SEG_hillshade<-hillShade(slope=slasp[[1]],aspect=slasp[[2]], angle=45.1044, direction=182.969, normalize=F)
# HEG_hillshade<-hillShade(slope=slasp[[1]],aspect=slasp[[2]], angle=51.8520, direction=177.336, normalize=F)
# AEG_hillshade<-hillShade(slope=slasp[[1]],aspect=slasp[[2]], angle=54.4921, direction=175.822, normalize=F)
}

compute=F

#read atmospheric corrected RE data
if(compute){
  fns <- c("atmoc_2015-04-24T110941_RE1_1B-NAC_20835999_303429_ortho_alb.tif",
           "atmoc_2015-04-24T110857_RE1_1B-NAC_20835994_303428_ortho_hai.tif",
           "atmoc_2015-04-10T111339_RE1_1B-NAC_20835979_303426_ortho_sch.tif")
  rel <- lapply(fns, function(f){
    rel <- stack(paste0(path_re_cor, f))
    names(rel) <- paste0("RE201504_", seq(5)) #in den names stehen die 5 b?nder mit RE...1,RE...2 usw.seq. zur Wdh. f?r alle b?nder
    return(rel)
  })
  names(rel) <- c("AEG", "HEG", "SEG") #changed from Alb,Sch,Hai to this- matching to poly
}

#read DEM- make a list with all DEM's
if(compute){
  dem <- c("2015-04-24T110941_RE1_1B-NAC_20835999_303429_dem_alb.tif",
           "2015-04-24T110857_RE1_1B-NAC_20835994_303428_dem_hai.tif",
           "2015-04-10T111339_RE1_1B-NAC_20835979_303426_dem_sch.tif")
  DEM <- lapply(dem, function(f){
    DEM <- stack(paste0(path_re, f))
    return(DEM)
  })
  names(DEM) <- c("AEG_DEM", "HEG_DEM", "SEG_DEM")
}

#### function for the topographic correction-------------
TOPO<-function(DEM, angle, direction, rel){
  require(raster)
  DEM<-resample(DEM,rel)
  slsp<-terrain(DEM, opt=c('slope','aspect'), unit='radians') # get slope and aspect with terrain
  hillshade<-hillShade(slope=slsp[[1]],aspect=slsp[[2]], angle, direction, normalize=F) # get hillshade because we need this for topo.correction
  rm(list="DEM","slsp") #delete not needed df because of risk of full environment
  correction<-calcTopoCorr(rel, hillshade , cloudmask= NULL) # topographic correction using the atmospheric corrected .tif and save the output, since the procedure takes a while
  
  return(correction)
}

# Funktionsaufruf für die Exploratorien (unterschiedliche Winkel, elevation)
AEG_topo<-if(compute){
 AEG_topo<- TOPO(DEM[[1]], angle=54.4921, direction=175.822, rel=rel[[1]])
  saveRDS(AEG_topo, file = paste0(path_rdata, "AEG_topo_corr.rds"))
  writeRaster(AEG_topo, filename = paste0(path_re_cor, "2015-04-24T110941_RE1_1B-NAC_20835999_303429_ortho_corrected_alb.tif"),
              format = "GTiff")
} else {
  AEG_topo<-readRDS(file=paste0(path_rdata,"AEG_topo_corr.rds"))}

HEG_topo<-if(compute){
  HEG_topo<- TOPO(DEM=DEM[[2]], angle=51.8520, direction=177.336, rel=rel[[2]])
  saveRDS(HEG_topo, file = paste0(path_rdata, "HEG_topo_corr.rds"))
  writeRaster(HEG_topo, filename = paste0(path_re_cor, "2015-04-24T110857_RE1_1B-NAC_20835994_303428_ortho_corrected_hai.tif"),
                             format = "GTiff")
} else {
  HEG_topo<-readRDS(file=paste0(path_rdata,"HEG_topo_corr.rds"))}

SEG_topo<-if(compute){
  SEG_topo<- TOPO(DEM=DEM[[3]],angle=45.1044, direction=182.969, rel=rel[[3]] )
  saveRDS(SEG_topo, file = paste0(path_rdata, "SEG_topo_corr.rds"))
   writeRaster(SEG_topo[[1:2]],"D:/exploratorien/data/RE2015_atmocor/bnd1to2_2015-04-10T111339_RE1_1B-NAC_20835979_303426_ortho_corrected_sch.tif") #file is to big- export to gtif splits in 1:2, 3 and 4:5
   writeRaster(SEG_topo[[3]],"D:/exploratorien/data/RE2015_atmocor/bnd3_2015-04-10T111339_RE1_1B-NAC_20835979_303426_ortho_corrected_sch.tif")
   writeRaster(SEG_topo[[4:5]],"D:/exploratorien/data/RE2015_atmocor/bnd4to5_2015-04-10T111339_RE1_1B-NAC_20835979_303426_ortho_corrected_sch.tif")
   } else {
  SEG_topo<-readRDS(file=paste0(path_rdata,"SEG_topo_corr.rds"))}