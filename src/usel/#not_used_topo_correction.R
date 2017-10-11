####topographic correction------------------------------------------------------
###first: adjust the geometry of ASTER DEM to RE
#read raster and dem
# SEG dem hat flasche projektion- mus snochmal korrgiert werden vor der geometr.korrektur!
# SEG_raster<-raster(paste0(path_re_cor, "atmcor_2015-04-10T111339_RE1_1B-NAC_20835979_303426_ortho_sch.tif"))
# SEG_DEM<-raster("data/atmoscorr/arcsidata/Raw/2015-04-10T111339_RE1_1B-NAC_20835979_303426_ortho_sch/2015-04-10T111339_RE1_1B-NAC_20835979_303426_dem_sch.tif")

#read DEm and corrected RE_tif
AEG_raster<-re[[1]]
AEG_DEM<-raster(paste0(path_re,"data/RE2015/2015-04-24T110941_RE1_1B-NAC_20835999_303429_dem_alb.tif"))
#project the RE geometry to the aster dem
RE_DEM_AEG<-projectRaster(AEG_DEM, AEG_raster, method="bilinear")


# get slope and aspect through R with terrain (cince slope&aspekt dont have same dim as DEM)
AEG_slasp<-terrain(RE_DEM_AEG, opt=c('slope','aspect'),unit='radians')
# get hillshade because we need this for topo.correction
AEG_hillshade<-hillShade(slope=AEG_slasp[[1]],aspect=AEG_slasp[[2]], angle=54.49211, direction=175.8216, normalize=F)
# topographic correction and save the output, since the procedure takes a while
rm(list = "AEG_DEM", "RE_DEM_AEG","AEG_slasp")
AEG_topo<- calcTopoCorr(AEG_raster,hillsh=AEG_hillshade, cloudmask= NULL)
saveRDS(AEG_topo, file = paste0(path_rdata, "AEG_final_topo_corr.rds"))

#this should work for all exploratories
scene<-raster(re[[2]]) #call the requiered listnumber from your dataset
HEG_DEM<-raster("data/atmoscorr/arcsidata/Raw/2015-04-24T110857_RE1_1B-NAC_20835994_303428_ortho_hai/2015-04-24T110857_RE1_1B-NAC_20835994_303428_dem_hai.tif")
dem<-HEG_DEM
if(compute) {
  RE_DEM<- projectRaster(dem,scene,method="bilinear")
  slasp<-terrain(RE_DEM, opt=c('slope','aspect'),unit='radians')
  hillshade<- hillShade(slope=slasp[[1]],aspect=slasp[[2]], angle=51.85199, direction=177.336, normalize=F) #customize angle and direction
  rm(list = "dem", "RE_DEM","slasp")
  saveRDS(hillshade, file=paste0(path_rdata,"HEG_hillshade.rds")) #give the requiered name in front, so you dont overwrite the rds in the next round!
} else {
  HEG_hillshade<-readRDS(file=paste0(path_rdata,"HEG_hillshade.rds")) #requiered name in front of the df so you can work with it in R
}

####try writing a function for the topographic correction-------------
#read DEM of the desired region
DEM_alb<-DEM
DEM_hai<-DEM
DEM_sch<-DEM


#write function
TOPO<-function(DEM, angle, direction, re){
  slsp<-terrain(DEM, opt=c('slope','angle'), unit='radians')
  hillshade<-hillShade(slope=slsp[[1]],aspect[[2]], angle, direction, normalize=F)
  rm(list="DEM","slsp")
  correction<-calcTopoCorr(re[[i]], hillshade , cloudmask= NULL)
  
  return(correction)
}

SEG_topo<-if(compute){
  TOPO(angle=45.1044, direction=182.969, re=re[3] )
  saveRDS(SEG_topo, file = paste0(path_rdata, "SEG_topo_corr.rds"))
} else {readRDS(file=paste0(path_rdata,"SEG_topo_corr.rds"))}

HEG_topo<-if(compute){
  TOPO(angle=51.8520, direction=177.336, re=re[2])
  saveRDS(SEG_topo, file = paste0(path_rdata, "HEG_topo_corr.rds"))
} else {readRDS(file=paste0(path_rdata,"HEG_topo_corr.rds"))}


AEG_topo<-if(compute){
  TOPO(angle=54.4921, direction=175.822, re=re[1])
  saveRDS(SEG_topo, file = paste0(path_rdata, "AEG_topo_corr.rds"))
} else {readRDS(file=paste0(path_rdata,"AEG_topo_corr.rds"))}


# topographic correction and save the output, since the procedure takes a while
HEG_topo<- calcTopoCorr(re[[2]],hillsh=HEG_hillshade, cloudmask= NULL) #adjust the name of your scene here and above!
saveRDS(HEG_topo, file = paste0(path_rdata, "HEG_final_topo_corr.rds"))

SEG_hillshade<-hillShade(slope=slasp[[1]],aspect=slasp[[2]], angle=45.1044, direction=182.969, normalize=F)
HEG_hillshade<-hillShade(slope=slasp[[1]],aspect=slasp[[2]], angle=51.8520, direction=177.336, normalize=F)
AEG_hillshade<-hillShade(slope=slasp[[1]],aspect=slasp[[2]], angle=54.4921, direction=175.822, normalize=F)
