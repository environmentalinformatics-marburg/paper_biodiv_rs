
# download the rPointDB package here: http://137.248.191.215:8081/files/
library(rPointDB)


# connect R to database
fileName <- "~/R/RatserDB_account.txt" # read user account
userpwd <- readChar(fileName, file.info(fileName)$size)

#----------------------
rs <- RemoteSensing$new(url = "http://137.248.191.215:8081",userpwd)


# show all available raster products
rs$rasterdbs

# alb rapideye
db <- rs$rasterdb("be_alb_rapideye_atm")

# we have more than one time available for rapideye data
db$timestamps # timestamp 60598080 2015-03-19 and timestamp 60650589 2015-04-24

# get the regions of interest
#rois <- rs$roi_group("be_alb_roi")
pois<-rs$poi_group("be_alb_poi")
pois<-pois[substr(pois$name, 1,3)=="AEG",] #only grassland plots
pois<- pois[-c(11,31),]

# create extent around poi of 100 meter diameter (ca.70m Kantenlänge)
AEG<-lapply(1:nrow(pois), function(i){
  buffer<-extent_diameter(pois[i,2],pois[i,3],70)
  raster<-db$raster(buffer, timestamp = 60598080)
})

names(AEG)<-paste0(rownames(pois))
  

# hai rapideye
db <- rs$rasterdb("be_hai_rapideye_atm")

# we have more than one time available for rapideye data
db$timestamps # timestamp 60598080 2015-03-19 and timestamp 60650588 2015-04-24

# get the regions of interest
#rois <- rs$roi_group("be_alb_roi")
pois<-rs$poi_group("be_hai_poi")
pois<-pois[substr(pois$name, 1,3)=="HEG",] #only grassland plots

# create extent around poi of 100 meter diameter (ca.70m Kantenlänge)
HEG<-lapply(1:nrow(pois), function(i){
  buffer<-extent_diameter(pois[i,2],pois[i,3],70)
  raster<-db$raster(buffer, timestamp = 60598080)
})
names(HEG)<-paste0(rownames(pois))

# sch rapideye
db <- rs$rasterdb("be_sch_rapideye_atm")

# we have more than one time available for rapideye data
db$timestamps # timestamp 60596640 2015-03-18 and timestamp 60644160 2015-04-20

# get the regions of interest
pois<-rs$poi_group("be_sch_poi")
pois<-pois[substr(pois$name, 1,3)=="SEG",] #only grassland plots

# create extent around poi of 100 meter diameter (ca.70m Kantenlänge)
SEG<-lapply(1:nrow(pois), function(i){
  buffer<-extent_diameter(pois[i,2],pois[i,3],70)
  raster<-db$raster(buffer, timestamp = 60596640)
})
names(SEG)<-paste0(rownames(pois))

march<-list(AEG,HEG,SEG)
names(march) <- c("AEG", "HEG", "SEG") 

#_____________________________________________________________________________________________
##_____________________________________________________________________
#___________________
## APRIL
#___________________

# alb rapideye
db <- rs$rasterdb("be_alb_rapideye_atm")

# we have more than one time available for rapideye data
db$timestamps # timestamp 60598080 2015-03-19 and timestamp 60650589 2015-04-24

# get the regions of interest
#rois <- rs$roi_group("be_alb_roi")
pois<-rs$poi_group("be_alb_poi")
pois<-pois[substr(pois$name, 1,3)=="AEG",] #only grassland plots
pois<- pois[-c(11,31),]

# create extent around poi of 100 meter diameter (ca.70m Kantenlänge)
AEG<-lapply(1:nrow(pois), function(i){
  buffer<-extent_diameter(pois[i,2],pois[i,3],70)
  raster<-db$raster(buffer, timestamp = 60650589)
})

names(AEG)<-paste0(rownames(pois))


# hai rapideye
db <- rs$rasterdb("be_hai_rapideye_atm")

# we have more than one time available for rapideye data
db$timestamps # timestamp 60598080 2015-03-19 and timestamp 60650588 2015-04-24

# get the regions of interest
#rois <- rs$roi_group("be_alb_roi")
pois<-rs$poi_group("be_hai_poi")
pois<-pois[substr(pois$name, 1,3)=="HEG",] #only grassland plots

# create extent around poi of 100 meter diameter (ca.70m Kantenlänge)
HEG<-lapply(1:nrow(pois), function(i){
  buffer<-extent_diameter(pois[i,2],pois[i,3],70)
  raster<-db$raster(buffer, timestamp = 60650588)
})
names(HEG)<-paste0(rownames(pois))

# sch rapideye
db <- rs$rasterdb("be_sch_rapideye_atm")

# we have more than one time available for rapideye data
db$timestamps # timestamp 60596640 2015-03-18 and timestamp 60644160 2015-04-20

# get the regions of interest
pois<-rs$poi_group("be_sch_poi")
pois<-pois[substr(pois$name, 1,3)=="SEG",] #only grassland plots

# create extent around poi of 100 meter diameter (ca.70m Kantenlänge)
SEG<-lapply(1:nrow(pois), function(i){
  buffer<-extent_diameter(pois[i,2],pois[i,3],70)
  raster<-db$raster(buffer, timestamp = 60644160)
})
names(SEG)<-paste0(rownames(pois))

april<-list(AEG,HEG,SEG)
names(april) <- c("AEG", "HEG", "SEG") 

save(april, file=paste0(path_rdata_pre,"RE_2015_April_A24_H24_S20.RData"))

save(march, file=paste0(path_rdata_pre,"RE_2015_March_A19_H19_S18.RData"))
