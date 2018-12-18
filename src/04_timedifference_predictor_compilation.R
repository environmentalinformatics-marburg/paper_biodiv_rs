load("the final RData file derived from script 02_predictor compilation")

# delta NDVI und delta RE_NDVI

#prepare timestamp difference between march and april

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