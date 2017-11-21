library(raster)
source("D:/UNI/Master/MA/exploratorien/scripts/project_biodiv_rs/src/usel/00_a_set_environment_until_gpm_compile.R")


str_name<-paste0(path_re_cor,"2015-04-24T110941_RE1_1B-NAC_20835999_303429_ortho_corrected_alb.tif")
imported_raster=raster(str_name)
aegrast<-imported_raster

# aegrast<-raster(paste0(path_re_cor,"2015-04-24T110941_RE1_1B-NAC_20835999_303429_ortho_corrected_alb.tif"))
# AEG_complete<-plotRGB(aegrast, scale=1200, stretch="lin")
plot(aegrast)
# mapview(aegrast)
plotModelCV(x$AEG@model$rf_ffs[[1]][[2]]$model)

#the polygons of the exploratories
poly <- readRDS(file = paste0(path_rdata, "preprocessing/poly.rds"))
plot(poly$Alb, add=T)

# load in the snips auf one exploratory
re_snip <- readRDS(file = paste0(path_rdata, "preprocessing/re_plots.rds"))
plot(re_snip[[1]][[1]]) # a raster-polygon snip

#load in the biodiv. data
vegrel15 <- readRDS(file = paste0(path_rdata, "preprocessing/vegrel15.rds"))

