library(raster)
source("D:/UNI/Master/MA/exploratorien/scripts/project_biodiv_rs/src/usel/00_a_set_environment_until_gpm_compile.R")

# look at one exploratory first
str_name<-paste0(path_re_cor,"2015-04-24T110941_RE1_1B-NAC_20835999_303429_ortho_corrected_alb.tif")
imported_raster=raster(str_name)
aegrast<-imported_raster

#AEG_complete<-plotRGB(aegrast, scale=1200, stretch="lin")
plot(aegrast)

# then add the polygons of all 50 plots to get an overview/idea of whats to do
poly <- readRDS(file = paste0(path_rdata, "preprocessing/poly.rds"))
plot(poly[[1]], add=T)

# load in the raster data which is snapped for each plot individually- so the polygon data became the raster
re_snip <- readRDS(file = paste0(path_rdata, "preprocessing/re_plots.rds"))

plotRGB(re_snip[[1]][[1]], r=3, g=2, b=1) # schaue dir nur 1 plot an als RGB Bild
plot(re_snip[[1]][[1]]) #alle kanäle einzeln

# read the biodiv. data
vegrel15 <- readRDS(file = paste0(path_rdata, "preprocessing/vegrel15.rds"))
plot(vegrel15$SPECRICH, add=T)

mapview(aegrast)
plotModelCV(x$AEG@model$pls_rfe[[1]][[2]]$model$bestTune)

summary(x$AEG@model[[1]][[1]][[1]])



var_imp <- compVarImp(x$AEG@model$pls_rfe[[1]], scale = FALSE)



var_imp_scale <- compVarImp(x$AEG@model$pls_ffs, scale = TRUE)



var_imp_plot <- plotVarImp(var_imp)



var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")