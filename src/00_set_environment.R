# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  filepath_base <- "D:/active/exploratorien/"
} else {
  filepath_base <- "/media/permanent/active/exploratorien/"
}

path_data <- paste0(filepath_base, "data/")
path_lui <- paste0(path_data, "lui/")
path_plots <- paste0(path_data, "plots/")
path_releves <- paste0(path_data, "releves/")
path_re <- paste0(path_data, "rapideye/")
path_rdata <- paste0(path_data, "rdata/")
path_temp <- paste0(path_data, "temp/")
path_output <- paste0(path_data, "output/")


# Set libraries ----------------------------------------------------------------
library(rgeos)
library(ggplot2)
library(mapview)
library(raster)
library(rgdal)
library(satellite)
library(satelliteTools)
library(sp)
