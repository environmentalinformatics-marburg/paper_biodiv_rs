# Set path ---------------------------------------------------------------------

#filepath_base<-"F:/exploratorien/"
filepath_base<-"C:/exploratorien/exploratorien/"
path_data <- paste0(filepath_base, "data/")
path_releves <- paste0(path_data, "releves/")
path_rdata <- paste0(path_data, "rdata/")
path_rdata_pre <- paste0(path_rdata,"preprocessing/")
path_temp <- paste0(path_data, "temp/")
path_lui <- paste0(path_data, "lui/")

# Set libraries ----------------------------------------------------------------
if (!require(raster)){install.packages("raster")}
#library(vegan)
library(biodivTools) #if (!require(vegan)){install.packages("vegan")} devtools::install_github("environmentalinformatics-marburg/biodivTools")
library(doParallel)
library(grid)
library(gridExtra)
library(gpm) # devtools::install_github("environmentalinformatics-marburg/gpm")
library(lavaan)
library(rgeos)
# library(ggplot2)
library(mapview)
library(metTools)  # devtools::install_github("environmentalinformatics-marburg/metTools")
library(raster)
library(rgdal)
library(satellite)
library(satelliteTools)  #if (!require(RStoolbox)){install.packages("RStoolbox")}, if (!require(Rtools)){install.packages("Rtools")} , if (!require(glcm)){install.packages("glcm")} , devtools::install_github("environmentalinformatics-marburg/satelliteTools")
#library(semPlot)
library(sp)
library(RStoolbox)


# Other settings ---------------------------------------------------------------
rasterOptions(tmpdir = path_temp)

saga_cmd <- "D:/UNI/Master/Portable_SAGA/saga_cmd.exe "
initOTB("D:/UNI/Master/MA/OTB-6.2.0-win64/OTB-6.2.0-win64/bin/")

#für Lab PC
#initOTB("C:/Users/seligu/Downloads/OTB-6.0.0-win64/bin/")
