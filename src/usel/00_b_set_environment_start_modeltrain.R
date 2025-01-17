# Set path ---------------------------------------------------------------------

#filepath_base <-"C:/Users/seligu/exploratorien/"
#filepath_base <- "K:/exploratorien/"
#filepath_base<-"F:/exploratorien/"
filepath_base<-"D:/UNI/Master/MA/exploratorien/"
path_data <- paste0(filepath_base, "data/")
# path_lui <- paste0(path_data, "lui/")
# path_plots <- paste0(path_data, "plots/plots/")
# path_releves <- paste0(path_data, "releves/")
# path_re <- paste0(path_data, "RE2015/")
# path_re_cor<-paste0(path_data, "RE2015_atmocor/")
path_rdata <- paste0(path_data, "rdata/")
# path_met_a <- paste0(path_data, "met_a/")
# path_met_m <- paste0(path_data, "met_m/")
path_temp <- paste0(path_data, "temp/")
path_output <- paste0(path_data, "output/")
path_results<- paste0(path_data, "results/")
path_stats<-paste0(path_data, "stats/")
path_jpg<-paste0(path_data, "jpgs/")


# Set libraries ----------------------------------------------------------------
 #if (!require(vegan)){install.packages("vegan")} devtools::install_github("environmentalinformatics-marburg/biodivTools")
library(doParallel)

library(gpm) # devtools::install_github("environmentalinformatics-marburg/gpm")

library(ggplot2)

library(data.table)
library(reshape2)
library(foreach)
library(wesanderson)
library(lattice)
library(latticeExtra)
