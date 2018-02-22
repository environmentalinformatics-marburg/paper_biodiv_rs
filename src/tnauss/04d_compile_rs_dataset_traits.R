# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  source("C:/Users/tnauss/permanent/plygrnd/exploratorien/project_biodiv_rs/src/00_set_environment.R")
} else {
  source("/media/permanent/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
}



#### Read rapid eye data
# Make some 
m = readRDS(paste0(path_rdata, "/re_predictors.rds"))

saveRDS(m[[1]], paste0(path_rdata, "/re_aeg_2015-04-24.rds"))
saveRDS(m[[2]], paste0(path_rdata, "/re_heg_2015-04-24.rds"))
saveRDS(m[[3]], paste0(path_rdata, "/re_seg_2015-04-10.rds"))


m = readRDS(paste0(path_rdata, "/re03_predictors.rds"))

saveRDS(m[substr(m$re03_EP, 1, 3) == "AEG", ], paste0(path_rdata, "/re_aeg_2015-03-19.rds"))
saveRDS(m[substr(m$re03_EP, 1, 3) == "HEG", ], paste0(path_rdata, "/re_heg_2015-03-19.rds"))
saveRDS(m[substr(m$re03_EP, 1, 3) == "SEG", ], paste0(path_rdata, "/re_seg_2015-03-18.rds"))


m = readRDS(paste0(path_rdata, "/re04_predictors.rds"))

saveRDS(m[substr(m$re04_EP, 1, 3) == "AEG", ], paste0(path_rdata, "/re_aeg_2015-04-09.rds"))
saveRDS(m[substr(m$re04_EP, 1, 3) == "HEG", ], paste0(path_rdata, "/re_heg_2015-04-09.rds"))
saveRDS(m[substr(m$re04_EP, 1, 3) == "SEG", ], paste0(path_rdata, "/re_seg_2015-04-20.rds"))

