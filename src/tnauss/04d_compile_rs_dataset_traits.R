# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  source("C:/Users/tnauss/permanent/plygrnd/exploratorien/project_biodiv_rs/src/00_set_environment.R")
} else {
  source("/media/permanent/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
}


# #### Read rapid eye snipets
# s = readRDS(paste0(path_re, "/snipets/indices/re_end04_idx.rds"))
# alb = s[[1]]
# hai = s[[2]]
# sch = s[[3]]
# saveRDS(alb, paste0(path_re, "/snipets/re_aeg_2015-04-24_idx"))
# saveRDS(hai, paste0(path_re, "/snipets/re_hai_2015-04-24_idx"))
# saveRDS(sch, paste0(path_re, "/snipets/re_sch_2015-04-20_idx"))
# 
# 
# 
# s = readRDS(paste0(path_re, "/snipets/indices/re_idx.rds"))
# alb = s[[1]]
# hai = s[[2]]
# sch = s[[3]]
# saveRDS(sch, paste0(path_re, "/snipets/re_sch_2015-04-10_idx"))
# 
# 
# s = readRDS(paste0(path_re, "/snipets/indices/re03_idx.rds"))
# alb = s[[1]]
# hai = s[[2]]
# sch = s[[3]]
# saveRDS(alb, paste0(path_re, "/snipets/re_aeg_2015-03-19_idx"))
# saveRDS(hai, paste0(path_re, "/snipets/re_hai_2015-03-19_idx"))
# saveRDS(sch, paste0(path_re, "/snipets/re_sch_2015-03-18_idx"))
# 
# 
# s = readRDS(paste0(path_re, "/snipets/indices/re04_idx.rds"))
# alb = s[[1]]
# hai = s[[2]]
# sch = s[[3]]
# saveRDS(alb, paste0(path_re, "/snipets/re_aeg_2015-04-09_idx"))
# saveRDS(hai, paste0(path_re, "/snipets/re_hai_2015-04-09_idx"))
# # saveRDS(sch, paste0(path_re, "/snipets/re_sch_2015-04-20_idx"))
# 
# 
# 
# 
# 
# s = readRDS(paste0(path_re, "/snipets/bands/re04_plots.rds"))
# alb = s[[1]]
# hai = s[[2]]
# sch = s[[3]]
# saveRDS(alb, paste0(path_re, "/snipets/re_aeg_2015-04-09_bands"))
# saveRDS(hai, paste0(path_re, "/snipets/re_hai_2015-04-09_bands"))
# saveRDS(sch, paste0(path_re, "/snipets/re_sch_2015-04-15_bands"))
# 
# s = readRDS(paste0(path_re, "/snipets/bands/re_end04_plots.rds"))
# alb = s[[1]]
# hai = s[[2]]
# sch = s[[3]]
# saveRDS(alb, paste0(path_re, "/snipets/re_aeg_2015-04-09_bands"))
# saveRDS(hai, paste0(path_re, "/snipets/re_hai_2015-04-09_bands"))
# saveRDS(sch, paste0(path_re, "/snipets/re_sch_2015-04-15_bands"))




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

