# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  source("D:/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
} else {
  source("/media/permanent/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
}

compute = TRUE

# Read and pre-process meteorologica data --------------------------------------
meta <- readMetData(filepath = paste0(path_met_a, "plots.csv"))
meta <- meta[meta$g_a > 2009, ]
saveRDS(meta, paste0(path_rdata, "meta.rds"))

