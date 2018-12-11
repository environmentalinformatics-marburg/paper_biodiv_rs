# Set path ---------------------------------------------------------------------
source("D:/UNI/Master/MA/exploratorien/scripts/00_set_environment.R")

compute = TRUE

# Read and pre-process meteorologica data --------------------------------------
meta <- readMetData(filepath = paste0(path_met_a, "plots.csv"))
meta <- meta[meta$g_a > 2009, ]
saveRDS(meta, paste0(path_rdata, "meta.rds"))
