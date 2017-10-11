.libPaths(c("F:/exploratorien/Documents/R", .libPaths()))


# devtools::install_github("environmentalinformatics-marburg/gpm")
library(gpm)
library(parallel)

tst <- readRDS(paste0(tmp, "gpm_obj_24pred.rds"))
tmp<- "data/rdata/"


cl <- makePSOCKcluster(length(tst))
drs <- .libPaths(); clusterExport(cl, "drs"); jnk <- clusterEvalQ(cl, .libPaths(drs))
clusterExport(cl, "tst"); jnk <- clusterEvalQ(cl, library(gpm))

rf_ffs_24_10 <- parLapply(cl, 1:length(tst), function(ba) {
  act <- trainModel(x = tst[[ba]]
             , n_var = NULL
             , mthd = "rf"
             , mode = "ffs"
             , seed_nbr = 11L
             , cv_nbr = 5L
             , var_selection = "indv"
             , response_nbr = c(2:3,5:6,13:18)
             #, filepath_tmp = tmp
             , runParallel = FALSE)
  saveRDS(act, paste0(tmp, "rf_ffs_24_10_", names(tst)[ba], ".rds"))
  return(act)
})

names(rf_ffs_24_10) <- names(tst)
saveRDS(rf_ffs_24_10, "rf_ffs_24_10.rds")

x<-tst

var_imp <- compVarImp(x[[1]]@model[[1]], scale = FALSE) #[[1]] is the first exploratory and [[2]] the second method(pls or rf)

var_imp_scale <- compVarImp(x[[1]]@model[[1]], scale = TRUE)

var_imp_plot <- plotVarImp(var_imp)

var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")
