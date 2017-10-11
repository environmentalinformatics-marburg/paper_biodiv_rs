
# after modeltraining, having the models in differnet dfs
rf<-readRDS(paste0(path_results, "rf_ffs_SPEC.rds"))
glm<-readRDS(paste0(path_results, "glm_ffs_SPEC.rds"))
gam<-readRDS(paste0(path_results, "gam_ffs_SPECRICH.rds"))
pls<-readRDS(paste0(path_results, "pls_ffs_SPEC.rds"))

for(be in names (rf)){
  rf[[be]]@model$glmboost_ffs <- glm[[be]]@model$glmboost_ffs
  rf[[be]]@model$gam_ffs <- gam[[be]]@model$gam_ffs
  rf[[be]]@model$pls_ffs <- pls[[be]]@model$pls_ffs
}
saveRDS(rf,paste0(path_results,"SPEC_4models.rds"))


# after modeltraining, having the exploratories seperately in a df
Ev$AEG@model$rf_ffs<-act_gpm_selected@model$rf_ffs
Ev$HEG@model$rf_ffs<-act_gpm_selected@model$rf_ffs
Ev$SEG@model$rf_ffs<-act_gpm_selected@model$rf_ffs

##### handeling RDATA (temporary files after abroad)

#if you have a "complete" temporary file of 1 response in 1 (!) exploratory
resp2<- load( "tmp/gpm_trainModel_model_instances_002.RData") 
compl[[3]]@model$pls_ffs[[2]]<-model_instances

#if you have a "complete" temporary file of 1 response for ALL exploratories
model_instances<- load( "tmp/gpm_trainModel_model_instances_001.RData")
for (be in names(x){
  x[[be]]@model$pls_ffs[[1]]<-model_instances
})

# if you have the list of 50 (or less) RDATA files to be attached in one exploratory
resp3 <- list.files(tmp, pattern = glob2rx("gpm_trainModel_model_instances_003_*.RData"), 
                    full.names = TRUE)
something<- lapply(seq(resp3), function(s){
  load(resp3[s])
  return(model_instances)
})
compl[[3]]@model$pls_ffs[[3]]<-something


  #tryone<-load("tmp/gpm_trainModel_model_instances_003_001.RData")
  model_instances[1]<-load(resp3[26])
x$SEG@model$pls_ffs[[3]][[26]]<-model_instances


