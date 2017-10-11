source("D:/UNI/Master/MA/exploratorien/scripts/00_set_environment.R")
library(mboost)
library(gpm)
library(gam)
# - Training LUI ----------------------------------------------------------------------------
# training LUI starts with 35 models
LUI <- readRDS(file = paste0(path_rdata, "LUImodel.rds"))

bio<-readRDS(paste0(path_rdata, "biomodel.rds"))
for(be in names(LUI)){
  act_gpm_selected <- LUI[[be]]
  
  act_gpm_selected <- trainModel(x = act_gpm_selected,
                                 n_var = NULL,             
                                 mthd = "pls", #rf, gam, glmboost
                                 mode = "ffs",
                                 seed_nbr = 11, 
                                 cv_nbr = 5,
                                 var_selection = "indv",
                                 response_nbr = 14,
                                 filepath_tmp = path_temp
                                 
  )
  #saveRDS(act_gpm_selected, file = paste0(path_results, "pls_ffs_LUI_", be, ".rds"))
  
  LUI[[be]] <- act_gpm_selected
}
#saveRDS(LUI, file = paste0(path_results, "pls_ffs_LUI.rds"))


# Training biomass ----------------------------------------------------------------
bio <- readRDS(file = paste0(path_rdata, "biomodel.rds"))
for(be in names(bio)){
  act_gpm_selected <- bio[[be]]
  
  act_gpm_selected <- trainModel(x = bio[[2]],
                                 n_var = NULL,             
                                 mthd = "gam", #pls, rf, glmboost
                                 mode = "ffs",
                                 seed_nbr = 11, 
                                 cv_nbr = 5,
                                 var_selection = "indv",
                                 response_nbr = 14,
                                 resample_nbr= c(1:4),
                                 filepath_tmp = path_temp
                                 )
                                 
      bio[[be]] <- act_gpm_selected
}
saveRDS( bio, file = paste0(path_results, "gam_ffs_bio.rds"))

# Training SpecRich mit neuer CV (Split at 5 und dann 9 subsets) --------------------------------------------------------------------
x<-readRDS(paste0(path_results,"COMPLETE24.rds"))
pred<-x[[1]]@meta$input$PREDICTOR_FINAL

specpred<-pred[c(3,4,6,7,9,10,15,16)]
SPECRICH<-x
for(be in names(SPECRICH)){
  SPECRICH[[be]]@meta$input$PREDICTOR_FINAL<-specpred
}
belc <- c("AEG", "HEG", "SEG")
newspec<- lapply(belc, function(be){
  SPE <- splitMultRespLSO(SPECRICH[[be]], nbr = 5)
})
names(newspec)<-belc
SPECRICH<-newspec
#SPECRICH <- readRDS(file = paste0(path_rdata, "SPECmodel.rds"))
for(be in names(SPECRICH)){
  act_gpm_selected <- SPECRICH[[be]]
  
  act_gpm_selected <- trainModel(x = act_gpm_selected,
                                 n_var = NULL,             
                                 mthd = "pls",
                                 mode = "ffs",
                                 seed_nbr = 11, 
                                 cv_nbr = 9,
                                 var_selection = "indv",
                                 response_nbr = c(2,3,22),
                                 filepath_tmp = path_temp
  )
  #saveRDS(act_gpm_selected, file = paste0(path_results, "pls_ffs_SPECRICH_", be, ".rds"))
  
  SPECRICH[[be]] <- act_gpm_selected
}
saveRDS(SPECRICH, file = paste0(path_results, "pls_ffs__9CV_Sh_Ev_LUI.rds"))



# - Shannon Training -----------------------------------------------------------------------------
Shan <- readRDS(file = paste0(path_rdata, "Shanmodel.rds"))
for(be in names(Shan)){
  act_gpm_selected <- Shan[[be]]
  
  act_gpm_selected <- trainModel(x = act_gpm_selected,
                                 n_var = NULL,             
                                 mthd = "gam", #glmboost, pls, rf
                                 mode = "ffs",
                                 seed_nbr = 11, 
                                 cv_nbr = 5,
                                 var_selection = "indv",
                                 response_nbr = 2,
                                 filepath_tmp = path_temp
  )
  saveRDS(act_gpm_selected, file = paste0(path_results, "gam_ffs_Shan_", be, ".rds"))
  
  Shan[[be]] <- act_gpm_selected
}
saveRDS(Shan, file = paste0(path_results, "gam_ffs_Shan.rds"))

