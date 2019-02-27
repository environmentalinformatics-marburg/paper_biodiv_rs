
source("C:/Users/uselig/Documents/GitHub/project_biodiv_rs/src/00_paths_libraries.R")
library(doParallel)
library(gpm)
library(foreach)

mods<-list.files(paste0(path_rdata,"gpm_models_paper/"), full.names=TRUE,pattern=glob2rx("*Model*"))
#set variable for the specific mod you want the statistic
x<-1
veg_model<-readRDS(mods[x])

for(be in names(veg_model)){
  cl <- makeCluster(3)
  registerDoParallel(cl)
  
  act_gpm_selected <- veg_model[[be]]
  act_gpm_selected <- trainModel(x = act_gpm_selected,
                                 n_var = NULL,             
                                 mthd = "pls", #rf, gam, glmboost
                                 mode = "ffs",
                                 seed_nbr = 11, 
                                 cv_nbr = 5,
                                 var_selection = "indv",
                                 response_nbr= c(1:3,5,12,14),
                                 filepath_tmp = path_temp)
  veg_model[[be]] <- act_gpm_selected
  
  stopCluster(cl)
}

Model_0_RE_pls_ffs
Model_3_RELUI_pls_ffs
Model_4_CLIMALUI_pls_ffs
Model_5_RECLIMALUI_pls_ffs


saveRDS(veg_model, paste0(path_results,"Model_4_CLIMALUI_pls_ffs.rds"))

