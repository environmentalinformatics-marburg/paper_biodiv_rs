
source("C:/Users/uselig/Documents/GitHub/project_biodiv_rs/src/00_paths_libraries.R")
library(doParallel)
library(gpm)
library(foreach)

model_1a<-readRDS(paste0(path_rdata,"gpm_models_paper/RE_Model_1a.rds"))

cl<-makeCluster(3)
registerDoParallel(cl)
# 
# r<- foreach(be=1:length(model_1a)) %dopar% {
#   act_gpm <- model_1a[[be]]
#   act_gpm <- trainModel(x = act_gpm_selected,
#                         n_var = NULL,             
#                         mthd = "pls", #rf, gam, glmboost
#                         mode = "ffs",
#                         seed_nbr = 11, 
#                         cv_nbr = 5,
#                         var_selection = "indv",
#                         response_nbr = 1,
#                         filepath_tmp = path_temp)
#   model_1a[[be]] <- act_gpm
# }
# stopCluster(cl)

veg_re_g_gpm_indv<- model_1a
for(be in names(veg_re_g_gpm_indv)){
  act_gpm_selected <- veg_re_g_gpm_indv[[be]]
  act_gpm_selected <- trainModel(x = act_gpm_selected,
                                 n_var = NULL,             
                                 mthd = "pls", #rf, gam, glmboost
                                 mode = "ffs",
                                 seed_nbr = 11, 
                                 cv_nbr = 5,
                                 var_selection = "indv",
                                 response_nbr = 1,
                                 filepath_tmp = path_temp)
  veg_re_g_gpm_indv[[be]] <- act_gpm_selected
}

saveRDS(veg_re_g_gpm_indv, "9CV_pls_model_7pred.rds")