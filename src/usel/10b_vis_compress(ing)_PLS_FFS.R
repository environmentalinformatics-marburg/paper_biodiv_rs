# This is for pls optimzed preds models. please note that the predictors in meta
# may not match the predictors actually used in models because they are unique for each response
# but than had been merged together as one result

# Set path ---------------------------------------------------------------------
source("F:/Ulli/exploratorien/scripts/00_set_environment.R")
library(ggplot2)
source("http://peterhaschke.com/Code/multiplot.R")
cbPalette <- c("#fbb4ae", "#80b1d3", "#b3de69")
########## check these coolrs cbPalette <- c("#8da0cb", "#fc8d62", "#66c2a5")
cbPalette<-c("#ffffff", "#525252", "#bdbdbd") #bnw palette

mr_all <- c("SPECRICH", "SHANNON", "EVENESS", "biomass_g","LUI_glb")
mr_all<-"SPECRICH"

# -the pls data  --------------
test_pls_new<-readRDS(paste0(path_stats,"test_pls_optimized.rds"))
train_pls_new<-readRDS(paste0(path_stats,"train_pls_optimized.rds"))
# -------------
# test_SPEC<-readRDS(paste0(path_stats, "test_pls_ffs_SPEC_stats.rds"))
# test_SHAN<-readRDS(paste0(path_stats, "test_pls_ffs_SHAN_stats.rds"))
# test_EV<-readRDS(paste0(path_stats, "test_pls_ffs_EV_stats.rds"))
# test_LUI<-readRDS(paste0(path_stats, "test_pls_ffs_LUI_stats.rds"))
# 
# train_SPEC<-readRDS(paste0(path_stats, "train_pls_ffs_SPEC_stats.rds"))
# train_SHAN<-readRDS(paste0(path_stats, "train_pls_ffs_SHAN_stats.rds"))
# train_EV<-readRDS(paste0(path_stats, "train_pls_ffs_EV_stats.rds"))
# train_LUI<-readRDS(paste0(path_stats, "train_pls_ffs_LUI_stats.rds"))
# 
# 
# test_pls_new<-rbind(test_SPEC,test_SHAN, test_EV,test_LUI)
# train_pls_new<-rbind(train_SPEC,train_SHAN, train_EV,train_LUI)
# saveRDS(train_pls_new,paste0(path_stats, "train_pls_optimized.rds"))
# saveRDS(test_pls_new,paste0(path_stats, "test_pls_optimized.rds"))
# ----------
pls_NEW<-ggplot(data = train_pls_new[train_pls_new$response %in% mr_all,], 
                aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = test_pls_new[test_pls_new$model_response %in% mr_all,], 
             aes(x = model_response, y = r_squared, color=be),  shape=21,color = "black", stroke=1, size=2) +
  labs(title = "PLS_FFS R squared ", 
       x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()

# rf data ---------------
test_rf_new<-readRDS(paste0(path_stats,"test_rf_optimized.rds"))
train_rf_new<-readRDS(paste0(path_stats,"train_rf_optimized.rds"))
# #read data-------
# test_SPEC<-readRDS(paste0(path_stats, "test_rf_ffs_SPEC_stats.rds"))
# test_SHAN<-readRDS(paste0(path_stats, "test_rf_ffs_SHAN_stats.rds"))
# test_EV<-readRDS(paste0(path_stats, "test_rf_ffs_EV_stats.rds"))
# test_LUI<-readRDS(paste0(path_stats, "test_rf_ffs_LUI_stats.rds"))
# 
# train_SPEC<-readRDS(paste0(path_stats, "train_rf_ffs_SPEC_stats.rds"))
# train_SHAN<-readRDS(paste0(path_stats, "train_rf_ffs_SHAN_stats.rds"))
# train_EV<-readRDS(paste0(path_stats, "train_rf_ffs_EV_stats.rds"))
# train_LUI<-readRDS(paste0(path_stats, "train_rf_ffs_LUI_stats.rds"))
# #merge data
# test_rf_new<-rbind(test_SPEC,test_SHAN, test_EV,test_LUI)
# train_rf_new<-rbind(train_SPEC,train_SHAN, train_EV,train_LUI)
# saveRDS(train_rf_new,paste0(path_stats, "train_rf_optimized.rds"))
# saveRDS(test_rf_new,paste0(path_stats, "test_rf_optimized.rds"))

# visualize data --------
rf<-ggplot(data = train_rf_new[train_rf_new$response %in% mr_all,], 
                aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = test_rf_new[test_rf_new$model_response %in% mr_all,], 
             aes(x = model_response, y = r_squared, color=be),  shape=21,color = "black", stroke=1, size=2) +
  labs(title = "rf_FFS R squared ", 
       x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()

# gam data ---------------
test_gam_new<-readRDS(paste0(path_stats,"test_gam_optimized.rds"))
train_gam_new<-readRDS(paste0(path_stats,"train_gam_optimized.rds"))
# # #read data -------------
# test_SPEC<-readRDS(paste0(path_stats, "test_gam_ffs_SPEC_stats.rds"))
# test_SHAN<-readRDS(paste0(path_stats, "test_gam_ffs_SHAN_stats.rds"))
# #test_EV<-readRDS(paste0(path_stats, "test_gam_ffs_EV_stats.rds"))
# test_LUI<-readRDS(paste0(path_stats, "test_gam_ffs_LUI_stats.rds"))
# 
# train_SPEC<-readRDS(paste0(path_stats, "train_gam_ffs_SPEC_stats.rds"))
# train_SHAN<-readRDS(paste0(path_stats, "train_gam_ffs_SHAN_stats.rds"))
# #train_EV<-readRDS(paste0(path_stats, "train_gam_ffs_EV_stats.rds"))
# train_LUI<-readRDS(paste0(path_stats, "train_gam_ffs_LUI_stats.rds"))
# # #merge data
# test_gam_new<-rbind(test_SPEC,test_SHAN,test_LUI)
# train_gam_new<-rbind(train_SPEC,train_SHAN,train_LUI)
# saveRDS(train_gam_new,paste0(path_stats, "train_gam_optimized.rds"))
# saveRDS(test_gam_new,paste0(path_stats, "test_gam_optimized.rds"))

# visualize data-----
gam<-ggplot(data = train_gam_new[train_gam_new$response %in% mr_all,], 
           aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = test_gam_new[test_gam_new$model_response %in% mr_all,], 
             aes(x = model_response, y = r_squared, color=be),  shape=21,color = "black", stroke=1, size=2) +
  labs(title = "gam_FFS R squared ", 
       x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()

# glm data ---------
# read data
train_glm_ffs<-readRDS(paste0(path_stats, "train_glm_optimized.rds"))
test_glm_ffs<-readRDS(paste0(path_stats, "test_glm_optimized.rds"))
# --------
# test_SPEC<-readRDS(paste0(path_stats, "test_glm_ffs_SPEC_stats.rds"))
# #test_SHAN<-readRDS(paste0(path_stats, "test_glm_ffs_SHAN_stats.rds"))
# #test_EV<-readRDS(paste0(path_stats, "test_glm_ffs_EV_stats.rds"))
# test_LUI<-readRDS(paste0(path_stats, "test_glm_ffs_LUI_stats.rds"))
# 
# train_SPEC<-readRDS(paste0(path_stats, "train_glm_ffs_SPEC_stats.rds"))
# #train_SHAN<-readRDS(paste0(path_stats, "train_glm_ffs_SHAN_stats.rds"))
# #train_EV<-readRDS(paste0(path_stats, "train_glm_ffs_EV_stats.rds"))
# train_LUI<-readRDS(paste0(path_stats, "train_glm_ffs_LUI_stats.rds"))
# 
# #merge data
# test_glm_new<-rbind(test_SPEC,test_LUI)
# train_glm_new<-rbind(train_SPEC,train_LUI)
# saveRDS(train_glm_new,paste0(path_stats, "train_glm_optimized.rds"))
# saveRDS(test_glm_new,paste0(path_stats, "test_glm_optimized.rds"))

# visualize data -----------
glm_ffs<-ggplot(data = train_glm_ffs[train_glm_ffs$response %in% mr_all,], aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = test_glm_ffs[test_glm_ffs$model_response %in% mr_all,], 
             aes(x = model_response, y = r_squared, color = be),  shape=21,color = "black", stroke=1,size=2) +
  labs(title = "glm FFS R Squared",
       x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()


multiplot(rf,pls_NEW,gam, glm_ffs, cols=3, rows=2)

# for rmse take the following plot because the units for each response is very different
pd = merge(train_glm_new[train_glm_new$response %in% mr_all,],
           test_glm_new[test_glm_new$model_response %in% mr_all,],
           by.x = c("be", "response"), by.y = c("be", "model_response"))

rmse_pls<-ggplot(data = pd[pd$response %in% mr_all,], aes(x = response, y = rmse.x, fill = be)) +
  geom_boxplot() +
  geom_point (aes(x = response , y = rmse.y, color=be),  shape=21,color = "black", stroke=1, size=2) +
  labs(title = "PLS_FFS RMSE predictors",
       x = NULL, y = "RMSE", fill = "Expl") +
  guides(color=FALSE) +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw() +
  facet_grid(response ~ ., scales = "free")

rmse_rf<-ggplot(data = pd[pd$response %in% mr_all,], aes(x = response, y = rmse.x, fill = be)) +
  geom_boxplot() +
  geom_point (aes(x = response , y = rmse.y, color=be),  shape=21,color = "black", stroke=1, size=2) +
  labs(title = "RF_FFS RMSE predictors",
       x = NULL, y = "RMSE", fill = "Expl") +
  guides(color=FALSE) +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw() +
  facet_grid(response ~ ., scales = "free")

rmse_gam<-ggplot(data = pd[pd$response %in% mr_all,], aes(x = response, y = rmse.x, fill = be)) +
  geom_boxplot() +
  geom_point (aes(x = response , y = rmse.y, color=be),  shape=21,color = "black", stroke=1, size=2) +
  labs(title = "GAM_FFS RMSE predictors",
       x = NULL, y = "RMSE", fill = "Expl") +
  guides(color=FALSE) +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw() +
  facet_grid(response ~ ., scales = "free")

rmse_glm<-ggplot(data = pd[pd$response %in% mr_all,], aes(x = response, y = rmse.x, fill = be)) +
  geom_boxplot() +
  geom_point (aes(x = response , y = rmse.y, color=be),  shape=21,color = "black", stroke=1, size=2) +
  labs(title = "GLM_FFS RMSE predictors",
       x = NULL, y = "RMSE", fill = "Expl") +
  guides(color=FALSE) +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw() +
  facet_grid(response ~ ., scales = "free")

multiplot(rmse_rf,rmse_pls,rmse_gam,rmse_glm)

multiplot(rf,rmse_rf,pls_NEW,rmse_pls,gam, rmse_gam,glm_ffs,rmse_glm, cols=5,rows=3 )
