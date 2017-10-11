# Set path ---------------------------------------------------------------------
source("F:/Ulli/exploratorien/scripts/00_set_environment.R")


#read data statistics for FFS MODE ----------------------------------------------------------------------
test_pls_ffs  <- readRDS(file=paste0(path_stats,"test_CV9.rds"))
train_pls_ffs <- readRDS(file = paste0(path_stats, "train_CV9.rds"))

test_glm_ffs <-readRDS(file=paste0(path_stats,"test_glm_optimized.rds"))
train_glm_ffs <- readRDS(file = paste0(path_stats, "train_glm_optimized.rds"))

test_rf_ffs <-readRDS(file=paste0(path_stats,"test_rf_optimized.rds"))
train_gam_ffs <- readRDS(file = paste0(path_stats, "train_gam_optimized.rds"))

cbPalette <- c("#fbb4ae", "#80b1d3", "#b3de69")
########## check these coolrs cbPalette <- c("#8da0cb", "#fc8d62", "#66c2a5")
cbPalette<-c("#ffffff", "#525252", "#bdbdbd") #bnw palette

mr_all <- c("SPECRICH", "SHANNON", "EVENESS", "cover_shrubs_pc", "cover_herbs_pc", "number_vascular_plants", "biomass_g","SPEC_glb", "G_std_glb","F_std_glb","M_std_glb")
mr_all<-"SPECRICH"

################# rf look at the traintest data (in a boxplot) and compare with the actually predicted data
############## check these coolrs cbPalette <- c("#8da0cb", "#fc8d62", "#66c2a5")
# PLS_FFS look at the traintest data (in a boxplot) and compare with the actually predicted data
pls_ffs<-ggplot(data = train_pls_ffs[train_pls_ffs$response %in% mr_all,], 
                aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = test_pls_ffs[test_pls_ffs$model_response %in% mr_all,], 
             aes(x = model_response, y = r_squared, color=be),  shape=21,color = "black", stroke=1, size=2) +
  labs(title = "PLS_FFS R squared ", 
       x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()


############### rf look at the traintest data (in a boxplot) and compare with the actually predicted data
rf_ffs<-ggplot(data = train_rf_ffs[train_rf_ffs$response %in% mr_all,], aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = test_rf_ffs[test_rf_ffs$model_response %in% mr_all,], 
             aes(x = model_response, y = r_squared, color = be),  shape=21,color = "black", stroke=1, size=2) +
  labs(title = "rf FFS R Squared",
       x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()

############### GAM look at the traintest data (in a boxplot) and compare with the actually predicted data
gam_ffs<-ggplot(data = train_gam_ffs[train_gam_ffs$response %in% mr_all,], aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = test_gam_ffs[test_gam_ffs$model_response %in% mr_all,], 
             aes(x = model_response, y = r_squared, color = be),  shape=21,color = "black", stroke=1, size=2) +
  labs(title = "gam FFS R Squared",
       x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()

############### GLM look at the traintest data (in a boxplot) and compare with the actually predicted data
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

R_sq<-multiplot(rf_ffs, gam_ffs, glm_ffs, cols=3)

# - RMSE -----------------------------------------------------------------------------------------------------------
# for rmse take the following plot because the units for each response is very different
#PLS
pd = merge(train_pls_ffs[train_pls_ffs$response %in% mr_all,], 
           test_pls_ffs[test_pls_ffs$model_response %in% mr_all,], 
           by.x = c("be", "response"), by.y = c("be", "model_response"))


rmse_pls<-ggplot(data = pd, aes(x = response, y = rmse.x, fill = be)) + 
  geom_boxplot() +
  geom_point(aes(x = response, y = rmse.y, color=be),  shape=21,color = "black", stroke=1, size=2) +
  labs(title = "glm_FFS R squared of the train and test data set compared with the actual prediction (colored dots)", 
       x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw() +
  facet_grid(response ~ ., scales = "free")

#RF
pd = merge(train_rf_ffs[train_rf_ffs$response %in% mr_all,],
           test_rf_ffs[test_rf_ffs$model_response %in% mr_all,],
           by.x = c("be", "response"), by.y = c("be", "model_response"))

rmse_rf<-ggplot(data = pd[pd$response %in% mr_all,], aes(x = response, y = rmse.x, fill = be)) +
  facet_grid(response ~ ., scales = "free")+
  geom_boxplot() +
  geom_point (aes(x = response , y = rmse.y, color=be),  shape=21,color = "black", stroke=1, size=2) +
  labs(title = "RF_FFS RMSE 24 predictors",
       x = NULL, y = "RMSE", fill = "Expl") +
  guides(color=FALSE) +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw() +
  facet_grid(response ~ ., scales = "free")

#GAM
pd = merge(train_gam_ffs[train_gam_ffs$response %in% mr_all,],
           test_gam_ffs[test_gam_ffs$model_response %in% mr_all,],
           by.x = c("be", "response"), by.y = c("be", "model_response"))

rmse_gam<-ggplot(data = pd[pd$response %in% mr_all,], aes(x = response, y = rmse.x, fill = be)) +
  facet_grid(response ~ ., scales = "free")+
  geom_boxplot() +
  geom_point (aes(x = response , y = rmse.y, color=be),  shape=21,color = "black", stroke=1, size=2) +
  labs(title = "GAM_FFS RMSE 24 predictors",
       x = NULL, y = "RMSE", fill = "Expl") +
  guides(color=FALSE) +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw() +
  facet_grid(response ~ ., scales = "free")

#GLM
pd = merge(train_glm_ffs[train_glm_ffs$response %in% mr_all,],
           test_glm_ffs[test_glm_ffs$model_response %in% mr_all,],
           by.x = c("be", "response"), by.y = c("be", "model_response"))

rmse_glm<-ggplot(data = pd[pd$response %in% mr_all,], aes(x = response, y = rmse.x, fill = be)) +
  facet_grid(response ~ ., scales = "free")+
  geom_boxplot() +
  geom_point (aes(x = response , y = rmse.y, color=be),  shape=21,color = "black", stroke=1, size=2) +
  labs(title = "GLM_FFS RMSE 24 predictors",
       x = NULL, y = "RMSE", fill = "Expl") +
  guides(color=FALSE) +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw() +
  facet_grid(response ~ ., scales = "free")

rmse<- multiplot(rmse_gam,rmse_glm,rmse_rf)

all<- multiplot(rf_ffs, gam_ffs, glm_ffs,rmse_rf,rmse_gam,rmse_glm, cols=3, rows=1 )  
                  rmse
