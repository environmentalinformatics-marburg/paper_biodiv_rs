# Set path ---------------------------------------------------------------------
source("F:/Ulli/exploratorien/scripts/00_set_environment.R")



#########Vis Rsq. for the rfe methods of 24 predictors -------------
#source("http://peterhaschke.com/Code/multiplot.R")


#read data statistics for FFS MODE ----------------------------------------------------------------------
test_rf_ffs  <- readRDS(file=paste0(path_stats, "test_rf_ffs_LUI_stats.rds"))
train_rf_ffs <- readRDS(file = paste0(path_stats, "train_rf_ffs_LUI_stats.rds"))

test_pls_ffs  <- readRDS(file=paste0(path_stats,"test_pls_ffs_LUI_stats.rds"))
train_pls_ffs <- readRDS(file = paste0(path_stats, "train_pls_ffs_LUI_stats.rds"))

test_gam_ffs  <- readRDS(file=paste0(path_stats,"test_gam_ffs_bio_stats.rds"))
train_gam_ffs <- readRDS(file = paste0(path_stats, "train_gam_ffs_bio_stats.rds"))

test_glm_ffs  <- readRDS(file=paste0(path_stats,"test_glm_ffs_LUI_stats.rds"))
train_glm_ffs <- readRDS(file = paste0(path_stats, "train_glm_ffs_LUI_stats.rds"))

cbPalette <- c("#fbb4ae", "#80b1d3", "#b3de69")
########## check these coolrs cbPalette <- c("#8da0cb", "#fc8d62", "#66c2a5")
cbPalette<-c("#ffffff", "#525252", "#bdbdbd") #bnw palette

mr_all <- c("SPECRICH", "SHANNON", "EVENESS", "biomass_g","LUI_glb", "LUI_reg","G_std_glb","F_std_glb","M_std_glb")
mr_all<-"SPECRICH"


# PLS_FFS look at the traintest data (in a boxplot) and compare with the actually predicted data
pls_ffs<-ggplot(data = train_pls_ffs[train_pls_ffs$response %in% mr_all,], 
                aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = test_pls_ffs[test_pls_ffs$model_response %in% mr_all,], 
             aes(x = model_response, y = r_squared, color=be),  shape=21,color = "black", stroke=1) +
  labs(title = "PLS_FFS R squared of the train and test data set compared with the actual prediction (colored dots)", 
       x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()


############### RF look at the traintest data (in a boxplot) and compare with the actually predicted data
rf_ffs<-ggplot(data = train_rf_ffs[train_rf_ffs$response %in% mr_all,], aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = test_rf_ffs[test_rf_ffs$model_response %in% mr_all,], 
             aes(x = model_response, y = r_squared, color = be),  shape=21,color = "black", stroke=1) +
  labs(title = "RF FFS
       s$squared of the train and test data set compared with the actual prediction (colored dots)",
       x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()

gam_ffs<-ggplot(data = train_gam_ffs[train_gam_ffs$response %in% mr_all,], aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = test_gam_ffs[test_gam_ffs$model_response %in% mr_all,], 
             aes(x = model_response, y = r_squared, color = be),  shape=21,color = "black", stroke=1) +
  labs(title = "gam FFS
       s$squared of the train and test data set compared with the actual prediction (colored dots)",
       x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()

glm_ffs<-ggplot(data = train_glm_ffs[train_glm_ffs$response %in% mr_all,], aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = test_glm_ffs[test_glm_ffs$model_response %in% mr_all,], 
             aes(x = model_response, y = r_squared, color = be),  shape=21,color = "black", stroke=1) +
  labs(title = "glm FFS
       s$squared of the train and test data set compared with the actual prediction (colored dots)",
       x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()

multiplot(rf_ffs,glm_ffs,pls_ffs, gam_ffs, cols=4)

# rmsepd

ggplot(data = pd, aes(x = response, y = rmse.x, fill = be)) +
  geom_boxplot() +
  geom_point(aes(x = response, y = rmse.y, color=be),  shape=21,color = "black", stroke=1, size=2) +
  labs(title = "PLS_FFS R squared of the train and test data set compared with the actual prediction (colored dots)",
       x = NULL, y = "R squared", fill = "Expl") +
  guides(color=FALSE) +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw() +
  facet_grid(response ~ ., scales = "free")