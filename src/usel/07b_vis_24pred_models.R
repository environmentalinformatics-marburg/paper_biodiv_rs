# Set path ---------------------------------------------------------------------
source("G:/exploratorien/scripts/00_set_environment.R")

#########Vis Rsq. for the rfe methods of 24 predictors -------------

#read data statistics for RFE MODE
test_rf_rfe <- readRDS(paste0(path_stats,"test_rf_rfe_24_stats.rds"))
train_rf_rfe<- readRDS(paste0(path_stats, "train_rf_rfe_24_stats.rds"))

test_pls_rfe <- readRDS(paste0(path_stats,"test_pls_rfe_24_stats.rds"))
train_pls_rfe<- readRDS(paste0(path_stats, "train_pls_rfe_24_stats.rds"))

# ---------------------------------------------------------------------

test_rf_NO <-readRDS(paste0(path_stats,"test_rf_NOMODE_24_stats.rds"))
train_rf_NO<-readRDS(paste0(path_stats,"train_rf_NOMODE_24_stats.rds"))
  
test_pls_NO <-readRDS(paste0(path_stats,"test_pls_NOMODE_24_stats.rds"))
train_pls_NO<-readRDS(paste0(path_stats,"train_pls_NOMODE_24_stats.rds"))

# ----------------------------------------------------------------------

test_pls_ffs  <- readRDS(file=paste0(path_stats,"test_pls_ffs_24_stats.rds"))
train_pls_ffs <- readRDS(file = paste0(path_stats, "train_pls_ffs_24_stats.rds"))

train_rf_ffs <- readRDS(file = paste0(path_stats, "train_rf_ffs_100.rds"))

cbPalette <- c("#fbb4ae", "#80b1d3", "#b3de69")
########## check these coolrs cbPalette <- c("#8da0cb", "#fc8d62", "#66c2a5")
cbPalette<-c("#ffffff", "#525252", "#bdbdbd") #bnw palette

mr_all <- c("SPECRICH", "SHANNON", "EVENESS", "cover_shrubs_pc", "cover_herbs_pc", "number_vascular_plants", "biomass_g","LUI_glb", "G_std_glb","F_std_glb","M_std_glb")
mr_all<-"SPECRICH"

################# RF look at the traintest data (in a boxplot) and compare with the actually predicted data
############## check these coolrs cbPalette <- c("#8da0cb", "#fc8d62", "#66c2a5")
rf_NO<-ggplot(data = train_rf_NO[train_rf_NO$response %in% mr_all,], aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = test_rf_NO[test_rf_NO$model_response %in% mr_all,], aes(x = model_response, y = r_squared, color = be),  shape=21,color = "black", stroke=1) +
  labs(title = "RF_NOMODE R squared of the train and test data set compared with the actual prediction (colored dots)", 
  		 x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()


# PLS look at the traintest data (in a boxplot) and compare with the actually predicted data
pls_NO<-ggplot(data = train_pls_NO[train_pls_NO$response %in% mr_all,], 
            aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = test_pls_NO[test_pls_NO$model_response %in% mr_all,], 
             aes(x = model_response, y = r_squared, color=be),  shape=21,color = "black", stroke=1) +
  labs(title = "PLS_NOMODE R squared of the train and test data set compared with the actual prediction (colored dots)", 
       x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()

# PLS RFE look at the traintest data (in a boxplot) and compare with the actually predicted data
pls_rfe<-ggplot(data = train_pls_rfe[train_pls_rfe$response %in% mr_all,], 
                aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = test_pls_rfe[test_pls_rfe$be %in% mr_all,], #lass dir nur AEG anzeigen
             aes(x = model_response, y = r_squared, color = be),  shape=21,color = "black", stroke=1) +
  labs(title = "PLS RFE R squared of the train and test data set compared with the actual prediction (colored dots)",
       x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()

# PLS_FFS look at the traintest data (in a boxplot) and compare with the actually predicted data
pls_ffs<-ggplot(data = train_pls_ffs[train_pls_ffs$response %in% mr_all,], 
               aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = test_pls_ffs[test_pls_ffs$model_response %in% mr_all,], 
             aes(x = model_response, y = r_squared, color=be),  shape=21,color = "black", stroke=1) +
  labs(title = "PLS_FFS R squared of the train and test data set compared with the actual prediction (colored dots)", 
       x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_y_continuous(position="left")+
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()

############### RF look at the traintest data (in a boxplot) and compare with the actually predicted data
rf_rfe<-ggplot(data = train_rf_rfe[train_rf_rfe$response %in% mr_all,], aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = test_rf_rfe[test_rf_rfe$model_response %in% mr_all,], 
  					 aes(x = model_response, y = r_squared, color = be),  shape=21,color = "black", stroke=1) +
  labs(title = "RF R squared of the train and test data set compared with the actual prediction (colored dots)",
  		 x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()

multiplot(rf_NO,rf_rfe, rows=2)
multiplot(pls_rfe, pls_NO,pls_ffs, rows=3)


# -----------------------------------------------------------------------------------------------------------------------


## Visualize model prediction preformance ---------------------------------------
########Visulize variable importance
x<-complete

var_imp <- compVarImp(x[[1]]@model$pls_rfe, scale = FALSE)
var_imp_scale <- compVarImp(x[[1]]@model$rf_ffs, scale = TRUE)
var_imp_plot <- plotVarImp(var_imp)
var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")

tstat <- compRegrTests(x[[2]]@model$rf_rfe)
tstat<-aggregate(tstat$r_squared, by = list(tstat$model_response), mean)

plotModelCV(x[[1]]@model$rf_rfe[[1]][[2]]$model)

#########Vis Rsq. for the rfe methods of 24 predictors -------------

#read data statistics
stats_rf_rfe24<-readRDS(paste0(path_results,"rf_rfe_24_stats.rds"))
stats_mod_rf_rfe24 <- readRDS(file = paste0(path_results, "tmp_mod_stats_rf_rfe.rds"))

stats_pls_rfe24<-readRDS(file=paste0(path_results,"pls_rfe_24_stats.rds"))
stats_mod_pls_rfe24 <- readRDS(file = paste0(path_results, "tmp_mod_stats_pls_rfe.rds"))

stats_pls_ffs24<-readRDS(file=paste0(path_results,"pls_ffs_24_stats.rds"))
stats_mod_pls_ffs24 <- readRDS(file = paste0(path_results, "mod_stats_pls_ffs.rds"))



# look at the statsitsics seperately ---------
pls_train<-ggplot(data = stats_mod_pls_rfe24[stats_mod_pls_rfe24$response%in% mr_all,], aes(x = response, y = r_squared, color = be, size=3))+labs(title ="pls_rfe_24_10_test_set") + 
  geom_point()+labs(title="pls_traintest_data")
pls_test<-ggplot(data = stats_pls_rfe24[stats_pls_rfe24$model_response %in% mr_all,], aes(x = model_response, y = r_squared, color = be, size= 3))+ 
  geom_point() +labs(title="pls_rfe_24_10_predicted_set") 
# for rf
rf_train<-ggplot(data = stats_mod_rf_rfe24[stats_mod_rf_rfe24$response %in% mr_all,], aes(x = response, y = r_squared, color = be, size= 3))+ 
  geom_point() +labs(title="rf_rfe_24_10_traintest_set") 
#theme(axis.text.x = element_text(angle = 45, hjust = 1))
train_rf<-ggplot(data = stats_rf_rfe24[stats_rf_rfe24$model_response %in% mr_all,], aes(x = model_response, y = r_squared, color = be, size= 3))+ 
  geom_point() +labs(title="rf_rfe_24_10_predicted_set") 
#theme(axis.text.x = element_text(angle = 45, hjust = 1))

multiplot(test_pls,train_pls,test_rf,train_rf,cols=2)

# RF look at the traintest data (in a boxplot) and compare with the actually predicted data
# check these coolrs cbPalette <- c("#8da0cb", "#fc8d62", "#66c2a5")
rf<-ggplot(data = stats_mod_rf_rfe24[stats_mod_rf_rfe24$response %in% mr_all,], aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = stats_rf_rfe24[stats_rf_rfe24$model_response %in% mr_all,], aes(x = model_response, y = r_squared, color = be)) +
  labs(title = "RF R squared of the train and test data set compared with the actual prediction (colored dots)", x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()
# PLS look at the traintest data (in a boxplot) and compare with the actually predicted data
cbPalette <- c("#fbb4ae", "#80b1d3", "#b3de69")
pls<-ggplot(data = stats_mod_pls_rfe24[stats_mod_pls_rfe24$response %in% mr_all,], aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = stats_pls_rfe24[stats_pls_rfe24$model_response %in% mr_all,], aes(x = model_response, y = r_squared, color = be)) +
  labs(title = "PLS R squared of the train and test data set compared with the actual prediction (colored dots)", x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()

multiplot(rf,pls, cols=2)


# any kind of combined visualisation --------------
# stats <- rbind(stats_rf_rfe, stats_pls_rfe)
# stats_mod <- rbind(stats_mod_rf_rfe, stats_mod_pls_rfe)
# # Combined methods
# mr_grassland <- c("SPECRICH", "cover_shrubs_pc", "cover_herbs_pc", "biomass_g", "LUI_reg")
# mr_comb <- c(mr_grassland, mr_forest)
# cbPalette <- c('#d53e4f','#fc8d59','#fee08b','#e6f598','#99d594','#3288bd')
# ggplot(data = stats_mod[stats_mod$response %in% mr_comb,], aes(x = response, y = r_squared, fill = be)) + 
#   geom_boxplot() +
#   geom_point(data = stats[stats$model_response %in% mr_comb,], aes(x = model_response, y = r_squared, color = be)) +
#   labs(title = "Single scene (04/2015) prediction of some grassland and forest attributes (Core 5, Core 6)", x = NULL, y = "R squared", fill = "Expl") + 
#   guides(color=FALSE) + 
#   scale_fill_manual(values=cbPalette) +
#   scale_colour_manual(values=cbPalette) +
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

