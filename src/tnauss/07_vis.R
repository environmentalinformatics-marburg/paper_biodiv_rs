# Set path ---------------------------------------------------------------------
source("F:/Ulli/exploratorien/scripts/00_set_environment.R")

# Read and pre-process biodiversity data ---------------------------------------
adf_clim <- readRDS(file = paste0(path_rdata, "veg_meta_g.rds"))
adf_clim$g_belc_a <- paste0(adf_clim$g_belc, "_", adf_clim$Year)

adf <- adf_clim
g1 <- ggplot(data = adf, aes(x = LUI_reg, y = SPECRICH, color = EPID)) +
  geom_point() +
  geom_smooth()

g2 <- ggplot(data = adf, aes(x = LUI_glb, y = SPECRICH, color = EPID)) +
  geom_point() +
  geom_smooth()

grid.arrange(g1, g2, ncol = 2)


g1 <- ggplot(data = adf, aes(x = LUI_glb, y = SPECRICH, color = g_belc)) +
  geom_point() +
  geom_smooth()

g2 <- ggplot(data = adf, aes(x = LUI_glb, y = SHANNON, color = g_belc)) +
  geom_point() +
  geom_smooth()

grid.arrange(g1, g2, ncol = 2)


g1 <- ggplot(data = adf, aes(x = Ta_200, y = SHANNON, color = g_belc)) +
  geom_point() +
  geom_smooth(method = "lm")

g2 <- ggplot(data = adf[adf$Year == 2014,], aes(x = SM_10, y = SPECRICH, color = g_belc)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Species richness vs soil moisture", x = "Soil moisture (-10cm)", y = "Species richness", color = "Expl") +
  theme_bw()
lmod <- lm(SPECRICH ~ SM_10 + g_belc, data = adf)
summary(lmod)
lmod <- lm(SPECRICH ~ SM_10, data = adf[adf$g_belc == "HEG" & adf$Year == 2014,])
summary(lmod)
lmod <- lm(SPECRICH ~ SM_10, data = adf[adf$Year == 2014,])
summary(lmod)

grid.arrange(g1, g2, ncol = 2)


g1 <- ggplot(data = adf, aes(x = Ta_200, y = LUI_glb, color = g_belc)) +
  geom_point() +
  geom_smooth(method = "lm")

g2 <- ggplot(data = adf[adf$Year == 2014,], aes(x = SM_10, y = LUI_glb, color = g_belc)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "LUI vs meteorology", x = "Soil moisture (-10cm)", y = "LUI (glb)", color = "Expl") +
  theme_bw()

lmod <- lm(SM_10 ~ LUI_glb + g_belc, data = adf)
summary(lmod)
lmod <- lm(SM_10 ~ LUI_glb, data = adf[adf$g_belc == "AEG",])
summary(lmod)
lmod <- lm(SM_10 ~ LUI_glb, data = adf)
summary(lmod)
plot(lmod)

grid.arrange(g1, g2, ncol = 2)

re_pca_h_HE <- readRDS(paste0(path_rdata, "re_pca_h_HE.rds"))
mapview(re_pca_h_HE[[1]])

# preprocessing visualisations of raster data

# plotting RapidEye data after atmospheric correction -----------------------------------------------------------
###see RGB image for atmos.corrected Alb

###see single bands for atmos.corrected Alb
alb_single<-raster(AEG_raster, layer=4)
alb_cor_sing_plot<-plot(alb_single, 
                        axes=T,
                        main="only first band of corrected image in Alb")


# visualize the atmospheric and final topo.correction of AEG
AEG_topo<-readRDS(paste0(path_rdata, "AEG_topo_corr.rds"))
  #AEG_topo[test_AEG_topo<0]=0
AEG_complete<-plotRGB(AEG_topo,r=3,g=2,b=1, scale=1200, stretch="lin")
AEG_atm_corr<-plotRGB(rel[[1]],r=3,g=2,b=1, scale=1200, stretch="lin")

# visualize the atmospheric and final topo.correction of HEG
HEG_topo<-readRDS(paste0(path_rdata, "HEG_topo_corr.rds"))
  #HEG_topo[test_HEG_topo<0]=0
HEG_complete<-plotRGB(HEG_topo,r=3,g=2,b=1, scale=1200, stretch="lin")
HEG_atm_corr<-plotRGB(rel[[2]],r=3,g=2,b=1, scale=1200, stretch="lin")

# visualize the atmospheric and final topo.correction of SEG
SEG_topo<-readRDS(paste0(path_rdata, "SEG_topo_corr.rds"))
  #SEG_topo[test_SEG_topo<0]=0
SEG_complete<-plotRGB(SEG_topo,r=3,g=2,b=1, scale=1200, stretch="lin")
SEG_atm_corr<-plotRGB(rel[[3]],r=3,g=2,b=1, scale=1200, stretch="lin")



# Visulize variable importance --------------------------------
x<-model24_10

var_imp <- compVarImp(x[[1]]@model$pls_rfe, scale = FALSE)

var_imp_scale <- compVarImp(x@model$pls_rfe, scale = TRUE)

var_imp_plot <- plotVarImp(var_imp)

var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")

tstat <- compRegrTests(x[[2]]@model$rf_rfe)

tstat<-aggregate(tstat$r_squared, by = list(tstat$model_response), mean)

plotModelCV(x[[1]]@model$rf_rfe[[1]][[2]]$model)

st<-compContTests(x$AEG@model$pls_rfe[[1]][[2]]$model, mean = TRUE) #(x$AEG@model$pls_rfe[[1]][[2]]$model)

# the rfe methods of 24 predictors: show R² with the 10 responses in a plot -------------
stats_pls_rfe24<-readRDS(paste0(path_results,"pls_rfe_24_stats.rds"))
stats_rf_rfe24<-readRDS(paste0(path_results,"rf_rfe_24_stats.rds"))

mr_all <- c("SPECRICH", "SHANNON", "EVENESS", "cover_shrubs_pc", "cover_herbs_pc", "biomass_g","LUI_glb", "G_std_glb","F_std_glb","M_std_glb")
#for pls
ggplot(data = stats_pls_rfe24[stats_pls_rfe24$model_response%in% mr_all,], aes(x = model_response, y = r_squared, color = be, size=3)) + 
  geom_point()
# for rf
ggplot(data = stats_rf_rfe24[stats_rf_rfe24$model_response %in% mr_all,], aes(x = model_response, y = r_squared, color = be, size= 3)) + 
  geom_point() #+ 
theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Visualize model prediction preformance ---------------------------------------
g_stats <- readRDS(file = paste0(path_rdata, "veg_re_g_gpm_indv_model_rf_stats.rds"))
g_stats_mod <- readRDS(file = paste0(path_rdata, "veg_re_g_gpm_indv_model_rf_mod_stats.rds"))

f_stats <- readRDS(file = paste0(path_rdata, "veg_re_f_gpm_indv_model_rf_stats.rds"))
f_stats_mod <- readRDS(file = paste0(path_rdata, "veg_re_f_gpm_indv_model_rf_mod_stats.rds"))

c_stats <- readRDS(file = paste0(path_rdata, "veg_meta_g_gpm_indv_model_rf_stats.rds"))
c_stats_mod <- readRDS(file = paste0(path_rdata, "veg_meta_g_gpm_indv_model_rf_stats_mod.rds"))

stats <- rbind(g_stats, f_stats)
stats_mod <- rbind(g_stats_mod, f_stats_mod)

# Grassland examples
cbPalette <- c("#fbb4ae", "#80b1d3", "#b3de69")
mr_grassland <- c("SPECRICH", "SHANNON", "EVENESS", "cover_shrubs_pc", "cover_herbs_pc", "biomass_g", "LUI_reg")
ggplot(data = g_stats_mod[g_stats_mod$response %in% mr_grassland,], aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = g_stats[g_stats$model_response %in% mr_grassland,], aes(x = model_response, y = r_squared, color = be)) +
  labs(title = "Single scene (04/2015) prediction of some grassland attributes (Core 5 | Plants)", x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()















# Forest examples
mr_forest <- c("SMId", "SMIr", "SMI", "dc_2D")
ggplot(data = f_stats_mod[f_stats_mod$response %in% mr_forest,], aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = f_stats[f_stats$model_response %in% mr_forest,], aes(x = model_response, y = r_squared, color = be)) +
  labs(title = "Single scene (04/2015) prediction of some forest attributes (Core 6 | Forest)", x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()


# Combined examples
mr_grassland <- c("SPECRICH", "cover_shrubs_pc", "cover_herbs_pc", "biomass_g", "LUI_reg")
mr_comb <- c(mr_grassland, mr_forest)
cbPalette <- c('#d53e4f','#fc8d59','#fee08b','#e6f598','#99d594','#3288bd')
ggplot(data = stats_mod[stats_mod$response %in% mr_comb,], aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() +
  geom_point(data = stats[stats$model_response %in% mr_comb,], aes(x = model_response, y = r_squared, color = be)) +
  labs(title = "Single scene (04/2015) prediction of some grassland and forest attributes (Core 5, Core 6)", x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# LUI and climate examples
mr_climate <- c("SPECRICH")
cbPalette <- c('#d53e4f','#fc8d59','#fee08b','#e6f598','#99d594','#3288bd')
ggplot(data = c_stats_mod[c_stats_mod$response %in% mr_climate, ], aes(x = response, y = r_squared, fill = be)) + 
  geom_boxplot() + 
  geom_point(data = c_stats[c_stats$model_response %in% mr_climate, ], aes(x = model_response, y = r_squared, color = be)) +
  labs(title = "Prediction based on climate data & LUI (Bl?thgen et al.)", x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()




