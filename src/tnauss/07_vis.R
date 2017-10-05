# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  source("D:/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
} else {
  source("/media/permanent/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
}

compute = TRUE

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
  labs(title = "Prediction based on climate data & LUI (Blüthgen et al.)", x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()




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
