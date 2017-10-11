# Set path ---------------------------------------------------------------------
source("F:/exploratorien/scripts/00_set_environment.R")

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

# see single bands for atmos.corrected Alb --------------
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
