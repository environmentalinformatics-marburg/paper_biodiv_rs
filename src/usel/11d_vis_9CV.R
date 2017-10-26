source("D:/UNI/Master/MA/exploratorien/scripts/project_biodiv_rs/src/usel/00_set_environment.R")

stats<-readRDS(paste0(path_stats,"allnewstats_GLM_9CV_allRESP.rds"))
# visualisation ideas
cbPalette<-c("#ffffff", "#525252", "#bdbdbd") #bnw palette
cbPalette <- c("#fbb4ae", "#80b1d3", "#b3de69")
palette<-c("#BuGn","#OrRd","#PuRd")
mr_all <- c("SPECRICH", "SHANNON", "EVENESS", "LUI_glb")
mr_all<-"SHANNON"

# lets try it with SPEC only
specerror<-errors[errors$response=="SHANNON",]
specerror<-specerror[specerror$be=="AEG",]

ggplot(data = specerror[specerror$smpl %in% seq(10) & specerror$variable == "r_squared",], 
       aes(x = response, y = value, color = stat)) + 
  geom_boxplot() + 
  geom_point(data = specerror[specerror$smpl == "Rmean" & specerror$variable == "r_squared",],
             aes(x = response, y = value, color = "Mean R2 from sets")) + 
  geom_point(data = specerror[specerror$smpl == "Rsum" & specerror$variable == "r_squared",],
             aes(x = response, y = value, color = stat)) + 
  theme_bw()

#thomas' plot
ggplot(data = specerror[specerror$smpl %in% seq(10) & specerror$variable == "r_squared",], 
       aes(x = response, y = value, color = stat)) + 
  geom_boxplot() + 
  geom_point(data = specerror[specerror$smpl == "Rmean" & specerror$variable == "r_squared",],
             aes(x = response, y = value, color = "Mean R2 from sets")) + 
  geom_point(data = specerror[specerror$smpl == "Rsum" & specerror$variable == "r_squared",],
             aes(x = response, y = value, color = "R2 over all")) + 
  theme_bw()

# with all exploratories
train_pls_ffs<-readRDS("results/train_pls_9CV_3resp_7_pred.rds") 
test_pls_ffs<-readRDS("results/test_pls_9CV_3resp_7_pred.rds") 
train_pls_ffs<-mstat
test_pls_ffs<-tstat_errors

ggplot(data = train_pls_ffs[train_pls_ffs$response %in% mr_all,], 
       aes(x = response, y = rmse, fill = be)) + 
  geom_boxplot() +
  geom_point(data = test_pls_ffs[test_pls_ffs$smpl=="Rmean"& test_pls_ffs$model_response== mr_all,], 
             aes(x = model_response, y = rmse, color=be),  shape=21,color = "black", stroke=1) +
  labs(title = "PLS_FFS R squared of the train and test data set compared with the actual prediction (colored dots)", 
       x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()


#my idea of visualisation of the Rmean values and the resamples
stats<-errorspls_1LOO_SPEC
#change factors to characters
for(i in names(stats)[1:4]){
  stats[[i]]<-as.character(stats[[i]])
}
# for Rsq
# Rsq_pls<-ggplot(data= stats[stats$smpl %in% seq(10) & stats$variable=="r_squared" ,], 
#        aes( x= factor(be) ,y=value, color=stat,  fill=stat),shape=21,color = "black", stroke=2)+
#       geom_boxplot()+
#   geom_point(data=stats[ stats$smpl == "Rmean" & stats$variable =="r_squared",],
#              aes(x= factor(be), y=value,  color="Rmean"),shape=21,color = "black", stroke=2)+
#   labs(title = "R squared Regression PLS after 9fold CV  ", 
#               x = stats$response, y = "R squared", fill = "Mean of R² of resamples")+ 
#    scale_colour_manual(values=cbPalette)+
#   theme_bw()
# 
# # for RMSE
# Rmse<-ggplot(data= stats[stats$smpl %in% seq(10) & stats$variable=="rmse" ,], 
#        aes( x= factor(be) ,y=value, color=stat,  fill=stat),shape=21,color = "black", stroke=2)+
#   geom_boxplot()+
#   geom_point(data=stats[ stats$smpl == "Rmean" & stats$variable =="rmse",],
#              aes(x= factor(be), y=value,  color="Rmean"),shape=21,color = "black", stroke=2)+
#   labs(title = "RMSE Regression PLS after 9fold CV  ", 
#        x = stats$response, y = "RMSE", fill = "Mean of RMSE of resamples")+ 
#   scale_colour_manual(values=cbPalette)+
#   theme_bw()
# 
# multiplot(Rsq,Rmse)


# Rsq. bastelküche für mehrere responses 
Rsq_glm<-ggplot(data= stats[stats$smpl %in% seq(10) & stats$variable=="r_squared"& stats$response %in% mr_all ,], 
            aes( x= response ,y=value, color=stat,  fill=be),shape=21,color = "black", stroke=5)+
            scale_color_hue(l=2, c=55)+ #l=lightness, c=intensity of color
  # geom_boxplot(lwd=1)+
  #stat_boxplot(geom = "errorbar",  width=0.1, size=1) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  geom_boxplot(lwd=0.7)+
  geom_point(data=stats[ stats$smpl == "Rmean" & stats$variable =="r_squared"&
                           stats$response %in% mr_all &stats$stat=="test",],
             aes(x= response, y=value, color="test"),shape=22,color = "black", stroke=1, size=2)+
  labs(title = "R squared Regression GLM after 9fold CV  ", 
       x = stats$response, y = "R squared", fill = "Mean of R² of resamples")+ 
  #guides(color=FALSE) + 
  #scale_fill_manual(values=c("#FFCC99","#CCFFFF","#CCCCFF"))+ #"#FFCC99","#CC99CC","#99FFCC" "#6666CC","#FFCC99","#66CC99"
  scale_fill_manual(values=wes_palette(n=3,name="Moonrise1"))+
  #scale_fill_hue(l=100)
  theme_bw()

mr_all="LUI_glb"
# RMSE bastelküche mehrere responses
GAM_rmse<-ggplot(data= stats[stats$smpl %in% seq(10) & stats$variable=="rmse"& stats$response %in% mr_all ,], 
       aes( x= be ,y=value, color=stat["train"],  fill=be),shape=21,color = "black", stroke=5)+
  scale_color_hue(l=2, c=55)+ #l=lightness, c=intensity of color
  # geom_boxplot(lwd=1)+
  #stat_boxplot(geom = "errorbar",  width=0.1, size=1) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  geom_boxplot(lwd=0.7)+
  geom_point(data=stats[ stats$smpl == "Rmean" & stats$variable =="rmse"&
                           stats$response %in% mr_all &stats$stat=="test",],
             aes(x= be, y=value, color="test"),shape=22,color = "black", stroke=1, size=2)+
  labs(title = "RMSE Regression GAM after 9fold CV  ", 
       x = "Exploratory", y = "RMSE", fill = "Mean of R² of resamples")+ 
  #guides(color=FALSE) + 
  #scale_fill_manual(values=c("#FFCC99","#CCFFFF","#CCCCFF"))+ #"#FFCC99","#CC99CC","#99FFCC" "#6666CC","#FFCC99","#66CC99"
  scale_fill_manual(values=wes_palette(n=3,name="Moonrise1"))+
  #scale_fill_hue(l=100)
  theme_bw()+
facet_grid(response ~ ., scales = "free")

multiplot(GAM_rmse, pls_rmse, rf_rmse)

rf<-readRDS(paste0(path_stats,"allnewstats_RF_9CV_allRESP.rds"))
rf_rmse<-ggplot(data= rf[rf$smpl %in% seq(10) & rf$variable=="rmse"& rf$response %in% mr_all ,], 
                 aes( x= response ,y=value, color=stat["train"],  fill=be),shape=21,color = "black", stroke=5)+
  scale_color_hue(l=2, c=55)+ #l=lightness, c=intensity of color
  # geom_boxplot(lwd=1)+
  #stat_boxplot(geom = "errorbar",  width=0.1, size=1) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  geom_boxplot(lwd=0.7)+
  geom_point(data=rf[ rf$smpl == "Rmean" & rf$variable =="rmse"&
                           rf$response %in% mr_all &rf$stat=="test",],
             aes(x= response, y=value, color="test"),shape=22,color = "black", stroke=1, size=2)+
  labs(title = "RMSE Regression RF after 9fold CV  ", 
       x = "Exploratory", y = "RMSE", fill = "Mean of R² of resamples")+ 
  #coord_cartesian(xlim=NULL, ylim=c(m, max_xy))+
  #guides(color=FALSE) + 
  #scale_fill_manual(values=c("#FFCC99","#CCFFFF","#CCCCFF"))+ #"#FFCC99","#CC99CC","#99FFCC" "#6666CC","#FFCC99","#66CC99"
  scale_fill_manual(values=c("#660066","#FFCC33","#CC6600"))+ #wes_palette(n=3,name="Moonrise1")
  #scale_y_continuous( expand=waiver())+
  #scale_fill_hue(l=100)
  #theme_bw()+
  #facet_grid(response ~ ., scales = "free")
  facet_wrap(~response  , scales = "free")

rf_range<-ggplot_build(rf_rmse)$layout$panel_ranges[[1]]$y.range

# plots for pls
pls<-readRDS(paste0(path_stats,"allnewstats_PLS_9CV_allRESP.rds"))
Rsq_pls<-ggplot(data= pls[pls$smpl %in% seq(10) & pls$variable=="r_squared"& pls$response %in% mr_all ,], 
                aes( x= response ,y=value, color=stat,  fill=be),shape=21,color = "black", stroke=5)+
  scale_color_hue(l=2, c=55)+ #l=lightness, c=intensity of color
  # geom_boxplot(lwd=1)+
  #stat_boxplot(geom = "errorbar",  width=0.1, size=1) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  geom_boxplot(lwd=0.7)+
  geom_point(data=pls[ pls$smpl == "Rmean" & pls$variable =="r_squared"&
                           pls$response %in% mr_all &pls$stat=="test",],
             aes(x= response, y=value, color="test"),shape=22,color = "black", stroke=1, size=2)+
  labs(title = "R squared Regression pls after 9fold CV  ", 
       x = pls$response, y = "R squared", fill = "Mean of R² of resamples")+ 
  #guides(color=FALSE) + 
  #scale_fill_manual(values=c("#FFCC99","#CCFFFF","#CCCCFF"))+ #"#FFCC99","#CC99CC","#99FFCC" "#6666CC","#FFCC99","#66CC99"
  scale_fill_manual(values=wes_palette(n=3,name="Moonrise1"))+
  #scale_fill_hue(l=100)
  theme_bw()
multiplot(Rsq_glm,Rsq_pls)

mr_all="LUI_glb"
# RMSE bastelküche mehrere responses
pls_rmse<-ggplot(data= pls[pls$smpl %in% seq(10) & pls$variable=="rmse"& pls$response %in% mr_all ,], 
                 aes( x= be ,y=value, color=stat["train"],  fill=be),shape=21,color = "black", stroke=5)+
  scale_color_hue(l=2, c=55)+ #l=lightness, c=intensity of color
  # geom_boxplot(lwd=1)+
  #stat_boxplot(geom = "errorbar",  width=0.1, size=1) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  geom_boxplot(lwd=0.7)+
  geom_point(data=pls[ pls$smpl == "Rmean" & pls$variable =="rmse"&
                           pls$response %in% mr_all &pls$stat=="test",],
             aes(x= be, y=value, color="test"),shape=22,color = "black", stroke=1, size=2)+
  labs(title = "RMSE Regression pls after 9fold CV  ", 
       x = "Exploratory", y = "RMSE", fill = "Mean of R² of resamples")+ 
  #guides(color=FALSE) + 
  #scale_fill_manual(values=c("#FFCC99","#CCFFFF","#CCCCFF"))+ #"#FFCC99","#CC99CC","#99FFCC" "#6666CC","#FFCC99","#66CC99"
  scale_fill_manual(values=wes_palette(n=3,name="Moonrise1"))+
  #scale_fill_hue(l=100)
  theme_bw()+
  facet_grid(response ~ ., scales = "free")

multiplot(GAM_rmse, pls_rmse)
