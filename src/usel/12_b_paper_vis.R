source("D:/UNI/Master/MA/exploratorien/scripts/project_biodiv_rs/src/usel/00_b_set_environment_start_modeltrain.R")

pls<-readRDS(paste0(path_stats,"allnewstats_PLS_FFS_9CV_allRESP.rds"))
rf<-readRDS(paste0(path_stats,"allnewstats_RF_FFS_9CV_allRESP.rds"))

##mr_all<-c("SPECRICH","LUI_glb")

# if you have factes and want to change the label, you have to change it in the df

levels(pls$response)<-c("Species richness","Shannon","Eveness","Land Use Intensity")
mr_all<-c("Species richness")
levels(rf$response)<-c("Species richness","Shannon","Eveness","Land Use Intensity")



## zur direkten Implemntierung von plots in latex
##library(tikzDevice)
##options(tz="Europe/Berlin")
#create tex file for the plot
##tikz(file = "D:/UNI/Master/MA/Latex/plot_test.tex", width = 5, height = 5)
pls_rmse<-ggplot(data= pls[pls$smpl %in% seq(10) & pls$variable=="rmse"& pls$response %in% mr_all ,],
             aes( x= be ,y=value, fill=stat),shape=21,
             stroke=5)+
  geom_boxplot(lwd=0.2)+
      labs(title = "Model performance of PLS",
       x = NULL, y = "RMSE", fill = "10 model instances")+
  #scale_fill_manual(values=c("#FFCC99","#CCFFFF","#CCCCFF"))+ #"#FFCC99","#CC99CC","#99FFCC" "#6666CC","#FFCC99","#66CC99"
  scale_fill_manual(labels=c("validation","prediction"),values=c("#FFCC99","#996666"))+ #values=c("#660033","#CC9900") #für boxplot farben
  scale_x_discrete(labels=c("Alb","Hainich","Schorfheide"))+
  facet_wrap(~response, scales = "free")+
  theme(axis.text.x=element_text(size=9), axis.title.x=element_text(size=9),
        axis.text.y=element_text(size=9), axis.title.y=element_text(size=9),
        legend.title=element_text(size=9, face=c("bold")),
        plot.title=element_text(size=10, face="bold", hjust=0.5))
##dev.off()

rf_rmse<-ggplot(data= rf[rf$smpl %in% seq(10) & rf$variable=="rmse"& rf$response %in% mr_all ,],
                 aes( x= be ,y=value, fill=stat),shape=21,
                 stroke=5)+
  geom_boxplot(lwd=0.2)+
  labs(title = "Model performance of RF",
       x = NULL, y = "RMSE", fill = "10 model instances")+
  #scale_fill_manual(values=c("#FFCC99","#CCFFFF","#CCCCFF"))+ #"#FFCC99","#CC99CC","#99FFCC" "#6666CC","#FFCC99","#66CC99"
  scale_fill_manual(labels=c("validation","prediction"),values=c("#FFCC33","#996666"))+ #values=c("#660033","#CC9900") #für boxplot farben
  scale_x_discrete(labels=c("Alb","Hainich","Schorfheide"))+
  facet_wrap(~response, scales = "free")+
  theme(axis.text.x=element_text(size=9), axis.title.x=element_text(size=9),
        axis.text.y=element_text(size=9), axis.title.y=element_text(size=9),
        legend.title=element_text(size=9, face=c("bold")),
        plot.title=element_text(size=10, face="bold", hjust=0.5))
multiplot( pls_rmse,rf_rmse, cols=2)

####################
## - only predicted values and observed values------------------------------------------------
pls_value_pred_test<-readRDS(paste0(path_stats,"obs_pred_pls_ffs.rds"))
levels(pls_value_pred_test$response)<-c("Species richness","Shannon","Eveness","Land Use Intensity")

# #plot the observed data points against predicted data points from all exploratories
# #if you want points, use instead geom_point(pch=22, colour="black", size=2)
# pls<-ggplot(data=pls_value_pred_test[pls_value_pred_test$response %in% mr_all ,],
#             aes( x= be ,y=value, fill=variable), size=1.5)+
#   scale_fill_manual(values=wes_palette(n=3,name="Chevalier"), labels=c("validated","observed"))+
#   geom_boxplot(position= position_dodge(width=0.6))+
#   labs(title = "distribution of observed values and validated values after PLS regression",
#        lineheigth=2, 
#        x = "", y = "unit of measurement", fill = "50 data points")+
#   scale_x_discrete(labels=c("Alb","Hainich","Schorfheide"))+
#   facet_wrap(~response,scale="free")+
#   theme(axis.text.x=element_text(size=9), axis.title.x=element_text(size=9),
#         axis.text.y=element_text(size=9), axis.title.y=element_text(size=9),
#         legend.title=element_text(size=9, face=c("bold")),
#         plot.title=element_text(size=9, face="bold"))
# 
# # for RF
# rf_value_pred_test<-readRDS(paste0(path_stats,"obs_pred_rf_ffs.rds"))
# levels(rf_value_pred_test$response)<-c("Species richness","Shannon","Eveness","Land Use Intensity")
# #plot the observed data points against predicted data points from all exploratories
# rf<-ggplot(data=rf_value_pred_test[rf_value_pred_test$response %in% mr_all ,],
#            aes( x= be ,y=value, fill=variable), size=1.5)+
#   scale_fill_manual(values=wes_palette(n=3,name="Chevalier"), labels=c("validated","observed"))+
#   geom_boxplot(position= position_dodge(width=0.6))+
#   labs(title = "distribution of observed values and validated values after RF regression",
#        lineheigth=2, 
#        x = "", y = "unit of measurement", fill = "50 data points")+
#   scale_x_discrete(labels=c("Alb","Hainich","Schorfheide"))+
#   facet_wrap(~response,scale="free")+
#   theme(axis.text.x=element_text(size=9), axis.title.x=element_text(size=9),
#         axis.text.y=element_text(size=9), axis.title.y=element_text(size=9),
#         legend.title=element_text(size=9, face=c("bold")),
#         plot.title=element_text(size=9, face="bold"))
# 
# multiplot(pls,rf)

#plot the observed data points against predicted data points from all exploratories
names=c(rep("A", 20) , rep("B", 8) , rep("C", 30))

test_pred<-readRDS(paste0(path_stats,"obs_pred_PLS_RF.rds"))
mr_all<-c("Species richness")
t<-ggplot(data=test_pred[test_pred$response %in% mr_all ,],
           aes( x= be ,y=value, fill=variable), size=10)+
  scale_fill_manual(values=c("#FFCC33","#663333","#FFCC99"), labels=c("RF validated","observed","PLS validated"))+
  geom_boxplot()+
  labs(title = "distribution of observed values and validated values after RF regression",
       lineheigth=2,
       x = "", y = "unit of measurement", fill = "50 data points")+
  scale_x_discrete(labels=c("Alb","Hainich","Schorfheide"))+
  facet_wrap(~response,scale="free")+
  #stat_summary(fun.data = pls$value, geom = "text", fun.y = mean, colour = "red")+
  theme(axis.text.x=element_text(size=9), axis.title.x=element_text(size=9),
        axis.text.y=element_text(size=9), axis.title.y=element_text(size=9),
        legend.title=element_text(size=9, face=c("bold")),
        plot.title=element_text(size=9, face="bold"))
  #geom_text(data=NULL, aes( x=be, y=3, label=lab), col='red', size=3)
#geom_text(x= test_pred$be=="AEG", y= 3, label= "red text", col= "red")     
t+annotate("text", x = unique(test_pred$be), y = c(19,13,11), label = c("34.09  33.84    33.82",
                                                               "34.18   34.70  34.48",
                                                               "23.55     23.74   23.64"), size=2)
           
# predictor frequency usage in final model
    ## we take the specrichsing from predictor_freq.skript output from single exploratories
    # 
    # sp = melt(specrichsing, id.var = c("response","AEG_singlebest_pred","HEG_singlebest_pred",
    #                                    "SEG_singlebest_pred"))
    # write.csv(sp,paste0(path_stats,"freq.csv")) # this is highy unproefessional but im tired, 
    #so i created one column for all the predictors and deleted the other stuff by hand
pls_fre<-read.csv(paste0(path_stats,"PLS_freq.csv"))
rf_fre<-read.csv(paste0(path_stats,"RF_freq.csv"))

rf<-ggplot(data=rf_fre, aes(x=reorder(best_pred, +value), y= value, fill=variable, round(value)))+
  coord_flip()+
  geom_bar(stat="identity",  position = "dodge",  width=0.15)+
  scale_fill_manual(values=c("#660066","#FFCC33","#CC6600"))+
  theme(legend.position="top")+
  labs(x = "", y = "frequency", fill = "RF")

pls<-ggplot(data=pls_fre, aes(x=reorder(best_pred, +value), y= value, fill=variable, round(value)))+
  coord_flip()+
  geom_bar(stat="identity",  position = "dodge",  width=0.15)+
  scale_fill_manual(values=c("#660066","#FFCC33","#CC6600"))+
  theme(legend.position="top")+
  labs(x = "", y = "frequency", fill = "PLS")

multiplot(pls,rf, cols=2)
+
  scale_y_discrete(labels=pls_fre$value)
  

#### BAUSTELLE-------------------
# varianz

ggplot(data= glm[glm$smpl %in% seq(10) & glm$variable=="rmse_var"& glm$response %in% mr_all ,], 
       aes( x= response ,y=value, color="train",  fill=be),shape=21,color = "black", stroke=5)+
  scale_color_hue(l=2, c=55)+ #l=lightness, c=intensity of color
  # geom_boxplot(lwd=1)+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  geom_boxplot(lwd=0.7)+
  geom_point(data=glm[ glm$smpl == "Rmean" & glm$variable =="var"&
                         glm$response %in% mr_all &glm$stat=="test",],
             aes(x= response, y=value, color="test"),shape=22,color = "black", stroke=1, size=2)+
  labs(title = "Variance of RMSE Regression glm after 9fold CV  ", 
       x = glm$response, y = "Variance_RMSE", fill = "Mean of Var_RMSE of resamples")+ 
  #scale_fill_manual(values=c("#FFCC99","#CCFFFF","#CCCCFF"))+ #"#FFCC99","#CC99CC","#99FFCC" "#6666CC","#FFCC99","#66CC99"
  scale_fill_manual(values=c("#660066","#FFCC33","#CC6600"))+
  #scale_fill_hue(l=100)
  facet_wrap(~response  , scales = "free")+
  theme_bw()
