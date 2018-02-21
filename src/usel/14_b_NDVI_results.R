source("D:/UNI/Master/MA/exploratorien/scripts/project_biodiv_rs/src/usel/00_b_set_environment_start_modeltrain.R")

# ----------------------read data products
x<-readRDS(paste0(path_results,"NDVI_pls_ffs_9CV.rds"))

  #frequent used predictors
pred_freq<-read.csv(paste0(path_stats,"pred_freq_NDVI.csv"))
  # data for box visualisation
plsNDVI<-readRDS(paste0(path_stats,"allnewstats_PLS_FFS_9CV_SPEC_NDVI.rds"))
pls<-readRDS(paste0(path_stats,"allnewstats_PLS_FFS_9CV_allRESP.rds"))
pls<-pls[pls$response=="SPECRICH",]

levels(plsNDVI$response)<-"Species richness"
mr_all<-"Species richness"

# -----------------------vis prediction & validation
pls_NDVI<-ggplot(data= plsNDVI[plsNDVI$smpl %in% seq(10) & plsNDVI$variable=="rmse"& plsNDVI$response %in% mr_all ,],
                 aes( x= be ,y=value, fill=stat),shape=21,
                 stroke=5)+
  geom_boxplot(lwd=0.2)+
  labs(title = "Model performance of PLS",
       x = NULL, y = "RMSE (number of species)", fill = "10 model instances")+
  #scale_fill_manual(values=c("#FFCC99","#CCFFFF","#CCCCFF"))+ #"#FFCC99","#CC99CC","#99FFCC" "#6666CC","#FFCC99","#66CC99"
  scale_fill_manual(labels=c("validation","training"),values=c("#FFCC99","#996666"))+ #values=c("#660033","#CC9900") #f?r boxplot farben
  scale_x_discrete(labels=c("Alb","Hainich","Schorfheide"))+
  facet_wrap(~response, scales = "free")+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12), axis.title.y=element_text(size=10),
        legend.title=element_text(size=10, face=c("bold")),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        legend.justification=c(1,0), legend.position=c(1,0.74))

pls_org<-ggplot(data= pls[pls$smpl %in% seq(10) & pls$variable=="rmse"& pls$response %in% mr_all ,],
                 aes( x= be ,y=value, fill=stat),shape=21,
                 stroke=5)+
  geom_boxplot(lwd=0.2)+
  labs(title = "Model performance of PLS",
       x = NULL, y = "RMSE (number of species)", fill = "10 model instances")+
  #scale_fill_manual(values=c("#FFCC99","#CCFFFF","#CCCCFF"))+ #"#FFCC99","#CC99CC","#99FFCC" "#6666CC","#FFCC99","#66CC99"
  scale_fill_manual(labels=c("validation","prediction"),values=c("#FFCC99","#996666"))+ #values=c("#660033","#CC9900") #f?r boxplot farben
  scale_x_discrete(labels=c("Alb","Hainich","Schorfheide"))+
  coord_cartesian( ylim = c(3:25),expand = FALSE)+
  scale_y_discrete(breaks=c(5,10,15,20,25), limits=5:25)+
  facet_wrap(~response, scales = "free")+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12), axis.title.y=element_text(size=10),
        legend.title=element_text(size=10, face=c("bold")),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        legend.justification=c(1,0), legend.position=c(1,0.8))

grid.arrange(pls_NDVI,pls_org, ncol=2)

# observation and prediction
pls_value_pred_testNDVI<-readRDS(paste0(path_stats,"obs_pred_pls_ffs_LUI.rds"))
levels(pls_value_pred_testNDVI$response)<-"Species richness"
pls_value_pred_test<-readRDS(paste0(path_stats,"obs_pred_pls_ffs.rds"))
pls_value_pred_test<-pls_value_pred_test[pls_value_pred_test$response=="SPECRICH",]
  #easier visualisation 
pls_obs_pred_NDVI = melt(pls_value_pred_testNDVI, id.var = c("response", "be"))
pls_obs_pred_test = melt(pls_value_pred_test, id.var = c("response", "be"))

# #plot the observed data points against predicted data points from all exploratories
pls_obs_NDVI<-ggplot(data=pls_obs_pred_NDVI[pls_obs_pred_NDVI$response %in% mr_all ,],
            aes( x= be ,y=value, fill=variable), size=1.5)+
  scale_fill_manual(values=wes_palette(n=3,name="Chevalier"), labels=c("validated","observed"))+
  geom_boxplot(position= position_dodge(width=0.6))+
  labs(title = "distribution of observed values and validated values after PLS regression",
       lineheigth=2,
       x = "", y = "unit of measurement", fill = "50 data points")+
  facet_wrap(~response,scale="free")+
  coord_cartesian( ylim = c(12:60),expand = FALSE)+
  scale_x_discrete(labels=c("Alb","Hainich","Schorfheide"))+
  scale_y_discrete(breaks=c(20,30,40,50,60), limits=15:60)+
  theme(axis.text.x=element_text(size=9), axis.title.x=element_text(size=9),
        axis.text.y=element_text(size=9), axis.title.y=element_text(size=9),
        legend.title=element_text(size=9, face=c("bold")),
        plot.title=element_text(size=9, face="bold"))

pls_obs<-ggplot(data=pls_obs_pred_test[pls_obs_pred_test$response %in% mr_all ,],
                     aes( x= be ,y=value, fill=variable), size=1.5)+
  scale_fill_manual(values=wes_palette(n=3,name="Chevalier"), labels=c("validated","observed"))+
  geom_boxplot(position= position_dodge(width=0.6))+
  labs(title = "distribution of observed values and validated values after PLS regression",
       lineheigth=2,
       x = "", y = "unit of measurement", fill = "50 data points")+
  facet_wrap(~response,scale="free")+
  coord_cartesian(ylim = c(12:60),expand = FALSE)+
  scale_x_discrete(labels=c("Alb","Hainich","Schorfheide"))+
  scale_y_discrete(breaks=c(20,30,40,50,60), limits=15:60)+
  theme(axis.text.x=element_text(size=9), axis.title.x=element_text(size=9),
        axis.text.y=element_text(size=9), axis.title.y=element_text(size=9),
        legend.title=element_text(size=9, face=c("bold")),
        plot.title=element_text(size=9, face="bold"))
grid.arrange(pls_obs_NDVI,pls_obs, ncol=2)

#-------- regression vis------------------
pls_reg_NDVI<-ggplot(data=pls_value_pred_testNDVI,
            aes( x= testing_response ,y=testing_predicted, fill=be, color=be))+
  geom_point(position="jitter",shape=21,color = "black", size=1.5)+
  geom_smooth(method='lm', span= 0.9)+
  scale_fill_manual(values=c("#660066","#FFCC33","#CC6600"),name="",labels=c("Alb","Hainich","Schorfheide"))+
  scale_color_manual(values=c("#660066","#FFCC33","#CC6600"),name="",labels=c("Alb","Hainich","Schorfheide"))+
  geom_abline(show.legend=F)+
  coord_cartesian(xlim = c(12:60), ylim = c(12:60),expand = FALSE)+
  scale_x_discrete(breaks=c(20,30,40,50,60), limits=15:60)+
  scale_y_discrete(breaks=c(20,30,40,50,60), limits=15:60)+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12), axis.title.y=element_text(size=12),
        legend.title=element_text(size=10, face=c("bold")),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        legend.justification=c(1,0), legend.position=c(1,0))+
  labs( x = "observed", y = "predicted")

pls_reg<-ggplot(data=pls_value_pred_test[pls_value_pred_test$response %in% mr_all ,],
                     aes( x= testing_response ,y=testing_predicted, fill=be, color=be))+
  geom_point(position="jitter",shape=21,color = "black", size=1.5)+
  geom_smooth(method='lm', span= 0.9)+
  scale_fill_manual(values=c("#660066","#FFCC33","#CC6600"),name="",labels=c("Alb","Hainich","Schorfheide"))+
  scale_color_manual(values=c("#660066","#FFCC33","#CC6600"),name="",labels=c("Alb","Hainich","Schorfheide"))+
  geom_abline(show.legend=F)+
  coord_cartesian(xlim = c(12:60), ylim = c(12:60),expand = FALSE)+
  scale_x_discrete(breaks=c(20,30,40,50,60), limits=15:60)+
  scale_y_discrete(breaks=c(20,30,40,50,60), limits=15:60)+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12), axis.title.y=element_text(size=12),
        legend.title=element_text(size=10, face=c("bold")),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        legend.justification=c(1,0), legend.position=c(1,0))+
  labs( x = "observed", y = "predicted", title="relation with original predictors")
grid.arrange(pls_reg_NDVI,pls_reg, ncol=2)

#-- some statistics
cor.test(pls_value_pred_test$testing_predicted[pls_value_pred_test$be=="AEG"],
         pls_value_pred_test$testing_response[pls_value_pred_test$be=="AEG"])$estimate
summary(lm(testing_predicted~testing_response, data=pls_value_pred_test[pls_value_pred_test$be=="AEG",]))

cor.test(pls_value_pred_testNDVI$testing_predicted[pls_value_pred_testNDVI$be=="AEG"],pls_value_pred_testNDVI$testing_response[pls_value_pred_testNDVI$be=="AEG"])$estimate
