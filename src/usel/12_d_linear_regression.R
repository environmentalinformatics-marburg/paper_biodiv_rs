source("D:/UNI/Master/MA/exploratorien/scripts/project_biodiv_rs/src/usel/00_b_set_environment_start_modeltrain.R")

library (lmtest)

## all validation and observation linear relationship
tstat<-readRDS(paste0(path_stats,"tstat_pls.rds"))
tstat<-readRDS(paste0(path_stats,"tstat_rf.rds"))

# getting data
var_pls<-readRDS(paste0(path_stats,"var_corel_std_err_rf_ffs.rds"))
var_pls<-var_pls[var_pls$response=="SPECRICH",]
var_rf<-readRDS(paste0(path_stats,"var_corel_std_err_rf_ffs.rds"))
var_rf<-var_rf[var_rf$response=="SPECRICH",]

val_obs_PLS<-readRDS(paste0(path_stats,"obs_pred_pls_ffs.rds"))
val_obs_PLS<-val_obs_PLS[val_obs_PLS$response=="SPECRICH",]
val_obs_RF<-readRDS(paste0(path_stats,"obs_pred_rf_ffs.rds"))
val_obs_RF<-val_obs_RF[val_obs_RF$response=="SPECRICH",]

# some info about observed values
sd(val_obs_PLS$testing_response[val_obs_PLS$be=="SEG"])


# ###### acessing predicted and tested values
# value_pred_test <-lapply(tstat, function(be){
#   
#   mr<-lapply(unique(be$tstat$model_response), function(mr){
#     dt_r<-be$tstat[be$tstat$model_response == mr,]
#     dt <- be$tstat[be$tstat$model_response == mr, ]
#     tstat_lm_smpl <- lm(testing_predicted ~ testing_response, 
#                         data = dt)
#     data.frame(be = substr(be$tstat$model_selector[1], 1, 3), 
#                response = dt$model_response[1], #auf 1 bei mehreren responses
#                testing_predicted=be$tstat[be$tstat$model_response ==mr,]$testing_predicted,
#                testing_response=be$tstat[be$tstat$model_response ==mr,]$testing_response)
#   })
#   do.call("rbind",mr)  
# })
# 
# value_pred_test_RF = do.call("rbind", value_pred_test)
# rownames(value_pred_test_RF) <- NULL
# val_obs_PLS<-value_pred_test_PLS[value_pred_test_PLS$response=="SPECRICH",]
# val_obs_RF<-value_pred_test_RF[value_pred_test_RF$response=="SPECRICH",]
# rm(tstat,value_pred_test)

#linear stuff

library(tikzDevice)
options(tz="Europe/Berlin")
#create tex file for the plot
tikz(file = "D:/UNI/Master/MA/Latex/plot_test.tex", width = 1/textwidth, height = 5)

# visualisation relationship between observed and validated data for all exploratories
pls<-ggplot(data=val_obs_PLS,
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

rf<-ggplot(data=val_obs_RF,
           aes( x= testing_response ,y=testing_predicted, fill=be, color=be))+
  geom_point(position="jitter",shape=21,color = "black", size=1.5)+
  geom_smooth(method='lm')+
  scale_fill_manual(values=c("#660066","#FFCC33","#CC6600"), name="",labels=c("Alb","Hainich","Schorfheide"))+
  scale_color_manual(values=c("#660066","#FFCC33","#CC6600"),name="",labels=c("Alb","Hainich","Schorfheide"))+
  geom_abline()+
  coord_cartesian(xlim = c(12:60), ylim = c(12:60),expand = FALSE)+
  scale_x_discrete(breaks=c(20,30,40,50,60), limits=15:60)+
  scale_y_discrete(breaks=c(20,30,40,50,60), limits=15:60)+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12), axis.title.y=element_text(size=12),
        legend.title=element_text(size=10, face=c("bold")),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        legend.justification=c(1,0), legend.position=c(1,0))+
  labs( x = "observed", y = "predicted")

grid.arrange(pls, rf, ncol=2)
#multiplot(pls, rf, cols=2)
dev.off()

ggplot(data=val_obs_RF,
           aes( x= testing_response ,y=testing_predicted, fill=be, color=be))+
  geom_point(position="jitter",shape=21,color = "black", size=1.5)+
  geom_smooth(method='lm')+
  scale_fill_manual(values=c("#660066","#FFCC33","#CC6600"), name="",labels=c("Alb","Hainich","Schorfheide"))+
  scale_color_manual(values=c("#660066","#FFCC33","#CC6600"),name="",labels=c("Alb","Hainich","Schorfheide"))+
  geom_abline()+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12), axis.title.y=element_text(size=12),
        legend.title=element_text(size=10, face=c("bold")),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        legend.justification=c(1,0), legend.position=c(1,0))+
  labs(title="Using RF regression", x = "observed", y = "predicted")


p<-value_pred_test_RF$testing_predicted[1:50]
o<-value_pred_test_RF$testing_response[1:50]
p<-value_pred_test_RF$testing_predicted[51:100]
o<-value_pred_test_RF$testing_response[51:100]
p<-spec_RF$testing_predicted[101:150]
o<-spec_RF$testing_response[101:150]
y=c(p,o)
group <- as.factor(c(rep(1, length(p)), rep(2, length(o))))
fligner.test(y~ group) 
#PLS estimates: AEG=7.665e-05 HEG=3.476e-06 SEG= 3.711e-09
#RF estimate: AEG= 0.001801 HEG=0.003592 SEG=0.0002262

# checking the linear relationship between obs. and val. data
summary(lm(testing_predicted~testing_response, data=val_obs_PLS[val_obs_PLS$be=="AEG",])) #Rsq:0.2127
cor.test(val_obs_PLS$testing_predicted[val_obs_PLS$be=="AEG"],val_obs_PLS$testing_response[val_obs_PLS$be=="AEG"])
summary(lm(testing_predicted~testing_response, data=val_obs_RF[val_obs_RF$be=="AEG",])) #Rsq: 0.2589
cor.test(val_obs_RF$testing_predicted[val_obs_RF$be=="AEG"],val_obs_RF$testing_response[val_obs_RF$be=="AEG"])
# PLS p-value: 0.0007482 ist <0,05 --> ich interpretiere den p-Wert aus Tabelle: 9.7e-14 ***
# RF  p-value: 0.0001613 p-Wert: 7.75e-11 ***

summary(lm(testing_predicted~testing_response, data=val_obs_PLS[val_obs_PLS$be=="HEG",])) #Rsq 0.0379
cor.test(val_obs_PLS$testing_predicted[val_obs_PLS$be=="HEG"],val_obs_PLS$testing_response[val_obs_PLS$be=="HEG"])
summary(lm(testing_predicted~testing_response, data=val_obs_RF[val_obs_RF$be=="HEG",])) #Rsq 0.06724
cor.test(val_obs_RF$testing_predicted[val_obs_RF$be=="HEG"],val_obs_RF$testing_response[val_obs_RF$be=="HEG"])
#PLS p-value: 0.1755 das Modell ist nichts wert
# RF p-value: 0.069 p-Wert: 2.18e-13 ***

summary(lm(testing_predicted~testing_response, data=val_obs_PLS[val_obs_PLS$be=="SEG",])) #Rsq 0.05756
cor.test(val_obs_PLS$testing_predicted[val_obs_PLS$be=="SEG"],val_obs_PLS$testing_response[val_obs_PLS$be=="SEG"])
summary(lm(testing_predicted~testing_response, data=val_obs_RF[val_obs_RF$be=="SEG",])) #Rsq 0.0002696
cor.test(val_obs_RF$testing_predicted[val_obs_RF$be=="SEG"],val_obs_RF$testing_response[val_obs_RF$be=="SEG"])
# PLS p-value: 0.09332 ich interpretiere p-Wert aus Tabelle: <2e-16 ***
# RF p-value: 0.9099 das mOdell ist nichts wert

summary(lm(val_obs_RF$value[val_obs_RF$variable=="testing_predicted"]~val_obs_RF$value[val_obs_RF$variable=="testing_predicted"], data=val_obs_RF[val_obs_RF$be=="SEG",])) 

#experiment ob statt aufwendige MDoellierung einfach der mean der obervierten Daten anstelle der predicted values genomen
# wird und diese danngengeneinadner plotten- Wie liegt der Zusammenhang?
experiment<-val_obs_PLS
experiment$testing_predicted<-23.74
ggplot(data=experiment,
       aes( x= testing_response ,y=testing_predicted, fill=be, color=be))+
  geom_point(shape=21,color = "black", stroke=1, size=1)+
  geom_smooth(method='lm', span= 0.9)+
  scale_fill_manual(values=c("#660066","#FFCC33","#CC6600"))+
  scale_color_manual(values=c("#660066","#FFCC33","#CC6600"))+
  labs(title="Using RF regression", x = "actual", y = "predicted")
