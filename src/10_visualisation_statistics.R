
### VISUALISATION
source("C:/Users/uselig/Documents/GitHub/project_biodiv_rs/src/00_paths_libraries.R")
mods<-list.files(path_results, full.names=TRUE,pattern=glob2rx("*rds"))

library(BBmisc) # für Normierung
library(wesanderson) # Farbpalette

vstat_all<-readRDS(paste0(path_stats, "overview_models_validation_results.rds"))
mstat_all<-readRDS(paste0(path_stats, "overview_models_training_results.rds"))
#change response names
levels(mstat_all$response)<-c("Species richness","Shannon","Eveness","Cover Shrubs","Cover bare soil","Biomass")
levels(vstat_all$response)<-c("Species richness","Shannon","Eveness","Cover Shrubs","Cover bare soil","Biomass")

#make a bunch of vectors
mr_all<-c("Species richness","Shannon","Eveness","Cover Shrubs","Cover bare soil","Biomass")

models=c("Model_5_RECLIMALUI","Model_4_CLIMALUI","Model_3_RELUI","Model_2_LUI","Model_1_RE","Model_0_RE")

# aus dem Skript 08-Statistik kommen 2 Tabellen:
# vstat_all /mstat_all = pure response and predicted values of all models and responses, rmse and rmseSD (only from vstat)

##### ABB.1##########
####################################################
##### box/bar der einzelnen CV Folds um die Stabilität der Modelle zu prüfen
####################################################

m= list() # create empty list for all models
for (x in seq(models)){
  m[[x]]<-ggplot(data= mstat_all[mstat_all$response %in% mr_all[x],], # füge hinzu wenn nur eine response güwnscht [mstat$response %in% "SPECRICH",]
                 aes( x= smpl ,y=rmse, fill=be)
                      #group=be, colour=be)
                      )+
    #geom_line()+
    #geom_point()+
    geom_bar(position="dodge", colour="black", stat="identity")+  #für folds 
    scale_fill_manual(labels=c("Alb", "Hainich","Schorfheide"),values=c("#660066","#FFCC33","#CC6600"))+
    facet_wrap(~model, scales = "free")+
    labs(title = mr_all[x],
         x = "Fold number", y = "RMSE")+
    theme(axis.text.x=element_text(size=8),
          axis.text.y=element_text(size=8),
          legend.title=element_blank(),
          plot.title=element_text(size=11, face="bold", hjust=0.5),
          legend.justification=c(1,0), legend.position="right")
}
do.call(grid.arrange,m)
#grid.arrange(m[[1]],m[[3]], nrow=2)

##### ABB.2##########
####################################################
##### Boxplots for entire modeltraining /model performance between validation and training
####################################################

#mutate dfs
# lets merge train and validation results
# melt the values all in one column
vstat<-vstat_all #from 08statistics
vstat[,c(1,4:5,7,8)]<- NULL # remove EPID, response values (predicted and observed), rmse, cor,
vstat<-vstat[!duplicated(vstat$rmse),] #remove dupilcaed rmse values
vstat<-melt(vstat, id.var=c("response","be","smpl", "model"))
vstat$stat = "test"

mstat<-mstat_all #from 08statistics
mstat[,c(3,5)]<- NULL # rEMOVE ncomp and Rsquared FROM mSTAT
mstat<-melt(mstat, id.var=c("response","be","smpl","model"))
mstat$stat = "train"
#join train and test data together
modperf = rbind(vstat, mstat)
  
  ######
  ###plot
  #######

  m= list() # create empty list for all models
  for (x in seq(models)){
    m[[x]]<-ggplot(data= modperf[modperf$smpl %in% seq(5) & modperf$variable=="rmse"& 
                                  modperf$response %in% mr_all[x],], # füge hinzu wenn nur eine response güwnscht [mstat$response %in% "SPECRICH",]
                 aes( x= be ,y=value,
                      fill=stat),shape=21,
                 stroke=5)+
      geom_boxplot(lwd=0.2)+ # übergreifend für alle folds, dann aber die Beschriftung ändern! 
      labs(title = mr_all[x],
           x = NULL, y = "RMSE", fill = "Model performance")+
      scale_fill_manual(labels=c("validation","training"),values=c("#FFCC99","#996666"))+ #values=c("#660033","#CC9900") #f?r boxplot farben
      scale_x_discrete(labels=c("Alb","Hainich","Schorfheide"))+
      facet_wrap(~model, scales="free_y")+
      stat_summary(fun.y=mean, geom="point", shape=23, size=2)+
      #stat_summary(fun.y=mean, geom="text", 
      #               vjust=+3, aes( label=round(..y.., digits=1)))+
      theme(axis.text.x=element_text(size=8),
            axis.text.y=element_text(size=8),
            legend.title=element_blank(),
            plot.title=element_text(size=10, face="bold", hjust=0.5),
            legend.justification=c(1,0), legend.position="right")
    }
  do.call(grid.arrange,m)
  #grid.arrange(m[[1]],m[[3]], nrow=2)
##### ABB.3##########
####################################################
##### Scatterplot einfache lineare Regression um die Annäerung der 
##### vorhergesagten Werte an die observierten zu prüfen
####################################################
#####################################

# #make a title if you have on single model as df
# tit<-paste("Predictions using the final model of each fold", 
#            substr(basename(mods[x]), 1, nchar(basename(mods[x])) - 12))

# remove all observations that are 0.0000
vstat_alle<-vstat_all[vstat_all$testing_response != 0,]

v= list() # create empty list for all models
  for (x in seq(models)){
    v[[x]]<-ggplot(data= vstat_alle[vstat_alle$response %in% mr_all[x],], #füge hinzu wenn nur eine response geünscht [ vstat$response %in% "SPECRICH",]
                   aes( x= testing_response ,y=testing_predicted,
                        colour=be, fill=be))+
      geom_point(position="jitter",shape=21,color = "black", size=1)+
      geom_smooth(method='lm', span= 0.9)+geom_abline(show.legend=F)+
      scale_color_manual(values=c("#660066","#FFCC33","#CC6600"),
                         name="",labels=c("Alb","Hainich","Schorfheide"))+
      scale_fill_manual(values=c("#660066","#FFCC33","#CC6600"),
                        name="",
                        labels=c("Alb","Hainich","Schorfheide"))+
      facet_wrap(~model, scales = "free")+
      theme(axis.text.x=element_text(size=8),
            axis.text.y=element_text(size=8),
            legend.title=element_blank(),
            plot.title=element_text(size=10, face="bold", hjust=0.5),
            legend.justification=c(1,0), legend.position="right")+
      labs(title = mr_all[x] ,
           x = "observed values", y = "predicted values")
    #print(v) # only if you want to see each model on single page
  }
do.call(grid.arrange, v)
#grid.arrange(v[[1]],v[[3]])

##### ABB.4##########
####################################################
##### boxplots 1--> Verteilung der Variabiliatät der CV-folds mit standartiesierten rmse
####################################################
### mstat kann ich die einzelnen vorhergesagten response nicht extrahieren, deswegen nur Abbildung für validation

  # lets merge train and validation results
    # melt the values all in one column
  vstat_errors<-vstat_all #from 08statistics
  vstat_errors[,c(1,4:6,8)]<- NULL # remove EPID, response values (predicted and observed), rmse, cor,
  vstat_errors<-vstat_errors[!duplicated(vstat_errors$rmse),] #remove dupilcaed rmse values
  vstat_errors<-melt(vstat_errors, id.var=c("response","be","smpl", "model"))
  vstat_errors$stat = "test"
  
  ######
  ###plot
  #######
  
  i= list() # create empty list for all models
  
  for (x in seq(models)){
  i[[x]]<- ggplot(data= vstat_errors[vstat_errors$variable=="rmseSD"& vstat_errors$response %in% mr_all[x] ,],
                   aes( x= be ,y=value, fill=be),shape=21,
                   stroke=5)+
    geom_boxplot(lwd=0.2, show.legend = F)+
    labs(title = mr_all[x],
         x = NULL, y = expression(RMSE["SD"]))+
    scale_fill_manual(values=c("#660066","#FFCC33","#CC6600"),
    name="")+
  scale_x_discrete(labels=c("Alb","Hainich","Schorfheide"))+
    facet_wrap(~model, scales="free_y")+
    theme(axis.text.x=element_text(size=8),
          axis.text.y=element_text(size=8),
          plot.title=element_text(size=11, face="bold", hjust=0.5),
          legend.justification="bottom", legend.position=c(1,0.7))
  #print(i[[x]])
  }
  do.call(grid.arrange, i)

##### ABB.5##########
####################################################
##### boxplots 2--> Vergleich der Werteverteilung (Boxplot) zwischen predictions und observations
####################################################

  # ändere Struktur des df aus skript o8 Statistik
  vbox<-vstat_all #from 08statistics
  vbox[,c(1,6:8)]<- NULL # remove EPID, cor, rmse, rmseSD
  vbox<-melt(vbox, id.var=c("response","be", "model", "smpl"))
  
  ######
  ###plot
  ####### 
  box=list()
  for (x in seq(models)){
    box[[x]]<-ggplot(data= vbox[vbox$smpl %in% seq(10) & vbox$response %in% mr_all[x],], # füge hinzu wenn nur eine response güwnscht [mstat$response %in% "SPECRICH",]
                     aes( x= be ,y=value,
                          fill=variable),shape=21,
                     stroke=5)+
      geom_boxplot(lwd=0.2)+ # übergreifend für alle folds, dann aber die Beschriftung ändern! 
      labs(title = mr_all[x],
           x = NULL, y = "unit of response", fill = "50 data plots")+
      scale_fill_manual(labels=c("field observations","PLS validation"),values=c("#663333","#FFCC99"))+ #values=c("#660033","#CC9900") #f?r boxplot farben
      scale_x_discrete(labels=c("Alb","Hainich","Schorfheide"))+
      facet_wrap(~model, scales="free_y")+
      stat_summary(fun.y=mean, geom="text", 
                   #vjust=-5, 
                   aes( label=round(..y.., digits=2)))+
      theme(axis.text.x=element_text(size=8),
            axis.text.y=element_text(size=8),
            legend.title=element_blank(),
            plot.title=element_text(size=10, face="bold", hjust=0.5),
            legend.position="bottom", legend.box="horizontal")
    #geom_text(data=NULL, aes( x=be, y=3, label=lab), col='red', size=3)
    #geom_text(x= test_pred$be=="AEG", y= 3, label= "red text", col= "red")     
    #t+annotate("text", x = unique(test_pred$be), y = c(19,13,11), label = c("34.09  33.84    33.82",
    #                                                                       "34.18   34.70  34.48",
    #                                                                        "23.55     23.74   23.64"), size=4)
    
  }
  do.call(grid.arrange, box)
  #grid.arrange(box[[1]],box[[3]])
  
  ### SPIELWIESE
 
  