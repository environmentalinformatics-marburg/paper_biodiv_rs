# To get the best predictors in all BE we have to go 2 steps, where 1 is a general predictor extraction
# read model result
source("C:/Users/uselig/Documents/GitHub/project_biodiv_rs/src/00_paths_libraries.R")
mods<-list.files(path_results, full.names=TRUE,pattern=glob2rx("*rds"))

# # read model 1
# mod<-readRDS(paste0(path_results, "RE_Model_1_pls_ffs.rds"))

#set variable for the specific mod you want the statistic
x<-6
mod<-readRDS(mods[x])

# get all predictors of all BE after ffs processing
best<-mod
r= 1 #replace with the required response
for(be in names(best)){
  best[[be]] <- lapply(1:5,function(i){ 
    best[[be]]@model[[1]][[r]][[i]]$model$finalModel$xNames 
  })
}
# make function to extract the predictos in each BE
frq_all<-function(best){
    singlebest<-best
    # now use the new df to make it for all BE seperately
  singlebest$AEG<- as.data.frame(table(unlist(best$AEG)))
  colnames(singlebest$AEG)<- c("AEG_singlebest_pred","AEG_Freq")
  singlebest$HEG<- as.data.frame(table(unlist(best$HEG)))
  colnames(singlebest$HEG)<- c("HEG_singlebest_pred","HEG_Freq")
  singlebest$SEG<- as.data.frame(table(unlist(best$SEG)))
  colnames(singlebest$SEG)<- c("SEG_singlebest_pred","SEG_Freq")
  # but the data for each explo. has a differnet extent, so we leave it in a 
  #list and just access the singlebest 5 most frequent
  for(be in names(singlebest)){
    singlebest[[be]] <- lapply(1:length(singlebest[[be]]),function(f){ 
      singlebest[[be]]<-head(singlebest[[be]][order(singlebest[[be]][f], decreasing  = TRUE),],4 ) #or 3
    })
  }
  singlebest<-bind_rows(singlebest$AEG[2],singlebest$HEG[2],singlebest$SEG[2])
  singlebest$response<-"SPECRICH"
  
  #neat table: use df on HEG col
  # all columns into charcters
  singlebest$HEG_singlebest_pred<-as.character(singlebest$HEG_singlebest_pred)
  singlebest$AEG_singlebest_pred<-as.character(singlebest$AEG_singlebest_pred)
  singlebest$SEG_singlebest_pred<-as.character(singlebest$SEG_singlebest_pred)
  
  #alle predicctoren in eine Spalte
  singlebest$HEG_singlebest_pred[1:4]<-singlebest$AEG_singlebest_pred[1:4] #prediktoren rüber schieben
  singlebest$variable[1:4]<-"AEG"
  singlebest$HEG_Freq[1:4]<-singlebest$AEG_Freq[1:4] #Frequenz rüber schieben
  
  singlebest$HEG_singlebest_pred[9:12]<-singlebest$SEG_singlebest_pred[9:12]
  singlebest$variable[9:12]<-"SEG"
  singlebest$HEG_Freq[9:12]<-singlebest$SEG_Freq[9:12]
  
  singlebest$variable[5:8]<-"HEG" # neue Spalte für Zuordnung Explo.
  
  #unnötige Zeilen löschen
  singlebest<-singlebest[,-c(1:2,5:6)]
  colnames(singlebest)[1]<-"best_pred" #umbenennen der Spaltenheader
  colnames(singlebest)[2]<-"value"
  singlebest$value<-as.numeric(singlebest$value)
  return(singlebest)
}
# call function
pred_frq<-frq_all(best)
#########
frq5<-ggplot(data=pred_frq, aes(x=reorder( best_pred,value), y= value, fill=variable))+
  coord_flip()+
  geom_bar(stat="identity",  position = "dodge",  width=0.15)+
  scale_fill_manual(values=c("#660066","#FFCC33","#CC6600"), 
                    name=substr(basename(mods[x]), 1, nchar(basename(mods[x])) - 12))+
  theme(legend.justification=c(1,0), legend.position=c(1,0))+
  #axis.text.x=element_text(size=14),
  #axis.text.y=element_text(size=12), axis.title.y=element_text(size=12),
  #legend.text = element_text(size=12))+
  scale_y_discrete(breaks=c(0,2,4,6,8,10), limits=0:10)+
  labs(x = "", y = "frequency", title = "PLS Model predictor frequency")

grid.arrange(frq0,frq1, frq2,frq3, frq4, frq5)
