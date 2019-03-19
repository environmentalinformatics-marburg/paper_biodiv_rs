# To get the best predictors in all BE we have to go 2 steps, where 1 is a general predictor extraction
# read model result
source("C:/Users/uselig/Documents/GitHub/project_biodiv_rs/src/00_paths_libraries.R")
mods<-list.files(path_results, full.names=TRUE,pattern=glob2rx("*rds"))

mr_all<-c("Species richness","Shannon","Eveness","Cover Shrubs","Cover bare soil","Biomass")
models=c("Model_5_RECLIMALUI","Model_4_CLIMALUI","Model_3_RELUI","Model_2_LUI","Model_1_RE","Model_0_RE")

# make 2 empty df. Ich habe bisher keine Lösung gefunden den loop für alle responses zu machen,
# deswegen muss das in Zeile 15 (r=resp.nr)
# eingetragen werden und dann immer an den df pred_frq_all angehängt werden. Außerdem funktioniert der loop nur
# wenn es min. 3prediktoren gab, bei dem LUI Model sind es nur 2, deswegen habe ich das händisch angefügt
pred_frq_all<-NULL
pred_frq_sin<-NULL
for (x in seq(mods)){
  r=6
  print(mods[x])
  mod<-readRDS(mods[x])
  # get all predictors of all BE after ffs processing
  best<-mod
  for(be in names(best)){
    best[[be]] <- lapply(1:5,function(i){ 
      #r= 1 #replace with the required response
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
        singlebest[[be]]<-head(singlebest[[be]][order(singlebest[[be]][f], decreasing  = TRUE),],3 ) #or 3
      })
    }
    singlebest<-bind_rows(singlebest$AEG[2],singlebest$HEG[2],singlebest$SEG[2])
    
    #neat table: use df on HEG col
    # all columns into charcters
    singlebest$HEG_singlebest_pred<-as.character(singlebest$HEG_singlebest_pred)
    singlebest$AEG_singlebest_pred<-as.character(singlebest$AEG_singlebest_pred)
    singlebest$SEG_singlebest_pred<-as.character(singlebest$SEG_singlebest_pred)
    
    #alle predicctoren in eine Spalte
    singlebest$HEG_singlebest_pred[1:3]<-singlebest$AEG_singlebest_pred[1:3] #prediktoren rüber schieben
    singlebest$variable[1:3]<-"AEG"
    singlebest$HEG_Freq[1:3]<-singlebest$AEG_Freq[1:3] #Frequenz rüber schieben
    
    singlebest$HEG_singlebest_pred[7:9]<-singlebest$SEG_singlebest_pred[7:9]
    singlebest$variable[7:9]<-"SEG"
    singlebest$HEG_Freq[7:9]<-singlebest$SEG_Freq[7:9]
    
    singlebest$variable[4:6]<-"HEG" # neue Spalte für Zuordnung Explo.
    
    #unnötige Spalten löschen
    singlebest<-singlebest[,-c(1:2,5:6)]
    colnames(singlebest)[1]<-"best_pred" #umbenennen der Spaltenheader
    colnames(singlebest)[2]<-"value"
    singlebest$value<-as.numeric(singlebest$value)
    return(singlebest)
  }
  pred_frq<-frq_all(best)
  #pred_frq<-do.call("rbind", pred_frq)
  
  pred_frq$model = substr(basename(mods[x]), 1, nchar(basename(mods[x])) - 12)
  pred_frq$response<-mr_all[r]
  
  
  pred_frq_sin<- rbind(pred_frq, pred_frq_sin)
}

pred_frq_all<-rbind(pred_frq_sin, pred_frq_all)

######
###plot
#######

i= list() # create empty list for all models

for (x in seq(models)){
  i[[x]]<- ggplot(data= pred_frq_all[pred_frq_all$response %in% mr_all[x] ,],
                  aes( x= best_pred,y=value, fill=variable))+
    coord_flip()+
    geom_bar(stat="identity",  position = "dodge",  width=0.15)+
    scale_fill_manual(values=c("#660066","#FFCC33","#CC6600"))+
    scale_y_discrete(breaks=c(1,2,3,4,5), limits=0:5)+
    labs(title = mr_all[x],
         x = NULL, y = "PLS predictor frequency", fill=element_blank())+
    facet_wrap(~model, scales="free_y")+
    theme(strip.text.x = element_text(size = 7),
          axis.text.x=element_text(size=8),
          axis.text.y=element_text( size=7),
          plot.title=element_text(size=11, face="bold", hjust=0.5),
          legend.key.size = unit(2, "mm"),
          legend.position="right"
          #print(i[[x]])
    )
}
plot(i[[1]])
do.call(grid.arrange, i)


###################################################################################################
