
source("D:/UNI/Master/MA/exploratorien/scripts/00_set_environment.R")
x<-readRDS(paste0(path_results, "complete_24_10.rds"))
library(ggplot2)
library(reshape2)
# choose response variable
resp <- c("SPECRICH","SHANNON","EVENESS")
resp <- "SHANNON"
resp <- "EVENESS"
resp <- "biomass_g"
resp <- "LUI_glb"

# take all predictors thta had been used in the models
pred <- x$AEG@meta$input$PREDICTOR_FINAL
dat <- x$AEG@data$input
dat <- dat[, colnames(dat) %in% c(resp, pred)]
dat <- melt(dat, id.vars = resp)
ggplot(data = dat, aes_string("value", resp, group = "variable")) + #oder tausche x/y Achse "value",resp
  geom_point() + geom_smooth() + facet_wrap(~ variable, scales = "free")
# the black dots in the plot show each model where a certain response
# is at a specific value of that predictor 


# extract the most used predictors for all 50 models in each exploratory
best<-x # if you use the complete model set, write it in a new df!

if (r=1){
  for(be in names(best)){
     best[[be]] <- lapply(1:50,function(i){ 
      best[[be]]@model[[1]][[r]][[i]]$model$fit$finalModel$xNames # this is different in ffs und rfe
     })
}
      Specrich_best_pred <- as.data.frame(table(unlist(best))) #this makes a list with all expl. merged! (2 entries!)
      Specrich_best_pred$response<-"SPECRICH"
      Specrich_best_pred[order(Specrich_best_pred$Freq),] #order after most frequently used predictors
    
  } else if (r=2){
  best[[be]] <- lapply(1:50,function(i){ 
    best_predictor= best[[be]]@model[[1]][[r]][[i]]$model$fit$finalModel$xNames # this is different in ffs und rfe
  
  Shan_best_pred <- as.data.frame(table(unlist(best))) #this makes a list with all expl. merged! (2 entries!)
  Shan_best_pred$response<-"SHANNON"
  Shan_best_pred[order(Shan_best_pred$Freq),] #order after most frequently used predictors
  })
} else (resp=="EVENESS"){
  best[[be]] <- lapply(1:50,function(i){ 
    best_predictor= best[[be]]@model[[1]][[resp]][[i]]$model$fit$finalModel$xNames # this is different in ffs und rfe
  
Shan_best_pred <- as.data.frame(table(unlist(best))) #this makes a list with all expl. merged! (2 entries!)
Shan_best_pred$response<-"SHANNON"
Shan_best_pred[order(Shan_best_pred$Freq),] #order after most frequently used predictors
  })
}
}


Specrich_best_pred <- as.data.frame(table(unlist(best))) #this makes a list with all expl. merged! (2 entries!)
Specrich_best_pred$response<-"SHANNON"
Specrich_best_pred[order(Specrich_best_pred$Freq),] 
}
# if you want to keep track for the different exploratories, use this:
singlebest<-best
singlebest$AEG<- as.data.frame(table(unlist(singlebest$AEG)))
colnames(singlebest$AEG)<- c("AEG_singlebest_pred","AEG_Freq")
singlebest$HEG<- as.data.frame(table(unlist(singlebest$HEG)))
colnames(singlebest$HEG)<- c("HEG_singlebest_pred","HEG_Freq")
singlebest$SEG<- as.data.frame(table(unlist(singlebest$SEG)))
colnames(singlebest$SEG)<- c("SEG_singlebest_pred","SEG_Freq")
# but the data for each explo. has a differnet extent, so we leave it in a 
#list and just access the singlebest 5 most frequent
for(be in names(singlebest)){
  singlebest[[be]] <- lapply(1:length(singlebest[[be]]),function(f){ 
    singlebest[[be]]<-head(singlebest[[be]][order(singlebest[[be]][f], decreasing  = TRUE),],5 )
  })
}
specrichsing<-bind_cols(singlebest$AEG[2],singlebest$HEG[2],singlebest$SEG[2])
specrichsing$response<-"SPECRICH"


