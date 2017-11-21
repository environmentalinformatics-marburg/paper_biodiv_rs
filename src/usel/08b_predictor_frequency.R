source("D:/UNI/Master/MA/exploratorien//scripts/00_set_environment.R")
x<-readRDS(paste0(path_results, "complete_24_11.rds"))

library(ggplot2)
library(reshape2)
# choose response variable
resp <- "SPECRICH"
resp <- "SHANNON"
resp <- "EVENESS"
resp <- "biomass_g"
resp <- "LUI_glb"

# take all predictors thta had been used in the models
pred <- x$HEG@meta$input$PREDICTOR_FINAL
dat <- x$HEG@data$input
dat <- dat[, colnames(dat) %in% c(resp, pred)]
dat <- melt(dat, id.vars = resp)
ggplot(data = dat, aes_string("value", resp, group = "variable")) + #oder tausche x/y Achse "value",resp
  geom_point() + geom_smooth() + facet_wrap(~ variable, scales = "free")
# the black dots in the plot show each model where a certain response
# is at a specific value of that predictor 

# FOR FFS
{# r = 3
# while(x$AEG@model[[2]][[2]][[1]]$response != resp){
#   r <- r + 1
#    
# }
# 
# x$AEG@model[[2]][[r]][[1]]$response
# best_pred <- lapply(1:50, function(i){
#   x$AEG@model[[2]][[r]][[i]]$model$finalModel$xNames # this is different in ffs und rfe
# })
# best_pred <- as.data.frame(table(unlist(best_pred)))
# best_pred[order(best_pred$Freq),]
}


# for FFS (me)
best<-x
r= 1 #replace with the required response
for(be in names(best)){
  best[[be]] <- lapply(1:10,function(i){ 
    best[[be]]@model[[4]][[r]][[i]]$model$finalModel$xNames # this is different in ffs und rfe
  })
}
Specrich_best_pred <- as.data.frame(table(unlist(best))) #this makes a list with all expl. merged! (2 entries!)
Specrich_best_pred$response<-"LUI"
Specrich_best_pred[order(Specrich_best_pred$Freq),] #order after most frequently used predictors
pls_ffs_mergeLUI<-Specrich_best_pred
# for RFE (me)
best<-x
for(be in names(best)){
  best[[be]] <- lapply(1:50,function(i){ 
    best[[be]]@model[[1]][[r]][[i]]$model$fit$finalModel$xNames # this is different in ffs und rfe
  })
}
Specrich_best_pred <- as.data.frame(table(unlist(best))) #this makes a list with all expl. merged! (2 entries!)
Specrich_best_pred$response<-"SPECRICH"
Specrich_best_pred[order(Specrich_best_pred$Freq),] #order after most frequently used predictors

# -------- if you want to keep track for the different exploratories, use this:--
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
    singlebest[[be]]<-head(singlebest[[be]][order(singlebest[[be]][f], decreasing  = TRUE),],4 ) #or 3
  })
}
specrichsing<-bind_rows(singlebest$AEG[2],singlebest$HEG[2],singlebest$SEG[2])
specrichsing$response<-"SPECRICH"
specrichsing$be<-NA

write.csv(specrichsing,paste0(path_stats,"RF_freq.csv"))
saveRDS(specrichsing,paste0(path_stats,"pred_frequency_PLS.rds"))
pls_ffs_singleSPEC<-pls_ffs_single1
rm(pls_ffs_single1)
pls_ffs_mergeSPEC<-pls_ffs_merge1
rm(pls_ffs_merge1)


# ------- if you want the sum of alle the beste frequent preictors
Specrich_best_pred <- as.data.frame(table(unlist(best))) #this makes a list with all expl. merged! (2 entries!)
Specrich_best_pred$response<-"SPEC"
Specrich_best_pred[order(Specrich_best_pred$Freq),] #order after most frequently used predictors
pls_ffs_mergeLUI<-Specrich_best_pred

# for RFE (me)
best<-x
for(be in names(best)){
  best[[be]] <- lapply(1:50,function(i){ 
    best[[be]]@model[[1]][[r]][[i]]$model$fit$finalModel$xNames # this is different in ffs und rfe
  })
}
Specrich_best_pred <- as.data.frame(table(unlist(best))) #this makes a list with all expl. merged! (2 entries!)
Specrich_best_pred$response<-"SPECRICH"
Specrich_best_pred[order(Specrich_best_pred$Freq),] #order after most frequently used predictors

