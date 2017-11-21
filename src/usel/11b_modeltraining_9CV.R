source("D:/UNI/Master/MA/exploratorien/scripts/project_biodiv_rs/src/usel/00_b_set_environment_start_modeltrain.R")

x<-readRDS(paste0(path_rdata,"preprocessing/gpm_obj_24pred.rds"))
pred<-x[[1]]@meta$input$PREDICTOR_FINAL

# overwrite Model for 9CV (leave 5plots out)
specpred<-pred[c(3,4,6,7,9,10,24)] #LUI
specpred<-pred[c(7,9,12,13,15,16,21)] #Shan
specpred<-pred[c(4,9,12,15,16)] #Even
LUI<-x
for(be in names(LUI)){
  LUI[[be]]@meta$input$PREDICTOR_FINAL<-specpred
}
belc <- c("AEG", "HEG", "SEG")
newspec<- lapply(belc, function(be){
  SPE <- splitMultRespLSO(LUI[[be]], nbr = 5)
})
names(newspec)<-belc
LUI<-newspec

for(be in names(LUI)){
  act_gpm_selected <- LUI[[be]]
  
  act_gpm_selected <- trainModel(x = act_gpm_selected,
                                 n_var = NULL,             
                                 mthd = "pls", #rf, gam, glmboost
                                 mode = "rfe",
                                 seed_nbr = 11, 
                                 cv_nbr = 9,
                                 var_selection = "indv",
                                 response_nbr = c(1:3,18),
                                 filepath_tmp = path_temp
  )
  LUI[[be]] <- act_gpm_selected
}
saveRDS(LUI, file = paste0(path_results, "pls_rfe_9CV.rds"))

#check for NULL values in the model
s<-lapply(LUI, function(be){
  e<-lapply(be@model$pls_ffs[[1]], function(p){
    p$testing$PREDICTED
  })
})
lapply(s, function(t){
  length(t[!unlist(lapply(t,is.null))])
})
