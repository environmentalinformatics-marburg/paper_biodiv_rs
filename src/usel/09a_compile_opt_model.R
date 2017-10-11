# new and optimized model cimpilation specific for certain responses

gpmobj<-readRDS(paste0(path_rdata, "preprocessing/gpm_obj_24pred.rds"))
pred<-gpmobj[[1]]@meta$input$PREDICTOR_FINAL #schriebe vektor und extrahiere die nötigen Prediktoren

# do the different responses
  # check the entire list and get te digits from predictor positions
gpmobj[[1]]@meta$input$PREDICTOR_FINAL

#specrich
specpred<-pred[c(3,4,6,7,9,10,15,16)]
SPECmodel<-gpmobj
for(be in names(SPECmodel)){
      SPECmodel[[be]]@meta$input$PREDICTOR_FINAL<-specpred
      }
saveRDS(SPECmodel,paste0(path_rdata,"SPECmodel.rds"))

#LUI
LUIpred<-pred[c(3,4,6,7,9,10,24)]
LUImodel<-gpmobj
for(be in names(LUImodel)){
  LUImodel[[be]]@meta$input$PREDICTOR_FINAL<-LUIpred}
saveRDS(LUImodel,paste0(path_rdata,"LUImodel.rds"))

#shannon
Shanpred<-pred[c(7,9,12,13,15,16,21)]
Shanmodel<-gpmobj
for(be in names(Shanmodel)){
  Shanmodel[[be]]@meta$input$PREDICTOR_FINAL<-Shanpred}
saveRDS(Shanmodel,paste0(path_rdata,"Shanmodel.rds"))
#eveness
EVpred<-pred[c(4,9,12,15,16)]
EVmodel<-gpmobj
for(be in names(EVmodel)){
  EVmodel[[be]]@meta$input$PREDICTOR_FINAL<-EVpred}
saveRDS(EVmodel,paste0(path_rdata,"EVmodel.rds"))

#biomass
biopred<-pred[c(3,4,6,7,9,12,13,16,24)]
biomodel<-gpmobj
for(be in names(biomodel)){
  biomodel[[be]]@meta$input$PREDICTOR_FINAL<-biopred}
saveRDS(biomodel,paste0(path_rdata,"biomodel.rds"))
