
source("D:/UNI/Master/MA/exploratorien/scripts/00_set_environment.R")
# a independet compilation model
veg_re_g <- readRDS(paste0(path_rdata, "preprocessing/veg_re_g.rds"))


levels(veg_re_g$explo)<-c("AEG","HEG","SEG")
veg_re_g$explo<- as.character(veg_re_g$explo)

col_selector <- which(names(veg_re_g) =="explo") #alternativ EPID

col_diversity <- c(seq(which(names(veg_re_g) == "SPECRICH"),
                       which(names(veg_re_g) == "EVENESS")),
                   (which(names(veg_re_g) == "biomass_g")),
                   which(names(veg_re_g) == "LUI_glb"))

col_precitors <-seq(which(names(veg_re_g) == "GLI_mean"),
                    which(names(veg_re_g) == "pca_Long_Run_Low_Grey.Level_Emphasis.b1r50o1_var"))

#col_precitors <- col_precitors[-grep("ep|id|type", tolower(names(act_veg_re_g[, col_precitors])))]

col_meta <- seq(length(veg_re_g))[-c(col_selector, col_diversity, col_precitors)]

meta <- createGPMMeta(veg_re_g, type = "input",
                      selector = col_selector,
                      response = col_diversity,
                      predictor = col_precitors,
                      meta= col_meta)

veg_re_g_gpm <- gpm(veg_re_g, meta, scale = FALSE)

# Clean predictor variables
# read the old varibale set with 24 redictors
gpmobj<-readRDS(paste0(path_rdata, "preprocessing/gpm_obj_24pred.rds"))
pred<-gpmobj[[1]]@meta$input$PREDICTOR_FINAL #schriebe vektor und extrahiere die nötigen Prediktoren
#for the specmodel we use these predictors
specpred<-pred[c(3,4,6,7,9,10,15,16)]
veg_re_g_gpm@meta$input$PREDICTOR_FINAL<-specpred

# Compute resamples following a leave location out approach
#veg_re_g_gpm <- splitMultRespLSO(x = veg_re_g_gpm, nbr = 50)
testA<-c("HEG","SEG")
veg_re_g_gpm1 <- splitMultRespLSO(x = veg_re_g_gpm, 
                                 #response=col_diversity,
                                 #resamples=51:150,
                                 #selector= veg_re_g_gpm@data$input$EPID[1:50],
                                 #use_selector=T,
                                 nbr = 1)

selector = veg_re_g_gpm@meta$input$SELECTOR
veg_re_g_gpm@data$input[, "explo"]

veg_re_g_gpm1@meta$input$TRAIN_TEST


sum(veg_re_g_gpm@data$input$explo=="AEG")

# trying to modify the split function

mysplit<-function(x, nbr = nbr, selector= selector){
  
  smr <- splitMultRespLSO(x = x@data$input, 
                          
                          response = x@meta$input$RESPONSE_FINAL,
                          
                          selector = x@data$input$explo,
                          
                          nbr = nbr)
  
  x@meta$input$TRAIN_TEST <- smr[[1]]
  
  x@meta$input$TRAIN_TEST_NSMPLS <- smr[[2]]
  
  return(x)
}
testA<-"AEG")
veg_re_g_gpm <- mysplit(veg_re_g_gpm, nbr=1), 
                        nbr=50),
                        selector=  1:50
                                 #use_selector=T,
                                 )


saveRDS(veg_re_g_gpm, file = paste0(path_rdata, "gpm_obj_24pred.rds"))

# try wihout a split (shuffles ramdom even with resample_nr. and LOO50)
act_gpm_selected<-veg_re_g_gpm
act_gpm_selected <- trainModel(x = act_gpm_selected,
                               n_var = NULL, 
                               mthd = "pls",
                               mode = "ffs",
                               seed_nbr = 11, 
                               cv_nbr = 5,
                               var_selection = "indv",
                               response_nbr = 1,
                               #resample_nbr= 51:150,
                               filepath_tmp = path_temp)

act_gpm_selected@model$pls_ffs[[1]]
