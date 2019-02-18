
source("C:/Users/uselig/Documents/GitHub/project_biodiv_rs/src/00_paths_libraries.R")

# Compile grassland diversity and remote sensing -------------------------------

# read veg.data
vegrel15 <- readRDS(file = paste0(path_rdata_pre, "vegrel15.rds"))

# read predictors
re_df<- readRDS(paste0(path_rdata_pre, "re2015_AprilMAIN_MarDIFF_predictors.rds"))

veg_re_g <- merge(vegrel15, re_df, by.x = "EPID",
                  by.y = "EPID")
max(colSums(is.na(veg_re_g)))

###############################
#################################################################################

### compile GPM object for RE with 45/5 (precitors werden hier schon ausgefiltert, dass LUI nicht drin ist)
belc <- c("AEG", "HEG", "SEG")
veg_re_g_gpm_indv <- lapply(belc, function(b){
  act_veg_re_g <- veg_re_g[veg_re_g$EP == b, ] # navigates to only character:"AEG", "HEG, "SEG"
  
  col_selector <- which(names(act_veg_re_g) == "EPID")
  
  col_diversity <- c(grep("specrich|shannon|eveness", 
                          tolower(names(act_veg_re_g))),
                     seq(grep("vegetation_height_mean_cm",
                              names(act_veg_re_g)),
                         grep("biomass_g",
                              names(act_veg_re_g))),
                     seq(grep("G_std_glb",
                              names(act_veg_re_g)),
                         grep("LUI_reg",
                              names(act_veg_re_g))))
  
  col_precitors <- seq(grep("LUI_reg", names(act_veg_re_g)), # erster predictor in Tabelle
                       grep("RaosQ_Haralick_Correlation", names(act_veg_re_g))) #letzter predictor mit meta (ohne: pca_Long_Run_Low_Grey.Level_Emphasis.b1r50o1_var)
  
  col_precitors <- col_precitors[-grep("ep|id|type", tolower(names(act_veg_re_g[, col_precitors])))]
  
  col_meta <- seq(length(act_veg_re_g))[-c(col_selector, col_diversity, col_precitors)]
  
  meta <- createGPMMeta(act_veg_re_g, type = "input",
                        selector = col_selector,
                        response = col_diversity,
                        predictor = col_precitors,
                        meta = col_meta)
  veg_re_g_gpm <- gpm(act_veg_re_g, meta, scale = FALSE)
  
  # Clean predictor variables 
  veg_re_g_gpm <- cleanPredictors(x = veg_re_g_gpm, nzv = TRUE,
                                  highcor = TRUE, cutoff = 0.80)
  
  # Compute resamples following a leave location out approach
  veg_re_g_gpm <- splitMultRespLSO(x = veg_re_g_gpm, nbr = 5)
})
names(veg_re_g_gpm_indv) <- belc

######################################################################################
#################################################
############################
# SPECIFY the model

# MODEL 1: RE predictors "model 1a": predictors are specific to the BE (dont change final predictors)
mod1a4505<-veg_re_g_gpm_indv

## choose FINAL predictors only RE (remove LUI) "LUI_reg"
for(be in names(mod1a4505)){
  mod1a4505[[be]]@meta$input$PREDICTOR_FINAL<- mod1a4505[[be]]@meta$input$PREDICTOR_FINAL[-1]
}

# save the model
saveRDS(mod1a4505, paste0(path_rdata, "gpm_models_paper/RE_Model_1a4505.rds"))
#________________________________________________________________________________________________
#__________________________________________


