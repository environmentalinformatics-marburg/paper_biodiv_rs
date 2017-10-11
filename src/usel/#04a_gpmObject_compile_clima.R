# Set path ---------------------------------------------------------------------
source("F:/exploratorien/scripts/00_set_environment.R")


compute = TRUE

# Grassland diversity and climate --------------------------------------------
vegrel0815_div <- readRDS(file = paste0(path_rdata, "vegrel0815_div.rds"))
meta <- readRDS(paste0(path_rdata, "meta.rds"))


veg_meta_g <- merge(vegrel0815_div, meta, by.x = c("EPID", "Year"),
                    by.y = c("plotID", "g_a"))
veg_meta_g <- veg_meta_g[!is.na(veg_meta_g$LUI_reg), ]
veg_meta_g <- veg_meta_g[, -grep("EP|Ta_200_max|Ta_200_min|rH_200|datetime", 
                                 colnames(veg_meta_g))[-1]]
veg_meta_g <- veg_meta_g[complete.cases(veg_meta_g), ]
saveRDS(veg_meta_g, paste0(path_rdata, "veg_meta_g.rds"))
veg_meta_g<-readRDS(paste0(path_rdata, "veg_meta_g.rds"))

stack_cor<-cor(veg_meta_g, use="everything", method="spearman")

# Prepare gpm data set used for remote sensing prediction study ----------------
## set dataframes and selectors which are needed to use the gpm model
col_selector <- which(names(veg_meta_g) == "EPID") #selectiere nach der ID
col_meta <- c(grep("year|g_belc|g_pa", tolower(names(veg_meta_g)))) #schreibe vektor- suche nach den 3 Eintr?gen year,g... und zus?tzlich schreibe alle colnames klein
col_diversity <- c(grep("specrich|shannon|eveness", tolower(names(veg_meta_g)))) # wie "col_meta"
col_precitors <- seq(length(veg_meta_g))[-c(col_selector, col_meta, col_diversity)] #f?r die pr?diktoren sollen selector,meta und div. nicht vorkommen

#create a metainformation dataset in a gpm format
#Geospatial predictive modeling using parameterized and unparameterized models.
meta <-  createGPMMeta(veg_meta_g, type = "input",
                      selector = col_selector,
                      response = col_diversity, #abh?ngige Variable ist biodiv.
                      predictor = col_precitors, #unabh?ngige variable sind alle au?er biodiv, year und Ortkennzeichen (AEG...)
                      meta = col_meta)
veg_meta_g_gpm <- gpm(veg_meta_g, meta, scale = FALSE)


# Clean predictor variables ----------------------------------------------------
veg_meta_g_gpm <- cleanPredictors(x = veg_meta_g_gpm, nzv = TRUE,
                                  highcor = TRUE, cutoff = 0.90)

# Compile model training and evaluation dataset --------------------------------
# Compute resamples following a leave location out approach
veg_meta_g_gpm <- splitMultRespLSO(x = veg_meta_g_gpm, nbr = 1)

saveRDS(veg_meta_g_gpm, file = paste0(path_rdata, "veg_meta_g_gpm.rds"))
