# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  source("D:/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
} else {
  source("/media/permanent/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
}

compute = TRUE

# Read and pre-process biodiversity data ---------------------------------------
if(compute){
  vegrel14 <- readBExpVegReleves(
    paste0(path_releves, "19807_header data vegetation releves 2014_1.1.7/19807.txt"))

  vegrel15 <- readBExpVegReleves(
    paste0(path_releves, "19809_header data vegetation releves 2015_1.1.5/19809.txt"))

  vegrel0815 <- readBExpVegReleves(
    paste0(path_releves, "19686_vegetation releves EP 2008-2015_1.2.5/19686.txt"))

  vegrel0815_div <- compSpecRichBExpVegReleves(vegrel0815)

  vegrel14 <- merge(vegrel0815_div[vegrel0815_div$Year == 2014, ],
                    vegrel14, by = "EPID")

  vegrel15 <- merge(vegrel0815_div[vegrel0815_div$Year == 2015, ],
                    vegrel15, by = "EPID")

  saveRDS(vegrel0815_div, file = paste0(path_rdata, "vegrel0815.rds"))
  saveRDS(vegrel14, file = paste0(path_rdata, "vegrel14.rds"))
  saveRDS(vegrel15, file = paste0(path_rdata, "vegrel15.rds"))
} else {
  vegrel0815_div <- readRDS(file = paste0(path_rdata, "vegrel0815.rds"))
  vegrel14 <- readRDS(file = paste0(path_rdata, "vegrel14.rds"))
  vegrel15 <- readRDS(file = paste0(path_rdata, "vegrel15.rds"))
}


# Read LUI data ----------------------------------------------------------------
if(compute){
  lui_glb <- readBExpLUI(
    paste0(path_lui, "LUI_glob_sep_23.12.2016+104549_standardized.txt"))
  colnames(lui_glb)[3:6] <- paste0(colnames(lui_glb)[3:6], "_glb")

  lui_reg <- readBExpLUI(
    paste0(path_lui, "LUI_reg_sep_23.12.2016+103914_standardized.txt"))
  colnames(lui_reg)[3:6] <- paste0(colnames(lui_reg)[3:6], "_reg")

  lui <- merge(lui_glb, lui_reg)
  lui$EP <- substr(lui$EP.Plotid, 1, 3)

  vegrel0815_div <- merge(vegrel0815_div, lui, by.x = c("EPID", "Year"),
                    by.y = c("EP.Plotid", "year"), all.x = TRUE)
  vegrel14 <- merge(vegrel14, lui, by.x = c("EPID", "Year"),
                    by.y = c("EP.Plotid", "year"), all.x = TRUE)
  vegrel15 <- merge(vegrel15, lui, by.x = c("EPID", "Year"),
                    by.y = c("EP.Plotid", "year"), all.x = TRUE)

  saveRDS(vegrel0815_div, file = paste0(path_rdata, "vegrel0815_div.rds"))
  saveRDS(vegrel14, file = paste0(path_rdata, "vegrel14.rds"))
  saveRDS(vegrel15, file = paste0(path_rdata, "vegrel15.rds"))
} else {
  vegrel0815_div <- readRDS(file = paste0(path_rdata, "vegrel0815_div.rds"))
  vegrel14 <- readRDS(file = paste0(path_rdata, "vegrel14.rds"))
  vegrel15 <- readRDS(file = paste0(path_rdata, "vegrel15.rds"))
}


# Read stand structural attributes and SMI data --------------------------------
if(compute){
  ssa <- readBExpStandStruc(paste0(
    path_forest,
    "20106_Forest_EP_Stand_structural_attributes_core_SSA_1.2.2/",
    "20106.txt"))

  smi <- readBExpSMI(
    paste0(path_forest,
           "17746_Forest_EP_SMI_Silvicultural_management_intensity_index_1.2.2/",
           "17746.txt"))

  sdiv <- merge(smi, ssa)
  sdiv$SMId <- as.numeric(as.character(sdiv$SMId))
  sdiv$SMIr <- as.numeric(as.character(sdiv$SMIr))
  sdiv$SMI <- as.numeric(as.character(sdiv$SMI))
  saveRDS(sdiv, file = paste0(path_rdata, "sdiv.rds"))
} else {
  sdiv <- readRDS(file = paste0(path_rdata, "sdiv.rds"))
}
