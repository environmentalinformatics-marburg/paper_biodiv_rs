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

  saveRDS(vegrel14, file = paste0(path_rdata, "vegrel14.rds"))
  saveRDS(vegrel15, file = paste0(path_rdata, "vegrel15.rds"))
} else {
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

  vegrel14 <- merge(vegrel14, lui, by.x = c("EPID", "Year"),
                    by.y = c("EP.Plotid", "year"))
  vegrel15 <- merge(vegrel15, lui, by.x = c("EPID", "Year"),
                    by.y = c("EP.Plotid", "year"))

  saveRDS(vegrel14, file = paste0(path_rdata, "vegrel14.rds"))
  saveRDS(vegrel15, file = paste0(path_rdata, "vegrel15.rds"))
} else {
  vegrel14 <- readRDS(file = paste0(path_rdata, "vegrel14.rds"))
  vegrel15 <- readRDS(file = paste0(path_rdata, "vegrel15.rds"))
}


# Read RapidEye data -----------------------------------------------------------
if(compute){
  fns <- c("2015-04-24T110941_RE1_1B-NAC_20835999_303429_ortho_alb.tif",
           "2015-04-24T110857_RE1_1B-NAC_20835994_303428_ortho_hai.tif",
           "2015-04-10T111339_RE1_1B-NAC_20835979_303426_ortho_sch.tif")
  re <- lapply(fns, function(f){
    stack(paste0(path_re, f))
  })
  names(re) <- c("Alb", "Hai", "Sch")
}


# Read plot polygon data -------------------------------------------------------
if(compute){
  fns <- c("polyAlbEp.shp", "polyHaiEp.shp", "polySchEp.shp")
  poly <- lapply(seq(length(fns)), function(i){
    poly <- readBExpPoly(shp = paste0(path_plots, fns[i]), crs = crs(re[[i]]))
    poly[grepl("EG", poly@data$EP),]
  })
  names(poly) <- c("Alb", "Hai", "Sch")

  # Create buffer
  poly_buffer <- lapply(poly, function(p){
    gBuffer(p, width = 50, byid = TRUE)
  })
}


# Prepare RapidEye data for plots ----------------------------------------------
if(compute){
  re_plots <- lapply(seq(length(re)), function(i){
    re_plots <- lapply(seq(length(poly_buffer[[i]])), function(p){
      plot <- crop(re[[i]], poly_buffer[[i]][p,])
      names(plot) <-paste0("re0", seq(5))
      return(plot)
    })
  })
  saveRDS(re_plots, file = paste0(path_rdata, "re_plots"))
} else {
  re_plots <- readRDS(file = paste0(path_rdata, "re_plots.rds"))
}


# Merge RapidEye and plot data -------------------------------------------------
if(compute){
  re_ext <- lapply(seq(length(re_plots)), function(i){
    ext <- lapply(seq(length(re_plots[[i]])), function(j){
      ext <- extract(re_plots[[i]][[j]], poly[[i]][j,], df = TRUE)
      cbind(ext, EP = poly[[i]][j,]@data$EP)
    })
    do.call("rbind", ext)
  })
  re_ext <- do.call("rbind", re_ext)
  saveRDS(re_ext, file = paste0(path_rdata, "re_ext.rds"))

  vegrel15 <- merge(vegrel15, re_ext)
  saveRDS(re_ext, file = paste0(path_rdata, "vegrel15.rds"))
} else {
  re_ext <- readRDS(file = paste0(path_rdata, "re_ext.rds"))
  vegrel15 <- readRDS(file = paste0(path_rdata, "vegrel15.rds"))
}
