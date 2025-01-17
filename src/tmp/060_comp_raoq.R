# Compute Rao's Q on original bands stack, scaled vegetation indices stack,
# and mean distance from centroid band and vegetation index data.

source("C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R")
# if(length(showConnections()) == 0){
#   cores = 2
#   cl = parallel::makeCluster(cores)
#   doParallel::registerDoParallel(cl)
# }

dir.create(paste0(path_hyp_raoq), showWarnings = FALSE)
windows = c(3)

hd_files = c(list.files(path_hyp_nrm, recursive = FALSE, full.names = TRUE), 
             list.files(path_hyp_vegidcs, recursive = FALSE, full.names = TRUE),
             list.files(path_hyp_kmdc, recursive = FALSE, full.names = TRUE))

h_meta = readRDS(paste0(path_meta, "hyp_meta.rds"))

foreach (i = seq(length(hd_files))) %do% {
  filename = basename(hd_files[i])
  productid = paste0(substr(filename, 1, nchar(filename)-4), "_raoq")
  
  r = readRDS(hd_files[[i]])
  
  # Scale vegetation indicies
  if(grepl("vegidcs", filename)){
    r = scale(r, center = TRUE, scale = TRUE)
  }
  
  # ra = aggregate(r, fact=2, fun=mean)
  for(w in windows){
    raomatrix <- spectralrao(as.list(r), 
                             mode="multidimension", 
                             distance_m="euclidean", 
                             window=w, 
                           shannon=FALSE, 
                             debugging=TRUE, 
                             simplify=3)
    raor = setValues(r[[1]], raomatrix[[1]])
    names(raor) = productid
    saveRDS(raor, file = paste0(path_hyp_raoq, 
                                productid, "_", w, ".rds"))
  }
} 


# stopCluster(cl)

# Visually check data
visCheck(datapath = path_hyp_raoq, polygonfile = paste0(path_plots, "BPolygon.shp"), band = 1)

