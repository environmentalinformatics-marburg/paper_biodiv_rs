# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  source("D:/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
} else {
  source("/media/permanent/active/exploratorien/project_biodiv_rs/src/00_set_environment.R")
}

compute = TRUE

# Read and pre-process biodiversity data ---------------------------------------
adf_clim <- readRDS(file = paste0(path_rdata, "adf_clim.rds"))
adf_rs <- readRDS(file = paste0(path_rdata, "adf_rs.rds"))

adf <- adf_clim
g1 <- ggplot(data = adf, aes(x = LUI_reg, y = SPECRICH, color = EP)) +
  geom_point() +
  geom_smooth()

g2 <- ggplot(data = adf, aes(x = LUI_glb, y = SPECRICH, color = EP)) +
  geom_point() +
  geom_smooth()

grid.arrange(g1, g2, ncol = 2)


g1 <- ggplot(data = adf, aes(x = LUI_glb, y = SPECRICH, color = EP)) +
  geom_point() +
  geom_smooth()

g2 <- ggplot(data = adf, aes(x = LUI_glb, y = SHANNON, color = EP)) +
  geom_point() +
  geom_smooth()

grid.arrange(g1, g2, ncol = 2)


g1 <- ggplot(data = adf, aes(x = Ta_200, y = SHANNON, color = EP)) +
  geom_point() +
  geom_smooth()

g2 <- ggplot(data = adf, aes(x = SM_10, y = SHANNON, color = EP)) +
  geom_point() +
  geom_smooth()

grid.arrange(g1, g2, ncol = 2)


g1 <- ggplot(data = adf, aes(x = Ta_200, y = LUI_glb, color = EP)) +
  geom_point() +
  geom_smooth()

g2 <- ggplot(data = adf, aes(x = SM_10, y = LUI_glb, color = EP)) +
  geom_point() +
  geom_smooth()

grid.arrange(g1, g2, ncol = 2)
