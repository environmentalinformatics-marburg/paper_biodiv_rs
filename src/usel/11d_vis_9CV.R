# visualisation ideas
cbPalette<-c("#ffffff", "#525252", "#bdbdbd") #bnw palette

mr_all <- c("SPECRICH", "SHANNON", "EVENESS")
mr_all<-"SHANNON"

# lets try it with SPEC only
specerror<-errors[errors$response=="SHANNON",]
specerror<-specerror[specerror$be=="AEG",]

ggplot(data = specerror[specerror$smpl %in% seq(10) & specerror$variable == "r_squared",], 
       aes(x = response, y = value, color = stat)) + 
  geom_boxplot() + 
  geom_point(data = specerror[specerror$smpl == "Rmean" & specerror$variable == "r_squared",],
             aes(x = response, y = value, color = "Mean R2 from sets")) + 
  geom_point(data = specerror[specerror$smpl == "Rsum" & specerror$variable == "r_squared",],
             aes(x = response, y = value, color = stat)) + 
  theme_bw()

#thoma's plot
ggplot(data = specerror[specerror$smpl %in% seq(10) & specerror$variable == "r_squared",], 
       aes(x = response, y = value, color = stat)) + 
  geom_boxplot() + 
  geom_point(data = specerror[specerror$smpl == "Rmean" & specerror$variable == "r_squared",],
             aes(x = response, y = value, color = "Mean R2 from sets")) + 
  geom_point(data = specerror[specerror$smpl == "Rsum" & specerror$variable == "r_squared",],
             aes(x = response, y = value, color = "R2 over all")) + 
  theme_bw()



# with all exploratories
train_pls_ffs<-readRDS("results/train_pls_9CV_3resp_7_pred.rds") 
test_pls_ffs<-readRDS("results/test_pls_9CV_3resp_7_pred.rds") 
train_pls_ffs<-mstat
test_pls_ffs<-tstat_errors

ggplot(data = train_pls_ffs[train_pls_ffs$response %in% mr_all,], 
       aes(x = response, y = rmse, fill = be)) + 
  geom_boxplot() +
  geom_point(data = test_pls_ffs[test_pls_ffs$smpl=="Rmean"& test_pls_ffs$model_response== mr_all,], 
             aes(x = model_response, y = rmse, color=be),  shape=21,color = "black", stroke=1) +
  labs(title = "PLS_FFS R squared of the train and test data set compared with the actual prediction (colored dots)", 
       x = NULL, y = "R squared", fill = "Expl") + 
  guides(color=FALSE) + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw()
