# trying to access clim model even with missing values

  #take the mean of predicted values because we can not run function comregrtest with NA values, 
  # so instead for the 5th row we assume the mean of predicted vals

veg<-readRDS(paste0(path_rdata,"vegrel15.rds"))
vega<-veg[veg$explo=="ALB",]
vega<-veg[1:3]

# rearange model selector, so we can easily attach the value in response and predicted in the end of df
act_gpm_selected@model$pls_ffs[[1]][[10]]$testing$SELECTOR<-c("AEG26", "AEG39" ,"AEG42", "AEG47", "AEG33") #AEG33 is missing, so we put it in the end

# now we get the response value from AEG33
vega$SPECRICH[which(vega$EPID=="AEG33")]
# and attach it to response
act_gpm_selected@model$pls_ffs[[1]][[10]]$testing$RESPONSE[5]<-41
# and finally because we need a value for predicted (regressiontest cant be made otherwise), we build the mean of all other predicted values in that model
act_gpm_selected@model$pls_ffs[[1]][[10]]$testing$PREDICTED[5,]<-mean(act_gpm_selected@model$pls_ffs[[1]][[10]]$testing$PREDICTED$pred)

# 6.Modell
vega[which(vega$EPID=="AEG23" | vega$EPID=="AEG37"| vega$EPID=="AEG34"| vega$EPID=="AEG48"| vega$EPID=="AEG21"),]
act_gpm_selected@model$pls_ffs[[1]][[6]]$testing$SELECTOR<-c("AEG23" , "AEG34" ,"AEG48" ,"AEG21","AEG37") #AEG37 is missing (you know by looking into RESPONSE)
act_gpm_selected@model$pls_ffs[[1]][[6]]$testing$RESPONSE[5]<-27 

act_gpm_selected@model$pls_ffs[[1]][[6]]$testing$PREDICTED[5,]<-mean(act_gpm_selected@model$pls_ffs[[1]][[6]]$testing$PREDICTED$pred)

# 1.Modell
vega[which(vega$EPID=="AEG14" | vega$EPID=="AEG19"| vega$EPID=="AEG28"| vega$EPID=="AEG43"| vega$EPID=="AEG10"),]
act_gpm_selected@model$pls_ffs[[1]][[1]]$testing$SELECTOR<-c("AEG14" ,"AEG19", "AEG28" , "AEG10","AEG43") #AEG43 fehlt

act_gpm_selected@model$pls_ffs[[1]][[1]]$testing$RESPONSE[5]<-25
act_gpm_selected@model$pls_ffs[[1]][[1]]$testing$PREDICTED[5,]<-mean(act_gpm_selected@model$pls_ffs[[1]][[1]]$testing$PREDICTED$pred)

tstat <- compRegrTests( act_gpm_selected@model$pls_ffs)
