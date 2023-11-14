#   CTI code 2: FIGURE 2c - CTI trend with LMM
#   Data: time series of 65 cases studies
#   last updated 06/09/2023
#   Project: H2020 FutureMARES, LIFE Urban Klima 2050
#   Authors: Guillem Chust, Leire Ibaibarriaga, Ernesto Villarino
#   Citation: Chust et al. Submitted NComm

# load libraries
library(dplyr)
library(ggplot2)
library(nlme)

# DATA INPUTS ------------------------------------------------------------------------------------------------------

# Load R objects 
load("2_CTI_DATA_ANALYSIS_AND_PLOTS/R_OBJECTS/R_objects.RData")  
output.wd <- "2_CTI_DATA_ANALYSIS_AND_PLOTS/OUTPUT" # name directory to save object

# END OF DATA INPUTS -----------------------------------------------------------------------------------------------

# 0. prediction with random effects ------------------------------------------

# prediction including the random effects in the intercept
mylme.pred3 <- predict(mylme.CTI, newdata=dd) 
plot(dd$Year, mylme.pred3, ylim=c(0, 23))

# include pred to df
dd$mylme.pred3 <- mylme.pred3

# 1. Residuals ---------------------------------------------------------------

dd$resid <- resid(mylme.CTI)
plot(dd$Year, dd$resid)

# 1.1 prediction for CTI + residuals
dd$pred_plus_resid <- dd$mylme.pred3 + dd$resid 
plot(dd$Year, dd$pred_plus_resid)

#plot
param.rand <- random.effects(mylme.CTI)
names(param.rand) <- c("Intercept")
param.rand$Case_study <- rownames(param.rand) 
tt <- merge(dd, param.rand, by="Case_study")
tt$CTI_minus_intercept <- tt$CTI - tt$Intercept
plot(tt$Year, tt$CTI_minus_intercept)

# 1.2 prediction of SST including the random effects in the intercept
mylme.pred.SST <- predict(mylme.SST, newdata=dd) 
# include pred to df
dd$mylme.pred.SST <- mylme.pred.SST
dd$resid.SST <- resid(mylme.SST)
plot(dd$Year, dd$resid.SST)
dd$pred_plus_resid.SST <- dd$mylme.pred.SST + dd$resid.SST # prediction for population + residuals
plot(dd$Year, dd$pred_plus_resid.SST)
param.rand.SST <- random.effects(mylme.SST)
names(param.rand.SST) <- c("Intercept.SST")
param.rand.SST$Case_study <- rownames(param.rand.SST) 
tt.SST <- merge(tt, param.rand.SST, by="Case_study")
tt.SST$SST_minus_intercept <- tt.SST$pottmp_surf - tt.SST$Intercept.SST
plot(tt.SST$Year, tt.SST$SST_minus_intercept)

# Figure 2c [CTI and SST]
p1 <- ggplot(tt.SST) +
  stat_summary(aes(y=CTI_minus_intercept, x=Year) , geom = "line", fun = mean, col="red") +
  stat_summary(aes(y=CTI_minus_intercept, x=Year), geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3, fill="red")+ # st dev, alpha: transparency
  geom_point(aes(y=CTI_minus_intercept, x=Year), col="grey", size = 0.5)+ # partial residuals CTI in grey
  geom_smooth(aes(y=CTI_minus_intercept, x=Year), method=lm, se = F, linetype="dashed", col=1)+ # black: overall line (fixed effects)
  ylab("CTI partial residuals (ºC)")+ # CTI partial residuals (CTI - random effects)
  xlab ("Year")+
  stat_summary(aes(y=SST_minus_intercept, x=Year) , geom = "line", fun = mean, col="blue") +
  stat_summary(aes(y=SST_minus_intercept, x=Year), geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3, fill="blue")+ # st dev, alpha: transparency
  theme_bw() + theme(text = element_text(size=35))

# save plot
ggsave(file=file.path(output.wd,"Fig2c.png"),  width=16, height=16, dpi=300)




