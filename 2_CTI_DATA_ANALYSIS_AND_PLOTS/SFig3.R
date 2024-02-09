#   Supplementary Figure 3 - Diagnostic plots
#   Data: time series of 65 cases studies
#   last updated 06/09/2023
#   Project: H2020 FutureMARES, LIFE Urban Klima 2050
#   Authors: Guillem Chust, Leire Ibaibarriaga, Ernesto Villarino
#   Citation: Chust et al. Submitted NComm


# load libraries ----------------------------------------------------------

library(ggplot2) 
library(nlme)
library (patchwork)

# DATA INPUTS ------------------------------------------------------------------------------------------------------

# Load R objects 
load("2_CTI_DATA_ANALYSIS_AND_PLOTS/R_OBJECTS/R_objects.RData")  
output.wd <- "2_CTI_DATA_ANALYSIS_AND_PLOTS/OUTPUT" # name directory to save object

# END OF DATA INPUTS -----------------------------------------------------------------------------------------------

# Diagnostic plots SUPPLEMENTARY FIGURE 3

tmp <- data.frame(fit=fitted(mylme.CTI_factors), res=resid(mylme.CTI_factors, type="p"))
tmp <- cbind(dd, tmp)

theme_set(theme_bw(base_size=16))

p1 <- ggplot(subset(tmp, Habitat.Group!= "demersal_pelagic"), aes(Habitat.Group, res))+
  geom_boxplot(fill="grey")+
  geom_hline(yintercept = 0, col=2, lwd=2, lty=2)+
  xlab("Habitat")+
  ylab("Residuals")

p2 <- ggplot(tmp, aes(Water_Mass, res))+
  geom_boxplot(fill="grey")+
  geom_hline(yintercept = 0, col=2, lwd=2, lty=2)+
  xlab("Water Mass")+
  ylab("Residuals")

# plot boxplots and pie charts
p1+p2+ plot_annotation(tag_levels=list(c('a')))& theme(plot.tag = element_text(size = 24))  

# save plot
ggsave(file=file.path(output.wd,"SFig3.png"),  width=18, height=10, dpi=300)

# Other diagnostic plots
ggplot(tmp, aes(fit, res))+
  geom_point()+
  geom_hline(yintercept = 0, col=2, lwd=2, lty=2)+
  xlab("Fitted")+
  ylab("Residuals")

ggplot(tmp, aes(CTI, res))+
  geom_point()+
  geom_hline(yintercept = 0, col=2, lwd=2, lty=2)+
  xlab("CTI")+
  ylab("Residuals")

ggplot(tmp, aes(CTI, fit))+
  geom_point()+
  geom_abline(intercept=0, slope=1, col=2, lwd=2, lty=2)+
  xlab("CTI")+
  ylab("Fitted")
