################################################################################################################
##   Error barplot showing the rate of change in Community Temperature Index (CTI) over time for each case study
##   Author: Ernesto Villarino, evillarino@azti.es 
##   Project: H2020 FutureMARES
##   Paper: Cross-basins and cross-taxon patterns in biodiversity turnover in warming seas   
################################################################################################################

#load libraries 
library (ggplot2)   #plot 
library (dplyr)     #data manipulation
library (patchwork) #plot layout

# DATA INPUTS ------------------------------------------------------------------------------------------------------

# Load R objects 
load("2_CTI_DATA_ANALYSIS_AND_PLOTS/R_OBJECTS/R_objects.RData")  
output.wd <- "2_CTI_DATA_ANALYSIS_AND_PLOTS/OUTPUT" # name directory to save object

# END OF DATA INPUTS -----------------------------------------------------------------------------------------------

#reoder according to CTIr
df_order <-  data.series65wGLS %>% arrange(slope_CTI_merged)
df_order$Case_study_1 <- rep(1:65)
df_order$Group<- factor(df_order$Group, (levels=c("Zooplankton","CHB benthos","CSB benthos", "Crustacea", "Cephalopods", "Fish"))) #reroder groups

#plot
p <- ggplot (df_order, aes (as.factor(Case_study_1),slope_CTI_merged*10, color=Group))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=ci.low.merged*10, ymax= ci.upp.merged*10), linewidth=1) +
  coord_flip() + 
  labs (y=(expression(paste("CTI",italic(" r")," (°C/decade)"))), x="Case study") +
  theme_bw()+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  #scale_color_manual (values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
  scale_color_viridis(discrete = TRUE, option = "D")+
  ylim(c(-2.5,2.5))+
  theme(axis.text = element_text(size = 14),
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16),
        legend.text = element_text(size=16),
        legend.position = (c(0.2,0.87)),
        legend.title=element_blank(),
        legend.background = element_rect(fill = "white", color = "black"))  

p + plot_layout (ncol=3)

# save plot
ggsave(file=file.path(output.wd,"Fig2b.png"),  width=16, height=16, dpi=300)


