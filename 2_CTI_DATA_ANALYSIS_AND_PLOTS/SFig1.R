####################################################################################################
##   Temporal trends of CTI and SST for each case study. For case study labelling see Suppl.Table 1 
##   Author: Ernesto Villarino, evillarino@azti.es 
##   Project: H2020 FutureMARES
##   Paper: Cross-basins and cross-taxon patterns in biodiversity turnover in warming seas   
#################################################################################################### 

#load libraries

library (ggplot2)   #plot
library (patchwork) #plot layout
library (tidyverse) #data manipulation
library (scales)    #plot axis customization

# DATA INPUTS ------------------------------------------------------------------------------------------------------

# Load R objects 
load("2_CTI_DATA_ANALYSIS_AND_PLOTS/R_OBJECTS/R_objects.RData")  
output.wd <- "2_CTI_DATA_ANALYSIS_AND_PLOTS/OUTPUT" # name directory to save object

# END OF DATA INPUTS -----------------------------------------------------------------------------------------------

# 0. add case study label according to CTI slope (order low to high)
df <-  df_trends_65 %>% group_by (Case_study, Group) %>% dplyr::summarize(slope_CTI = summary(lm(CTI ~ Year))$coefficients[2])
df_order <-  df %>% arrange(slope_CTI)
df_order$Case_study_1 <- rep(1:65)
df1 <- right_join(df_trends_65, df_order, by=c("Case_study","Group"))

#plot
p1 <- ggplot(df1, aes(x=Year, y=CTI, color = Group)) + 
  geom_line(linewidth=1) + 
  geom_line(linewidth=1,data=df1,aes(x=Year,y=pottmp_surf, color="SST")) +
  facet_wrap(~Case_study_1, scales = "free_y", nrow=10) + 
  scale_x_continuous(breaks = pretty_breaks()) + 
  theme_bw() + 
  theme(legend.text = element_text(size=14),
        axis.text = element_text(size = 12),
        strip.text.x = element_text(size = 14),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))+ 
  ylab ("Community Temperature Index")+
  scale_color_manual (values = c("#999999", "#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2", "black"), # "#D55E00", "#CC79A7", 
                      breaks = c("Zooplankton", "CHB benthos", "CSB benthos", "Crustacea", "Cephalopods", "Fish", "SST")) 

# save plot
ggsave(file=file.path(output.wd,"SFig1.png"),  width=16, height=16, dpi=300)

