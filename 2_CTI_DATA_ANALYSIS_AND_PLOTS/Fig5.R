###############################################################################################################################
##   CTI underlying ecological processes of tropicalization minus deborealization for positive CTI rate of change by basin type. 
##   (Tropicalization + Borealization) - (Detropicalization + Deborealization) by basin type for all case studies.
##   Author: Ernesto Villarino, evillarino@azti.es 
##   Project: H2020 FutureMARES
##   Paper: Cross-basins and cross-taxon patterns in biodiversity turnover in warming seas   
############################################################################################################################### 

#load libraries

library (ggplot2) #plot
library (patchwork) #plot layout
library (tidyverse) #data manipulation

# DATA INPUTS ------------------------------------------------------------------------------------------------------

# Load R objects 
load("2_CTI_DATA_ANALYSIS_AND_PLOTS/R_OBJECTS/R_objects.RData")  
output.wd <- "2_CTI_DATA_ANALYSIS_AND_PLOTS/OUTPUT" # name directory to save object

# END OF DATA INPUTS -----------------------------------------------------------------------------------------------

# 0. rename vars
data.series65wGLS<- mutate(data.series65wGLS, Open_Close= recode(Open_Close, "Open"="Non-enclosed"))
data.series65wGLS<- mutate(data.series65wGLS, Open_Close= recode(Open_Close, "Semi-enclosed"="Semi-enclosed")) 

# 1. filter data
df_CTI_increase <-  data.series65wGLS  %>% filter(slope_CTI_merged> 0) # 52 case-studies
df_CTI_decrease <-  data.series65wGLS  %>% filter(slope_CTI_merged< 0) # 13 case-studies
df_CTI_increase[df_CTI_increase=="demersal_pelagic"] <- NA # remove demersal_pelagic case-study

# 2. calculate index
df_CTI_increase$tropi_minus_deboreal <- df_CTI_increase$tropicalization - df_CTI_increase$deborealization #trop-deb
data.series65wGLS$expansion <-  data.series65wGLS$tropicalization + data.series65wGLS$borealization #expansion
data.series65wGLS$retraction <- data.series65wGLS$detropicalization + data.series65wGLS$deborealization #retraction
data.series65wGLS$exp_minus_ret <- data.series65wGLS$expansion - data.series65wGLS$retraction #exp-retr.

#plot 1
df_CTI_increase$var <- "Tropicalization - Deborealization" #facet label
p1 <- ggplot(df_CTI_increase, aes(y = tropi_minus_deboreal, x = Open_Close, fill=Open_Close)) +
  geom_violin(trim=FALSE, fill="gray", width=1) +
  geom_boxplot(width=0.07, size=0.6, color="black") + ylim (c(-1.8,1.8))+
  theme_bw() + facet_grid(~var)+
  theme(legend.text = element_text(size=18),
        legend.position = "none",
        axis.text = element_text(size = 18),
        strip.text.x = element_text(size = 18),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(size=18),
        plot.title = element_text(size=18))+ 
  xlab ("") + ylab ("(-) Deborealization     Tropicalization (+)")+
  geom_hline(yintercept=0, linetype="dashed", color = "black") 

#plot2 
data.series65wGLS$var <- "(Trop+Bor) - (Det+Deb)" #facet label
p2 <- ggplot(data.series65wGLS, aes(y = exp_minus_ret, x = Open_Close, fill=Open_Close)) +
  geom_violin(trim=FALSE, fill="gray", width=1) +
  geom_boxplot(width=0.07, size=0.6, color="black") + ylim (c(-1.8,1.8))+
  theme_bw() +  facet_grid(~var)+
  theme(legend.text = element_text(size=18),
        legend.position = "none",
        axis.text = element_text(size = 18),
        strip.text.x = element_text(size = 18),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(size=18),
        plot.title = element_text(size=18))+
  xlab ("") + ylab ("(Trop+Bor)   -   (Det+Deb)")+
  geom_hline(yintercept=0, linetype="dashed", color = "black") 

p1+p2 +plot_annotation(tag_levels=list(c('a')))& theme(plot.tag = element_text(size = 25))

# save plot
ggsave(file=file.path(output.wd,"Fig5.png"),  width=20, height=14, dpi=300)
