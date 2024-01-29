#   CTI code 6: Figure 4 CTI underlying processes by Species - density plot 
#   Data: 65 cases studies temporal trends (metadata)
#   last updated 19/09/2023
#   Project: H2020 FutureMARES, LIFE Urban Klima 2050
#   Authors: Leire Ibaibarriaga, Guillem Chust

# load libraries
library(ggplot2) 
library(dplyr)

# DATA INPUTS ------------------------------------------------------------------------------------------------------

# Load R objects 
load("2_CTI_DATA_ANALYSIS_AND_PLOTS/R_OBJECTS/R_objects.RData")  
output.wd <- "2_CTI_DATA_ANALYSIS_AND_PLOTS/OUTPUT" # name directory to save object

# END OF DATA INPUTS -----------------------------------------------------------------------------------------------

#rename variable
df <- df_process

# Plots -------------------------------------------------------------------

# contingency table
table(df$process, df$Case_study)

# data manipulation, arrow generation with mean of underlying process 
df.arrow <- df %>% 
  group_by(process) %>% 
  summarise(change=mean(change),
            sst_MP_diff=mean(sst_MP_diff))

#plot
theme_set(theme_bw(base_size = 16))
p1 <- ggplot(df, aes(x=sst_MP_diff, y=change, col=process))+
  geom_point(cex=1.5, alpha=0.2)+
  geom_segment(data=df.arrow, aes(x=0, y=0, xend=sst_MP_diff, yend=change, col=process),
               arrow = arrow(length = unit(0.7,"cm")), linewidth=2)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  theme(legend.position="none",
        text = element_text(size=20))+
  ylab("Species' abundance change")+
  xlab("Species' thermal bias")+
  scale_color_manual(values = c("borealization"="#2166AC", 
                                "detropicalization"="#92C5DE",
                                "deborealization"="#F4A582",
                                "tropicalization"="#B2182B"))+
  geom_segment(data=df.arrow, aes(x=0, y=0, xend=sst_MP_diff, yend=change),
             arrow = arrow(length = unit(0.7,"cm")), linewidth=0.8, col="black")+
  annotate("text", x = -5, y =  0.4, label = "Borealization     = 20.4% ",  size=6) +
  annotate("text", x =  5, y =  0.4, label = "Tropicalization   = 33.9% ",  size=6) +
  annotate("text", x = -5, y = -0.4, label = "Deborealization   = 25.4% ",  size=6) +
  annotate("text", x =  5, y = -0.4, label = "Detropicalization = 20.3 % ", size=6) +
  annotate("text", x =  4.5, y =  0.48, label = "Womersleyella setacea",  size=4, fontface="italic") +
  annotate("text", x =  0, y =  -0.63, label = "Paramuricea clavate",  size=4, fontface="italic") 

# save plot
ggsave(file=file.path(output.wd,"Fig4.png"),  width=16, height=16, dpi=300)

 

