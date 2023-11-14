############################################################################################################
##   Violin plots showing change in the Community Temperature Index (CTIr) by factor
##   Pie charts showing CTIr underlying ecological process dominance for each factor. 
##   Author: Ernesto Villarino, evillarino@azti.es 
##   Project: H2020 FutureMARES
##   Paper: Cross-basins and cross-taxon patterns in biodiversity turnover in warming seas   
############################################################################################################# 

# load libraries
library (ggplot2) #plot
library (patchwork) #plot layout
library (viridis) #plot color
library (tidyverse) #data manipulation

# DATA INPUTS ------------------------------------------------------------------------------------------------------

# Load R objects 
load("2_CTI_DATA_ANALYSIS_AND_PLOTS/R_OBJECTS/R_objects.RData")  
output.wd <- "2_CTI_DATA_ANALYSIS_AND_PLOTS/OUTPUT" # name directory to save object

# END OF DATA INPUTS -----------------------------------------------------------------------------------------------

# 0. rename vars
data.series65wGLS <- mutate(data.series65wGLS, Open_Close= recode(Open_Close, "Open"="Non-enclosed"))
data.series65wGLS <- mutate(data.series65wGLS, Open_Close= recode(Open_Close, "Semi-enclosed"="Semi-enclosed"))
data.series65wGLS <- mutate(data.series65wGLS, Water_Mass= recode(Water_Mass, "North East Atlantic"="NE Atlantic"))
data.series65wGLS <- mutate(data.series65wGLS, Water_Mass= recode(Water_Mass, "Baltic_Kattegat"="Baltic"))

# 1. CTI rate of change----

# 1.1 CTI change by biological group
text1 <- data.frame(Label = c("n=4", "n=7", "n=19","n=7", "n=10", "n=18"),
                    Group =c("Zooplankton","CHB benthos","CSB benthos", "Crustacea", "Cephalopods", "Fish"),
                    x=1:6,y=-0.7) #add n label

data.series65wGLS$Taxon <- "Biological group" #facet label
data.series65wGLS$Group<- factor(data.series65wGLS$Group, (levels=c("Zooplankton","CHB benthos","CSB benthos","Crustacea", "Cephalopods", "Fish")))# reorder group

#plot 1
p2.0 <- ggplot (data.series65wGLS, aes(x=Group, y=slope_CTI_merged*10, fill=Group)) +  
  geom_violin(trim=FALSE, fill="gray", width=1.4) + 
  geom_boxplot(width=0.08, outlier.size = 1)  +
  scale_fill_viridis(discrete = TRUE, option = "D")+facet_wrap(~Taxon)+
  theme_bw() + theme(legend.text = element_text(size=16),
                     axis.text = element_text(size = 16),
                     strip.text.x = element_text(size = 16),
                     axis.title.y = element_text(size=16),
                     axis.title.x = element_text(size=16),
                     legend.position="none")+
  xlab ("") + ylab (expression(paste("CTI",italic(" r")," (°C/decade)")))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")  + 
  geom_text(data = text1,aes(x = x, y=y,label=Label),color="black",size=5)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+  
  ylim (c(-0.7,2.5))


# 1.2 CTI change by habitat (remove demersal_pelagic case)
text2 <- data.frame(Label = c("n=51", "n=3","n=10"),
                    Habitat.Group =c("benthic_demersal","estuarine", "pelagic"),
                    x=1:3,y=-0.7) #add n label

data.series65wGLS$Habitat <- rep (c("Habitat")) # facet label

#plot 2
p2.1 <- ggplot (data.series65wGLS[-5,], aes(x=Habitat.Group, y=slope_CTI_merged*10, fill=Habitat.Group)) +  
  geom_violin(trim=FALSE, fill="gray", width=1) + 
  geom_boxplot(width=0.08, outlier.size = 1)  +
  scale_fill_viridis(discrete = TRUE, option = "D")+facet_wrap(~Habitat)+
  theme_bw() + theme(legend.text = element_text(size=16),
                     axis.text = element_text(size = 16),
                     strip.text.x = element_text(size = 16),
                     axis.title.y = element_text(size=16),
                     axis.title.x = element_text(size=16),
                     legend.position="none")+
  xlab ("") + ylab (expression(paste("CTI",italic(" r")," (°C/decade)")))+
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_text(data = text2,aes(x = x, y=y,label=Label),color="black",size=5)+ 
  ylim (c(-0.7,2.5))

# 1.3 CTI change by ocean
text3 <- data.frame(Label = c("n=4", "n=30", "n=31"),
                    Water_Mass =c("Baltic_Kattegat","Mediterranean","NE Atlantic"),
                    x=1:3,y=-0.7) #add n label

data.series65wGLS$Water_body <- rep (c("Region"))#facet label

#plot3
p2.2 <- ggplot (data.series65wGLS, aes(x=Water_Mass, y=slope_CTI_merged*10, fill=Water_Mass)) +  
  geom_violin(trim=FALSE, fill="gray", width=1) + 
  geom_boxplot(width=0.08, outlier.size = 1)  +
  scale_fill_viridis(discrete = TRUE, option = "D")+facet_wrap(~Water_body)+
  theme_bw() + theme(legend.text = element_text(size=16),
                     axis.text = element_text(size = 16), 
                     strip.text.x = element_text(size = 16),
                     axis.title.y = element_text(size=16),
                     axis.title.x = element_text(size=16),
                     legend.position="none")+
  xlab ("") + ylab (expression(paste("CTI",italic(" r")," (°C/decade)")))+
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_text(data = text3,aes(x = x, y=y,label=Label),color="black",size=5)+ 
  ylim (c(-0.7,1.85))


# 1.4 CTI change by basin type
text4 <- data.frame(Label = c("n=29", "n=34"),
                    Open_Close =c("Open","Atlantic_Ocean"),
                    x=1:2,y=-0.7) # add n label

data.series65wGLS$Basin_type <- rep (c("Basin type")) #facet label

#plot 4
p2.3 <- ggplot (data.series65wGLS, aes(x=Open_Close, y=slope_CTI_merged*10, fill=Open_Close)) +  
  geom_violin(trim=FALSE, fill="gray", width=1) + 
  geom_boxplot(width=0.08, outlier.size = 1)  +
  scale_fill_viridis(discrete = TRUE, option = "D")+facet_wrap(~Basin_type)+
  theme_bw() + theme(legend.text = element_text(size=16),
                     axis.text = element_text(size = 16),
                     strip.text.x = element_text(size = 16),
                     axis.title.y = element_text(size=16),
                     axis.title.x = element_text(size=16),
                     legend.position="none")+
  xlab ("") + ylab (expression(paste("CTI",italic(" r")," (°C/decade)")))+
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_text(data = text4,aes(x = x, y=y,label=Label),color="black",size=5)+ ylim (c(-0.7,1.85))

#detach("package:plyr", unload = TRUE)

# 2. Underlying ecological process----

# 2.1 Group
#calculate process dominance
df_process_group_dom <-data.series65wGLS  %>% 
  group_by(Group) %>%
  select  (23:26,38)   %>%
  summarize(tropicalization_dom=sum(tropicalization>borealization     & tropicalization>detropicalization & tropicalization>deborealization)/n(),
            borealization_dom=sum(borealization>tropicalization       & borealization>detropicalization   & borealization>deborealization)/n(),
            detropicalization_dom=sum(detropicalization>borealization & detropicalization>tropicalization & detropicalization>deborealization)/n(),
            deborealization_dom=sum(deborealization>tropicalization   & deborealization>detropicalization & deborealization>borealization)/n()) #
            
#rename and convert wide to long
colnames(df_process_group_dom)[2:5] <- c("tropicalization", "borealization",  "detropicalization", "deborealization")
df_process_group_dom <- gather(df_process_group_dom,Process, Value_scale,-c(Group))
df_process_group_dom$Process<- factor(df_process_group_dom$Process, (levels=c("borealization", "detropicalization", "deborealization", "tropicalization"))) #reorder process
df_process_group_dom$Group  <- factor(df_process_group_dom$Group, (levels=c("Zooplankton","CHB benthos","CSB benthos","Crustacea", "Cephalopods", "Fish"))) #reorder

#plot cheese 1
p3.0 <- ggplot(df_process_group_dom, aes(x = "", y = Value_scale, fill = Process)) + 
  geom_bar(width = 1, stat = "identity") +
  facet_wrap( ~ Group, nrow=1)+
  scale_fill_manual(values = c("#2166AC", "#92C5DE","#F4A582","#B2182B"))+
  theme_bw() + 
  theme(legend.text = element_text(size=14),
        axis.text = element_text(size = 14), 
        legend.title = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        strip.text.x = element_text(size = 11)) +
  xlab ("") + ylab ("") + 
  coord_polar("y", start=0) + 
  theme(axis.text.x=element_blank())+
  theme(plot.margin=unit(c(0, 0, 0, 0), "lines")) + 
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill = "transparent",colour = NA_character_))

# 2.2 Habitat
#calculate process dominance (remove demersal-pelagic case)
df_process_hab_dom<-data.series65wGLS [-5,] %>% 
  select  (23:26,5)  %>% 
  group_by(Habitat.Group) %>%
  summarize(tropicalization_dom=sum(tropicalization>borealization     & tropicalization>detropicalization & tropicalization>deborealization)/n(),
            borealization_dom=sum(borealization>tropicalization       & borealization>detropicalization   & borealization>deborealization)/n(),
            detropicalization_dom=sum(detropicalization>borealization & detropicalization>tropicalization & detropicalization>deborealization)/n(),
            deborealization_dom=sum(deborealization>tropicalization   & deborealization>detropicalization & deborealization>borealization)/n()) #

#rename and convert wide to long
colnames(df_process_hab_dom)[2:5] <- c("tropicalization", "borealization",  "detropicalization", "deborealization")
df_process_hab_dom <- gather(df_process_hab_dom,Process, Value_scale,-c(Habitat.Group))
df_process_hab_dom$Process<- factor(df_process_hab_dom$Process, (levels=c("borealization", "detropicalization", "deborealization", "tropicalization"))) #reorder process

# plot cheese 2
p3.1 <- ggplot(df_process_hab_dom, aes(x = "", y = Value_scale, fill = Process)) + 
  geom_bar(width = 1, stat = "identity") +
  facet_wrap( ~ Habitat.Group, nrow=1)+
  scale_fill_manual(values = c("#2166AC", "#92C5DE","#F4A582","#B2182B"))+
  theme_bw() + 
  theme(legend.text = element_text(size=14),
        axis.text = element_text(size = 14), 
        legend.title = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        strip.text.x = element_text(size = 8)) +
  xlab ("") + ylab ("") + 
  coord_polar("y", start=0) + 
  theme(axis.text.x=element_blank())+
  theme(plot.margin=unit(c(0, 0, 0, 0), "lines")) + 
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill = "transparent",colour = NA_character_))

# 2.3 Region
#calculate process dominance 
df_process_wb_dom<-data.series65wGLS  %>% 
  select  (23:26,10)  %>% 
  group_by(Water_Mass) %>%
  summarize(tropicalization_dom=sum(tropicalization>borealization     & tropicalization>detropicalization & tropicalization>deborealization)/n(),
            borealization_dom=sum(borealization>tropicalization       & borealization>detropicalization   & borealization>deborealization)/n(),
            detropicalization_dom=sum(detropicalization>borealization & detropicalization>tropicalization & detropicalization>deborealization)/n(),
            deborealization_dom=sum(deborealization>tropicalization   & deborealization>detropicalization & deborealization>borealization)/n()) #

#rename and convert wide to long
colnames(df_process_wb_dom)[2:5] <- c("tropicalization", "borealization",  "detropicalization", "deborealization")
df_process_wb_dom <- gather(df_process_wb_dom,Process, Value_scale,-c(Water_Mass))
df_process_wb_dom$Process<- factor(df_process_wb_dom$Process, (levels=c("borealization", "detropicalization", "deborealization", "tropicalization"))) #reorder process

#plot cheese 3
p3.2 <- ggplot(df_process_wb_dom, aes(x = "", y = Value_scale, fill = Process)) + 
  geom_bar(width = 1, stat = "identity") +
  facet_wrap( ~ Water_Mass, nrow=1)+
  scale_fill_manual(values = c("#2166AC",  "#92C5DE","#F4A582","#B2182B"))+
  theme_bw() + 
  theme(legend.text = element_text(size=14),
        axis.text = element_text(size = 14), 
        legend.title = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        strip.text.x = element_text(size = 10)) +
  xlab ("") + ylab ("") + 
  coord_polar("y", start=0) + 
  theme(axis.text.x=element_blank())+ 
  theme(plot.margin=unit(c(0, 0, 0, 0), "lines")) + 
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill = "transparent",colour = NA_character_))

# 2.4 Basin type
#calculate process dominance 
df_process_bt_dom<-data.series65wGLS  %>% 
  select  (23:26,11)  %>% 
  group_by(Open_Close) %>% 
  summarize(tropicalization_dom=sum(tropicalization>borealization     & tropicalization>detropicalization & tropicalization>deborealization)/n(),
            borealization_dom=sum(borealization>tropicalization       & borealization>detropicalization   & borealization>deborealization)/n(),
            detropicalization_dom=sum(detropicalization>borealization & detropicalization>tropicalization & detropicalization>deborealization)/n(),
            deborealization_dom=sum(deborealization>tropicalization   & deborealization>detropicalization & deborealization>borealization)/n()) #

#rename and convert wide to long
colnames(df_process_bt_dom)[2:5] <- c("tropicalization", "borealization",  "detropicalization", "deborealization")
df_process_bt_dom <- gather(df_process_bt_dom,Process, Value_scale,-c(Open_Close))
df_process_bt_dom$Process<- factor(df_process_bt_dom$Process, (levels=c("borealization", "detropicalization", "deborealization", "tropicalization"))) #reorder process

#plot cheese 4
p3.3 <- ggplot(df_process_bt_dom, aes(x = "", y = Value_scale, fill = Process)) + 
  geom_bar(width = 1, stat = "identity") +
  facet_wrap( ~ Open_Close, nrow=1)+
  scale_fill_manual(values = c("#2166AC",  "#92C5DE","#F4A582","#B2182B"))+
  theme_bw() + 
  theme(legend.text = element_text(size=14),
        axis.text = element_text(size = 14), 
        legend.title = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        strip.text.x = element_text(size = 11)) +
  xlab ("") + ylab ("") + 
  coord_polar("y", start=0) + 
  theme(axis.text.x=element_blank())+
  theme(plot.margin=unit(c(0, 0, 0, 0), "lines")) + 
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill = "transparent",colour = NA_character_))

# plot boxplots and pie charts
p2.0+
  # inset_element  (p3.0, left = -0.02,  bottom = 0.49, right = 0.97,  top = 1.22)  +
  inset_element  (p3.0, left = -0.02,  bottom = 0.55, right = 0.97,  top = 1.2)  +
  p2.1+inset_element(p3.1, left =  -0.02,   bottom = 0.22,  right = 0.45,  top = 1.4)  + 
  p2.2+inset_element(p3.2, left = -0.02,  bottom = 0.22, right = 0.45,  top = 1.4)  +
  p2.3+inset_element(p3.3, left = -0.02,  bottom = 0.5,  right = 0.5,   top = 1.05)  + 
  plot_layout(guides = "collect", ncol=2, nrow=2)+
  plot_annotation(tag_levels=list(c('a','','b','','c','','d')))&
  theme(plot.tag = element_text(size = 24))  

# save plot
ggsave(file=file.path(output.wd,"Fig3.png"),  width=16, height=16, dpi=300)
