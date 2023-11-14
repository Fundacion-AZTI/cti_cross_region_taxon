#   CTI code 7: Statistics of CTI underlying processes by Species - models
#   Data: 65 cases studies temporal trends (data of processes)
#   last updated 19/09/2023
#   Project: H2020 FutureMARES, LIFE Urban Klima 2050
#   Authors: Guillem Chust, Leire Ibaibarriaga, Ernesto Villarino

# load libraries 
library(dplyr)
library(car)
library(nlme)
library(Hmisc)
library(MuMIn)

# DATA INPUTS ------------------------------------------------------------------------------------------------------

# Load R objects 
load("2_CTI_DATA_ANALYSIS_AND_PLOTS/R_OBJECTS/R_objects.RData")  
output.wd <- "2_CTI_DATA_ANALYSIS_AND_PLOTS/OUTPUT" # name directory to save object

# END OF DATA INPUTS -----------------------------------------------------------------------------------------------

# 0. assign factors from metadata (df_sint) to species process (df_trends_65)
df_process_sint <- merge(df_process, df_sint, by="Case_study")

# 0.1 Processes Tropicalization and deborealization 

# statistics 

tropicalization <- subset(df_process_sint, process=="tropicalization")
trop_intensity <- sum(abs(tropicalization$sst_MP_wtd_change))

deborealization <- subset(df_process_sint, process=="deborealization")
deb_intensity <- sum(abs(deborealization$sst_MP_wtd_change)) 

borealization <- subset(df_process_sint, process=="borealization")
bor_intensity <- sum(abs(borealization$sst_MP_wtd_change))

detropicalization <- subset(df_process_sint, process=="detropicalization")
detrop_intensity <- sum(abs(detropicalization$sst_MP_wtd_change))

process_sum <- sum(trop_intensity, deb_intensity, bor_intensity, detrop_intensity)

trop_intensity <- trop_intensity/process_sum ; trop_intensity      
deb_intensity <- deb_intensity/process_sum ; deb_intensity         
bor_intensity <- bor_intensity/process_sum ; bor_intensity         
detrop_intensity <- detrop_intensity/process_sum ;detrop_intensity 

trop_intensity + deb_intensity   
bor_intensity + detrop_intensity 

mean(df_process_sint$sst_MP_wtd_change[df_process_sint$process=="borealization"])     
mean(df_process_sint$sst_MP_wtd_change[df_process_sint$process=="deborealization"])   
mean(df_process_sint$sst_MP_wtd_change[df_process_sint$process=="detropicalization"]) 
mean(df_process_sint$sst_MP_wtd_change[df_process_sint$process=="tropicalization"])   

boxplot(df_process_sint$sst_MP_wtd_change ~df_process_sint$process, outline=F, ylim=c(-0.1,0.1))

mean(df_process_sint$change[df_process_sint$process=="borealization"])     
mean(df_process_sint$change[df_process_sint$process=="deborealization"])   
mean(df_process_sint$change[df_process_sint$process=="detropicalization"]) 
mean(df_process_sint$change[df_process_sint$process=="tropicalization"])   

mean(df_process_sint$sst_MP_diff[df_process_sint$process=="borealization"])
mean(df_process_sint$sst_MP_diff[df_process_sint$process=="deborealization"])
mean(df_process_sint$sst_MP_diff[df_process_sint$process=="detropicalization"])
mean(df_process_sint$sst_MP_diff[df_process_sint$process=="tropicalization"])

# Tropicalization and deborialization in positive CTI_change>0

# CTI>0
df_process_sint_CTIpos <- subset(df_process_sint, process=="tropicalization" | process=="deborealization")
100*nrow(df_process_sint_CTIpos)/nrow(df_process_sint)

# 1. Processes by factors (Supp Table 3) -----
df_CTI_increase <-  subset(df_process_sint, process=="tropicalization" | process=="deborealization")
df_CTI_decrease <-  subset(df_process_sint, process=="detropicalization" | process=="borealization")

table(df_CTI_increase$Community.acronym)
table(df_CTI_increase$Habitat.Group)
table(df_CTI_increase$Water_Mass)
table(df_CTI_increase$Open_Close)

# Transform sst_MP_wtd_change into negative values where deborealization

for (i in 1:nrow(df_CTI_increase))
(if (df_CTI_increase$process[i]=="deborealization")
{
  df_CTI_increase$tropi_minus_deboreal[i] <- -df_CTI_increase$sst_MP_wtd_change[i]
} 
else {df_CTI_increase$tropi_minus_deboreal[i] <- df_CTI_increase$sst_MP_wtd_change[i]}
)
df_CTI_increase["tropi_minus_deboreal"]

# Biological groups

options(na.action = "na.omit")

mylme <-lme(tropi_minus_deboreal ~ Community.acronym-1, random=~1|Case_study, data=df_CTI_increase) 
summary (mylme)
anova(lme(tropi_minus_deboreal ~ Community.acronym, random=~1|Case_study, data=df_CTI_increase) ) 
# p=0.0941

# Habitat.Group

df_CTI_increase_sub <- subset(df_CTI_increase, Habitat.Group!="demersal_pelagic")

mylme <-lme(tropi_minus_deboreal ~ Habitat.Group-1, random=~1|Case_study, data=df_CTI_increase_sub) 
summary (mylme)
anova(lme(tropi_minus_deboreal ~ Habitat.Group, random=~1|Case_study, data=df_CTI_increase_sub) ) 
# p<.0001

# Water_Mass / Sea region

mylme <-lme(tropi_minus_deboreal ~ Water_Mass-1, random=~1|Case_study, data=df_CTI_increase) 
summary (mylme)
anova(lme(tropi_minus_deboreal ~ Water_Mass, random=~1|Case_study, data=df_CTI_increase) ) 
# p=0.0571

# Basin / Seascape connectivity

mylme <-lme(tropi_minus_deboreal ~ Open_Close-1, random=~1|Case_study, data=df_CTI_increase) 
summary (mylme)
anova(lme(tropi_minus_deboreal ~ Open_Close, random=~1|Case_study, data=df_CTI_increase)) 

# all factors (selection)
options(na.action = "na.omit")
# mylme.0 <-lme(tropi_minus_deboreal ~ Community.acronym + Habitat.Group + Water_Mass + Open_Close, random=~1|Case_study, data=df_CTI_increase_sub) 
# singularity problem between water mass and Open_close

AIC(lme(tropi_minus_deboreal ~ Community.acronym, random=~1|Case_study, data=df_CTI_increase_sub))
AIC(lme(tropi_minus_deboreal ~ Habitat.Group, random=~1|Case_study, data=df_CTI_increase_sub))
AIC(lme(tropi_minus_deboreal ~ Water_Mass, random=~1|Case_study, data=df_CTI_increase_sub))
AIC(lme(tropi_minus_deboreal ~ Open_Close, random=~1|Case_study, data=df_CTI_increase_sub))

mylme.1 <-lme(tropi_minus_deboreal ~ Community.acronym + Habitat.Group + Open_Close, random=~1|Case_study, data=df_CTI_increase_sub) 
AIC(mylme.1)
options(na.action = "na.fail")
mylme.1.dredge <- dredge(mylme.1)
# visualizing the best models:
mylme.1.dredge 
# 1st best model
summary(get.models(mylme.1.dredge, 1)[[1]]) 
# best: Habitat, and Habitat and open_cls

mylme.1 <-lme(tropi_minus_deboreal ~ Community.acronym + Habitat.Group + Water_Mass, random=~1|Case_study, data=df_CTI_increase_sub) 
AIC(mylme.1) # -3670.485
options(na.action = "na.fail")
mylme.1.dredge <- dredge(mylme.1)
# visualizing the best models:
mylme.1.dredge
# 1st best model
summary(get.models(mylme.1.dredge, 1)[[1]]) 
# best: Habitat

# 2. Processes on fish (results in Discussion) ------------------------------------------------------------------------

df_CTI_increase_fish <-  subset(df_CTI_increase,Community.acronym == "Fish") 

mylme.fish <-lme(tropi_minus_deboreal ~ Open_Close, random=~1|Case_study, data=df_CTI_increase_fish) 
summary (mylme.fish)
anova(mylme.fish) # p=0.3837
boxplot(df_CTI_increase_fish$tropi_minus_deboreal~df_CTI_increase_fish$Open_Close)

# 3. Processes of colonization and local extinction ------------------------------------------------------------------
# Processes of colonization (expansion): (tropicalization + borealization)
# Processes local extinction (retraction): detropicalization + deborealization
# Hypothesis H1: in communities of dispersal limitation (semi-enclosed seas), colonization should be lower than local extinction

# Transform sst_MP_wtd_change into negative values where deborealization

df_process_sint <- merge(df_process, df_sint, by="Case_study")
df_process_sint$TpB_minus_deTpdeB <- df_process_sint$sst_MP_wtd_change
for (i in 1:nrow(df_process_sint))
  (if (df_process_sint$process[i]=="deborealization" | df_process_sint$process[i]=="borealization")
  {
    df_process_sint$TpB_minus_deTpdeB[i] <- -df_process_sint$sst_MP_wtd_change[i]
  } 
  else {df_process_sint$TpB_minus_deTpdeB[i] <- df_process_sint$sst_MP_wtd_change[i]}
  )

lme.2 <-lme(TpB_minus_deTpdeB ~ Open_Close-1, random=~1|Case_study, data=df_process_sint) 
boxplot(df_process_sint$TpB_minus_deTpdeB~df_process_sint$Open_Close, ylab="(-) Retraction       Expansion (+)")
summary(lme.2)
anova(lme(TpB_minus_deTpdeB ~ Open_Close, random=~1|Case_study, data=df_process_sint) )
