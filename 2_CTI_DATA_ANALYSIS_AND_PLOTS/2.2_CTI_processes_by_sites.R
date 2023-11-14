#   CTI code 4: Statistics of CTI underlying processes
#   Data: 65 cases studies temporal trends (metadata)
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


# 1. Processes Tropicalization and deborialization -------------------------------------------------------------

# mean values
mean(df_sint$`Trop.Debor`)    
# 0.6401386
mean(df_sint$`Boreal.Detrop`) 
# 0.3598614

# (% of cases)
a<-100*nrow(subset(df_sint, (tropicalization+deborealization)>(borealization+detropicalization))) / nrow(df_sint)
a 
# 76.92%
100-a 
# 23.07692%

# (% of cases per processes)
100*nrow(subset(df_sint, tropicalization>deborealization)) / nrow(df_sint) 
# 67.69%

# Stats for FIGURE 2d (% of cases per processes)

df_sin_max <- subset(df_sint, tropicalization>borealization & tropicalization>detropicalization & tropicalization>deborealization)
100*nrow (df_sin_max)/ nrow(df_sint) 
# tropicalization 53.84615%

df_sin_max <- subset(df_sint, borealization>tropicalization & borealization>detropicalization & borealization>deborealization)
100*nrow (df_sin_max)/ nrow(df_sint) 
# borealization 13.84615%

df_sin_max <- subset(df_sint, detropicalization>borealization & detropicalization>tropicalization & detropicalization>deborealization)
100*nrow (df_sin_max)/ nrow(df_sint) 
# detropicalization 13.84615%

df_sin_max <- subset(df_sint, deborealization>tropicalization & deborealization>detropicalization & deborealization>borealization)
100*nrow (df_sin_max)/ nrow(df_sint) 
# deborealization 18.46154%

# Tropicalization and deborialization in positive CTI_change>0

# CTI>0
df_sint_CTIpos <- subset(data.series65wGLS, slope_CTI_merged>0)
# 52
100*nrow(df_sint_CTIpos)/nrow(df_sint) 
# 80.0%
100*nrow(subset(df_sint_CTIpos, tropicalization>deborealization))/nrow(df_sint_CTIpos) 
# 69.23077%
100*nrow(subset(df_sint_CTIpos, df_sint_CTIpos$deborealization>df_sint_CTIpos$tropicalization))/nrow(df_sint_CTIpos) 
# 30.76923%

mean(df_sint_CTIpos$tropicalization) 
# 0.4680078
mean(df_sint_CTIpos$deborealization) 
# 0.2338113

# 2. Processes by factors (Table 2) -------------------------------------------------------------
df_CTI_increase <-  data.series65wGLS  %>% filter(slope_CTI_merged> 0) 
# 52 series
df_CTI_decrease <-  data.series65wGLS %>% filter(slope_CTI_merged< 0) 
# 13 series

table(df_CTI_increase$Community.acronym)
table(df_CTI_increase$Habitat.Group)
table(df_CTI_increase$Water_Mass)
table(df_CTI_increase$Open_Close)

df_CTI_increase[df_CTI_increase=="demersal_pelagic"] <- NA
df_CTI_increase$tropi_minus_deboreal <- df_CTI_increase$tropicalization - df_CTI_increase$deborealization

# Biological groups

options(na.action = "na.omit")
boxplot(df_CTI_increase$tropi_minus_deboreal~df_CTI_increase$Community.acronym)
mylm.means <-lm(tropi_minus_deboreal ~ Community.acronym-1, data=df_CTI_increase) 
summary (mylm.means)

# Habitat.Group
boxplot(df_CTI_increase$tropi_minus_deboreal~df_CTI_increase$Habitat.Group)
mylm.means <-lm(tropi_minus_deboreal ~ Habitat.Group-1, data=df_CTI_increase) 
summary (mylm.means)

# Water_Mass / Sea region
boxplot(df_CTI_increase$tropi_minus_deboreal~df_CTI_increase$Water_Mass)
mylm.means <-lm(tropi_minus_deboreal ~ Water_Mass-1, data=df_CTI_increase) 
summary (mylm.means)

# Basin / Seascape connectivity
boxplot(df_CTI_increase$tropi_minus_deboreal~df_CTI_increase$Open_Close, ylab="(-) Deboralization       Tropicalization (+)")
lm1.3.means <-lm(tropi_minus_deboreal ~ Open_Close-1, data=df_CTI_increase) 
summary (lm1.3.means)

# all factors (selection)
df_CTI_increase2 <- subset(df_CTI_increase, !is.na(Habitat.Group))
options(na.action = "na.omit")
myANOVA <-lm(tropi_minus_deboreal ~ Community.acronym + Habitat.Group + Water_Mass + Open_Close, data=df_CTI_increase2) 
AIC(myANOVA)
options(na.action = "na.fail")
myANOVA.dredge <- dredge(myANOVA)
# visualizing the best models:
myANOVA.dredge 
# 1st best model
summary(get.models(myANOVA.dredge, 1)[[1]]) 
# best model: 2 models were selected with same AICc:
# 1) Habitat + Water_Mass
# 2) Habitat + Water_Mass + Open_Close
  
AIC(lm(tropi_minus_deboreal ~ Community.acronym, data=df_CTI_increase2))
AIC(lm(tropi_minus_deboreal ~ Habitat.Group, data=df_CTI_increase2))
AIC(lm(tropi_minus_deboreal ~ Water_Mass, data=df_CTI_increase2))
AIC(lm(tropi_minus_deboreal ~ Open_Close, data=df_CTI_increase2))

# Levene's test to test if levels of a factor have equal variances
leveneTest(df_CTI_increase2$tropi_minus_deboreal ~ df_CTI_increase2$Community.acronym)
leveneTest(df_CTI_increase2$tropi_minus_deboreal ~ df_CTI_increase2$Water_Mass)
leveneTest(df_CTI_increase2$tropi_minus_deboreal ~ df_CTI_increase2$Habitat.Group)
leveneTest(df_CTI_increase2$tropi_minus_deboreal ~ df_CTI_increase2$Open_Close)    

# Non parametric test

kruskal.test(tropi_minus_deboreal ~ Water_Mass, data = df_CTI_increase2) 
pairwise.wilcox.test(df_CTI_increase2$tropi_minus_deboreal, df_CTI_increase2$Water_Mass, p.adjust.method = "BH")
kruskal.test(tropi_minus_deboreal ~ Habitat.Group, data = df_CTI_increase2) 
kruskal.test(tropi_minus_deboreal ~ Open_Close, data = df_CTI_increase2) 
pairwise.wilcox.test(df_CTI_increase2$tropi_minus_deboreal, df_CTI_increase2$Open_Close, p.adjust.method = "BH")

# 3. Process on fish (results in Discussion) ------------------------------------------------------------------------

df_CTI_increase_fish <-  subset(df_CTI_increase,Community.acronym == "Fish") 
lm2 <-lm(tropicalization ~ Open_Close -1, data=df_CTI_increase_fish) 
summary (lm2)
anova(lm2)
boxplot(df_CTI_increase_fish$tropicalization~df_CTI_increase_fish$Open_Close)

# 4. Processes of colonization and local extinction ------------------------------------------------------------------
# Processes of colonization (expansion): (tropicalization + borealization)
# Processes local extinction (retraction): detropicalization + deborealization
# Hypothesis H1: in communities of dispersal limitation (semi-enclosed seas), colonization should be lower than local extinction

df_sint$expansion <-  df_sint$tropicalization + df_sint$borealization
df_sint$retraction <- df_sint$detropicalization + df_sint$deborealization
df_sint$exp_minus_ret <- df_sint$expansion - df_sint$retraction

lm3.4 <-lm(exp_minus_ret ~ Open_Close, data=df_sint) 
boxplot(df_sint$exp_minus_ret~df_sint$Open_Close, ylab="(-) Retraction       Expansion (+)")
summary(lm3.4)
# Conclusion: expansion is significantly higher in open than in semi-enclosed, supporting H1