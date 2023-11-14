#   CTI code 1: temporal trend analysis of CTI and ST 
#         1. Estimation of mean slope of SST with LMM
#         2. Estimation of slopes of CTI with GLS and autocorrelation
#         3. Estimation of mean slope of CTI with LMM
#         4. Analysis of CTI by factors and model selection
#   Data: time series of 65 cases studies
#   last updated 06/09/2023
#   Project: H2020 FutureMARES, LIFE Urban Klima 2050
#   Authors: Guillem Chust, Leire Ibaibarriaga, Ernesto Villarino
#   Citation: Chust et al. Submitted NComm

# load libraries
library(dplyr)
library(car)
library(nlme)
library(MuMIn)

# DATA INPUTS ------------------------------------------------------------------------------------------------------

# Load R objects 
load("2_CTI_DATA_ANALYSIS_AND_PLOTS/R_OBJECTS/R_objects.RData")  
output.wd <- "2_CTI_DATA_ANALYSIS_AND_PLOTS/OUTPUT" # name directory to save object

# END OF DATA INPUTS -----------------------------------------------------------------------------------------------

# 0. assign factors from metadata (df_sint) to CTI time series (df_trends_65)
df_trends_65_all <- merge(df_trends_65, df_sint, by="Case_study")

# 0.1 rename df
mydata.d2 <- df_trends_65_all
dd <- subset(mydata.d2, !is.na(CTI))

# 0.2 Statistics time period
df_sint$period_range <- df_sint$period_end - df_sint$period_init
summary(df_sint$period_range)
summary(df_sint$period_init)

# 1. LME for ST vs Year with temporal random effects (Case Study) and temporal autocorrelation ------------------

# SST
mylm.SST <- lm(pottmp_surf~Year, data=dd); summary(mylm.SST)
mylme0.SST <- lme(pottmp_surf~Year, random=~1|Case_study, data=dd); summary(mylme0.SST)
mylme.SST <- lme(pottmp_surf~Year, random=~1|Case_study, data=dd, correlation = corAR1())
summary(mylme.SST)

# save model
save (mylme.SST, file=file.path(output.wd, "mylme.SST.RData"))

# ST 100 m
dd_sub <- subset(dd, !is.na(pottmp_0_100m))
mylme.ST100 <- lme(pottmp_0_100m~Year, random=~1|Case_study, data=dd_sub, correlation = corAR1())
summary(mylme.ST100)

# 2. Estimate CTIr independently with GLS ----

# function confidence intervals for pacf
get_clim <- function(x, ci=0.95, ci.type="white"){
  #' Gets confidence limit data from acf object `x`
  if (!ci.type %in% c("white", "ma")) stop('`ci.type` must be "white" or "ma"')
  if (class(x) != "acf") stop('pass in object of class "acf"')
  clim0 <- qnorm((1 + ci)/2) / sqrt(x$n.used)
  if (ci.type == "ma") {
    clim <- clim0 * sqrt(cumsum(c(1, 2 * x$acf[-1]^2))) 
    return(clim[-length(clim)])
  } else {
    return(clim0)
  }
}

# loop to calculate acf, CTI slope and ci of 65 time series
df2<-NULL
df2_char<-NULL
i=0
for (Case_study in unique(dd$Case_study)) {
  # Steps: 1. if there is not autocorrelation in the series, we keep with lm
  #        2. f there is not autocorrelation in the series, we perform a gls with autocorrelation
  i=i+1
  ddi <- subset(dd, Case_study==unique(dd$Case_study)[i])
  nts <- max(ddi$Year) - min(ddi$Year) + 1
  Group = as.character(ddi$Group[1])
  
  # Autocorrelation and Residuals
  mypacf<-pacf(ddi$CTI, lag.max = 1, plot=F)
  # Extract confidence interval values from ACF correlogram https://stackoverflow.com/questions/14266333/extract-confidence-interval-values-from-acf-correlogram
  clim <- get_clim(mypacf, ci.type = "white") # returns a single ci limit value (ci is plus or minus this value)
  mypacf.ci.low <- mypacf$acf[1]-clim
  mypacf.ci.upp <- mypacf$acf[1]+clim
  if (mypacf.ci.low<0 & mypacf.ci.upp>0) 
  {mypacf.mean=0} else {mypacf.mean=mypacf$acf[1]}
  
  # Model lm without autocorrelation
  m.lmi <-  lm(CTI ~ Year, data=ddi)
  slope_CTI_lm <- summary(m.lmi)$coefficients[2]
  SE_lm <- coef(summary(m.lmi))[2,2]
  pval_lm <- coef(summary(m.lmi))[2,4]
  lm.ci.low=slope_CTI_lm + qt(0.025, nts-2)*SE_lm
  lm.ci.upp=slope_CTI_lm - qt(0.025, nts-2)*SE_lm
  
  m.gls0 <- gls(CTI ~ Year, data=ddi) # equivalent to a lm
  
  # Model GLS with autocorrelation
  m.gls1 <- gls(CTI ~ Year, correlation = corAR1(form = ~ Year), data=ddi)
  slope_CTI_gls <- summary(m.gls1)$coefficients[2]
  SE_gls <- coef(summary(m.gls1))[2,2]
  pval_gls <- coef(summary(m.gls1))[2,4]
  ci.low=slope_CTI_gls + qt(0.025, nts-2)*SE_gls
  ci.upp=slope_CTI_gls - qt(0.025, nts-2)*SE_gls

  aux <- try(intervals(m.gls1)$corStruct[,1], silent=T)
  if (class(aux)== "try-error"){
    Phi.ci.low <- NA
    Phi.ci.upp <- NA
    Phi        <- NA
  }else{
    Phi.ci.low <- intervals(m.gls1)$corStruct[,1]
    Phi.ci.upp <- intervals(m.gls1)$corStruct[,3]
    if (Phi.ci.low<0 & Phi.ci.upp>0) 
    {Phi=0} else {Phi=intervals(m.gls1)$corStruct[,2]}
  }
  
  anova.gls<-anova(m.gls0, m.gls1) # OMITTED select the best model based on AIC
  AIC.gls0 = anova.gls[1,4]
  AIC.gls1.wACF = anova.gls[2,4]

  if (mypacf.ci.low<0 & mypacf.ci.upp>0) 
  {LM=1
  slope_CTI_merged=slope_CTI_lm
  pval_merged=pval_lm
  ci.low.merged=lm.ci.low
  ci.upp.merged=lm.ci.upp} else {LM=0
  slope_CTI_merged=slope_CTI_gls
  pval_merged=pval_gls
  ci.low.merged=ci.low
  ci.upp.merged=ci.upp}
  
  dfi <- cbind(nts, mypacf.mean, mypacf.ci.low, mypacf.ci.upp, Phi, Phi.ci.low, Phi.ci.upp, slope_CTI_gls, SE_gls, pval_gls, ci.low, ci.upp, AIC.gls0, AIC.gls1.wACF, LM, slope_CTI_lm, SE_lm, pval_lm, lm.ci.low, lm.ci.upp, slope_CTI_merged, pval_merged, ci.low.merged, ci.upp.merged)
  df2 <- rbind(df2, dfi)
  
  dfi_char <- cbind(Case_study, Group)
  df2_char <- rbind(df2_char, dfi_char)
}
df2 <- as.data.frame(df2) ; str(df2)
df2 <- cbind(df2_char, df2); str(df2)


str(df2)
summary(df2)

# merging df2 (new GLS results) and 65 time series metadata
data.series65wGLS <- merge(df_sint, df2, by="Case_study")

# save dataset
save (data.series65wGLS, file=file.path(output.wd, "data.series65wGLS.RData"))

# check merged data
plot(CTI_change~slope_CTI_lm, data = data.series65wGLS); summary(lm(CTI_change~slope_CTI_lm, data = data.series65wGLS))
data.series65wGLS$CTI_change-data.series65wGLS$slope_CTI_lm
plot(Lenght_time_series~nts, data = data.series65wGLS)

# comparing slopes
plot(CTI_change~slope_CTI_gls, data = data.series65wGLS); summary(lm(CTI_change~slope_CTI_gls, data = data.series65wGLS))
plot(CTI_change~slope_CTI_merged, data = data.series65wGLS); summary(lm(CTI_change~slope_CTI_merged, data = data.series65wGLS))

# comparing ci
plot(lm.ci.low~ci.low, data = data.series65wGLS); summary(lm(lm.ci.low~ci.low, data = data.series65wGLS))
plot(lm.ci.low~ci.low.merged, data = data.series65wGLS); summary(lm(lm.ci.low~ci.low.merged, data = data.series65wGLS))
plot(lm.ci.upp~ci.upp, data = data.series65wGLS); summary(lm(lm.ci.upp~ci.upp, data = data.series65wGLS))
plot(lm.ci.upp~ci.upp.merged, data = data.series65wGLS); summary(lm(lm.ci.upp~ci.upp.merged, data = data.series65wGLS))

df_order <-  data.series65wGLS %>% arrange(slope_CTI_merged)
plot(df_order$slope_CTI_merged, ty="l",col=c(2))
lines(df_order$ci.upp.merged,ty="l", col=c(1))
lines(df_order$ci.low.merged, col=c(3))

# statistics
# CTIr>0
n<-nrow(data.series65wGLS)
nrow(subset(data.series65wGLS,CTI_change>0)); 100*nrow(subset(data.series65wGLS,CTI_change>0))/n 
nrow(subset(data.series65wGLS,slope_CTI_gls>0)); 100*nrow(subset(data.series65wGLS,slope_CTI_gls>0))/n
nrow(subset(data.series65wGLS,slope_CTI_merged>0)); 100*nrow(subset(data.series65wGLS,slope_CTI_merged>0))/n
nrow(subset(data.series65wGLS,slope_CTI_merged<0)); 100*nrow(subset(data.series65wGLS,slope_CTI_merged<0))/n
CTI_pos <- subset(data.series65wGLS,slope_CTI_merged>0)

# CTIr pval <0.05
nrow(subset(CTI_pos,pval_lm<0.05)); 100*nrow(subset(CTI_pos,pval_lm<0.05))/n
nrow(subset(CTI_pos,pval_gls<0.05)); 100*nrow(subset(CTI_pos,pval_gls<0.05))/n
nrow(subset(CTI_pos,pval_merged<0.05)); 100*nrow(subset(CTI_pos,pval_merged<0.05))/n

# 3. LME for CTI vs Year with temporal random effects (Case Study) and temporal autocorrelation ----

mylme.CTI <- lme(CTI~Year, random=~1|Case_study, data=dd, correlation = corAR1())
summary(mylme.CTI)

# save model
save (mylme.CTI, file=file.path(output.wd, "mylme_CTI.Rdata"))
plot.lme(mylme.CTI)

# LME for CTI vs ST with temporal random effects (Case Study) and temporal autocorrelation
mylme.CTIvsSST <- lme(CTI~pottmp_surf, random=~1|Case_study, data=dd, correlation = corAR1())
summary(mylme.CTIvsSST)
mylme.CTIvsST100 <- lme(CTI~pottmp_0_100m, random=~1|Case_study, data=dd_sub, correlation = corAR1())
summary(mylme.CTIvsST100)

# 4. LME for CTI vs Year with temporal random effects (Case Study) and temporal autocorrelation and by Groups (Table 1) ----

# Factors table df_sint

table(df_sint$Community.acronym)
table(df_sint$Group)
table(df_sint$Habitat.Group)
table(df_sint$Water_Mass)
table(df_sint$Open_Close)

# Factors table dd

table(dd$Community.acronym)
table(dd$Group.x)
table(dd$Habitat.Group)
table(dd$Water_Mass)
table(dd$Open_Close)

# Biological group
mylme.1 <- lme(CTI ~ Community.acronym + Year:Community.acronym, random=~1|Case_study, data=dd, correlation = corAR1())
summary(mylme.1)
mylme.1 <- lme(CTI ~ Group.x + Year:Group.x, random=~1|Case_study, data=dd, correlation = corAR1())
summary(mylme.1)

anova(mylme.1)
mylme.1$coefficients$fixed

plot(mylme.1)

res <- resid(mylme.1)
boxplot(res~dd$Community.acronym)
  
mylme.1 <- lme(CTI ~ Habitat.Group + Year:Habitat.Group, random=~1|Case_study, data=dd, correlation = corAR1())
summary(mylme.1)
anova(mylme.1)

mylme.1 <- lme(CTI ~ Water_Mass + Year:Water_Mass, random=~1|Case_study, data=dd, correlation = corAR1())
summary(mylme.1)
anova(mylme.1)

mylme.1 <- lme(CTI ~ Open_Close + Year:Open_Close, random=~1|Case_study, data=dd, correlation = corAR1())
summary(mylme.1)
anova(mylme.1)

# STEP to select the best model according to factors 

# Selection of best factors (test)
#mylme.2 <-lme(CTI ~ Year*Community.acronym + Year*Habitat.Group + Year*Water_Mass + Year*Open_Close, random=~1|Case_study, data=dd, correlation = corAR1())
# all factors in the model causes error due to singularities:
# Error in MEEM(object, conLin, control$niterEM) : 
#  Singularity in backsolve at level 0, block 1
# Therefore, enter factor by factor
 
AIC(lme(CTI ~ Year*Community.acronym, random=~1|Case_study, data=dd, correlation = corAR1()))
AIC(lme(CTI ~ Year*Habitat.Group, random=~1|Case_study, data=dd, correlation = corAR1()))
AIC(lme(CTI ~ Year*Water_Mass, random=~1|Case_study, data=dd, correlation = corAR1()))
AIC(lme(CTI ~ Year*Open_Close, random=~1|Case_study, data=dd, correlation = corAR1()))

options(na.action = "na.fail")

mylme.2 <-lme(CTI ~ Year*Water_Mass + Year*Community.acronym, random=~1|Case_study, data=dd, correlation = corAR1())
mylme.dredge <- dredge(mylme.2)
mylme.dredge
summary(get.models(mylme.dredge, 1)[[1]])
# best: Water_Mass, AICc=1745.1

# check other combinations
mylme.2 <-lme(CTI ~ Year*Water_Mass + Year*Habitat.Group, random=~1|Case_study, data=dd, correlation = corAR1())
mylme.dredge <- dredge(mylme.2)
mylme.dredge
# best: Water_Mass + Habitat
# AICc = 1702.2   

mylme.2 <-lme(CTI ~ Year*Water_Mass + Year*Habitat.Group + Year*Community.acronym, random=~1|Case_study, data=dd, correlation = corAR1())
mylme.dredge <- dredge(mylme.2)
mylme.dredge
# best model: Water_Mass + Habitat
# AICc: 1702.2

mylme.2 <-lme(CTI ~ Year*Open_Close + Year*Habitat.Group + Year*Community.acronym, random=~1|Case_study, data=dd, correlation = corAR1())
mylme.dredge <- dredge(mylme.2)
mylme.dredge
# Best model: Community + Habitat + Open_Close, AICc = 1756.8   

mylme.2 <-lme(CTI ~ Year*Water_Mass + Year*Habitat.Group, random=~1|Case_study, data=dd, correlation = corAR1())

# save model and dataset
mylme.CTI_factors <-lme(CTI ~ Year*Water_Mass + Year*Habitat.Group, random=~1|Case_study, data=dd, correlation = corAR1())
save (mylme.CTI_factors, file=file.path(output.wd, "mylme.2_CTI_factors.Rdata"))
save (dd, file=file.path(output.wd, "dd_dataframe.Rdata")) 
