###########################################################################################################
##   CTI code STEP 3/3, Trends and Process
##   last updated 20/07/2021
##   Project: H2020 FutureMARES
##   Authors: Matthew McLean, E. Villarino & G. Chust, contacts: mcleamj@gmail.com, evillarino@azti.es; gchust@azti.es 
#     1. Read midpoint data from CTI code Step 1/3 and calculate Community Temperature Index (CTI) 
#                   for each year of the time-series.
#     2. Analyze CTI trends over time and process behind (tropicalization, borealization, ...)
#     3. References: Villarino et al. 2020 MEPS 636:47-61. https://www.int-res.com/abstracts/meps/v636/p47-61/
#                    McLean et al. 2021 Current Biology
############################################################################################################

# LOAD libraries 
library (ggplot2)              # plot
library (patchwork)

# DATA INPUTS ------------------------------------------------------------------------------------------------------
Sample.name    <- "Hard_bottom_benthos_BoB"
Community      <- "Hard Bottom" # types: Soft bottom, Hard bottom, Fish, Phytoplankton, Zooplankton
Region         <- "Bay of Biscay"
Species_number <-  146 # number of species analyzed
longitud_st    <-  -5  # longitude of the Station (Western: -180 to 0, Eastern: 0 to 180)
latitude_st    <-  45  # N Latitude of the station (-74 to 65)

# Load temperature file from Script 1: "all_years_depth.RData"
# Load time series file from Script 2: "Data_thermal_time_W.RData" #
load("1_EXAMPLE_CTI_CALCULATION/OUTPUT/all_years_depth.RData")            # time series of local sea temperatures
load("1_EXAMPLE_CTI_CALCULATION/OUTPUT/Data_thermal_time_W_HB_BoB.RData") # species thermal preferences over time

# END OF DATA INPUTS -----------------------------------------------------------------------------------------------

# name directory to save object
output.wd <- "1_EXAMPLE_CTI_CALCULATION/OUTPUT"

####################
# 1. CALCULATE CTI #
####################

Data_abund_HB <- Data_thermal_time_W_HB_BoB[,11:ncol(Data_thermal_time_W_HB_BoB)] # select interest variables
Data_CTI_W_HB<- colSums(log10(Data_abund_HB+1)*Data_thermal_time_W_HB_BoB$sst_MP) / colSums(log10(Data_abund_HB+1)) #calculate CTI
Data_CTI_W_HB.df <- as.data.frame(Data_CTI_W_HB) # do dataframe
Data_CTI_W_HB.df$Year <- as.numeric(row.names(Data_CTI_W_HB.df))
names(Data_CTI_W_HB.df) <- c("CTI", "Year")

#############################################
## CALCULATE RATE OF CHANGE (SLOPE) IN CTI ##
#############################################

CTI_change <- summary(lm(CTI ~ Year, data = Data_CTI_W_HB.df))$coefficients[2]
CTI_change_p <- summary(lm(CTI ~ Year, data = Data_CTI_W_HB.df))$coefficients[2,4]

##################################### 
## CALCULATE MEAN CTI ACROSS YEARS ##
#####################################

CTI_mean <- mean(Data_CTI_W_HB.df$CTI)

############################################################
## CALCULATE RATE OF CHANGE IN ABUNDANCE FOR EACH SPECIES ##
############################################################

species_abund <- Data_abund_HB
rownames (species_abund) <- Data_thermal_time_W_HB_BoB$Species
species_change <- NULL

for(i in 1:nrow(species_abund)){
  species_slope <- summary(lm(as.numeric(log10(species_abund[i,]+1)) ~ as.numeric(Data_CTI_W_HB.df$Year)))$coefficients[2]
  species_change <- rbind(species_change, data.frame(species=rownames(species_abund)[i], change=species_slope))
  }

species_change$sst_MP <- Data_thermal_time_W_HB_BoB$sst_MP


#data check
sum(species_change$change==0)

#######################################################
## 2. CALCULATE UNDERLYING ECOLOGICAL PROCESS INTENSITY ## 
#######################################################

tropicalization <- subset(species_change, species_change$change > 0 & species_change$sst_MP > CTI_mean)
tropicalization$sst_MP_diff <- tropicalization$sst_MP - CTI_mean
tropicalization$sst_MP_wtd_change <- tropicalization$change * tropicalization$sst_MP_diff
tropicalization$process <- "tropicalization"
trop_intensity <- sum(abs(tropicalization$sst_MP_wtd_change))

deborealization <- subset(species_change, species_change$change < 0 & species_change$sst_MP < CTI_mean)
deborealization$sst_MP_diff <- deborealization$sst_MP - CTI_mean
deborealization$sst_MP_wtd_change <- deborealization$change * deborealization$sst_MP_diff
deborealization$process <- "deborealization"
deb_intensity <- sum(abs(deborealization$sst_MP_wtd_change)) 

borealization <- subset(species_change, species_change$change > 0 & species_change$sst_MP < CTI_mean)
borealization$sst_MP_diff <- borealization$sst_MP - CTI_mean
borealization$sst_MP_wtd_change <- borealization$change * borealization$sst_MP_diff 
borealization$process <- "borealization"
bor_intensity <- sum(abs(borealization$sst_MP_wtd_change))

detropicalization <- subset(species_change, species_change$change < 0 & species_change$sst_MP > CTI_mean)
detropicalization$sst_MP_diff <- detropicalization$sst_MP - CTI_mean
detropicalization$sst_MP_wtd_change <- detropicalization$change * detropicalization$sst_MP_diff
detropicalization$process <- "detropicalization"
detrop_intensity <- sum(abs(detropicalization$sst_MP_wtd_change))

process_sum <- sum(trop_intensity, deb_intensity, bor_intensity, detrop_intensity)

trop_intensity <- trop_intensity/process_sum ; trop_intensity
deb_intensity <- deb_intensity/process_sum ; deb_intensity
bor_intensity <- bor_intensity/process_sum ; bor_intensity
detrop_intensity <- detrop_intensity/process_sum ;detrop_intensity

# 2.1 save species change process
species_change_process <- rbind(tropicalization,detropicalization,borealization,deborealization)
species_change_process$CTI_mean <- CTI_mean
save (species_change_process, file=file.path(output.wd, "Species_change_process.RData")) 

########################
## 3. SEA TEMPERATURE ##
########################

## 3.1 Merge local sea temperature and CTI dataset
seatemperature.df <- all_years_depths  
seatemperature.df$Year <- seatemperature.df$year
dat.df_aggr_HB <- merge(Data_CTI_W_HB.df, seatemperature.df, by="Year")

# 3.2. lm SST vs Year vs CTI
summary(lm(CTI~Year,data=dat.df_aggr_HB))         #NS
summary(lm(pottmp_surf~Year,data=dat.df_aggr_HB)) #NS
summary(lm(CTI~pottmp_surf, data=dat.df_aggr_HB)) #NS

# 3.3 save lm models
sink("1_EXAMPLE_CTI_CALCULATION/OUTPUT/model1_summary.txt", type="output") #CTI vs Year
print(summary(lm(CTI~Year,data=dat.df_aggr_HB)))
sink()

sink("1_EXAMPLE_CTI_CALCULATION/OUTPUT/model2_summary.txt", type="output") # SST vs Year
print(summary(lm(pottmp_surf~Year,data=dat.df_aggr_HB)))
sink()

sink("1_EXAMPLE_CTI_CALCULATION/OUTPUT/model3_summary.txt", type="output")
print(summary(lm(CTI~pottmp_surf,data=dat.df_aggr_HB)))
sink()

# 4. generate more variables to add to the "results" dataset 
SST_change      <- summary(lm(pottmp_surf ~ Year, data = dat.df_aggr_HB))$coefficients[2]
SST_change_p    <- summary(lm(pottmp_surf ~ Year, data = dat.df_aggr_HB))$coefficients[2,4]
ST100m_change   <- summary(lm(pottmp_0_100m ~ Year, data = dat.df_aggr_HB))$coefficients[2]
ST100m_change_p <- summary(lm(pottmp_0_100m ~ Year, data = dat.df_aggr_HB))$coefficients[2,4]
CTI_vs_SST      <- summary(lm(CTI ~ pottmp_surf, data = dat.df_aggr_HB))$r.squared
CTI_vs_SST_p    <- summary(lm(CTI ~ pottmp_surf, data = dat.df_aggr_HB))$coefficients[2,4]
CTI_vs_ST100m   <- summary(lm(CTI ~ pottmp_0_100m, data = dat.df_aggr_HB))$r.squared
CTI_vs_ST100m_p <- summary(lm(CTI ~ pottmp_0_100m, data = dat.df_aggr_HB))$coefficients[2,4]

## 4.1 writing tab results
period_init <- min(Data_CTI_W_HB.df$Year)
period_end  <- max(Data_CTI_W_HB.df$Year)

# 5. do result dataset and metadata
results_hard_bottom_benthos_BoB       <- cbind(Sample.name, Community, Region, Species_number, latitude_st, longitud_st, period_init, period_end, 
                                         SST_change, SST_change_p, ST100m_change, ST100m_change_p, CTI_change, CTI_change_p, 
                                         trop_intensity, deb_intensity, bor_intensity, detrop_intensity, trop_intensity+deb_intensity, bor_intensity+detrop_intensity,
                                         CTI_vs_SST, CTI_vs_SST_p, CTI_vs_ST100m, CTI_vs_ST100m_p)
results_hard_bottom_benthos_BoB        <- as.data.frame(results_hard_bottom_benthos_BoB)
names(results_hard_bottom_benthos_BoB) <- c("Sample.name", "Community", "Region", "Species_number", "latitude_st", "longitud_st", "period_init", "period_end", 
                                            "SST_change", "SST_change_p", "ST100m_change", "ST100m_change_p", "CTI_change", "CTI_change_p", 
                                            "tropicalization", "deborealization", "borealization", "detropicalization", "Trop+Debor","Boreal+Detrop",
                                            "r2.CTI_vs_SST", "r2_p.CTI_vs_SST", "r2.CTI_vs_ST100m", "r2_p.CTI_vs_ST100m")

# 6. write results in output folder
write.csv (results_hard_bottom_benthos_BoB, file=file.path(output.wd, "results_hard_bottom_benthos_BoB.csv")) 

############################
## 7. PLOT CTI and PROCESSES
############################

# 7.1 create process dataset
df_process_hb_benthos_bob <- rbind (tropicalization, deborealization, borealization, detropicalization)
df_process_hb_benthos_bob$Group <- "HB Benthos"
df_process_hb_benthos_bob$CTI_mean <- 15.4768

#plot processes
p1 <- ggplot(df_process_hb_benthos_bob, aes(x=sst_MP, y=change, color=process)) +
  geom_point(size=2.5) + 
  scale_color_manual(values = c("dodgerblue","orange", "purple", "firebrick1")) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        text = element_text(size=16),
        axis.text = element_text(size = 16),
        strip.text.x = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.text=element_text(size=14),
        legend.title = element_blank(),
        legend.position="bottom") + facet_grid (~Group) +
  ylab ("Species abundance change")+  xlab("Species' thermal preference") + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  geom_vline(xintercept = CTI_mean)+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

# 7.2 create CTI dataset
df_CTI_hbb_bob           <- dat.df_aggr_HB
df_CTI_hbb_bob$Group     <- "HB Benthos"
df_CTI_hbb_bob$CTI_mean  <- 15.4768
df_CTI_hbb_bob$CTI_year_sign   <- summary(lm(CTI~Year,data=dat.df_aggr_HB))$coefficients[2,4]< 0.05
df_CTI_hbb_bob$CTI_pottmp_sign <- summary(lm(CTI~pottmp_surf,data=dat.df_aggr_HB))$coefficients[2,4]< 0.05

# 7.3 save CTI dataset
save (df_CTI_hbb_bob, file=file.path(output.wd, "df_CTI_hbb_bob.RData")) 

# plot cti vs year
p2 <- ggplot(df_CTI_hbb_bob, aes(x=Year, y=CTI)) + 
  geom_line(size=1.5) + facet_grid(~Group)+
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        text = element_text(size=16),
        axis.text = element_text(size = 16),
        strip.text.x = element_text(size = 16, color = "black"))+
  ylab("CTI")+xlab("Year")
  
# plot obs sst vs cti
p3 <- ggplot(df_CTI_hbb_bob, aes(x=pottmp_surf, y=CTI)) + 
  geom_point(size=1.5) + facet_grid(~Group)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        text = element_text(size=16),
        axis.text = element_text(size = 16),
        strip.text.x = element_text(size = 16, color = "black")) +
  ylab("CTI")+xlab("SST")

# plot p1,p2,p3
(p1|(p2/p3)) + plot_annotation(tag_levels=list(c('a')))& theme(plot.tag = element_text(size = 25))

# save plot
ggsave(file=file.path(output.wd,"Figure_CTI_processes.png"),  width=20, height=14, dpi=300)

