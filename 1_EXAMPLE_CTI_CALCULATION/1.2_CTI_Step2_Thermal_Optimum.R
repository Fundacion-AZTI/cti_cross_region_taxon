##########################################################################################################
##   CTI code STEP 2/3 : Calculate species thermal optimum
##   last updated 20/07/2021,
##   Project: H2020 FutureMARES
##   Authors: E. Villarino, G. Chust, M. McLean, contacts: evillarino@azti.es; gchust@azti.es 
#    1.  Read species data from "our dataframe"
#    2.  Get global ocurrences from OBIS https://obis.org/, https://ropensci.org/blog/2017/01/25/obis/ 
#    3.  Get OBIS SST data for each occurrence 
#    4.  Calculate species thermal optimum (Burrows et al., Stuart-Smith et al., Villarino et al., Webb et al.) 
##########################################################################################################

# load libraries 
library(robis)       # obis in r
library(worrms)      # OBIS uses the World Register of Marine Species (WoRMS) as its taxonomic backbone
library(devtools)    # to install robis 
library(gtools)      # smartbind
library(readxl)      # read excel files
library(dplyr)       # to squeeze data and go for statistics

# DATA INPUTS ------------------------------------------------------------------------------------------------------
load("1_EXAMPLE_CTI_CALCULATION/DATA_HARD_BOTTOM_BENTHOS_BASQUE_COAST/Dat_hb_benthos.RData") # biological data, species in rows, year abundances in cols

# name directory to save object
output.wd <- "1_EXAMPLE_CTI_CALCULATION/OUTPUT"

# END OF DATA INPUTS -----------------------------------------------------------------------------------------------

# 1. Get global occurrences from OBIS----  
my_occs_hb_bob <- data.frame()
for (i in 1:nrow(Dat_hb_benthos)) {
   ## get species occurrences
   mydata.obis0<-robis::occurrence(scientificname=Dat_hb_benthos$OBIS[i])

   ## select variables and name them with the same name
   if("sst" %in% colnames(mydata.obis0))
   {
   cat("\n");
   mydata.obis <- subset(mydata.obis0, select=c(scientificName,decimalLongitude,decimalLatitude,sst)) # get only useful data
   my_occs_hb_bob <- rbind(my_occs_hb_bob,mydata.obis) 
   }
}

# 2. Get SST data from OBIS ----
# OBIS has a column with climatological means (2000-2014) of SST for each occurrence taken from Bio-ORACLE 2 database at 1/4 resolution.
# Bio-ORACLE sources the data from ARMOR via Copernicus. See Assis et al. 2017

# 2.1 rename
data2W <- my_occs_hb_bob  
   
# 2.2 select one exmaple species and plot occurrence frequency vs sst
data_example  <- data2W [ which(data2W$scientificName=='Actinia equina' ),]
hist (data_example$sst, breaks=12, xlab= 'SST', main = 'Actinia equina')

# 2.3 Calculate species thermal midpoint (STI) and quartiles distribution ----
data_thermal_W <- data2W %>%
  group_by(scientificName) %>%
  summarise(sst_mean = mean(sst, na.rm=T), 
            sst_median = median(sst,na.rm=T),
            sst_q5  = quantile(sst, probs = 0.05,na.rm=T),
            sst_q10 = quantile(sst, probs = 0.10,na.rm=T),
            sst_q50 = quantile(sst, probs = 0.50,na.rm=T),
            sst_q90 = quantile(sst, probs = 0.90,na.rm=T),
            sst_q95 = quantile(sst, probs = 0.95,na.rm=T),
            sst_MP  = (sst_q5 + sst_q95)/2,
            count   = n()) %>% 
  ungroup()

#check 
str (data_thermal_W) # 227 obs. subspecies are included when we extract data from OBIS.

# 2.4  mind names of dataset to be merged 
colnames(data_thermal_W)[1]   <- "Species"
colnames(Dat_hb_benthos)[1]   <- "Species"

# 3. merge data_thermal and Dat_algae and delete < 100 obs  
Data_thermal_time_W1  <- merge(data_thermal_W, Dat_hb_benthos, by="Species") # 227|199|177 sp in common

# 4. get species with occurrence > 100 obs. and save
Data_thermal_time_W_HB_BoB   <-  subset(Data_thermal_time_W1, count > 100) #146 sp.

# 5. save
save (Data_thermal_time_W_HB_BoB, file=file.path(output.wd, "Data_thermal_time_W_HB_BoB.RData")) 
