###############################################################################################################################

CTI analysis in R taken for each case study
FutureMARES project Task 1.1 (CTI)
Authors: Guillem Chust, Ernesto Villarino, Martin Lindegren, Matthew McLean
Contact: mcleamj@gmail.com, evillarino@azti.es; gchust@azti.es
last updated: 23/10/2023

###############################################################################################################################

Script 1: Get local potential temperature from GODAS  (0.3x1, 1980-2020)   https://www.psl.noaa.gov/data/gridded/data.godas.html
 
- Input: ../Global_1980_2020/pottmp.1980.nc (include all files from 1980-2020)
- Output: all_years_depth.RData

Script 2: Estimate thermal preferences for each species from OBIS occurrences and local temperatures derived from GODAS, which 
are available from OBIS for each observation. 

- Input
           Species  Year1	           Year2	            …
           species1 Abundance sp1 in Y1	   Abundance sp1 in Y2	
           species2 Abundance sp2 in Y1    Abundance sp2 in Y2		

- Output: Data_thermal_time_W.RData

Script 3: Analyze CTI trends over time and underlying ecological processes (tropicalization, borealization, detropicalization, 
deborealization)

- Input: Data_thermal_time_W.RData (from Script 2)
- Input: time series of local ocean temperatures (dat.df_GODAS.RData, from Script 1)
- Output: csv file with trends on SST, CTI and underlying ecological processes, linear model results and exploratory plots 

