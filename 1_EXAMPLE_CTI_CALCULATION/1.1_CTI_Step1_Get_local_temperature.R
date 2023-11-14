###########################################################################################################
##   CTI code STEP 1/3: Get local sea temperature
##   Get local potential temperature from GODAS (0.3x1, 1980-2020) https://www.psl.noaa.gov/data/gridded/data.godas.html
##   last updated 20/07/2021,
##   Project: H2020 FutureMARES
##   Authors : E. Villarino, G. Chust, M. McLean, evillarino@azti.es, gchust@azti.es
############################################################################################################ 

# load libraries
library(tidync)
library(ncmeta)
library(plyr)
library(ggplot2)

# Set working directory in folder cloned from github 

# download files from NOAA GODAS. WARNING : Downloads ~4GB of data, take it easy
options(timeout=6000)
years  <- 1980:2020
godas_base_url <- "https://downloads.psl.noaa.gov/Datasets/godas/pottmp"
#dir.create("../Test")
dir.create("1_EXAMPLE_CTI_CALCULATION/DATA_GODAS_POTTMP/Global_1980_2020")
for(i in years){
  
  url_year <-paste(godas_base_url,i,"nc",sep=".")
  #dest_file <- paste("../Test/","pottmp.",i,".nc",sep="")
  dest_file <- paste("1_EXAMPLE_CTI_CALCULATION/DATA_GODAS_POTTMP/Global_1980_2020/","pottmp.",i,".nc",sep="")
  print("Downloading from :")
  print(url_year)
  print("Saving to :")
  print(dest_file)
  download.file(url_year,dest_file,mode = "wb")
}


# INPUTS: select coordinates of the monitoring station -------------------------------------------------

longitud_st = -5  # longitude of the Station (Western: -180 to 0, Eastern: 0 to 180)
latitude_st = 45 # N Latitude of the station (-74 to 65)
range_Lat = 4   # range in degrees (>2)
range_Lon = 6   # range in degrees (>2)

year_init = 1980 # initial year of the period (>1979)
year_end  = 2020 # end year of the period (<2021))

#depth_m = NULL # nc have 40 levels from 5.0 m to 4478 m depth
memory.limit(size = 25000) # needed for large ranges

# END of INPUTS ----------------------------------------------------------------------------------------

# 0. name working directory where files are and output where output files will be
dat.wd    <- "1_EXAMPLE_CTI_CALCULATION/DATA_GODAS_POTTMP/Global_1980_2020"
output.wd <- "1_EXAMPLE_CTI_CALCULATION/OUTPUT"

# 1. set coordinate ranges
lon_neg <- ifelse(longitud_st<0, 360+longitud_st, longitud_st) # transform -180+180º long to 0-360º
lonrange <- c(lon_neg-range_Lon, lon_neg+range_Lon)
latrange <- c(latitude_st-range_Lat, latitude_st+range_Lat)

# 1.1 check how the data looks like for one year, for example 1980
oisstfile <- file.path("1_EXAMPLE_CTI_CALCULATION/DATA_GODAS_POTTMP/Global_1980_2020/pottmp.1980.nc")
oisst <- tidync(oisstfile)
oisst_data <- oisst %>% hyper_array()
trans <- attr(oisst_data, "transforms")
image(trans$lon$lon, trans$lat$lat, oisst_data[[1]][,,1,6]) # for example 5m layer 6th month
rect(lonrange[1], latrange[1], lonrange[2], latrange[2])

# 2. do dataframe for interest area and all years

years<-c(year_init:year_end)

make_year<-function(x,latrange,lonrange,folder){
  print(x)
  oisstfile <- paste(folder,"/pottmp.",
                     x,".nc",sep="")
  #print(oisstfile)
  oisst <- tidync(oisstfile)
  res<-oisst %>% hyper_filter(lon = lon > lonrange[1] & lon <= lonrange[2], 
                              lat = lat > latrange[1] & lat <= latrange[2])%>%hyper_tibble()
  res$year<-x
  res
}

all_years<-ldply(years,make_year,latrange,lonrange, dat.wd)

# 2.1 add date variable to dataframe
all_years$date <- as.Date(all_years$time, origin="1800-01-01")
names(all_years)[names(all_years) == "level"] <- "depth" #change variable name

# 2.2 convert Kelvin to Celsius
all_years$pottmp <- all_years$pottmp - 273.15 

# 2.3 select interest variables,  and calculate year means
dat.df_mean <- aggregate(pottmp ~ year , data=all_years, FUN="mean")

# 2.4 do sst, 0-100m, and 0-200m depth integrated means
all_years_depths <- ddply(all_years,"year",function(x){
  data.frame(pottmp_surf=mean(x[x$depth<=5,"pottmp"]),
             pottmp_0_100m=mean(x[x$depth<=100,"pottmp"]),
             pottmp_0_200m=mean(x[x$depth<=200,"pottmp"]))
  
}) 

# 2.5 plot Sea temperature sst, 0-100m, and 0-200m
ggplot(all_years_depths, aes(x= year)) +
  geom_line(aes(y=pottmp_surf),size=1) + 
  geom_line(aes(y=pottmp_0_100m),color = "darkred",size=1) + 
  geom_line(aes(y=pottmp_0_200m),color="steelblue",size=1) + 
  theme_bw() +  
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position = "none")

summary(lm(formula =  pottmp_surf   ~ year, data = all_years_depths))   # trend SST
summary(lm(formula =  pottmp_0_100m ~ year, data = all_years_depths)) # trend pottmp_0_100m
summary(lm(formula =  pottmp_0_200m ~ year, data = all_years_depths)) # trend pottmp_0_200m

# 3. save
save (all_years_depths, file=file.path(output.wd, "all_years_depth.RData"))


