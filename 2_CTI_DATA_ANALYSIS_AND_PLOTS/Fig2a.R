############################################################################################################
##   SST change map and locations of the case studies 
##   Author: Ernesto Villarino, evillarino@azti.es 
##   Project: H2020 FutureMARES
##   Paper: Cross-basins and cross-taxon patterns in biodiversity turnover in warming seas  
#############################################################################################################  

#load libraries
library (ggplot2)  #plot
library (dplyr)    #data manipulation
library (ggrepel)  #plot customization
library(sf)        #spatial vector data
library(sp)        #spatial data
library(gstat)     #spatial geostatistics 

# DATA INPUTS ------------------------------------------------------------------------------------------------------

# Load R objects to plot map and sampling points of case studies
load("2_CTI_DATA_ANALYSIS_AND_PLOTS/R_OBJECTS/R_objects.RData")  
output.wd <- "2_CTI_DATA_ANALYSIS_AND_PLOTS/OUTPUT" # name directory to save object

# END OF DATA INPUTS -----------------------------------------------------------------------------------------------

# 1. rearrange case studies according to CTI slope
df_order <-  data.series65wGLS %>% arrange(slope_CTI_merged)
df_order$Case_study_1 <- rep(1:65)

# 2. do interpolation for the study area and create a gridded structure
grd <- expand.grid(lon = unique(slopes$lon), lat =  unique(slopes$lat)) 
slopes2 <- st_as_sf(slopes,coords =c("lon","lat"))
grd <- st_as_sf(grd,coords=c("lon","lat"),remove=FALSE)

# 3. nterpolate surface and fix the output. Apply idw model for the data
idw <- gstat::idw(formula = slope_surf ~ 1, locations = slopes2, newdata = grd)
idw[,c("lon","lat")]<-coordinates(as(idw,"Spatial"))
idw$geometry<-NULL
names(idw)[1:4] <- c("slope_surf_pred", "slope_surf_var","lon", "lat")
final_slopes <- merge(idw,slopes,by=c("lat","lon"),all.x=TRUE)
final_slopes$slope_surf_final <- ifelse(is.na(final_slopes$slope_surf),final_slopes$slope_surf_pred,final_slopes$slope_surf)

# 3.1 remove polygon points
df_order <- df_order [-c(38,42,47,55), ]

# 4. plot
slopes[,2]<-round(slopes[,2]) 
p1 <- ggplot(final_slopes)+
  geom_tile(aes(x=lon, y=lat, fill=slope_surf_final*10))     +
  borders(fill="gray",colour="NA") +
  geom_sf(data=df_polys3,col="black",lty=3, fill=NA, lwd=1.3) +
  geom_point(data=df_order, aes(x=longitude_st, y=latitude_st),fill="black",stroke=1,size=4, shape=21)+
  coord_sf(xlim = c(-9.5,33),ylim=c(33,60.5)) +
  theme_bw()  + 
  geom_text_repel (data=df_order, aes(x=longitude_st, y=latitude_st, label=Case_study_1), size=8, max.overlaps = Inf)+
  geom_text (x=20, y=57, label="42",   size=8, color="blue")+
  geom_text (x=-5, y=47, label="38",   size=8, color="blue")+
  geom_text (x=3, y=57, label="47",    size=8, color="blue")+
  geom_text (x=-8.5, y=57, label="55", size=8, color="blue")+
  theme(strip.text.x = element_text(size = 14),
        legend.text = element_text(size=18),
        axis.text = element_text(size = 14),
        legend.title = element_text(size=18),
        legend.position = c(.9, .7))+
  scale_fill_gradient2(high="red", low="blue", limits =  c(-0.1,0.57),guide = guide_colorbar(label = T, draw.ulim = T, draw.llim = T,frame.colour = "black", ticks = T))+
  labs (y="", x = "") + labs(fill = "SST change \n(ºC / decade)") 
p1

# save plot
ggsave(file=file.path(output.wd,"Fig2a.png"),  width=16, height=16, dpi=300)




