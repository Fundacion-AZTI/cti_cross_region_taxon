####################################################################################################
##   Spatial Distribution of rates of change in CTI over time (ºC decade-1) across the biological 
##   groups within European seas
##   Author: Ernesto Villarino, evillarino@azti.es 
##   Project: H2020 FutureMARES
##   Paper: Cross-basins and cross-taxon patterns in biodiversity turnover in warming seas   
#################################################################################################### 

# load libraries
library (tidyverse) #data manipulation
library (ggplot2)   #plot
library (alphahull) #spatial data
library (igraph)    #spatial analysis
library (plyr)      #data manipulation
library (sf)        #spatial analysis

# DATA INPUTS ------------------------------------------------------------------------------------------------------

# Load R objects 
load("2_CTI_DATA_ANALYSIS_AND_PLOTS/R_OBJECTS/R_objects.RData")  
output.wd <- "2_CTI_DATA_ANALYSIS_AND_PLOTS/OUTPUT" # name directory to save object

# END OF DATA INPUTS -----------------------------------------------------------------------------------------------

# 1. Map CTI change, points + areas----- 

# 1.1 Do polygon for DATRAS fish dataset
names (lat_long_datras) <- c("long", "lat", "area")

# 1.2 create df_area dataframe with data.series65wGLS CTI slope 
df_area <- lat_long_datras
df_area$lat <- as.numeric(df_area$lat)
df_area$Group <- rep("Fish")
df_area$CTI_change <- rep(c(0.0236780474,0.0230389672,0.0261239229,0.0291228921),times=c(464,320,1745,284)) 


# 1.3 do polygons to represent areas
ashape2poly <- function(ashape){
  # Convert node numbers into characters
  ashape$edges[,1] <- as.character(ashape$edges[,1])
  ashape_graph <- graph_from_edgelist(ashape$edges[,1:2], directed = FALSE)
  if (!is.connected(ashape_graph)) {
    stop("Graph not connected")
  }
  if (any(degree(ashape_graph) != 2)) {
    stop("Graph not circular")
  }
  if (clusters(ashape_graph)$no > 1) {
    stop("Graph composed of more than one circle")
  }
  # Delete one edge to create a chain
  cut_graph <- ashape_graph - E(ashape_graph)[1]
  # Find chain end points
  ends = names(which(degree(cut_graph) == 1))
  path = get.shortest.paths(cut_graph, ends[1], ends[2])$vpath[[1]]
  # this is an index into the points
  pathX = as.numeric(V(ashape_graph)[path]$name)
  # join the ends
  pathX = ashape$x[c(pathX, pathX[1]),]
  colnames(pathX)<- c("long", "lat")
  
  return(pathX)
}
area0<-df_area[df_area$area=="0",]
area0<- area0[!duplicated(paste(area0$long, area0$lat)), ]
area0<- area0[!(area0$long>11.25&area0$long<12&area0$lat>54.5&area0$lat<55.5), ]
df_area_temp<-df_area[!df_area$area=="0",]
df_area_temp<-rbind(df_area_temp,area0)

# 1.4 merge with point dataset
polys<-dlply(df_area_temp,"area",function(x){
  
  print(x$area[1])
  print(dim(x))
  x<- x[!duplicated(paste(x$long, x$lat)), ]
  if(dim(x)[1]<3){
    return(NULL)
  }else{
    a<-try({
      border<-ashape(x[,c(1,2)], alpha = 1e10)
      poly <- ashape2poly (border)
      sf_BITS<-st_sf(data.frame(name=x$area[1]),
                     geometry=st_sfc(st_polygon(list(poly)))) 
    })
    print(class(a))
    if(inherits(a,"try-error")){
      print(x$area[1])
      return(NULL)
    }else{
      return(a)
    }
  }
})
polys<-dplyr::bind_rows(polys)
polys<-st_sf(polys)
colnames(polys)[1]<-"area"
df_area_names<-ddply(df_area,c("area","Group"),function(x){
  data.frame(CTI_change=x$CTI_change[1]) 
})
polys3<-merge(polys,df_area_names,by="area") 
polys3$slope_CTI_merged <- rep(c(0.0236780474,0.0230389672,0.0261239229,0.0291228921)) #check!

# 1.5 change some areas by points (only datras polygon) 
df2 <- data.series65wGLS  [-c(1:4),]  %>% select(Sample.name, latitude_st, longitude_st, slope_CTI_merged, Group) # remove points 
df2$Group<- factor(df2$Group, (levels=c("CHB benthos","CSB benthos", "Zooplankton","Crustacea", "Cephalopods", "Fish")))

# 1.6 rename to remove Wadden Sea labels
df2 [grep ("Wadden", df2$Sample.name), "Sample.name"] <- "Wadden" # 

# 1.7 do CTIr mean across case studies
df3 <- df2  %>% 
  dplyr:: group_by  (Sample.name, Group, latitude_st, longitude_st) %>% 
  dplyr:: summarize (slope_CTI_merged = mean(slope_CTI_merged))

# 1.8 rename polys
df_polys3$Group<- factor(df_polys3$Group, (levels=c("CHB benthos","CSB benthos", "Zooplankton","Cephalopods", "Mollusca", "Fish")))

#plot
p1 <- ggplot(df3)+
  geom_sf(data=df_polys3,aes(fill=CTI_change),col="black",lty=2) +
  borders(fill="darkgrey",colour="NA")  +  
  geom_point(aes(x=longitude_st, y=latitude_st, fill=slope_CTI_merged),size=6, shape=21)+
  coord_sf(xlim = c(-11,35),ylim=c(32,62)) +
  theme_bw() + facet_wrap( ~ Group, nrow=3) + 
  theme(strip.text.x = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightgrey", colour = "lightgrey", linewidth = 0.5, linetype = "solid"))+
  scale_fill_gradient2(high="darkred", low="blue", limits =  c(-0.05,0.17),
                       guide = guide_colorbar(label = T, draw.ulim = T, draw.llim = T,
                                              frame.colour = "black", ticks = T))+
  labs (fill=(expression(paste("CTI",italic(" r")," (°C/decade)"))))
p1 

# save plot
ggsave(file=file.path(output.wd,"SFig2.png"),  width=16, height=16, dpi=300)
