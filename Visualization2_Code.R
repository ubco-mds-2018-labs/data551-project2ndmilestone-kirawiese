# Map Visualization
# Kira Wiese

air<-read.csv("AQHI_full.csv",header=T)
stations<-read.csv("monitoring_stations.csv")
cities<-read.csv("cities.csv")

##############################################################################
#Preprocessing

library(sp) #classes and methods for spatial data
library(maptools) #tools for reading and handling spatial objects
library(maps) #for creating geographical maps
library(mapdata) #contains basic data to go along with 'maps' (topographic and geologic)
#library(sfsmisc) #utilities from Seminar fuer Statistik ETH Zurich
library(mapproj) #for creating projected maps
library(raster) #tools to deal with raster maps
library(rgeos) #interface to geometry engine - open source (GEOS)
library(rgdal) #bindings for the geospatial data abstraction library
library(scales)
library(RgoogleMaps)
library(ggmap)
library(ggplot2)

library(rgdal)
if (!require(geojsonio)) {
  install.packages("geojsonio")
  library(geojsonio)
}

# Reading a GeoJSON file to overlay on map as a polygon
zones <- geojson_read("bcairzones.geojson", what = "sp")
plot(zones)
zones_df <- fortify(zones)

#Initializing Google API key
key='AIzaSyARC7Hkv0pw56P6ezMTDNCoQA9StAp-YpU'
register_google(key = key)   


mymarkers <- cbind.data.frame(stations$LAT, stations$LONG, stations$ELEVATION)
names(mymarkers) <- c("lat", "lon", "Elevation")

bc_center = as.numeric(geocode("British Columbia"))
BCMap = ggmap(get_googlemap(center=bc_center, zoom=5, size = c(640,640) ,scale = 4, maptype = "roadmap"))
dev.off()

####################################################################################
# Implementation of Visual

image = BCMap +
  geom_polygon(aes(long, lat, group = group), fill=NA,data = zones_df, size =0.8, 
               colour = "coral2", lty="dashed")+
  geom_point(data=mymarkers, aes(x=lon, y=lat, col=Elevation), size=5, alpha=0.7) +
  ggtitle("Air Quality Monitoring Stations Across BC")+
  geom_text(data = cities, aes(x = LONG, y = LAT, label = CITY), size = 4)+
  scale_color_gradient(low="yellow", high="red") +
  #coord_fixed(xlim=c(-135,-115),ylim = c(47, 59), ratio = 2/1)+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=20,face="bold"),
        title =element_text(size=20, face='bold'),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=16),
        legend.key.size = unit(1, "cm"))

# Write image to file
ggsave(filename="test_mine.jpeg",plot=image,width=50,height=30,units="cm")
