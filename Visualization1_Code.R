# Visualization 1
# Kira Wiese

air<-read.csv("AQHI_full.csv",header=T)
stations<-read.csv("monitoring_stations.csv")
cities<-read.csv("cities.csv")


library(ggplot2)


################################################################################
#Formatting Data

#Convert date to date format
air$DATE_PST<-as.character(air$DATE_PST)
air$DATE_PST<-as.POSIXct(air$DATE_PST, format="%Y-%m-%d %H:%M")

#Aggregating data
air_new <- data.frame(air, Day = as.Date(format(air$DATE_PST)))
ad<-data.frame(aggregate((REPORTED_AQHI) ~ Day+AQHI_AREA, air_new, mean))
air_new1<-data.frame(aggregate(cbind(REPORTED_AQHI, PM25,NO2,O3,SO2) ~ Day+AQHI_AREA, air_new, mean))

#Analyze data to determine higher risk areas
by(ad, ad$AQHI_AREA, summary)


#Create subset of top 5 highest AQHI (baseed on mean)
my_final <- subset(air_new1 , AQHI_AREA == "Kamloops" |AQHI_AREA == "Williams Lake"|
                     AQHI_AREA == "Central Okanagan" |AQHI_AREA == "Quesnel" |AQHI_AREA == "Prince George")

#Create subset of top 5 highest AQHI (baseed on median)
my_med <- subset(air_new1 , AQHI_AREA == "Kamloops" |AQHI_AREA == "West Shore"|
                     AQHI_AREA == "Prince George" |AQHI_AREA == "Central Okanagan" |AQHI_AREA == "Victoria")


aggregate(REPORTED_AQHI ~AQHI_AREA, my_med, median)

#####################################################################################
#First Visual

library(RColorBrewer)
ggplot(data=my_med, aes(x=Day, y=REPORTED_AQHI, color=AQHI_AREA)) +
  ggtitle("Air Quality Measurements for 2017 by Region")+
  ylab("Reported Air Quality Index")+
  geom_hline(yintercept=10, linetype="dashed", color="red", size=1)+
  geom_rect(aes(xmin=as.Date("2017-01-01", "%Y-%m-%d"), xmax=as.Date("2018-01-01", "%Y-%m-%d"), 
                ymin = 0, ymax = 3),
            fill = "darkseagreen1", alpha = 0.08, color = NA)+
  geom_rect(aes(xmin=as.Date("2017-01-01", "%Y-%m-%d"), xmax=as.Date("2018-01-01", "%Y-%m-%d"), 
                ymin = 3, ymax = 7),
            fill = "lightyellow", alpha = 0.05, color = NA)+
  geom_rect(aes(xmin=as.Date("2017-01-01", "%Y-%m-%d"), xmax=as.Date("2018-01-01", "%Y-%m-%d"), 
                ymin = 7, ymax = 19),
                fill = "lightpink", alpha = 0.01, color = NA)+
  geom_line(size=0.8) +
  scale_y_continuous(limits=c(0,19), expand = c(0, 0))+
  theme(
  axis.text=element_text(size=16), axis.title=element_text(size=20,face="bold"),
  title =element_text(size=18, face='bold'),
  legend.title=element_text(size=18),
  legend.text=element_text(size=16),
  legend.position="bottom",
  legend.box = "horizontal",
  legend.key.size = unit(2, "cm"))

ggsave("my_neww.pdf",width=35,height=18,units="cm")


#####################################################################################
#Reformatting data for further visualization

avgs<-data.frame(aggregate(cbind(my_med$REPORTED_AQHI,my_med$PM25, 
                                 my_med$NO2,my_med$O3,my_med$SO2), 
                           by=list(Category=my_med$AQHI_AREA), FUN=mean))
colnames(avgs)<-c("Region","AQHI","PM25","NO2","O3","SO2")

d1 <- data.frame(a=unlist(avgs, use.names = FALSE))
d2<-data.frame(d1[6:30,])
Region<-rep(c("Central Okanagan", "Kamloops", "Prince George", "Victoria","West Shore"), 5)
Measure<-c(rep("AQHI",5),rep("PM25",5),rep("NO2",5),rep("O3",5),rep("SO2",5))
my_data<-cbind(d2,Region,Measure)
colnames(my_data)<-c("Amount","Region","Measure")

#####################################################################################
#Sub Visuals

ggplot(data=my_data, aes(x=Region, y=Amount, fill=Region, color=Region))+
  geom_bar(stat="identity")+
  ggtitle("Pollutant Levels by Region")+
  facet_wrap(~Measure, scales = "free")+
  theme(axis.text.x=element_blank())

library(scales)
coul = brewer.pal(3, "Pastel2")

ggplot(data=subset(my_data,Measure!="AQHI"), aes(x=Region, y=Amount,fill=Measure)) + 
  ggtitle("Contribution of Each Pollutant to AQHI Makeup")+
  geom_bar(position = "fill",stat = "identity") +
  scale_fill_brewer(palette="Dark2")+
  coord_flip() +
  scale_y_continuous(labels = percent_format())