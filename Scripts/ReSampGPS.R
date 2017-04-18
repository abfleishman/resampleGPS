# Clear workspace
rm(list = ls())

dir='~/Dropbox/RLKI/'

# Load packages
library(adehabitatLT)
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(argosfilter)

# Load functions in the SGRK/Functions folder
for(i in 1:length(list.files("Functions"))) {
  source(list.files("Functions",full.names = T)[i])
}

# import gps data
data<-readRDS(paste0(dir,"SGRK_DATA/processedDATA_2015/GPSrlki_raw.rda"))

data<-arrange(data,CaptureID,DateTime)


# import handeling data
Hand<-readRDS(paste0(dir,"SGRK_DATA/processedDATA_2015/CaptureData_2015.rda"))

# ################################`
##### Add the distance to colony for each sub colony with the AddDist2Colony function ####
# ################################`

# add column called SiteShort to directly merge match the SiteShort column in the capture site data
Hand$SiteShort<-Hand$Site
str(Hand)

# Add Site names to location data
BirdSites<-select(Hand,CaptureID,SiteShort)
BirdSites$CaptureID<-as.factor(BirdSites$CaptureID)

data<-left_join(data, BirdSites, by="CaptureID")
head(data)
data$SiteShort[data$SiteShort=="V-A-Far"]<-"V-A"
# Import capture site information
CapSites<-read.csv("~/Dropbox/RLKI/SGRK_DATA/SGRK_Capture_Sites_Proofed_20Jul16.csv")

# drop extra columns from capture site data
CapSitesSel<-select(CapSites,SiteShort,Lat, Lon)
unique(data$SiteShort)[!unique(data$SiteShort)%in%CapSitesSel$SiteShort]
# run the Dist2Colony function <- I couldn't get it to work right
# uses distance (great circle from argosfilter, output in km)
data$Dist2Colony<-AddDist2Colony(data = data,CaptureSitesData = CapSitesSel,SiteName = "SiteShort")

# adds two columns ColonyMovement and TripNum  to the input, trims trips within a given distance (km) from colony, 

dataT<-MakeTrip(data,ID = "CaptureID",DistCutOff = 10,Dist2Colony ="Dist2Colony") 

# Adds in a unique identifier for each trip with year, capture number, and trip number
A<-str_pad(dataT$CaptureNum, width=3, pad="0")
B<-str_pad(dataT$TripNum, width=3, pad="0")
C<-str_pad(dataT$Band, width=4, pad="0")
dataT$UniID<-paste0(dataT$Year,A,B,C)


#deleates one point not eliminated by speed filter
dataT <- dataT[-c(31830), ]

# Interpolate to 180 seconds ----------------------------------------------
dataTsp<-dataT
coordinates(dataTsp)<- ~Longitude+Latitude
proj4string(dataTsp) <-  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
tracks <- spTransform(dataTsp,
                      CRS("+proj=laea +lat_0=56 +lon_0=-156 +x_0=0 +y_0=0
                          +ellps=WGS84 +units=m +no_defs"))
tracks
datalt<-as.ltraj( coordinates(dataTsp),date = dataT$DateTime,id = dataT$CaptureNum,burst = dataT$UniID,typeII = T,infolocs = dataT,proj4string =CRS("+proj=laea +lat_0=56 +lon_0=-156 +x_0=0 +y_0=0
                          +ellps=WGS84 +units=m +no_defs"))
head(datalt)
datalt<-redisltraj(datalt,u=180,type = "time")
datalt<-rec(datalt)
fpt(datalt,r)
dataTlt<-ld(datalt)
head(dataTlt)
dataTlt<-filter(dataTlt,burst==20150350030791)
summary(dataTlt)
head(dataTlt)
dataTlt$speed<-(dataTlt$dist/dataTlt$dt)
dataTlt$speed<-dataTlt$speed/180
dataTlt$speed<-dataTlt$speed*60^2

dataTlt$IPD<-InterpointDist(dataTlt,ID = "id",lat = "y",lon = "x")
dataTlt$speed<-(dataTlt$IPD/dataTlt$dt)


# Landings ----------------------------------------------------------------

dataTlt$onwater<-dataTlt$speed<5
dataTlt$landingNumber<-data.table::rleid(dataTlt$onwater)
dataTlt$landingNumber[dataTlt$onwater==F]<-0

dataTlt<-dataTlt %>% 
  group_by(landingNumber) %>% 
  mutate(numLandingPoints=n())

ggplot(dataTlt[dataTlt$landingNumber!=0,],aes(x,y,colour=factor(landingNumber),size=numLandingPoints))+
  geom_point()



ggplot(dataTlt,aes(date,dist,colour=dist<.001))+geom_point()
ggplot(dataTlt,aes(date,speed))+geom_smooth(span=.15)+geom_point()
ggplot(dataTlt,aes(date,dist))+geom_smooth(span=.15)+geom_point()
ggplot(dataTlt,aes(x,y,colour=speed<5))+geom_point()
ggplot(dataTlt,aes(x,y,colour=date))+geom_point()
plot(dataTlt$date,dataTlt$abs.angle)
head(dataTlt)

# dataT<-filter(dataT,CaptureNum==35)

# plot(dataT$Longitude[dataT$TripNum==0],dataT$Latitude[dataT$TripNum==0])
# plot(dataT$Longitude[dataT$TripNum==1],dataT$Latitude[dataT$TripNum==1])
# plot(dataT$Longitude[dataT$TripNum==10],dataT$Latitude[dataT$TripNum==10])
# plot(dataT$Longitude[dataT$TripNum==16],dataT$Latitude[dataT$TripNum==16])
# plot(dataT$Longitude[dataT$TripNum==21],dataT$Latitude[dataT$TripNum==21])
# plot(dataT$DateTime)
quartz()
par(mfrow=c(4,1))
plot(dataT$date,dataT$x)
plot(dataT$date,dataT$y)
plot(dataT$date,dataT$dist)
plot(dataT$date,dataT$R2n)
head(dataT)

dataT$Bering<-BearingFromPoint(dataIn = dataT,ID = "CaptureID",lat = "Latitude",lon = "Longitude")
library(circular)
dataT$Bering<-as.circular(dataT$Bering,type="directions",units = "degrees")

# Trip summary stats departure -------------------------------------------------------
TripTab<-dataT %>% 
  group_by(CaptureID,TripNum) %>% 
  filter(n()>30) %>% 
  summarise(Points=n(),
            DepartA=mean(Bering[1:5]),
            ReturnA=mean(Bering[length(Bering)-5:length(Bering)]),
            Start=min(DateTime),
            End=max(DateTime),
            Max_Dist_Time=DateTime[Dist2Colony==max(Dist2Colony,na.rm=T)],
            Dist_km=sum(InterPointDist,na.rm=T)/1000,
            Max_Dist=max(Dist2Colony,na.rm=T),
            Mean_Speed=mean(Speed,na.rm=T)
            ) %>% 
  mutate(Duration=round(as.numeric(difftime(End,Start,units="hours")),digits=2))



# Save as kml -------------------------------------------------------------

dataTsp<-as.data.frame(dataT)
dataTsp$Bering<-as.numeric(dataTsp$Bering)
coordinates(dataTsp)<-~Longitude+Latitude
proj4string(dataTsp)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

writeOGR(obj = dataTsp, dsn="dataTsp.kml", layer="dataTsp", driver="KML")



