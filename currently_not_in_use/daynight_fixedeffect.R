#Clear environment and set working directory
rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")
#Load packages
library(move)
library(momentuHMM)
library(stats)
library(tidyverse)
library(lubridate)
library(raster)
library(sf)
library(terra)
library(dplyr)
##Already saved in Rdata
load(file="birds.Rdata")
F139<-birds[['SYSUL139']]

#Interpolation
##crawrap
##momentuHMM package crawlwrap
#convert WGS84 EPSG4326 to EPSG32649 unit in meter
swdata<-as.data.frame(F139)
LatLong.proj <- "EPSG:4326"
UtmZone.proj <- "EPSG:32649"
birdsproj <- swdata %>% 
  st_as_sf(coords = c('location_long', 'location_lat'), 
           crs = LatLong.proj) %>% 
  st_transform(UtmZone.proj)
dat139 <- birdsproj %>%
  as_tibble() %>%
  mutate(X = st_coordinates(birdsproj)[ ,1],
         Y = st_coordinates(birdsproj)[ ,2]) %>%
  dplyr::select(-geometry)
##Change time zone
dat139$timestamps<-with_tz(dat139$timestamps,"Asia/Shanghai")
#select ID, time, location coordinates for crawlwrap simulation
crwdat<-dat139%>%
  dplyr::select(ID=local_identifier,
                timestamps,
                X,
                Y)
crwOut<-crawlWrap(crwdat,timeStep = "20 mins", Time.name = "timestamps",attempts = 2,coord = c("X","Y"),proj = "EPSG:32649",ncores = 3)
dat <- prepData(crwOut)
plot(dat)

##Move package
ip139 <- interpolateTime(F139, time=as.difftime(20, units="mins"), spaceMethod='greatcircle')
dat139<-as.data.frame(ip139)
##Change time zone
dat139$timestamps<-with_tz(dat139$timestamps,"Asia/Shanghai")
dat139 <- filter(dat139, timestamps >= ymd_hms("2023-01-01 00:00:00") &timestamps <=ymd_hms ("2023-04-18 23:40:19"))

##Clean the dataset
f139<-dat139 %>%
  dplyr::select(long=location_long.1,
                lat=location_lat.1,
                timestamps,
                ID=local_identifier)

##Add hour and day to datasets
f139$hr <- hour(f139$timestamps)

#Filter out only day/night track data
nf139<-filter(f139,hr==0|hr==1|hr==2|hr==3|hr==4|hr==23|hr==22|hr==21|hr==20)
nf139$class<-"night"
df139<-filter(f139,hr==8|hr==9|hr==10|hr==11|hr==12|hr==13|hr==14|hr==15|hr==16)
df139$class<-"day"
cf139<-filter(f139,hr==5|hr==6|hr==7|hr==17|hr==18|hr==19)
cf139$class<-"crespuscular"

nf139<-filter(f139,hr==0|hr==1|hr==2|hr==3|hr==4|hr==23|hr==22|hr==21|hr==20|)
nf139$class<-"night"
df139<-filter(f139,hr==8|hr==9|hr==10|hr==11|hr==12|hr==13|hr==14|hr==15|hr==16|)
df139$class<-"day"

dat139<-rbind(nf139,df139)
dat139<-dat139%>%
  arrange(timestamps)

#Extract environmental data
##Extract monthly ALAN data for f139
sw01 <- filter(dat139, timestamps >= ymd_hms("2023-01-01 00:00:00") &timestamps <=ymd_hms ("2023-02-01 00:00:00"))
points<-sw01[1:2]
m01<-rast('ALAN_month/202301.tif')
A1<-raster::extract(m01,points,buffer=2000,fun=mean, ID=FALSE)

sw02 <- filter(dat139, timestamps >= ymd_hms("2023-02-01 00:00:01") &timestamps <=ymd_hms ("2023-03-01 00:00:00"))
points<-sw02[1:2]
m02<-rast('ALAN_month/202302.tif')
A2<-raster::extract(m02,points,buffer=2000,fun=mean, ID=FALSE)

sw03 <- filter(dat139, timestamps >= ymd_hms("2023-03-01 00:00:01") &timestamps <=ymd_hms ("2023-04-01 00:00:00"))
points<-sw03[1:2]
m03<-rast('ALAN_month/202303.tif')
A3<-raster::extract(m03,points,buffer=2000,fun=mean, ID=FALSE)

sw04 <- filter(dat139, timestamps >= ymd_hms("2023-04-01 00:00:01") &timestamps <=ymd_hms ("2023-05-01 00:00:00"))
points<-sw04[1:2]
m04<-rast('ALAN_month/202304.tif')
A4<-raster::extract(m04,points,buffer=2000,fun=mean, ID=FALSE)

sw05 <- filter(dat139, timestamps >= ymd_hms("2023-05-01 00:00:01") &timestamps <=ymd_hms ("2023-06-01 00:00:00"))
points<-sw05[1:2]
m05<-rast('ALAN_month/202305.tif')
A5<-raster::extract(m05,points,buffer=2000,fun=mean, ID=FALSE)

sw06 <- filter(dat139, timestamps >= ymd_hms("2023-06-01 00:00:01") &timestamps <=ymd_hms ("2023-07-01 00:00:00"))
points<-sw06[1:2]
m06<-rast('ALAN_month/202306.tif')
A6<-raster::extract(m06,points,buffer=2000,fun=mean, ID=FALSE)

sw07 <- filter(dat139, timestamps >= ymd_hms("2023-07-01 00:00:01") &timestamps <=ymd_hms ("2023-08-01 00:00:00"))
points<-sw07[1:2]
m07<-rast('ALAN_month/202307.tif')
A7<-raster::extract(m07,points,buffer=2000,fun=mean, ID=FALSE)

sw08 <- filter(dat139, timestamps >= ymd_hms("2023-08-01 00:00:01") &timestamps <=ymd_hms ("2023-09-01 00:00:00"))
points<-sw08[1:2]
m08<-rast('ALAN_month/202308.tif')
A8<-raster::extract(m08,points,buffer=2000,fun=mean, ID=FALSE)
ALANm<-rbind(A1,A2,A3,A4,A5,A6,A7,A8)
dat139<-dat139%>%
  mutate(ALANm)
colnames(dat139)[7]<-"ALAN"

ALANm<-rbind(A1,A2,A3,A4)
dat139<-dat139%>%
  mutate(ALANm)
colnames(dat139)[7]<-"ALAN"


#Implement HMM in momentuHMM
##Prepare data (angle and step length calculated)
datf139 <- prepData(dat139, type = 'LL', coordNames = c("long","lat"))
##Check if there are 0 in step (if yes need zeromass parameter)
summary(datf139$step)
###No zero step
hist(datf139$step)
hist(datf139$step, xlim = c(0, 4), breaks = 30, main = "", xlab = "step length", border = "white")
hist(datf139$angle, main = "", xlab = "turning angle", border = "white")
#Fit 3 states HMM models
set.seed(0805)
##Fit model for F139 
### initial step distribution natural scale parameters
stepPar0 <- c(0.78,2.41,10.68,0.78,2.41,10.68)
#guess based on Behavioural mapping of a pelagic seabird: combining multiple sensors and a hidden Markov model reveals the distribution of at-sea behaviour
### initial angle distribution natural scale parameters
#anglePar0 <- c(0,3,0,1,10,1)
anglePar0 <- c(0,pi,0,1,0.5,1)
##Null model
ALAN_3states <- fitHMM(data = datf139,
                       nbStates = 3,
                       dist = list(step = "gamma", angle = "vm"),
                       Par0 = list(step = stepPar0, angle = anglePar0),
                       formula = ~ ALAN+(1|class),
                       stateNames = c('Resting', 'Foraging', 'Travelling'),
                       estAngleMean = list(angle=TRUE)
)
ALAN_3states
plot(ALAN_3states)
class_3states <- fitHMM(data = datf139,
                       nbStates = 3,
                       dist = list(step = "gamma", angle = "vm"),
                       Par0 = list(step = stepPar0, angle = anglePar0),
                       formula = ~ factor(class),
                       stateNames = c('Resting', 'Foraging', 'Travelling'),
                       estAngleMean = list(angle=TRUE)
)
class_3states

plot(ALAN_3states)
states<-viterbi(ALAN_3states)
f139statesbbn<-cbind(nf139,states)
write.csv(f139statesbbn,file = "f139statesbbn.csv")

Null3states <- fitHMM(data = dat,
                       nbStates = 3,
                       dist = list(step = "gamma", angle = "vm"),
                       Par0 = list(step = stepPar0, angle = anglePar0),
                       formula = ~1,
                       stateNames = c('Resting', 'Foraging', 'Travelling'),
                       estAngleMean = list(angle=TRUE)
)
Null3states
plot(ALAN_3states)