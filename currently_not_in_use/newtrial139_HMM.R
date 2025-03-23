#Clear environment and set working directory
rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")
#Load packages
library(move)
library(raster)
library(sf)
library(terra)
library(dplyr)
library(momentuHMM)
library(tidyverse)
library(lubridate)
library(suncalc)
#Data preparation
##Load dataset
cred <- movebankLogin(username="Yachang Cheng", password="welove107")
searchMovebankStudies(x="Qingdao", login=cred)
winter21_22 <- getMovebankData(study="Tracking seabirds in the Yellow Sea of Qingdao", login=cred,timestamp_start="20211101000000000",timestamp_end="20220401000000000")
save(winter21_22,file="winter21_22.Rdata")
winter22_23 <- getMovebankData(study="Tracking seabirds in the Yellow Sea of Qingdao", login=cred,timestamp_start="20221101000000000",timestamp_end="20230401000000000")
save(winter22_23,file="winter22_23.Rdata")
winter23_24 <- getMovebankData(study="Tracking seabirds in the Yellow Sea of Qingdao", login=cred,timestamp_start="20231101000000000",timestamp_end="20240401000000000")
save(winter23_24,file="winter23_24.Rdata")

##Already saved in Rdata
#load(file="winter21_22.Rdata")
load(file="winter22_23.Rdata")
#load(file="winter23_24.Rdata")

##If want to do individual model
F139<-winter22_23[['SYSUL139']]
#F139<-as.data.frame(F139)
#Interpolation - two different methods
##Move package
ip139 <- interpolateTime(F139, time=as.difftime(20, units="mins"), spaceMethod='greatcircle')
##Convert move object to dataframe
dat139<-as.data.frame(ip139)
##Change time zone
dat139$timestamps<-with_tz(dat139$timestamps,"Asia/Shanghai")

##Clean the dataset
data139<-dat139 %>%
  dplyr::select(timestamps,
                ID=local_identifier,
                long=location_long.1,
                lat=location_lat.1)
data<-as.data.frame(data139)
#Extract environmental data
##Extract monthly ALAN data for f139
sw01 <- filter(data, timestamps >= ymd_hms("2023-01-01 00:00:01") &timestamps <=ymd_hms ("2023-02-01 00:00:00"))
points<-sw01[3:4]
m01<-rast('ALAN_month/202301.tif')
A1<-raster::extract(m01,points,method='bilinear',ID=FALSE)

sw02 <- filter(data, timestamps >= ymd_hms("2023-02-01 00:00:01") &timestamps <=ymd_hms ("2023-03-01 00:00:00"))
points<-sw02[3:4]
m02<-rast('ALAN_month/202302.tif')
A2<-raster::extract(m02,points,method='bilinear',ID=FALSE)

sw03 <- filter(data, timestamps >= ymd_hms("2023-03-01 00:00:01") &timestamps <=ymd_hms ("2023-04-01 00:00:00"))
points<-sw03[3:4]
m03<-rast('ALAN_month/202303.tif')
A3<-raster::extract(m03,points,method='bilinear',ID=FALSE)

sw11 <- filter(data, timestamps >= ymd_hms("2022-11-01 00:00:00") &timestamps <=ymd_hms ("2022-12-01 00:00:00"))
points<-sw11[3:4]
m11<-rast('ALAN_month/202211.tif')
A11<-raster::extract(m11,points,method='bilinear',ID=FALSE)

sw12 <- filter(data, timestamps >= ymd_hms("2022-12-01 00:00:01") &timestamps <=ymd_hms ("2023-01-01 00:00:00"))
points<-sw12[3:4]
m12<-rast('ALAN_month/202212.tif')
A12<-raster::extract(m12,points,method='bilinear',ID=FALSE)

ALANm<-rbind(A11,A12,A1,A2,A3)
#ALANm$avg_rad<-ALANm$avg_rad*1000000000
data<-data%>%
  arrange(timestamps)
data<-data%>%
  mutate(ALANm)

#colnames(nf139)[7]<-"class"
#colnames(nf139)[8]<-"ALAN"
#nf139<-nf139%>%
#  select(-ID)
#colnames(nf139)[5]<-"ID"

##Add day/night
dat<-data%>%
  dplyr::select(date=timestamps,
                datetime=timestamps,
                lat,
                lon=long)
##Get moon illumination
moon<-getMoonIllumination(dat$date, keep = c("fraction", "phase"))
dat$date <- as.Date(dat$date)
sun <- getSunlightTimes(data = dat, tz = "Asia/Shanghai", 
                        keep = c("sunrise", "sunset", "night", "nightEnd"))
xy<-cbind(dat,sun)
#xy <- merge(x = dat, 
#            y = sun[, c("lat", "sunrise", "sunset", "night", "nightEnd")], 
#            by = "lat")

xy$period <- rep(" ", length.out = nrow(xy))
xy$period[xy$datetime > xy$sunrise & xy$datetime < xy$sunset] <- "day"
xy$period[xy$datetime > xy$sunset & xy$datetime < xy$night] <- "night"
xy$period[xy$datetime > xy$nightEnd & xy$datetime < xy$sunrise] <- "night"
xy$period[xy$period == " "] <- "night"

datafinal <- cbind(data,xy$period)
datafinal<- cbind(datafinal,moon$fraction)
datafinal<- cbind(datafinal,moon$phase)
colnames(datafinal)[6]<-"period"
colnames(datafinal)[7]<-"fraction"
colnames(datafinal)[8]<-"phase"
datafinal$avg_rad <- ifelse(datafinal$avg_rad < 0.2, 0.00001,datafinal$avg_rad)
datafinal$rad_log <- log(datafinal$avg_rad)
#add time of day
hours <- as.numeric(format(datafinal$timestamp, "%H"))
minutes <- as.numeric(format(datafinal$timestamp, "%M"))
datafinal$tod <- hours + minutes / 60
#Fit 3 states HMM models
set.seed(0805)
#prep data for HMM
dat <- prepData(datafinal,type='LL',coordNames = c("long","lat"))
# Remove errorneous step lengths & only select night
dat <- subset(dat, step < 90)
dat <- subset(dat,period=="night")
#hist(dat$step)
##Fit model for F139 
### initial step distribution natural scale parameters
stepPar0 <- c(0.78,2.41,10.68,0.78,2.41,10.68)
#guess based on Behavioural mapping of a pelagic seabird: combining multiple sensors and a hidden Markov model reveals the distribution of at-sea behaviour
### initial angle distribution natural scale parameters
#anglePar0 <- c(0,3,0,1,10,1)
anglePar0 <- c(0,pi,0,1,0.5,1)
##Null model
null_3states <- fitHMM(data = dat,
                       nbStates = 3,
                       dist = list(step = "gamma", angle = "vm"),
                       Par0 = list(step = stepPar0, angle = anglePar0),
                       formula = ~ 1,
                       stateNames = c('Rafting', 'Foraging', 'Travelling'),
                       estAngleMean = list(angle=TRUE)
)
null_3states
plot(null_3states)

##tod model
Par0 <- getPar0(null_3states,formula= ~ rad_log + sin(2*pi*timeOfDay/24) + cos(2*pi*timeOfDay/24))
tod_3states <- fitHMM(data = dat,
                      nbStates = 3,
                      dist = list(step = "gamma", angle = "vm"),
                      Par0 = list(step=Par0$Par$step, angle=Par0$Par$angle,delta0 = Par0$delta),
                      beta0 = Par0$beta,
                      formula = ~ rad_log,
                      stateNames = c('Rafting', 'Foraging', 'Travelling'),
                      estAngleMean = list(angle=TRUE)
                      
)
tod_3states
plot(tod_3states,plotCI=TRUE,plotStationary=TRUE)

##ALAN model
Par0 <- getPar0(null_3states,formula=~rad_log)
alan_3states <- fitHMM(data = dat,
                       nbStates = 3,
                       dist = list(step = "gamma", angle = "vm"),
                       Par0 = list(step=Par0$Par$step, angle=Par0$Par$angle,delta0 = Par0$delta),
                       beta0 = Par0$beta,
                       formula = ~ rad_log,
                       stateNames = c('Rafting', 'Foraging', 'Travelling'),
                       estAngleMean = list(angle=TRUE)
                       
)
alan_3states
plot(alan_3states,plotCI=TRUE,plotStationary=TRUE)

#stateprob<-stateProbs(alan_3states)
states<-viterbi(alan_3states)
f139states<-cbind(dat,states)

##moon fraction model
Par0 <- getPar0(null_3states,formula=~fraction)
f_3states <- fitHMM(data = dat,
                       nbStates = 3,
                       dist = list(step = "gamma", angle = "vm"),
                       Par0 = list(step=Par0$Par$step, angle=Par0$Par$angle,delta0 = Par0$delta),
                       beta0 = Par0$beta,
                       formula = ~ fraction,
                       stateNames = c('Rafting', 'Foraging', 'Travelling'),
                       estAngleMean = list(angle=TRUE)
                       
)
f_3states
plot(f_3states,plotCI=TRUE,plotStationary=TRUE)

##ALAN + moon fraction model
Par0 <- getPar0(null_3states,formula=~rad_log+fraction)
af_3states <- fitHMM(data = dat,
                     nbStates = 3,
                     dist = list(step = "gamma", angle = "vm"),
                     Par0 = list(step=Par0$Par$step, angle=Par0$Par$angle,delta0 = Par0$delta),
                     beta0 = Par0$beta,
                     formula = ~ rad_log+fraction,
                     stateNames = c('Rafting', 'Foraging', 'Travelling'),
                     estAngleMean = list(angle=TRUE)
                     
)
af_3states
plot(af_3states,plotCI=TRUE,plotStationary=TRUE)

##ALAN * moon fraction model
Par0 <- getPar0(null_3states,formula=~rad_log:fraction)
atf_3states <- fitHMM(data = dat,
                     nbStates = 3,
                     dist = list(step = "gamma", angle = "vm"),
                     Par0 = list(step=Par0$Par$step, angle=Par0$Par$angle,delta0 = Par0$delta),
                     beta0 = Par0$beta,
                     formula = ~ rad_log:fraction,
                     stateNames = c('Rafting', 'Foraging', 'Travelling'),
                     estAngleMean = list(angle=TRUE)
                     
)
atf_3states
plot(atf_3states,plotCI=TRUE,plotStationary=TRUE)

##moon phase model
Par0 <- getPar0(null_3states,formula=~phase)
p_3states <- fitHMM(data = dat,
                       nbStates = 3,
                       dist = list(step = "gamma", angle = "vm"),
                       Par0 = list(step=Par0$Par$step, angle=Par0$Par$angle,delta0 = Par0$delta),
                       beta0 = Par0$beta,
                       formula = ~ phase,
                       stateNames = c('Rafting', 'Foraging', 'Travelling'),
                       estAngleMean = list(angle=TRUE)
                       
)
p_3states
plot(p_3states,plotCI=TRUE,plotStationary=TRUE)

##ALAN + moon phase model
Par0 <- getPar0(null_3states,formula=~rad_log+phase)
ap_3states <- fitHMM(data = dat,
                       nbStates = 3,
                       dist = list(step = "gamma", angle = "vm"),
                       Par0 = list(step=Par0$Par$step, angle=Par0$Par$angle,delta0 = Par0$delta),
                       beta0 = Par0$beta,
                       formula = ~ rad_log+phase,
                       stateNames = c('Rafting', 'Foraging', 'Travelling'),
                       estAngleMean = list(angle=TRUE)
                       
)
ap_3states
plot(ap_3states,plotCI=TRUE,plotStationary=TRUE)

