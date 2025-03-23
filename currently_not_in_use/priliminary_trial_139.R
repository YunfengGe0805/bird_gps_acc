#看下月度VIIRS差异gis
#提取不同radius
#问郭师兄怎么api提取灯光数据EOG or GEE的
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
#Data preparation
##Load dataset
cred <- movebankLogin(username="Yachang Cheng", password="welove107")
searchMovebankStudies(x="Qingdao", login=cred)
birds <- getMovebankData(study="Tracking seabirds in the Yellow Sea of Qingdao", login=cred,timestamp_start="20230101000000000",timestamp_end="20231231235959000")
save(birds,file="birds.Rdata")
##Already saved in Rdata
load(file="birds.Rdata")
##Select F139 and M140
F139<-birds[['SYSUL139']]

##Check time lag
tl139<-timeLag(F139,units="mins")
summary(tl139)
plot(tl139)
###Mainly 20min but some large gaps exist

#Interpolation
##Move package
ip139 <- interpolateTime(F139, time=as.difftime(20, units="mins"), spaceMethod='greatcircle')

##Check time lag
tlip139<-timeLag(ip139,units="mins")
summary(tlip139)

###Equal 20min interval achieved

##Move2 package

##momentuHMM package crawlwrap

#Bursting by day (using yday as burst)
##Interpolation from move
##Convert move object to dataframe
dat139<-as.data.frame(ip139)

##Change time zone
dat139$timestamps<-with_tz(dat139$timestamps,"Asia/Shanghai")

##Clean the dataset
f139<-dat139%>%
  dplyr::select(long=location_long.1,
                lat=location_lat.1,
                timestamps,
                ID=local_identifier)

##Add hour to datasets
f139$hr <- hour(f139$timestamps)


#Filter out only day/night track data
nf139<-filter(f139,hr==0|hr==1|hr==2|hr==3|hr==4|hr==23|hr==22|hr==21|hr==20)
nf139$class<-"night"
df139<-filter(f139,hr==8|hr==9|hr==10|hr==11|hr==12|hr==13|hr==14|hr==15|hr==16)
df139$class<-"day"
cf139<-filter(f139,hr==5|hr==6|hr==7|hr==19|hr==17|hr==18)
cf139$class<-"crespuscular"

#Extract environmental data
##Extract monthly ALAN data for f139
sw01 <- filter(nf139, timestamps >= ymd_hms("2023-01-01 00:00:00") &timestamps <=ymd_hms ("2023-02-01 00:00:00"))
points<-sw01[1:2]
m01<-rast('ALAN_month/202301.tif')
A1<-raster::extract(m01,points,buffer=2000,fun=mean, ID=FALSE)

sw02 <- filter(nf139, timestamps >= ymd_hms("2023-02-01 00:00:01") &timestamps <=ymd_hms ("2023-03-01 00:00:00"))
points<-sw02[1:2]
m02<-rast('ALAN_month/202302.tif')
A2<-raster::extract(m02,points,buffer=2000,fun=mean, ID=FALSE)

sw03 <- filter(nf139, timestamps >= ymd_hms("2023-03-01 00:00:01") &timestamps <=ymd_hms ("2023-04-01 00:00:00"))
points<-sw03[1:2]
m03<-rast('ALAN_month/202303.tif')
A3<-raster::extract(m03,points,buffer=2000,fun=mean, ID=FALSE)

sw04 <- filter(nf139, timestamps >= ymd_hms("2023-04-01 00:00:01") &timestamps <=ymd_hms ("2023-05-01 00:00:00"))
points<-sw04[1:2]
m04<-rast('ALAN_month/202304.tif')
A4<-raster::extract(m04,points,buffer=2000,fun=mean, ID=FALSE)

sw05 <- filter(nf139, timestamps >= ymd_hms("2023-05-01 00:00:01") &timestamps <=ymd_hms ("2023-06-01 00:00:00"))
points<-sw05[1:2]
m05<-rast('ALAN_month/202305.tif')
A5<-raster::extract(m05,points,buffer=2000,fun=mean, ID=FALSE)

sw06 <- filter(nf139, timestamps >= ymd_hms("2023-06-01 00:00:01") &timestamps <=ymd_hms ("2023-07-01 00:00:00"))
points<-sw06[1:2]
m06<-rast('ALAN_month/202306.tif')
A6<-raster::extract(m06,points,buffer=2000,fun=mean, ID=FALSE)

sw07 <- filter(nf139, timestamps >= ymd_hms("2023-07-01 00:00:01") &timestamps <=ymd_hms ("2023-08-01 00:00:00"))
points<-sw07[1:2]
m07<-rast('ALAN_month/202307.tif')
A7<-raster::extract(m07,points,buffer=2000,fun=mean, ID=FALSE)

sw08 <- filter(nf139, timestamps >= ymd_hms("2023-08-01 00:00:01") &timestamps <=ymd_hms ("2023-09-01 00:00:00"))
points<-sw08[1:2]
m08<-rast('ALAN_month/202308.tif')
A8<-raster::extract(m08,points,buffer=2000,fun=mean, ID=FALSE)

ALANm<-rbind(A1,A2,A3,A4,A5,A6,A7,A8)
nf139<-nf139%>%
  mutate(ALANm)
colnames(nf139)[7]<-"ALAN"
## Combine day/might dataset
df139$ALAN<-rep(NA,times=nrow(df139))
cf139$ALAN<-rep(NA,times=nrow(cf139))
f139rs<-rbind(nf139,df139,cf139)
f139rs<-f139rs%>%
  arrange(timestamps)


#Implement HMM in momentuHMM
##Prepare data (angle and step length calculated)
datf139 <- prepData(f139rs, type = 'LL', coordNames = c("long","lat"))

##Check if there are 0 in step (if yes need zeromass parameter)
summary(datf139$step)
###No zero step


#Fit 3 states HMM models
set.seed(0805)

##Fit model for F139 
### initial step distribution natural scale parameters
stepPar0 <- c(0.78,2.41,10.68,0.78,2.41,10.68)
#guess based on Behavioural mapping of a pelagic seabird: combining multiple sensors and a hidden Markov model reveals the distribution of at-sea behaviour
### initial angle distribution natural scale parameters
anglePar0 <- c(0,3,0,1,10,1)

##Null model
null_3states_f139 <- fitHMM(data = datf139,
                            nbStates = 3,
                            dist = list(step = "gamma", angle = "vm"),
                            Par0 = list(step = stepPar0, angle = anglePar0),
                            formula = ~ 1,
                            stateNames = c('Resting', 'Foraging', 'Travelling'),
                            estAngleMean = list(angle=TRUE)
)
null_3states_f139
plot(null_3states_f139)
states<-viterbi(null_3states_f139)
f139states<-cbind(f139rs,states)
write.csv(f139states,file = "f139states.csv")

library(ggplot2)
library(hrbrthemes)
library(viridis)
#Violin plot
f139<-read.csv(file = "f139ALANstates.csv")
dat<-subset(data,class=="night")
dat<-subset(dat,ALAN>0)
datf%>%
  ggplot(aes(fill=as.character(states),x=sex, y=log(ALAN))) +
  geom_violin(position=position_dodge(0.75),width=1.4) +
  scale_x_discrete(labels=c('Male', 'Female'))+
  geom_boxplot(position=position_dodge(0.75),width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("ALAN effect in fishing period") +
  xlab("")
