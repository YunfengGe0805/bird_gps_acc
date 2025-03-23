#Clear environment and set working directory
rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")

#Load packages
library(move)
library(move2)
library(momentuHMM)
library(stats)
library(tidyverse)
library(lubridate)
library(terra)
library(sf)

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
M140<-birds[['SYSUL140']]
##Check time lag
tl139<-timeLag(F139,units="mins")
tl140<-timeLag(M140,units="mins")
summary(tl139)
summary(tl140)
plot(tl139)
plot(tl140)
###Mainly 20min but some large gaps exist

#Interpolation
##Move package
ip139 <- interpolateTime(F139, time=as.difftime(20, units="mins"), spaceMethod='greatcircle')
ip140 <- interpolateTime(M140, time=as.difftime(20, units="mins"), spaceMethod='greatcircle')
##Check time lag
tlip139<-timeLag(ip139,units="mins")
tlip140<-timeLag(ip140,units="mins")
summary(tlip139)
summary(tlip140)
###Equal 20min interval achieved

##Move2 package

##momentuHMM package crawlwrap

#Bursting by day (using yday as burst)
##Interpolation from move
##Convert move object to dataframe
dat139<-as.data.frame(ip139)
dat140<-as.data.frame(ip140)
##Change time zone
dat139$timestamps<-with_tz(dat139$timestamps,"Asia/Shanghai")
dat140$timestamps<-with_tz(dat140$timestamps,"Asia/Shanghai")
##Clean the dataset
f139<-dat139%>%
  select(long=location_long.1,
         lat=location_lat.1,
         timestamps,
         ID=local_identifier)
  
m140<-dat140%>%
  filter(timestamps<ymd_hms("2023-12-31 00:00:00"))%>%
  select(long=location_long.1,
         lat=location_lat.1,
         timestamps,
         ID=local_identifier)
  
##Add year-day (yday), and hour to datasets
f139$yday <- yday(f139$timestamps)
f139$hr <- hour(f139$timestamps)
m140$yday <- yday(m140$timestamps)
m140$hr <- hour(m140$timestamps)

#Filter out only day/night track data
nf139<-filter(f139,hr==0|hr==1|hr==2|hr==3|hr==4|hr==5|hr==6|hr==23|hr==22|hr==21|hr==20|hr==19)
nf139$class<-"night"
df139<-filter(f139,hr==7|hr==8|hr==9|hr==10|hr==11|hr==12|hr==13|hr==14|hr==15|hr==16|hr==17|hr==18)
df139$class<-"day"
nm140<-filter(m140,hr==0|hr==1|hr==2|hr==3|hr==4|hr==5|hr==6|hr==23|hr==22|hr==21|hr==20|hr==19)
nm140$class<-"night"
dm140<-filter(m140,hr==7|hr==8|hr==9|hr==10|hr==11|hr==12|hr==13|hr==14|hr==15|hr==16|hr==17|hr==18)
dm140$class<-"day"
#Extract environmental data
##Extract monthly ALAN data for f139
sw01 <- filter(nf139, timestamps >= ymd_hms("2023-01-01 00:00:00") &timestamps <=ymd_hms ("2023-02-01 00:00:00"))
points<-sw01[1:2]
m01<-rast('ALAN_month/202301.tif')
A1<-terra::extract(m01,points,method="simple",ID=FALSE)

sw02 <- filter(nf139, timestamps >= ymd_hms("2023-02-01 00:00:01") &timestamps <=ymd_hms ("2023-03-01 00:00:00"))
points<-sw02[1:2]
m02<-rast('ALAN_month/202302.tif')
A2<-terra::extract(m02,points,method="simple",ID=FALSE)

sw03 <- filter(nf139, timestamps >= ymd_hms("2023-03-01 00:00:01") &timestamps <=ymd_hms ("2023-04-01 00:00:00"))
points<-sw03[1:2]
m03<-rast('ALAN_month/202303.tif')
A3<-terra::extract(m03,points,method="simple",ID=FALSE)

sw04 <- filter(nf139, timestamps >= ymd_hms("2023-04-01 00:00:01") &timestamps <=ymd_hms ("2023-05-01 00:00:00"))
points<-sw04[1:2]
m04<-rast('ALAN_month/202304.tif')
A4<-terra::extract(m04,points,method="simple",ID=FALSE)

sw05 <- filter(nf139, timestamps >= ymd_hms("2023-05-01 00:00:01") &timestamps <=ymd_hms ("2023-06-01 00:00:00"))
points<-sw05[1:2]
m05<-rast('ALAN_month/202305.tif')
A5<-terra::extract(m05,points,method="simple",ID=FALSE)

sw06 <- filter(nf139, timestamps >= ymd_hms("2023-06-01 00:00:01") &timestamps <=ymd_hms ("2023-07-01 00:00:00"))
points<-sw06[1:2]
m06<-rast('ALAN_month/202306.tif')
A6<-terra::extract(m06,points,method="simple",ID=FALSE)

sw07 <- filter(nf139, timestamps >= ymd_hms("2023-07-01 00:00:01") &timestamps <=ymd_hms ("2023-08-01 00:00:00"))
points<-sw07[1:2]
m07<-rast('ALAN_month/202307.tif')
A7<-terra::extract(m07,points,method="simple",ID=FALSE)

sw08 <- filter(nf139, timestamps >= ymd_hms("2023-08-01 00:00:01") &timestamps <=ymd_hms ("2023-09-01 00:00:00"))
points<-sw08[1:2]
m08<-rast('ALAN_month/202308.tif')
A8<-terra::extract(m08,points,method="simple",ID=FALSE)

ALANm<-rbind(A1,A2,A3,A4,A5,A6,A7,A8)

nf139<-nf139%>%
  mutate(ALANm)
colnames(nf139)[8]<-"ALAN"
## Combine day/might dataset
df139$ALAN<-NA
f139rs<-rbind(nf139,df139)
##Extract ChollA data for f139
points<-f139rs[1:2]
ChollA2023<-rast('ChollA2023.tif')
ChollAdata<-terra::extract(ChollA2023,points,method="simple",ID=FALSE)
f139rs<-f139rs%>%
  mutate(ChollAdata)%>%
  arrange(timestamps)



##Extract monthly ALAN data for m140
sw01 <- filter(nm140, timestamps >= ymd_hms("2023-01-01 00:00:00") &timestamps <=ymd_hms ("2023-02-01 00:00:00"))
points<-sw01[1:2]
m01<-rast('ALAN_month/202301.tif')
A1<-terra::extract(m01,points,method="simple",ID=FALSE)

sw02 <- filter(nm140, timestamps >= ymd_hms("2023-02-01 00:00:01") &timestamps <=ymd_hms ("2023-03-01 00:00:00"))
points<-sw02[1:2]
m02<-rast('ALAN_month/202302.tif')
A2<-terra::extract(m02,points,method="simple",ID=FALSE)

sw03 <- filter(nm140, timestamps >= ymd_hms("2023-03-01 00:00:01") &timestamps <=ymd_hms ("2023-04-01 00:00:00"))
points<-sw03[1:2]
m03<-rast('ALAN_month/202303.tif')
A3<-terra::extract(m03,points,method="simple",ID=FALSE)

sw04 <- filter(nm140, timestamps >= ymd_hms("2023-04-01 00:00:01") &timestamps <=ymd_hms ("2023-05-01 00:00:00"))
points<-sw04[1:2]
m04<-rast('ALAN_month/202304.tif')
A4<-terra::extract(m04,points,method="simple",ID=FALSE)

sw05 <- filter(nm140, timestamps >= ymd_hms("2023-05-01 00:00:01") &timestamps <=ymd_hms ("2023-06-01 00:00:00"))
points<-sw05[1:2]
m05<-rast('ALAN_month/202305.tif')
A5<-terra::extract(m05,points,method="simple",ID=FALSE)

sw06 <- filter(nm140, timestamps >= ymd_hms("2023-06-01 00:00:01") &timestamps <=ymd_hms ("2023-07-01 00:00:00"))
points<-sw06[1:2]
m06<-rast('ALAN_month/202306.tif')
A6<-terra::extract(m06,points,method="simple",ID=FALSE)

sw07 <- filter(nm140, timestamps >= ymd_hms("2023-07-01 00:00:01") &timestamps <=ymd_hms ("2023-08-01 00:00:00"))
points<-sw07[1:2]
m07<-rast('ALAN_month/202307.tif')
A7<-terra::extract(m07,points,method="simple",ID=FALSE)

sw08 <- filter(nm140, timestamps >= ymd_hms("2023-08-01 00:00:01") &timestamps <=ymd_hms ("2023-09-01 00:00:00"))
points<-sw08[1:2]
m08<-rast('ALAN_month/202308.tif')
A8<-terra::extract(m08,points,method="simple",ID=FALSE)

sw09 <- filter(nm140, timestamps >= ymd_hms("2023-09-01 00:00:01") &timestamps <=ymd_hms ("2023-10-01 00:00:00"))
points<-sw09[1:2]
m09<-rast('ALAN_month/202309.tif')
A9<-terra::extract(m09,points,method="simple",ID=FALSE)

sw10 <- filter(nm140, timestamps >= ymd_hms("2023-10-01 00:00:01") &timestamps <=ymd_hms ("2023-11-01 00:00:00"))
points<-sw10[1:2]
m10<-rast('ALAN_month/202310.tif')
A10<-terra::extract(m10,points,method="simple",ID=FALSE)

sw11 <- filter(nm140, timestamps >= ymd_hms("2023-11-01 00:00:01") &timestamps <=ymd_hms ("2023-12-01 00:00:00"))
points<-sw11[1:2]
m11<-rast('ALAN_month/202311.tif')
A11<-terra::extract(m11,points,method="simple",ID=FALSE)

sw12 <- filter(nm140, timestamps >= ymd_hms("2023-12-01 00:00:01") &timestamps <=ymd_hms ("2024-01-01 00:00:00"))
points<-sw12[1:2]
m12<-rast('ALAN_month/202312.tif')
A12<-terra::extract(m12,points,method="simple",ID=FALSE)

ALANm<-rbind(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12)

nm140<-nm140%>%
  mutate(ALANm)
colnames(nm140)[7]<-"ALAN"
##Extract ChollA data for m140
points<-nm140[1:2]
ChollA2023<-rast('ChollA2023.tif')
ChollAdata<-terra::extract(ChollA2023,points,method="simple",ID=FALSE)
nm140<-nm140%>%
  mutate(ChollAdata)

#Implement HMM in momentuHMM
##Change yday to ID and delete original ID
new139<-nf139%>%
  select(long,
         lat,
         timestamps,
         ID=yday,
         hr,
         ALAN)

new140<-nm140%>%
  select(long,
         lat,
         timestamps,
         ID=yday,
         hr,
         ALAN,
         CHLA_AVE)
##Prepare data (angle and step length calculated)
datm140 <- prepData(new140, type = 'LL', coordNames = c("long","lat"))
datf139 <- prepData(f139rs, type = 'LL', coordNames = c("long","lat"))
datnf139 <- prepData(new139, type = 'LL', coordNames = c("long","lat"))

##Check if there are 0 in step (if yes need zeromass parameter)
summary(datm140$step)
summary(datf139$step)

###No zero step

##K-means clustering of angle and step for setting starting parameter
set.seed(0805)
###F139 step
cluster_step_139 <- kmeans(na.omit(data.frame(datf139$step)), 3)
boxplot(cluster_step_140[[1]])
boxplot()
####priors for mean of steps
mu139_1 <- sort(cluster_step_139$centers)[1] #resting
mu139_2 <- sort(cluster_step_139$centers)[2] #foraging
mu139_3 <- sort(cluster_step_139$centers)[3] #travelling
####priors for std of steps
sd139_1 <- sd(na.omit(datf139$step)[cluster_step_139[[1]] == which(cluster_step_139$centers == sort(cluster_step_139$centers)[1])])
sd139_2 <- sd(na.omit(datf139$step)[cluster_step_139[[1]] == which(cluster_step_139$centers == sort(cluster_step_139$centers)[2])])
sd139_3 <- sd(na.omit(datf139$step)[cluster_step_139[[1]] == which(cluster_step_139$centers == sort(cluster_step_139$centers)[3])])
###F139 angle
cluster_angle_139 <- kmeans(na.omit(data.frame(datf139$angle)), 3)
####priors for mean of steps
mu139_a1 <- sort(cluster_angle_139$centers)[1]
mu139_a2 <- sort(cluster_angle_139$centers)[2]
mu139_a3 <- sort(cluster_angle_139$centers)[3] 
##Essentially 2 categories:2 & 0

###M140 step
cluster_step_140 <- kmeans(na.omit(data.frame(datm140$step)), 3)
####priors for mean of steps
mu140_1 <- sort(cluster_step_140$centers)[1] #resting
mu140_2 <- sort(cluster_step_140$centers)[2] #foraging
mu140_3 <- sort(cluster_step_140$centers)[3] #travelling
####priors for std of steps
sd140_1 <- sd(na.omit(datm140$step)[cluster_step_139[[1]] == which(cluster_step_140$centers == sort(cluster_step_140$centers)[1])])
sd140_2 <- sd(na.omit(datm140$step)[cluster_step_139[[1]] == which(cluster_step_140$centers == sort(cluster_step_140$centers)[2])])
sd140_3 <- sd(na.omit(datm140$step)[cluster_step_139[[1]] == which(cluster_step_140$centers == sort(cluster_step_140$centers)[3])])

###M140 angle
cluster_angle_140 <- kmeans(na.omit(data.frame(datm140$angle)), 3)
####priors for mean of steps
mu140_a1 <- sort(cluster_angle_140$centers)[1]
mu140_a2 <- sort(cluster_angle_140$centers)[2]
mu140_a3 <- sort(cluster_angle_140$centers)[3] 
##Essentially 2 categories:2 & 0

#Fit 3 states HMM models
##Fit model for F139 
### initial step distribution natural scale parameters
stepPar0 <- c(mu139_1,mu139_2,mu139_3,sd139_1,sd139_2,sd139_3)#use kmeans
stepPar0 <- c(0.78,2.41,10.68,0.78,2.41,10.68)#guess based on McClintock et al,2017

### initial angle distribution natural scale parameters
anglePar0 <- c(0,3,0,1,10,1)
##Set up model
set.seed(0805)
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

ALAN_3states_f139 <- fitHMM(data = datnf139,
                            nbStates = 3,
                            dist = list(step = "gamma", angle = "vm"),
                            Par0 = list(step = stepPar0, angle = anglePar0),
                            formula = ~ ALAN,
                            estAngleMean = list(angle=TRUE),
                            stateNames = c('Resting', 'Foraging', 'Travelling')
                            )

ALAN_3states_f139
plot(ALAN_3states_f139)

##Try with both day and night data no environmental annotation
datm140<- prepData(m140, type = 'LL', coordNames = c("long","lat"))
##K-means clustering of angle and step for setting starting parameter
set.seed(0805)
###M140 step
cluster_step_140 <- kmeans(na.omit(data.frame(datm140$step)), 3)
####priors for mean of steps
mu140_1 <- sort(cluster_step_140$centers)[1] #resting
mu140_2 <- sort(cluster_step_140$centers)[2] #foraging
mu140_3 <- sort(cluster_step_140$centers)[3] #travelling
####priors for std of steps
sd140_1 <- sd(na.omit(datm140$step)[cluster_step_139[[1]] == which(cluster_step_140$centers == sort(cluster_step_140$centers)[1])])
sd140_2 <- sd(na.omit(datm140$step)[cluster_step_139[[1]] == which(cluster_step_140$centers == sort(cluster_step_140$centers)[2])])
sd140_3 <- sd(na.omit(datm140$step)[cluster_step_139[[1]] == which(cluster_step_140$centers == sort(cluster_step_140$centers)[3])])
###M140 angle
cluster_angle_140 <- kmeans(na.omit(data.frame(datm140$angle)), 3)
####priors for mean of steps
mu140_a1 <- sort(cluster_angle_140$centers)[1]
mu140_a2 <- sort(cluster_angle_140$centers)[2]
mu140_a3 <- sort(cluster_angle_140$centers)[3] 
##Essentially 2 categories:2 & 0
### initial step distribution natural scale parameters
stepPar0 <- c(mu140_1,mu140_2,mu140_3,sd140_1,sd140_2,sd140_3)
### initial angle distribution natural scale parameters
anglePar0 <- c(0,2,0,1,10,1)
##Set up model
null_3states_m140 <- fitHMM(data = datm140,
                            nbStates = 3,
                            dist = list(step = "gamma", angle = "vm"),
                            Par0 = list(step = stepPar0, angle = anglePar0),
                            formula = ~ 1,
                            estAngleMean = list(angle=TRUE)
)
null_3states_m140







##notes##
##Assess models
null_3states_f139
plot(null_3states_f139)  
plotStates(null_3states_f139)  
timeInStates(null_3states_f139) 
plotPR(null_3states_f139, ncores = 5) 
trProbs <- getTrProbs(m)

##Compare among models

AIC(fit_hmm_2states, fit_hmm_3states, fit_hmm_2states_covar1, fit_hmm_2states_covar2,
    fit_hmm_3states_covar1)
AICweights(fit_hmm_2states, fit_hmm_3states, fit_hmm_2states_covar1, fit_hmm_2states_covar2,
           fit_hmm_3states_covar1)