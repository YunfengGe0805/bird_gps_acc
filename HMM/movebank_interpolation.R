#Clear environment and set working directory
rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")
#Load packages
library(move)
library(tidyverse)
library(lubridate)
#Data preparation
##Load dataset
cred <- movebankLogin(username="Yachang Cheng", password="welove107")
searchMovebankStudies(x="Qingdao", login=cred)
bird_mb <- getMovebankData(study="Tracking seabirds in the Yellow Sea of Qingdao", login=cred,timestamp_start="20210101000000000",timestamp_end="20241231235959000")
save(bird_mb,file="RawData_movebank/bird_mb.Rdata")
load(file="RawData_movebank/bird_mb.Rdata")
#Interpolation for 2022-2023 non-breeding period
## Set up a list of raw data
swall<-list()
swall[['SYSUL126']]<-bird_mb[['SYSUL126']]
swall[['SYSUL127']]<-bird_mb[['SYSUL127']]
swall[['SYSUL128']]<-bird_mb[['SYSUL128']]
swall[['SYSUL129']]<-bird_mb[['SYSUL129']]
swall[['SYSUL130']]<-bird_mb[['SYSUL130']]
swall[['SYSUL131']]<-bird_mb[['SYSUL131']]
swall[['SYSUL132']]<-bird_mb[['SYSUL132']]
swall[['SYSUL133']]<-bird_mb[['SYSUL133']]
swall[['SYSUL134']]<-bird_mb[['SYSUL134']]
swall[['SYSUL135']]<-bird_mb[['SYSUL135']]
swall[['SYSUL136']]<-bird_mb[['SYSUL136']]
swall[['SYSUL137']]<-bird_mb[['SYSUL137']]
swall[['SYSUL138']]<-bird_mb[['SYSUL138']]
swall[['SYSUL139']]<-bird_mb[['SYSUL139']]
swall[['SYSUL140']]<-bird_mb[['SYSUL140']]
swall[['SYSUL141']]<-bird_mb[['SYSUL141']]
swall[['SYSUL142']]<-bird_mb[['SYSUL142']]
swall[['SYSUL143']]<-bird_mb[['SYSUL143']]
#set up a name vector
names<-c("SYSUL126","SYSUL127","SYSUL128","SYSUL129","SYSUL130","SYSUL131","SYSUL132","SYSUL133","SYSUL134","SYSUL135","SYSUL136","SYSUL137","SYSUL138","SYSUL139","SYSUL140","SYSUL141","SYSUL142","SYSUL143")
# Specify output lists of length formula
sw.list <- vector(mode = "list")
#Create a for loop for a batched interpolation for 22-23 and 23-24 individuals and clean the datasets
for (i in names) {
  print(i)
  sw.list[[i]] <- interpolateTime(swall[[i]], time=as.difftime(20, units="mins"), spaceMethod='greatcircle')
  sw.list[[i]] <- as.data.frame(sw.list[[i]])
  sw.list[[i]]<-sw.list[[i]] %>%
    dplyr::select(t=timestamps,
                  ID=local_identifier,
                  long=location_long.1,
                  lat=location_lat.1)%>%
    mutate(sex=ifelse(ID=="SYSUL137"|ID=="SYSUL139"|ID=="SYSUL126"|ID=="SYSUL129"|ID=="SYSUL131"|ID=="SYSUL133"|ID=="SYSUL134"|ID=="SYSUL136","F","M"))
  #This can be used to output each individual data separately, not in use now
  #sw<-sw.list[[i]]
  #file.out <- paste0("./RawData/", paste0(i, ".csv"))
  #write.csv(sw, file = file.out)
}

#Combine individuals into one big dataframe
bird_interp20 <- do.call("rbind", sw.list)
write.csv(bird_interp20,file="RawData_movebank/bird_interp20.csv")
save(bird_interp20,file="RawData_movebank/bird_interp20.Rdata")