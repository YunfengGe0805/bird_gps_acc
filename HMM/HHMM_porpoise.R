library(momentuHMM)
library(tidyverse)
library(lubridate)
library(data.tree)
load(url(paste0("https://static-content.springer.com/esm/", "art%3A10.1007%2Fs13253-017-0282-9/MediaObjects/", "13253_2017_282_MOESM2_ESM.rdata")))
data <- lapply(data,function(x) {x$date_time <- as.POSIXct(x$date_time,tz="UTC"); x}) 
porpoiseData <- NULL 
for(i in 1:length(data)){ 
  coarseInd <- data.frame(date_time=as.POSIXct(format(data[[i]]$date_time[1], format="%Y-%m-%d %H:%M"), tz="UTC"), 
                          level=c("1","2i"), 
                          dive_duration=NA, 
                          maximum_depth=NA, 
                          dive_wiggliness=NA) 
  tmp <- rbind(coarseInd,data.frame(data[[i]],level="2")) 
  porpoiseData <- rbind(porpoiseData,tmp) 
}
porpoiseData <- prepData(data = porpoiseData, coordNames = NULL, hierLevels = c("1", "2i", "2"))
