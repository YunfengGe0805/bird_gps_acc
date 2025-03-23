library(tidyverse)
library(lubridate)
data<-read.csv("C:/ALAN_StreakedShearwater/Rawdata_interp_env/M140_interp_moonlit_lcc_rad_202301.csv")
load(file="C:/ALAN_StreakedShearwater/acc/M140/M140202301metrics.Rdata")
data<-data[,-1]
data$t<-as.POSIXct(data$t,tz="UTC")
data$t <- floor_date(data$t, unit = "minute")
merged_data <- merge(data, result, by = "t", all.x = TRUE)
save(merged_data,file="C:/ALAN_StreakedShearwater/Rawdata_interp_env/M140_interp_moonlit_lcc_rad_acc_202301.Rdata")

