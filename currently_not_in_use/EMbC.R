install.packages("EMbC")
library(EMbC)
load(file="swdat.Rdata")
swdata<-as.data.frame(swdat)
swdata<-arrange(swdata,ID,t)
SYSUL140<-swdata%>%filter(ID=="SYSUL140")
E140<-as.data.frame(M140)
dff140<- data.frame(timeStamp=E140$timestamp,lon=E140$location_long.1,lat=E140$location_lat.1)
#speed/turning angle clustering. Other variables can be used for clustering (see vignette)
bc140 <- stbc(dff140,smth=0.33)
sctr(bc140)
view(bc140)
stts(bc140)
pkml(bc140)
pkml(bc140,showClst=c(1))
bc140A <- as.data.frame(bc140@A)
full140 <- cbind(SYSUL140,bc140A)
full140<-as.data.frame(full140)
colnames(full140)[13] <- "type"
save(full140,file="full140.Rdata")


ll140 <- full134[full134$type == "2", ]
write.csv(ll134,"ll134.csv")

