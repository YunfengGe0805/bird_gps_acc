dat<-read.csv(file = "f131states.csv")
f131night<-subset(dat,class=="night")

F131night$ALANclass
write.csv(f131night,file = "f131night.csv")

dat<-read.csv(file = "m140states.csv")
m140night<-subset(dat,class=="night")
write.csv(m140night,file = "m140night.csv")

dat<-read.csv(file = "f126states.csv")
f126night<-subset(dat,class=="night")
write.csv(f126night,file = "f126night.csv")

dat<-read.csv(file = "f139states.csv")
f139night<-subset(dat,class=="night")
write.csv(f139night,file = "f139night.csv")

dat<-read.csv(file = "m127states.csv")
m127night<-subset(dat,class=="night")
write.csv(m127night,file = "m127night.csv")