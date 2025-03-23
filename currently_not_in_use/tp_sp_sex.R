#Clear environment and set working directory
rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")
library(momentuHMM); 
library(ggplot2)
library(dplyr);library(tidyverse)
## LOAD MODELS

# best model
file.in <- paste0("./Data_outputs_sex/", paste0("Mod_", 4, ".rda"))
load(file = file.in)
best.mod <- model
#plot(best.mod,plotCI=TRUE,plotStationary=TRUE)

#get transition probability
tpm<-getTrProbs(best.mod,getCI=TRUE)
save(tpm,file="Data_outputs_sex/tpm.Rdata")
load(file="Data_outputs_sex/tpm.Rdata")
mean<-as.data.frame(t(as.data.frame(tpm$est)))
mean$class<-rep(c("Raft","Forage","Travel"),times=3,length=nrow(mean))
u<-as.data.frame(t(as.data.frame(tpm$upper)))
u$class<-rep(c("Raft","Forage","Travel"),times=3,length=nrow(mean))
l<-as.data.frame(t(as.data.frame(tpm$lower)))
l$class<-rep(c("Raft","Forage","Travel"),times=3,length=nrow(mean))

#AlltoF
AlltoF<-subset(mean,class=="Forage")%>%
  mutate(ALAN=best.mod$data$rad_log_5,
         moon=best.mod$data$fraction,
         ur=subset(u,class=="Forage"),
         lr=subset(l,class=="Forage"),
         sex=best.mod$data$sex)
write.csv(AlltoF,file="Data_outputs_sex/AlltoF.csv")
save(AlltoF,file="Data_outputs_sex/AlltoF.Rdata")

#AlltoT
AlltoT<-subset(mean,class=="Travel")%>%
  mutate(ALAN=best.mod$data$rad_log_5,
         moon=best.mod$data$fraction,
         ur=subset(u,class=="Travel"),
         lr=subset(l,class=="Travel"),
         sex=best.mod$data$sex)
write.csv(AlltoT,file="Data_outputs_sex/AlltoT.csv")
save(AlltoT,file="Data_outputs_sex/AlltoT.Rdata")

#AlltoR
AlltoR<-subset(mean,class=="Raft")%>%
  mutate(ALAN=best.mod$data$rad_log_5,
         moon=best.mod$data$fraction,
         ur=subset(u,class=="Raft"),
         lr=subset(l,class=="Raft"),
         sex=best.mod$data$sex)
write.csv(AlltoR,file="Data_outputs_sex/AlltoR.csv")
save(AlltoR,file="Data_outputs_sex/AlltoR.Rdata")

### BUILD PLOT 
#################################################To forage
load(file="Data_outputs_sex/AlltoF.Rdata")
AlltoF$Moon<-ifelse(AlltoF$moon<0.1|AlltoF$moon==0.1,"New",AlltoF$moon)
AlltoF$Moon<-ifelse(AlltoF$moon>0.1|AlltoF$moon==0.9,"Full",AlltoF$Moon)
AlltoF$Moon<-ifelse(AlltoF$moon>0.1&AlltoF$moon<0.9,"Mid",AlltoF$Moon)

RtoF<-ggplot(AlltoF, aes(x = ALAN, y=Raft))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr$Raft, ymax=ur$Raft,col=Moon), alpha=0.15) + 
  geom_smooth(aes(col = Moon, linetype = Moon))+
  ggtitle("Transition Probability from Raft to Forage") +
  ylab("Probability")
RtoF + theme_bw()

ggsave("Data_outputs_sex/RtoF.png",width = 15, height = 7, units = "in", dpi = 350)

#Travel to forage
TtoF<-ggplot(AlltoF, aes(x = ALAN, y=Travel))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr$Travel, ymax=ur$Travel,col=Moon), alpha=0.15) + 
  geom_smooth(aes(col = Moon, linetype = Moon))+
  ggtitle("Transition Probability from Travel to Forage") +
  ylab("Probability")
TtoF + theme_bw()

ggsave("tp_outputs/TtoF.png",width = 15, height = 7, units = "in", dpi = 350)

#forage to forage
FtoF<-ggplot(AlltoF, aes(x = ALAN, y=Forage))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr$Forage, ymax=ur$Forage,col=Moon), alpha=0.15) + 
  geom_smooth(aes(col = Moon, linetype = Moon))+
  ggtitle("Transition Probability from Forage to Forage") +
  ylab("Probability")
FtoF + theme_bw()

FtoF<-ggplot(AlltoF, aes(x = ALAN, y=Forage))+facet_wrap(~Moon) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr$Forage, ymax=ur$Forage,col=sex), alpha=0.15) + 
  geom_smooth(aes(col = sex, linetype = sex))+
  ggtitle("Transition Probability from Forage to Forage") +
  ylab("Probability")
FtoF + theme_bw()


ggsave("Data_outputs_sex/FtoF.png",width = 15, height = 7, units = "in", dpi = 350)


##################################To raft
load(file="Data_outputs_sex/AlltoR.Rdata")
AlltoR$Moon<-ifelse(AlltoR$moon<0.1|AlltoR$moon==0.1,"New",AlltoR$moon)
AlltoR$Moon<-ifelse(AlltoR$moon>0.1|AlltoR$moon==0.9,"Full",AlltoR$Moon)
AlltoR$Moon<-ifelse(AlltoR$moon>0.1&AlltoR$moon<0.9,"Mid",AlltoR$Moon)

RtoR<-ggplot(AlltoR, aes(x = ALAN, y=Raft))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr$Raft, ymax=ur$Raft,col=Moon), alpha=0.15) + 
  geom_smooth(aes(col = Moon, linetype = Moon))+
  ggtitle("Transition Probability from Raft to Raft") +
  ylab("Probability")
RtoR + theme_bw()

ggsave("Data_outputs_sex/RtoR.png",width = 15, height = 7, units = "in", dpi = 350)

#Travel to raft no Oil
TtoR<-ggplot(AlltoR, aes(x = ALAN, y=Travel))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr$Travel, ymax=ur$Travel,col=Moon), alpha=0.15) + 
  geom_smooth(aes(col = Moon, linetype = Moon))+
  ggtitle("Transition Probability from Travel to Raft") +
  ylab("Probability")
TtoR + theme_bw()

ggsave("Data_outputs_sex/TtoR.png",width = 15, height = 7, units = "in", dpi = 350)

#forage to raft no Oil
FtoR<-ggplot(AlltoR_no, aes(x = ALAN, y=Forage))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr$Forage, ymax=ur$Forage,col=Moon), alpha=0.15) + 
  geom_smooth(aes(col = Moon, linetype = Moon))+
  ggtitle("Transition Probability from Forage to Raft") +
  ylab("Probability")
FtoR + theme_bw()

ggsave("Data_outputs_sex/FtoR.png",width = 15, height = 7, units = "in", dpi = 350)

##################################To travel
load(file="Data_outputs_sex/AlltoT.Rdata")
AlltoT$Moon<-ifelse(AlltoT$moon<0.1|AlltoT$moon==0.1,"New",AlltoT$moon)
AlltoT$Moon<-ifelse(AlltoT$moon>0.1|AlltoT$moon==0.9,"Full",AlltoT$Moon)
AlltoT$Moon<-ifelse(AlltoT$moon>0.1&AlltoT$moon<0.9,"Mid",AlltoT$Moon)

RtoT<-ggplot(AlltoT, aes(x = ALAN, y=Raft))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr$Raft, ymax=ur$Raft,col=Moon), alpha=0.15) + 
  geom_smooth(aes(col = Moon, linetype = Moon))+
  ggtitle("Transition Probability from Raft to Travel") +
  ylab("Probability")
RtoT + theme_bw()

ggsave("Data_outputs_sex/RtoT.png",width = 15, height = 7, units = "in", dpi = 350)

#Travel to raft
TtoT<-ggplot(AlltoR_no, aes(x = ALAN, y=Travel))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr$Travel, ymax=ur$Travel,col=Moon), alpha=0.15) + 
  geom_smooth(aes(col = Moon, linetype = Moon))+
  ggtitle("Transition Probability from Travel to Travel") +
  ylab("Probability")
TtoT + theme_bw()

ggsave("Data_outputs_sex/TtoT.png",width = 15, height = 7, units = "in", dpi = 350)

#forage to raft
FtoT<-ggplot(AlltoR_no, aes(x = ALAN, y=Forage))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr$Forage, ymax=ur$Forage,col=Moon), alpha=0.15) + 
  geom_smooth(aes(col = Moon, linetype = Moon))+
  ggtitle("Transition Probability from Forage to Travel") +
  ylab("Probability")
FtoT + theme_bw()

ggsave("Data_outputs_sex/FtoT.png",width = 15, height = 7, units = "in", dpi = 350)
