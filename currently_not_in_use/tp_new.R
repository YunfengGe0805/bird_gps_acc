#Clear environment and set working directory
rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")
library(momentuHMM); 
library(ggplot2)
library(dplyr);library(tidyverse)
## LOAD MODELS

# F best
file.in <- paste0("./Data_outputs/", paste0("F_mod_", 4, ".rda"))
load(file = file.in)
f.best.mod <- model

# M best
file.in <- paste0("./Data_outputs/", paste0("M_mod_", 3, ".rda"))
load(file = file.in)
m.best.mod <- model

#get transition probability
#Female
tpm<-getTrProbs(best.mod,getCI=TRUE)
save(tpm,file="Data_outputs_sex/tpm.Rdata")
load(file="Data_outputs_sex/tpm.Rdata")
mean<-as.data.frame(t(as.data.frame(tpm$est)))
mean$class<-rep(c("Raft","Forage","Travel"),times=3)
u<-as.data.frame(t(as.data.frame(tpm$upper)))
u$class<-rep(c("Raft","Forage","Travel"),times=3)
l<-as.data.frame(t(as.data.frame(tpm$lower)))
l$class<-rep(c("Raft","Forage","Travel"),times=3)

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

### BUILD PLOT no Oil
#################################################To forage no Oil
load(file="Data_outputs_sex/AlltoF.Rdata")
AlltoF$Moon<-ifelse(AlltoF$moon<0.1|AlltoF$moon==0.1,"New",AlltoF$moon)
AlltoF$Moon<-ifelse(AlltoF$moon>0.1|AlltoF$moon==0.9,"Full",AlltoF$Moon)
AlltoF$Moon<-ifelse(AlltoF$moon>0.1&AlltoF$moon<0.9,"Mid",AlltoF$Moon)

RtoF<-ggplot(AlltoF, aes(x = ALAN, y=Raft))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr.Raft, ymax=ur.Raft,col=Moon), alpha=0.15) + 
  geom_smooth(aes(col = Moon, linetype = Moon))+
  ggtitle("Transition Probability from Raft to Forage") +
  ylab("Probability")
RtoF + theme_bw()

ggsave("Data_outputs_sex/RtoF.png",width = 15, height = 7, units = "in", dpi = 350)

#Travel to forage no Oil
load(file="tp_outputs/AlltoF.Rdata")
AlltoF$lunarCat<-ifelse(AlltoF$moon<0.1|AlltoF$moon==0.1,"New",AlltoF$moon)
AlltoF$lunarCat<-ifelse(AlltoF$moon>0.1|AlltoF$moon==0.9,"Full",AlltoF$lunarCat)
AlltoF$lunarCat<-ifelse(AlltoF$moon>0.1&AlltoF$moon<0.9,"Mid",AlltoF$lunarCat)
AlltoF_no<-subset(AlltoF,AlltoF$Oil=="No")
TtoF_nooil<-ggplot(AlltoF_no, aes(x = ALAN, y=Travel))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr.Travel, ymax=ur.Travel,col=lunarCat), alpha=0.15) + 
  geom_smooth(aes(col = lunarCat, linetype = lunarCat))+
  ggtitle("Transition Probability from Travel to Forage") +
  ylab("Probability")
TtoF_nooil + theme_bw()

ggsave("tp_outputs/TtoF_nooil.png",width = 18, height = 7, units = "in", dpi = 350)

#forage to forage no Oil
FtoF_nooil<-ggplot(AlltoF_no, aes(x = ALAN, y=Forage))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr.Forage, ymax=ur.Forage,col=lunarCat), alpha=0.15) + 
  geom_smooth(aes(col = lunarCat, linetype = lunarCat))+
  ggtitle("Transition Probability from Forage to Forage") +
  ylab("Probability")
FtoF_nooil + theme_bw()

ggsave("tp_outputs/FtoF_nooil.png",width = 18, height = 7, units = "in", dpi = 350)


##################################To raft no Oil
load(file="tp_outputs/AlltoR.Rdata")
AlltoR$lunarCat<-ifelse(AlltoR$moon<0.1|AlltoR$moon==0.1,"New",AlltoR$moon)
AlltoR$lunarCat<-ifelse(AlltoR$moon>0.1|AlltoR$moon==0.9,"Full",AlltoR$lunarCat)
AlltoR$lunarCat<-ifelse(AlltoR$moon>0.1&AlltoR$moon<0.9,"Mid",AlltoR$lunarCat)
AlltoR_no<-subset(AlltoR,AlltoR$Oil=="No")
RtoR_nooil<-ggplot(AlltoR_no, aes(x = ALAN, y=Raft))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr.Raft, ymax=ur.Raft,col=lunarCat), alpha=0.15) + 
  geom_smooth(aes(col = lunarCat, linetype = lunarCat))+
  ggtitle("Transition Probability from Raft to Raft") +
  ylab("Probability")
RtoR_nooil + theme_bw()

ggsave("tp_outputs/RtoR_nooil.png",width = 18, height = 7, units = "in", dpi = 350)

#Travel to raft no Oil
TtoR_nooil<-ggplot(AlltoR_no, aes(x = ALAN, y=Travel))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr.Travel, ymax=ur.Travel,col=lunarCat), alpha=0.15) + 
  geom_smooth(aes(col = lunarCat, linetype = lunarCat))+
  ggtitle("Transition Probability from Travel to Raft") +
  ylab("Probability")
TtoR_nooil + theme_bw()

ggsave("tp_outputs/TtoR_nooil.png",width = 18, height = 7, units = "in", dpi = 350)

#forage to raft no Oil
FtoR_nooil<-ggplot(AlltoR_no, aes(x = ALAN, y=Forage))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr.Forage, ymax=ur.Forage,col=lunarCat), alpha=0.15) + 
  geom_smooth(aes(col = lunarCat, linetype = lunarCat))+
  ggtitle("Transition Probability from Forage to Raft") +
  ylab("Probability")
FtoR_nooil + theme_bw()

ggsave("tp_outputs/FtoR_nooil.png",width = 18, height = 7, units = "in", dpi = 350)

##################################To travel no Oil
load(file="tp_outputs/AlltoT.Rdata")
AlltoT$lunarCat<-ifelse(AlltoT$moon<0.1|AlltoT$moon==0.1,"New",AlltoT$moon)
AlltoT$lunarCat<-ifelse(AlltoT$moon>0.1|AlltoT$moon==0.9,"Full",AlltoT$lunarCat)
AlltoT$lunarCat<-ifelse(AlltoT$moon>0.1&AlltoT$moon<0.9,"Mid",AlltoT$lunarCat)
AlltoT_no<-subset(AlltoT,AlltoT$Oil=="No")
RtoT_nooil<-ggplot(AlltoT_no, aes(x = ALAN, y=Raft))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr.Raft, ymax=ur.Raft,col=lunarCat), alpha=0.15) + 
  geom_smooth(aes(col = lunarCat, linetype = lunarCat))+
  ggtitle("Transition Probability from Raft to Travel") +
  ylab("Probability")
RtoT_nooil + theme_bw()

ggsave("tp_outputs/RtoT_nooil.png",width = 18, height = 7, units = "in", dpi = 350)

#Travel to raft no Oil
TtoT_nooil<-ggplot(AlltoR_no, aes(x = ALAN, y=Travel))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr.Travel, ymax=ur.Travel,col=lunarCat), alpha=0.15) + 
  geom_smooth(aes(col = lunarCat, linetype = lunarCat))+
  ggtitle("Transition Probability from Travel to Travel") +
  ylab("Probability")
TtoT_nooil + theme_bw()

ggsave("tp_outputs/TtoT_nooil.png",width = 18, height = 7, units = "in", dpi = 350)

#forage to raft no Oil
FtoT_nooil<-ggplot(AlltoR_no, aes(x = ALAN, y=Forage))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr.Forage, ymax=ur.Forage,col=lunarCat), alpha=0.15) + 
  geom_smooth(aes(col = lunarCat, linetype = lunarCat))+
  ggtitle("Transition Probability from Forage to Travel") +
  ylab("Probability")
FtoT_nooil + theme_bw()

ggsave("tp_outputs/FtoT_nooil.png",width = 18, height = 7, units = "in", dpi = 350)

############################################################################################
##############################################################################################
### BUILD PLOT with Oil
#################################################To forage Oil
load(file="tp_outputs/AlltoF.Rdata")
AlltoF$lunarCat<-ifelse(AlltoF$moon<0.1|AlltoF$moon==0.1,"New",AlltoF$moon)
AlltoF$lunarCat<-ifelse(AlltoF$moon>0.1|AlltoF$moon==0.9,"Full",AlltoF$lunarCat)
AlltoF$lunarCat<-ifelse(AlltoF$moon>0.1&AlltoF$moon<0.9,"Mid",AlltoF$lunarCat)
AlltoF_no<-subset(AlltoF,AlltoF$Oil=="Yes")
RtoF_nooil<-ggplot(AlltoF_no, aes(x = ALAN, y=Raft))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr.Raft, ymax=ur.Raft,col=lunarCat), alpha=0.15) + 
  geom_smooth(aes(col = lunarCat, linetype = lunarCat))+
  ggtitle("Transition Probability from Raft to Forage") +
  ylab("Probability")
RtoF_nooil + theme_bw()

ggsave("tp_outputs/RtoF_oil.png",width = 18, height = 7, units = "in", dpi = 350)

#Travel to forage Oil
load(file="tp_outputs/AlltoF.Rdata")
AlltoF$lunarCat<-ifelse(AlltoF$moon<0.1|AlltoF$moon==0.1,"New",AlltoF$moon)
AlltoF$lunarCat<-ifelse(AlltoF$moon>0.1|AlltoF$moon==0.9,"Full",AlltoF$lunarCat)
AlltoF$lunarCat<-ifelse(AlltoF$moon>0.1&AlltoF$moon<0.9,"Mid",AlltoF$lunarCat)
AlltoF_no<-subset(AlltoF,AlltoF$Oil=="No")
TtoF_nooil<-ggplot(AlltoF_no, aes(x = ALAN, y=Travel))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr.Travel, ymax=ur.Travel,col=lunarCat), alpha=0.15) + 
  geom_smooth(aes(col = lunarCat, linetype = lunarCat))+
  ggtitle("Transition Probability from Travel to Forage") +
  ylab("Probability")
TtoF_nooil + theme_bw()

ggsave("tp_outputs/TtoF_oil.png",width = 18, height = 7, units = "in", dpi = 350)

#forage to forage Oil
FtoF_nooil<-ggplot(AlltoF_no, aes(x = ALAN, y=Forage))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr.Forage, ymax=ur.Forage,col=lunarCat), alpha=0.15) + 
  geom_smooth(aes(col = lunarCat, linetype = lunarCat))+
  ggtitle("Transition Probability from Forage to Forage") +
  ylab("Probability")
FtoF_nooil + theme_bw()

ggsave("tp_outputs/FtoF_oil.png",width = 18, height = 7, units = "in", dpi = 350)


##################################To raft Oil
load(file="tp_outputs/AlltoR.Rdata")
AlltoR$lunarCat<-ifelse(AlltoR$moon<0.1|AlltoR$moon==0.1,"New",AlltoR$moon)
AlltoR$lunarCat<-ifelse(AlltoR$moon>0.1|AlltoR$moon==0.9,"Full",AlltoR$lunarCat)
AlltoR$lunarCat<-ifelse(AlltoR$moon>0.1&AlltoR$moon<0.9,"Mid",AlltoR$lunarCat)
AlltoR_no<-subset(AlltoR,AlltoR$Oil=="Yes")
RtoR_nooil<-ggplot(AlltoR_no, aes(x = ALAN, y=Raft))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr.Raft, ymax=ur.Raft,col=lunarCat), alpha=0.15) + 
  geom_smooth(aes(col = lunarCat, linetype = lunarCat))+
  ggtitle("Transition Probability from Raft to Raft") +
  ylab("Probability")
RtoR_nooil + theme_bw()

ggsave("tp_outputs/RtoR_nooil.png",width = 18, height = 7, units = "in", dpi = 350)

#Travel to raft Oil
TtoR_nooil<-ggplot(AlltoR_no, aes(x = ALAN, y=Travel))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr.Travel, ymax=ur.Travel,col=lunarCat), alpha=0.15) + 
  geom_smooth(aes(col = lunarCat, linetype = lunarCat))+
  ggtitle("Transition Probability from Travel to Raft") +
  ylab("Probability")
TtoR_nooil + theme_bw()

ggsave("tp_outputs/TtoR_nooil.png",width = 18, height = 7, units = "in", dpi = 350)

#forage to raft Oil
FtoR_nooil<-ggplot(AlltoR_no, aes(x = ALAN, y=Forage))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr.Forage, ymax=ur.Forage,col=lunarCat), alpha=0.15) + 
  geom_smooth(aes(col = lunarCat, linetype = lunarCat))+
  ggtitle("Transition Probability from Forage to Raft") +
  ylab("Probability")
FtoR_nooil + theme_bw()

ggsave("tp_outputs/FtoR_nooil.png",width = 18, height = 7, units = "in", dpi = 350)

##################################To travel Oil
load(file="tp_outputs/AlltoT.Rdata")
AlltoT$lunarCat<-ifelse(AlltoT$moon<0.1|AlltoT$moon==0.1,"New",AlltoT$moon)
AlltoT$lunarCat<-ifelse(AlltoT$moon>0.1|AlltoT$moon==0.9,"Full",AlltoT$lunarCat)
AlltoT$lunarCat<-ifelse(AlltoT$moon>0.1&AlltoT$moon<0.9,"Mid",AlltoT$lunarCat)
AlltoT_no<-subset(AlltoT,AlltoT$Oil=="Yes")
RtoT_nooil<-ggplot(AlltoT_no, aes(x = ALAN, y=Raft))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr.Raft, ymax=ur.Raft,col=lunarCat), alpha=0.15) + 
  geom_smooth(aes(col = lunarCat, linetype = lunarCat))+
  ggtitle("Transition Probability from Raft to Travel") +
  ylab("Probability")
RtoT_nooil + theme_bw()

ggsave("tp_outputs/RtoT_oil.png",width = 18, height = 7, units = "in", dpi = 350)

#Travel to raft no Oil
TtoT_nooil<-ggplot(AlltoR_no, aes(x = ALAN, y=Travel))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr.Travel, ymax=ur.Travel,col=lunarCat), alpha=0.15) + 
  geom_smooth(aes(col = lunarCat, linetype = lunarCat))+
  ggtitle("Transition Probability from Travel to Travel") +
  ylab("Probability")
TtoT_nooil + theme_bw()

ggsave("tp_outputs/TtoT_oil.png",width = 18, height = 7, units = "in", dpi = 350)

#forage to raft Oil
FtoT_nooil<-ggplot(AlltoR_no, aes(x = ALAN, y=Forage))+facet_wrap(~sex) +
  geom_ribbon(size = 1, linetype = "blank", aes(ymin=lr.Forage, ymax=ur.Forage,col=lunarCat), alpha=0.15) + 
  geom_smooth(aes(col = lunarCat, linetype = lunarCat))+
  ggtitle("Transition Probability from Forage to Travel") +
  ylab("Probability")
FtoT_nooil + theme_bw()

ggsave("tp_outputs/FtoT_oil.png",width = 18, height = 7, units = "in", dpi = 350)
