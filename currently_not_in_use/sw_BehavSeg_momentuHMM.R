rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")

library(momentuHMM)
library(doFuture)
library(stats)
library(tidyverse)
library(lubridate)

#not in use
library(sf)
library(tictoc)
library(plotly)
library(ctmm)
library(amt)
library(svDialogs)
library(tmap)
library(gt)


load(file="swdat.Rdata")
swdata<-as.data.frame(swdat)
swdata<-arrange(swdata,ID,t)
SYSUL140<-swdata%>%filter(ID=="SYSUL140")
SYSUL128<-swdata%>%filter(ID=="SYSUL128")
#get equal time interval data using simulation
crwOut<-crawlWrap(SYSUL140,timeStep = "30 mins", Time.name = "t",attempts = 20,coord = c("X","Y"),proj = "EPSG:32649",ncores = 3)
dat <- prepData(SYSUL140, type = 'UTM', coordNames = c("X","Y"))
dat <- prepData(crwOut) 
plot(dat)

swtrack<- mk_track(SW.data,.x=X,.y=Y,.t=t_,crs = crs("EPSG:32649"),
                   ID=id,lat=lat,long=long,ALAN=ALAN,ChollA=ChollA,temp=temp,speed=speed,order_by_ts = T)
swtrack<- mk_track(swdat,.x=X,.y=Y,.t=t,crs = "EPSG:32649",
                   all_cols=TRUE, order_by_ts = T)
#Resample the data at 20min interval 
sw.discrete<-track_resample(swtrack,rate = minutes(30), tolerance = minutes(10), start = 1)%>%
  arrange(ID,t_)


# Plot time series and distributions of step lengths
ggplot(dat, aes(step)) +
  geom_histogram() +
  theme_bw()

ggplot(dat, aes(t, step)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~ID, scales = "free_x")

# Plot time series and distributions of turning angles
ggplot(dat, aes(angle)) +
  geom_histogram(binwidth = pi/8) +
  theme_bw()

ggplot(dat, aes(t, angle)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~ID, scales = "free_x")

#Plot time series and distributions of speed
ggplot(dat, aes(speed)) +
  geom_histogram() +
  theme_bw()

ggplot(dat, aes(t, speed)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~ID, scales = "free_x")


#Get summary statistics of step and angle
summary(dat$step)
quantile(dat$step,
         probs = c(0.3,0.5,0.90),
         na.rm = TRUE)
summary(dat$angle)
quantile(dat$angle,
         probs = c(0.10,0.5,0.90),
         na.rm = TRUE)

#Fit 2 states HMM models
# initial step distribution natural scale parameters
stepPar0 <- c(500,9000, 500, 9000, 0.1, 0.05) # (mu_1, mu_2, sd_1, sd_2,zeromass_1,zeromass_2)

# initial angle distribution natural scale parameters
anglePar0 <- c(pi, 0, 0.99, 0.1) # (mean_1, mean_2, concentration_1, concentration_2)

#initial speed distribution
speedPar0<- c(5, 10, 5, 10, 0.1, 0.05)

#set.seed(2002)  #if states get flipped, try adjusting initial params or random seed number (e.g., seed 2022 gave problems w/ flipped states)
fit_hmm_2states <- fitHMM(data = dat,
                          nbStates = 2,
                          dist = list(step = "gamma", angle = "wrpcauchy"),
                          Par0 = list(step = stepPar0, angle = anglePar0),
                          formula = ~ 1,
                          estAngleMean = list(angle=TRUE),
                          stateNames = c('Forage', 'Transit'),
                          retryFits = 100)  #may be necessary to run more fits

fit_MIhmm_2states <- MIfitHMM(miData = dat,
                          nSims = 5,
                          nbStates = 2,
                          dist = list(step = "gamma", angle = "wrpcauchy"),
                          Par0 = list(step = stepPar0, angle = anglePar0),
                          formula = ~ 1,
                          estAngleMean = list(angle=TRUE),
                          stateNames = c('Forage', 'Transit'),
                          retryFits = 100)  #may be necessary to run more fits


fit_hmm_2states_ALAN <- fitHMM(data = dat,
                          nbStates = 2,
                          dist = list(step = "gamma", angle = "wrpcauchy"),
                          Par0 = list(step = stepPar0, angle = anglePar0),
                          formula = ~ ALAN,
                          estAngleMean = list(angle=TRUE),
                          stateNames = c('Forage', 'Transit'),
                          retryFits = 10)  #may be necessary to run more fits

fit_hmm_2states
fit_hmm_2states_ALAN
plot(fit_hmm_2states)
plot(fit_hmm_2states_ALAN)
plotPR(fit_hmm_2states, ncores = 5)  #plot of pseudo-residuals show that there are likely some problems

#Fit 3 states HMM models
# initial step distribution natural scale parameters
stepPar0 <- c(500, 3500, 10000, 500, 10000, 0.1, 0.05) # (mu_1, mu_2, sd_1, sd_2,zeromass_1,zeromass_2)
stepPar0 <- c(500, 2000, 10000, 500, 2000, 10000)
# initial angle distribution natural scale parameters
anglePar0 <- c(pi/6, pi, 0, 0.99, 0.99, 0.1) # (mean_1, mean_2, concentration_1, concentration_2)

#initial speed distribution natural scale parameters
speedPar0 <- c(2,8,8,2,8,8,0.01,0.005,0.005)

set.seed(123)  #if states get flipped, try adjusting initial params or random seed number (e.g., seed 2022 gave problems w/ flipped states)
fit_hmm_3states <- fitHMM(data = dat,
                          nbStates = 3,
                          dist = list(step = "gamma", angle = "wrpcauchy", speed = "gamma"),
                          Par0 = list(step = stepPar0, angle = anglePar0, speed = speedPar0),
                          formula = ~ 1,
                          estAngleMean = list(angle=TRUE),
                          stateNames = c('Drifting', 'Foraging', 'Transiting'),
                          retryFits = 50)  #may be necessary to run more fits

fit_hmm_3statesMI <- MIfitHMM(miData = crwOut,
                          nSims = 2,
                          nbStates = 3,
                          dist = list(step = "gamma", angle = "wrpcauchy", speed = "gamma"),
                          Par0 = list(step = stepPar0, angle = anglePar0, speed = speedPar0),
                          formula = ~ 1,
                          estAngleMean = list(angle=TRUE),
                          stateNames = c('Drifting', 'Foraging', 'Transiting'),
                          )  #may be necessary to run more fits

fit_hmm_3states140 <- fitHMM(data = dat,
                          nbStates = 3,
                          dist = list(step = "gamma", angle = "wrpcauchy", speed = "gamma"),
                          Par0 = list(step = stepPar0, angle = anglePar0, speed = speedPar0),
                          formula = ~ 1,
                          estAngleMean = list(angle=TRUE),
                          stateNames = c('Drifting', 'Foraging', 'Transiting'),
)  #may be necessary to run more fits

fit_hmm_3states140ALAN <- fitHMM(data = dat,
                             nbStates = 3,
                             dist = list(step = "gamma", angle = "wrpcauchy", speed = "gamma"),
                             Par0 = list(step = stepPar0, angle = anglePar0, speed = speedPar0),
                             formula = ~ ALAN,
                             estAngleMean = list(angle=TRUE),
                             stateNames = c('Drifting', 'Foraging', 'Transiting'),
)  #may be necessary to run more fits

fit_hmm_3states_ALAN <- fitHMM(data = dat,
                               nbStates = 3,
                               dist = list(step = "gamma", angle = "wrpcauchy"),
                               Par0 = list(step = stepPar0, angle = anglePar0),
                               formula = ~ ALAN,
                               estAngleMean = list(angle=TRUE),
                               stateNames = c('Forage', 'Transit'),
                               retryFits = 10)  #may be necessary to run more fits

fit_hmm_3states
plot(fit_hmm_3statesMI)  #model can't really differentiate between the breeding and foraging ground movements; but does appear to detect a 3rd state
plotStates(fit_hmm_3states)  #possible that 1st state is 'Encamped' and 2nd state is 'ARS'
timeInStates(fit_hmm_3states)  #primarily in Encamped/Breeding or Foraging state; only 3% Migratory
plotPR(fit_hmm_3states, ncores = 5)  #ACF and QQ plot for SL looks better

## ----Model Comparison, message=FALSE, warning=FALSE, echo=TRUE---------------------------------------
# Which of these two models is a better fit to the data?
# Results indicate that there is next to no difference between the models
AIC(WB.null,
    WB.temp,
    WB.tod,
    WB.tod.3state)

