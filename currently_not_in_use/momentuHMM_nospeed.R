rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")
library(momentuHMM)
library(doFuture)
library(stats)
library(tidyverse)
library(lubridate)

#load 2023 UTC+8 data
load(file="swdat.Rdata")

#select ID, time, location coordinates for crawlwrap simulation
crwdat<-swdat%>%
  select(ID,
         t,
         X,
         Y)
crwOut<-crawlWrap(crwdat,timeStep = "10 mins", Time.name = "t",attempts = 50,coord = c("X","Y"),proj = "EPSG:32649",ncores = 3)
dat <- prepData(crwOut)
plot(dat)

#Get summary statistics of step and angle
summary(dat$step)
quantile(dat$step,
         probs = c(0.3,0.5,0.90),
         na.rm = TRUE)
summary(dat$angle)
quantile(dat$angle,
         probs = c(0.10,0.5,0.90),
         na.rm = TRUE)   

#Fit 3 states HMM models
# initial step distribution natural scale parameters
stepPar0 <- c(390,1200,5340,390,1200,5340) # (mu_1, mu_2, sd_1, sd_2,zeromass_1,zeromass_2)

# initial angle distribution natural scale parameters
anglePar0 <- c(0, pi, 0,0.01,0.5,0.01) # (mean_1, mean_2, concentration_1, concentration_2)

set.seed(123)  #if states get flipped, try adjusting initial params or random seed number (e.g., seed 2022 gave problems w/ flipped states)
fit_hmm_3states <- fitHMM(data = dat,
                          nbStates = 3,
                          dist = list(step = "gamma", angle = "wrpcauchy"),
                          Par0 = list(step = stepPar0, angle = anglePar0),
                          formula = ~ 1,
                          estAngleMean = list(angle=TRUE),
                          stateNames = c('drifting', 'foraging', 'transiting'),
                          retryFits = 2)  #may be necessary to run more fits

fit_hmm_3states_ALANm <- fitHMM(data = dat,
                          nbStates = 3,
                          dist = list(step = "gamma", angle = "wrpcauchy"),
                          Par0 = list(step = stepPar0, angle = anglePar0),
                          formula = ~ ALANm,
                          estAngleMean = list(angle=TRUE),
                          stateNames = c('drifting', 'foraging', 'transiting'),
                          retryFits = 100)  #may be necessary to run more fits

fit_hmm_3states
fit_hmm_3states_ALANm

plot(fit_hmm_3states)  #model can't really differentiate between the breeding and foraging ground movements; but does appear to detect a 3rd state
plotStates(fit_hmm_3states)  #possible that 1st state is 'Encamped' and 2nd state is 'ARS'
timeInStates(fit_hmm_3states)  #primarily in Encamped/Breeding or Foraging state; only 3% Migratory
plotPR(fit_hmm_3states, ncores = 5)  #ACF and QQ plot for SL looks better

plot(fit_hmm_3states_ALANm)  #model can't really differentiate between the breeding and foraging ground movements; but does appear to detect a 3rd state
plotStates(fit_hmm_3states_ALANm)  #possible that 1st state is 'Encamped' and 2nd state is 'ARS'
timeInStates(fit_hmm_3states_ALANm)  #primarily in Encamped/Breeding or Foraging state; only 3% Migratory
plotPR(fit_hmm_3states_ALANm, ncores = 5)  #ACF and QQ plot for SL looks better
