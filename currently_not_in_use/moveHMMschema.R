library(moveHMM)
library(lubridate)
library(tidyverse)
library(move)
library(dplyr)
library(lubridate)
library(sf)
library(stats)






##Prepare data (angle and step length calculated)
datf139 <- prepData(f139rs, type = 'LL', coordNames = c("long","lat"))

##Check if there are 0 in step (if yes need zeromass parameter)
summary(datf139$step)
#Fit 3 states HMM models
set.seed(0805)

##Fit model for F139 
### initial step distribution natural scale parameters
stepPar0 <- c(0.78,2.41,10.68,0.78,2.41,10.68)
#guess based on Behavioural mapping of a pelagic seabird: combining multiple sensors and a hidden Markov model reveals the distribution of at-sea behaviour
### initial angle distribution natural scale parameters
anglePar0 <- c(0,3,0,1,10,1)
SW.null <- fitHMM(data = datf139, 
                  nbStates = 3, 
                  stepPar0 = stepPar0, 
                  anglePar0 = anglePar0, 
                  stepDist = "gamma",
                  angleDist = "vm",
                  formula = ~ 1)
SW.null