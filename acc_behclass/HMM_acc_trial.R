#Clear environment and set working directory
rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")
load("C:/ALAN_StreakedShearwater/Rawdata_interp_env/M140_interp_moonlit_lcc_rad_acc_202301.Rdata")
#load packages
library(momentuHMM); library(ggplot2); library(tidyverse)
dat <- prepData(merged_data,type='LL',coordNames = c("long","lat"),
                covNames = c("sex","illumination","moonlight","twilight","night","lcc","AOT","rad","log_rad"))

# INITIALISE HMM DATA ---------------------------------------------------------
#Note: initial value guessed based on the paper Behavioural mapping of a pelagic seabird: combining multiple sensors and a hidden Markov model reveals the distribution of at-sea behaviour
### initial step distribution natural scale parameters
stepPar0 <- c(0.78,2.41,10.68,0.78,2.41,10.68) #This is in km for long/lat prepdata
#stepPar0 <- c(780,2410,10680,780,2410,10680) #This is in m for UTM
### initial angle distribution natural scale parameters
anglePar0 <- c(0,pi,0,1,0.5,1)

###Four states
stepPar0 <- c(0.78,2.41,2.41,10.68,0.78,2.41,2.41,10.68) #This is in km for long/lat prepdata
#stepPar0 <- c(780,2410,10680,780,2410,10680) #This is in m for UTM
### initial angle distribution natural scale parameters
anglePar0 <- c(0,pi,pi,0,1,0.5,0.5,1)

sdZPar0 <- c(0.05,0.2,0.05,0.05,0.1,0.05,0.1,0.05)

sdvedbaPar0 <- c()
#meanrPar0 <- c(-0.1,-0.2,-0.4,-0.1,0.1,0.1,0.1,0.1)
#varpPar0 <- c(0.0001,0.1,0.03,0.0001,0.0001,0.1,0.03,0.0001)
# RUN THE NULL MODEL ------------------------------------------------------

#  first run null models with no covariates on transition probabilities
stateNames <- c("Raft","Forage", "Travel")

m0 <- fitHMM(data=dat, nbStates=3,
             dist=list(step="gamma",angle="vm"),
             Par0=list(step=stepPar0, angle=anglePar0),
             estAngleMean = list(angle=TRUE),
             stateNames = stateNames)
plot(m0)
state<-viterbi(m0)
merged_data$state<-state
state3_viterbi<-merged_data
#save viterbi
save(state3_viterbi,file="C:/ALAN_StreakedShearwater/M140202301_test/state3_viterbi.Rdata")
# Plot overlapping histograms
ggplot(merged_data, aes(x = mean_vedba, fill = factor(state))) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 80) +
  scale_fill_manual(
    values = c("blue","yellow", "red"), # Assign colors to states
    labels = c("raft", "Forage", "travel") # Update legend labels
  ) +
  labs(title = "Histogram of mean_vedba Grouped by State",
       x = "mean_vedba",
       y = "Frequency",
       fill = "State") +
  theme_minimal()

# Plot overlapping histograms
ggplot(merged_data, aes(x = sd_vedba, fill = factor(state))) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 80) +
  scale_fill_manual(
    values = c("blue","yellow", "red"), # Assign colors to states
    labels = c("raft", "Forage", "travel") # Update legend labels
  ) +
  labs(title = "Histogram of sd_vedba Grouped by State",
       x = "sd_vedba",
       y = "Frequency",
       fill = "State") +
  theme_minimal()

# Plot overlapping histograms
ggplot(merged_data, aes(x = sd_Z, fill = factor(state))) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 80) +
  scale_fill_manual(
    values = c("blue","yellow", "red"), # Assign colors to states
    labels = c("raft", "Forage", "travel") # Update legend labels
  ) +
  labs(title = "Histogram of sd_Z Grouped by State",
       x = "sd_Z",
       y = "Frequency",
       fill = "State") +
  theme_minimal()
#save the null model
save(m0, file = "C:/ALAN_StreakedShearwater/M140202301_test/NUll_3states.rda")
