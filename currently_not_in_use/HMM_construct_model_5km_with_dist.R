### CITATION: This code is modified from scripts shared in Clay et al. 2020, J. Anim. Ecol.

### AIM: Run HMM combinations based on wind and personality predictors; use AIC to identify the best model


# Preamble ----------------------------------------------------------------

#Clear environment and set working directory
rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")

#load packages
library(momentuHMM); library(ggplot2); library(dplyr)

### Create outputs folder if not exist already
#out.path <- "./Data_outputs_infra/"
#if(dir.exists(out.path) == FALSE){
#  dir.create(out.path)
#}

### LOAD IN TRACKS
load(file="RawData/sw_NB_22to24_envall.Rdata")
data<-as.data.frame(sw_NB_22to24_envall)
dat <- prepData(data,type='UTM',coordNames = c("UTMx","UTMy"))
# Remove errorneous step lengths & only select night
dat <- subset(dat, step < 90000)
dat <- subset(dat,period=="night")
hist(dat$step)

# INITIALISE HMM DATA ---------------------------------------------------------
#Note: initial value guessed based on the paper Behavioural mapping of a pelagic seabird: combining multiple sensors and a hidden Markov model reveals the distribution of at-sea behaviour
### initial step distribution natural scale parameters
#stepPar0 <- c(0.78,2.41,10.68,0.78,2.41,10.68) This is in km for long/lat prepdata
stepPar0 <- c(780,2410,10680,780,2410,10680)
### initial angle distribution natural scale parameters
anglePar0 <- c(0,pi,0,1,0.5,1)

# RUN THE NULL MODEL ------------------------------------------------------

#  first run null models with no covariates on transition probabilities
set.seed(0805)
stateNames <- c("Raft","Forage", "Travel")
dat_male <- subset(dat, sex == "M")
dat_female <- subset(dat, sex == "F")

m1_M <- fitHMM(data=dat_male, nbStates=3,
               dist=list(step="gamma",angle="vm"),
               Par0=list(step=stepPar0, angle=anglePar0),
               estAngleMean = list(angle=TRUE),
               stateNames = stateNames)

m1_F <- fitHMM(data=dat_female, nbStates=3,
               dist=list(step="gamma",angle="vm"),
               Par0=list(step=stepPar0, angle=anglePar0),
               estAngleMean = list(angle=TRUE),
               stateNames = stateNames)

#save the null model
save(m1_M, file = "Data_outputs/M_mod_1.rda")
save(m1_F, file = "Data_outputs/F_mod_1.rda")

#look at model results
m1_F
m1_M
plot(m1_F)
plot(m1_M)
# Plot pseudo-residuals
plotPR(m1_F)
plotPR(m1_M)
# Look at acf of step length over longer time lag
acf(dat$step[!is.na(dat$step)], lag.max = 300)

# SET UP FORMULA FOR ALL MODEL COMBINATIONS -------------------------------

## Run next couple of models for males and females separately - make sure to change model names as appropriate

#Load null model
load("Data_outputs/M_mod_1.rda")
load("Data_outputs/F_mod_1.rda")

## Set up personality formulae 

formula <- list()	

formula[[2]] <- ~ rad_log_5
formula[[3]] <- ~ rad_log_5+fraction
formula[[4]] <- ~ rad_log_5+dist
formula[[5]] <- ~ rad_log_5:dist
formula[[6]] <- ~ rad_log_5:fraction
formula[[7]] <- ~ rad_log_5*fraction
formula[[8]] <- ~ rad_log_5*dist
formula[[9]] <- ~ rad_log_5+dist+fraction
formula[[10]] <- ~ rad_log_5:dist+fraction
formula[[11]] <- ~ rad_log_5:fraction+dist
formula[[12]] <- ~ rad_log_5+fraction:dist
formula[[13]] <- ~ rad_log_5*dist*fraction

# this function gets starting values for each model from existing null model fit for a specified covariate formula
Par <- list()
for (i in 2:length(formula)){
  Par[[i]] <- getPar0(model=m1_F, nbStates=3, formula = formula[[i]])
}



# RUN ALL THE MODELS ------------------------------------------------------

## the following code iterates through and runs all 40 models, pasting each out in turn
stateNames <- c("Raft","Forage", "Travel")
# Specify output lists of length formula (Female)
m.list <- vector(mode = "list", length =length(formula))

for (i in 2:length(formula)) {
  print(i)
  m.list[[i]] <- fitHMM(data=dat_female, nbStates=3,
                        dist=list(step="gamma",angle="vm"),
                        Par0=list(step=Par[[i]]$Par$step, angle=Par[[i]]$Par$angle,delta0 = Par[[i]]$delta),
                        estAngleMean = list(angle=TRUE), beta0 = Par[[i]]$beta,
                        stateNames = stateNames, 
                        formula = formula[[i]])
  model <- m.list[[i]]
  file.out <- paste0("./Data_outputs_infra/", paste0("F_mod_", i, ".rda"))
  save(model, file = file.out)
}

# Specify output lists of length formula (Male)
m.list <- vector(mode = "list", length =length(formula))

for (i in 2:length(formula)) {
  print(i)
  m.list[[i]] <- fitHMM(data=dat_male, nbStates=3,
                        dist=list(step="gamma",angle="vm"),
                        Par0=list(step=Par[[i]]$Par$step, angle=Par[[i]]$Par$angle,delta0 = Par[[i]]$delta),
                        estAngleMean = list(angle=TRUE), beta0 = Par[[i]]$beta,
                        stateNames = stateNames, 
                        formula = formula[[i]])
  model <- m.list[[i]]
  file.out <- paste0("./Data_outputs_infra/", paste0("M_mod_", i, ".rda"))
  save(model, file = file.out)
}

# FIND THE BEST MODEL -----------------------------------------------------

# Iterate through each model set, load in models, output autocorrelation plots for step lengths and turning angles, pseudo-residual 
# and qq plots, extract model coefficients for each transition and plot, and paste out AIC table

#################################################For Female
# Specifying output lists of length formula
m.list <- vector(mode = "list", length =length(formula))
out.df <- vector(mode = "list", length =length(formula))

for (i in 1:length(formula)) {
  print(i)
  
  file.in <- paste0("./Data_outputs_infra/", paste0("F_mod_", i, ".rda"))
  load(file = file.in)
  if (i == 1) { m.list[[i]] <- m1_F} else { m.list[[i]] <- model}
  
  ## Extract AIC of model
  if (i == 1) { form_out <- 1} else { form_out <- as.character(formula[[i]])[2]}
  out.df[[i]] <- data.frame(Model = paste0("F_M", i),
                            Formula = form_out, AIC = AIC(m.list[[i]]))
  
  ## Construct autocorrelation plot
  
  pr <- pseudoRes(m.list[[i]])
  
  # Step length
  par(mfrow=c(1,1))
  ylimit <- qnorm((1 + 0.95)/2)/sqrt(length(pr$stepRes[!is.na(pr$stepRes)])) + 1
  acf(pr$stepRes[is.finite(pr$stepRes)],lag.max = 300)
  name.plot <- paste0("./Figures/", paste0("F_mod_", i, "_acf_step.png"))
  dev.copy(png, name.plot,  width = 800, height = 500)
  dev.off()
  
  # Turning angle
  par(mfrow=c(1,1))
  acf(pr$angleRes[!is.na(pr$angleRes)],lag.max = 300)
  name.plot <- paste0("./Figures/", paste0("F_mod_", i, "_acf_angle.png"))
  dev.copy(png, name.plot, width = 800, height = 500)
  dev.off()
  
  # outputting pseudo residual plots
  res.df <- data.frame(stepres = pr$stepRes, angleres = pr$angleRes,
                       sex = m.list[[i]]$data$Sex, ws = m.list[[i]]$data$WindSp, 
                       lod = m.list[[i]]$data$LoD)
  res.df <- subset(res.df, is.finite(stepres))
  
  par(mfrow=c(2,2))
  boxplot(stepres ~ lod, data = res.df)
  plot(stepres ~ ws, data = res.df)
  boxplot(angleres ~ lod, data = res.df)
  plot(angleres ~ ws, data = res.df)
  name.plot <- paste0("./Figures/", paste0("F_mod_", i, "_pseudo-residuals.png"))
  dev.copy(png, name.plot, width = 800, height = 500)
  dev.off()
  
  # outputting qq plots
  plotPR(m.list[[i]])
  name.plot <- paste0("./Figures/", paste0("F_mod_", i, "_acf_qq.png"))
  dev.copy(png, name.plot, width = 800, height = 500)
  dev.off() 
  
  # extracting and plotting model coefficients for each transition
  beta.full <- CIbeta(m.list[[i]])$beta
  beta.full.est <- as.data.frame(beta.full$est)
  beta.full.upr <- as.data.frame(beta.full$upper)
  beta.full.lwr <- as.data.frame(beta.full$lower)
  beta.df <- data.frame(Est = c(beta.full.est$`1 -> 2`, beta.full.est$`1 -> 3`,
                                beta.full.est$`2 -> 1`, beta.full.est$`2 -> 3`,
                                beta.full.est$`3 -> 1`, beta.full.est$`3 -> 2`), 
                        Upr = c(beta.full.upr$`1 -> 2`, beta.full.upr$`1 -> 3`,
                                beta.full.upr$`2 -> 1`, beta.full.upr$`2 -> 3`,
                                beta.full.upr$`3 -> 1`, beta.full.upr$`3 -> 2`), 
                        Lwr = c(beta.full.lwr$`1 -> 2`, beta.full.lwr$`1 -> 3`,
                                beta.full.lwr$`2 -> 1`, beta.full.lwr$`2 -> 3`,
                                beta.full.lwr$`3 -> 1`, beta.full.lwr$`3 -> 2`), 
                        Transitions = rep(colnames(beta.full.est), each = nrow(beta.full.est)),
                        Covariates = rep(rownames(beta.full.est), 3))
  beta.df$Covariates <- as.factor(as.character(beta.df$Covariates))
  beta.df$Transitions <- as.factor(as.character(beta.df$Transitions))
  pd <- position_dodge(width=0.7)
  
  ## Removing intercept to plot
  beta.df2 <- subset(beta.df, Covariates != "(Intercept)",)
  
  pl <- ggplot(beta.df2, aes(Covariates, Est)) + geom_hline(yintercept=0, linetype="dashed", size=1)+
    geom_point(aes(colour = Transitions),position=pd)+
    geom_errorbar(aes(ymin=Lwr, ymax=Upr, colour = Transitions), width=.8, position=pd) +theme_bw()
  print(pl)
  name.plot <- paste0("./Figures/", paste0("F_mod_", i, "_coefficients.png"))
  dev.copy(png, name.plot, width = 800, height = 500)
  dev.off()
}

# Warning messages appear ("removed containing missing values") due to upper and lower CIs which are sometimes "NA"

all.out <- do.call(rbind, out.df)
all.out <- all.out[order(all.out$AIC),]

# Print out AIC table
out.path <- "./Data_outputs_infra/AIC_table_F.csv"
write.csv(all.out, out.path, row.names=T)

###############################################For Male
# Specifying output lists of length formula
m.list <- vector(mode = "list", length =length(formula))
out.df <- vector(mode = "list", length =length(formula))

for (i in 1:length(formula)) {
  print(i)
  
  file.in <- paste0("./Data_outputs/", paste0("F_mod_", i, ".rda"))
  load(file = file.in)
  if (i == 1) { m.list[[i]] <- m1_F} else { m.list[[i]] <- model}
  
  ## Extract AIC of model
  if (i == 1) { form_out <- 1} else { form_out <- as.character(formula[[i]])[2]}
  out.df[[i]] <- data.frame(Model = paste0("F_M", i),
                            Formula = form_out, AIC = AIC(m.list[[i]]))
  
  ## Construct autocorrelation plot
  
  pr <- pseudoRes(m.list[[i]])
  
  # Step length
  par(mfrow=c(1,1))
  ylimit <- qnorm((1 + 0.95)/2)/sqrt(length(pr$stepRes[!is.na(pr$stepRes)])) + 1
  acf(pr$stepRes[is.finite(pr$stepRes)],lag.max = 300)
  name.plot <- paste0("./Figures/", paste0("F_mod_", i, "_acf_step.png"))
  dev.copy(png, name.plot,  width = 800, height = 500)
  dev.off()
  
  # Turning angle
  par(mfrow=c(1,1))
  acf(pr$angleRes[!is.na(pr$angleRes)],lag.max = 300)
  name.plot <- paste0("./Figures/", paste0("F_mod_", i, "_acf_angle.png"))
  dev.copy(png, name.plot, width = 800, height = 500)
  dev.off()
  
  # outputting pseudo residual plots
  res.df <- data.frame(stepres = pr$stepRes, angleres = pr$angleRes,
                       sex = m.list[[i]]$data$Sex, ws = m.list[[i]]$data$WindSp, 
                       lod = m.list[[i]]$data$LoD)
  res.df <- subset(res.df, is.finite(stepres))
  
  par(mfrow=c(2,2))
  boxplot(stepres ~ lod, data = res.df)
  plot(stepres ~ ws, data = res.df)
  boxplot(angleres ~ lod, data = res.df)
  plot(angleres ~ ws, data = res.df)
  name.plot <- paste0("./Figures/", paste0("F_mod_", i, "_pseudo-residuals.png"))
  dev.copy(png, name.plot, width = 800, height = 500)
  dev.off()
  
  # outputting qq plots
  plotPR(m.list[[i]])
  name.plot <- paste0("./Figures/", paste0("F_mod_", i, "_acf_qq.png"))
  dev.copy(png, name.plot, width = 800, height = 500)
  dev.off() 
  
  # extracting and plotting model coefficients for each transition
  beta.full <- CIbeta(m.list[[i]])$beta
  beta.full.est <- as.data.frame(beta.full$est)
  beta.full.upr <- as.data.frame(beta.full$upper)
  beta.full.lwr <- as.data.frame(beta.full$lower)
  beta.df <- data.frame(Est = c(beta.full.est$`1 -> 2`, beta.full.est$`1 -> 3`,
                                beta.full.est$`2 -> 1`, beta.full.est$`2 -> 3`,
                                beta.full.est$`3 -> 1`, beta.full.est$`3 -> 2`), 
                        Upr = c(beta.full.upr$`1 -> 2`, beta.full.upr$`1 -> 3`,
                                beta.full.upr$`2 -> 1`, beta.full.upr$`2 -> 3`,
                                beta.full.upr$`3 -> 1`, beta.full.upr$`3 -> 2`), 
                        Lwr = c(beta.full.lwr$`1 -> 2`, beta.full.lwr$`1 -> 3`,
                                beta.full.lwr$`2 -> 1`, beta.full.lwr$`2 -> 3`,
                                beta.full.lwr$`3 -> 1`, beta.full.lwr$`3 -> 2`), 
                        Transitions = rep(colnames(beta.full.est), each = nrow(beta.full.est)),
                        Covariates = rep(rownames(beta.full.est), 3))
  beta.df$Covariates <- as.factor(as.character(beta.df$Covariates))
  beta.df$Transitions <- as.factor(as.character(beta.df$Transitions))
  pd <- position_dodge(width=0.7)
  
  ## Removing intercept to plot
  beta.df2 <- subset(beta.df, Covariates != "(Intercept)",)
  
  pl <- ggplot(beta.df2, aes(Covariates, Est)) + geom_hline(yintercept=0, linetype="dashed", size=1)+
    geom_point(aes(colour = Transitions),position=pd)+
    geom_errorbar(aes(ymin=Lwr, ymax=Upr, colour = Transitions), width=.8, position=pd) +theme_bw()
  print(pl)
  name.plot <- paste0("./Figures/", paste0("F_mod_", i, "_coefficients.png"))
  dev.copy(png, name.plot, width = 800, height = 500)
  dev.off()
}

# Warning messages appear ("removed containing missing values") due to upper and lower CIs which are sometimes "NA"

all.out <- do.call(rbind, out.df)
all.out <- all.out[order(all.out$AIC),]

# Print out AIC table
out.path <- "./Data_outputs_infra/AIC_table_F.csv"
write.csv(all.out, out.path, row.names=T)



