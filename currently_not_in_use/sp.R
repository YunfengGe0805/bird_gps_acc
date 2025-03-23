### CITATION: This code is modified from scripts shared in Clay et al. 2020, J. Anim. Ecol.

### AIM: lot predicted stationary probabilities for a range of specified covariates 
#Clear environment and set working directory
rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")

# PREAMBLE ----------------------------------------------------------------

library(ggplot2); library(momentuHMM)


# LOAD MODEL OUTPUTS ------------------------------------------------------

# load in best models for males and females (based on AIC table)
file.in <- paste0("./Data_outputs/", paste0("F_mod_", 4, ".rda"))
load(file = file.in)
f.mod <- model

file.in <- paste0("./Data_outputs/", paste0("M_mod_", 3, ".rda"))
load(file = file.in)
m.mod <- model

f.sp<-stationary(f.mod,f.mod$data)
f.sp<-as.data.frame(f.sp)
f.sp.F<-data.frame(f.sp$Forage)%>%
  mutate(ALAN=f.mod$data$rad_log_5,
         moon=f.mod$data$fraction,
         Oil=f.mod$data$Oil,
         sex=f.mod$data$sex)
f_sp<-cbind(f.sp.F,f.sp$Raft,f.sp$Travel)

m.sp<-stationary(m.mod,m.mod$data)
m.sp<-as.data.frame(m.sp)
m.sp.F<-data.frame(m.sp$Forage)%>%
  mutate(ALAN=m.mod$data$rad_log_5,
         moon=m.mod$data$fraction,
         Oil=m.mod$data$Oil,
         sex=m.mod$data$sex)
m_sp<-cbind(m.sp.F,m.sp$Raft,m.sp$Travel)
######################################################################################################
ci.stationary <- function(model, cov, alpha) {
  gridLength <- nrow(cov)
  model2 <- momentuHMM:::delta_bc(model) # extended format allowing for backwards compatibility
  nbStates <- length(model2$stateNames)
  Sigma <- model2$mod$Sigma
  
  lci <- matrix(NA,gridLength,nbStates)
  uci <- matrix(NA,gridLength,nbStates)
  
  formula <- model2$conditions$formula # for covariates
  newForm <- momentuHMM:::newFormulas(formula, nbStates) # a formula for each state transition
  newformula <- newForm$newformula
  
  nbCovs <- ncol(model.matrix(newformula, model2$data)) - 1 # model.matrix gives values of covariate terms of the formula for each observation
  # in my opinion, nbCovs gives exactly the same number of terms as formula (-1 is minus intercept)
  
  gamInd <- (length(model2$mod$estimate) - (nbCovs + 1) * nbStates * (nbStates - 1) 
             + 1):(length(model2$mod$estimate)) - ncol(model2$covsDelta) * (nbStates - 1) * (!model2$conditions$stationary) # if the mode is stationary there wouldn't be any covariates
  # here we just enumerate parameters leaving out the first ones which are step and angle related, and the last ones which are delta related
  
  rawCovs <- model2$rawCovs
  # changing order of variables tomatch with raw data
  rawCovs <- rawCovs[,order(names(rawCovs))]
  tempCovs <- cov
  tempCovs <- tempCovs[,sort(names(rawCovs))]
  
  # fixing format problems
  for (i in which(unlist(lapply(rawCovs, is.factor)))) {
    tempCovs[[i]] <- factor(tempCovs[[i]], levels = levels(rawCovs[, 
                                                                   i])) 
  }
  tmpSplineInputs <- momentuHMM:::getSplineFormula(newformula, model2$data, 
                                                   tempCovs) # just a format for spline use later
  desMat <- model.matrix(tmpSplineInputs$formula, data = tmpSplineInputs$covs) # tmpSplineInputs$covs is tempCovs
  # model.matrix gives the design matrix for the formula in the argument
  
  probs <- as.data.frame(stationary(model2, covs=desMat)) # the stationary probability is computed based on desMat, which has only a range of values for one of the covariates
  
  for(state in 1:nbStates) {
    dN <- t(apply(desMat, 1, function(x)
      numDeriv::grad(momentuHMM:::get_stat,model2$mod$estimate[gamInd][unique(c(model2$conditions$betaCons))],covs=matrix(x,nrow=1),nbStates=nbStates,i=state,betaRef=model2$conditions$betaRef,betaCons=model2$conditions$betaCons,workBounds=model2$conditions$workBounds$beta)))
    
    se <- t(apply(dN, 1, function(x)
      suppressWarnings(sqrt(x%*%Sigma[gamInd[unique(c(model2$conditions$betaCons))],gamInd[unique(c(model2$conditions$betaCons))]]%*%x))))
    
    lci[,state] <- 1/(1 + exp(-(log(probs[,state]/(1-probs[,state])) -
                                  qnorm(1-(1-alpha)/2) * (1/(probs[,state]-probs[,state]^2)) * se)))
    uci[,state] <- 1/(1 + exp(-(log(probs[,state]/(1-probs[,state])) +
                                  qnorm(1-(1-alpha)/2) * (1/(probs[,state]-probs[,state]^2)) * se)))
  }
  lci <- as.data.frame(lci)
  names(lci) <- model$stateNames
  uci <- as.data.frame(uci)
  names(uci) <- model$stateNames
  ci.list <- list(lci, uci)
  names(ci.list) <- c("lower", "upper")
  return(ci.list)
}


# Create confidence intervals around stationary probabilities
ci.f <- ci.stationary(model = f.mod, cov = f.mod$data, alpha = 0.95) 
ci.m <- ci.stationary(model = m.mod, cov = f.mod$data, alpha = 0.95) 


# Combine into dataframe for plotting
prob_cov.f <- cbind(s.f, cov.f)
prob_cov.m <- cbind(s.m, cov.m)

prob.f <- tidyr::gather(prob_cov.f, state, prob, travel:rest, factor_key=TRUE)
prob.m <- tidyr::gather(prob_cov.m, state, prob, travel:rest, factor_key=TRUE)

stat.df <- rbind(prob.f, prob.m)

lwr.f <- tidyr::gather(stat.f[[1]], state, lwr, travel:rest, factor_key = T)$lwr
lwr.m <- tidyr::gather(stat.m[[1]], state, lwr, travel:rest, factor_key = T)$lwr

lwr <- c(lwr.f, lwr.m)

upr.f <- tidyr::gather(stat.f[[2]], state, lwr, travel:rest, factor_key = T)$lwr
upr.m <- tidyr::gather(stat.m[[2]], state, lwr, travel:rest, factor_key = T)$lwr

upr <- c(upr.f, upr.m)

stat.df$lwr <- lwr
stat.df$upr <- upr
stat.df$sex <- c(rep("F", nrow(prob.f)), rep("M", nrow(prob.m)))
stat.df$pers <- ifelse(stat.df$mean_BLUP_logit < -1.5, "shy", "bold")

stat.df[,c(9,10)] <- lapply(stat.df[,c(9,10)], as.factor)

stat.df$pers_state <- as.factor(as.character(paste(stat.df$pers, stat.df$state, sep = "_")))

save(stat.df, file = "Data_outputs/stationary_probabilities.Rdata")
load("Data_outputs/stationary_probabilities.Rdata")



# FIGURE S4: PLOT STATIONARY PROBABILITIES BY SPEED FOR TAILWIND/CROSSWIND/HEADWIND ----------

shy_col <- "#00DD2F"
bold_col <- "purple"

stat.df$sex <- factor(stat.df$sex, labels = c("Female", "Male"))
dirs <- unique(stat.df$WindDir)
dirLabels <- c("tail", "cross", "head")

for (i in 1:length(dirs)){
  
  plot.df <- subset(stat.df, WindDir == dirs[i])
  
  myplot <- ggplot(plot.df, aes(WindSp, prob)) + facet_wrap(.~sex, labeller=label_value) + 
    geom_ribbon(size = 1.3, aes(ymin=lwr, ymax=upr, fill = pers_state), alpha=0.15)+
    scale_fill_manual(values = c("grey0", "grey0", "grey0", "grey30", "grey30", "grey30", "grey60", "grey60", "grey60")) +
    geom_line(aes(colour = pers, linetype = state), size = 1.3) + 
    scale_linetype_manual(values=c("solid", "dotted", "dashed"),labels = c("Travel", "Search", "Rest"), name = "Behaviour") +
    ylim(0, 1) +
    scale_color_manual(name = "", labels = c("Bold", "Shy"), values = c(bold_col, shy_col)) +
    theme_bw() + ylab("Stationary probability")+
    scale_x_continuous(limits=c(0, 23)) + xlab(expression(paste("Wind speed (", ms^-1, ")", sep="")))+
    theme(axis.text.x=element_text(size=16), 
          axis.text.y=element_text(size=16), 
          axis.title.x=element_text(size=18),
          axis.title.y=element_blank(),
          legend.position = "none",
          strip.placement = "outside",
          strip.background = element_blank(),
          strip.text.x = element_text(size = 16)) +
    guides(fill = FALSE)
  
  png(paste0("Figures/Stationary estimates/stationary_transitions_", dirLabels[i], "wind.png"), width = 9, height = 7, units = "in", res = 600)
  print(myplot)
  dev.off()
  
}