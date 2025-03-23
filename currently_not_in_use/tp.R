### CITATION: This code is modified from scripts shared in Clay et al. 2020, J. Anim. Ecol.

### AIM: Plot predicted transition probabilities for wind speeds, directions and boldness

# PREAMBLE --------------------------------------------------------------

library(ggplot2); library(momentuHMM); library(dplyr)


### Create outputs and figures folders if they don't currently exist
#out.path <- "./Data_outputs/"
#if(dir.exists(out.path) == FALSE){
#  dir.create(out.path)
#}

#figures.path <- "./Figures/"
#if(dir.exists(figures.path) == FALSE){
#  dir.create(figures.path)
#}
#Clear environment and set working directory
rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")
## LOAD MODELS

# F best
file.in <- paste0("./Data_outputs/", paste0("F_mod_", 4, ".rda"))
load(file = file.in)
f.best.mod <- model

# M best
file.in <- paste0("./Data_outputs/", paste0("M_mod_", 3, ".rda"))
load(file = file.in)
m.best.mod <- model



# GET TRANSITION ESTIMATES BY SPEED FOR AVERAGE WIND DIRECTION -------------------------------------------

## plots are for either with or without Oil platform around
## Transition predictions are made separately for each sex and for the extremes of moon fraction score

### FEMALES

# Get alan*moon combinations
cov.alan <- seq(from = min(f.best.mod$data$rad), max(f.best.mod$data$rad), by = 0.2)
cov.moon <- c(min(f.best.mod$data$fraction), max(f.best.mod$data$fraction))
cov.comb <- expand.grid(cov.alan, cov.moon)

# Make the dataframe for predictions
cov.f_noOil <- data.frame(alan=cov.comb$Var1, moon = cov.comb$Var2, Oil = "No")
cov.f_noOil$Oil <- as.factor(cov.f_speed$Oil)

head(cov.f_speed)


# CIreal only allows single row of covariates, so this function iterates through range of covariate values 
# and outputs list of predicted transition probabilities and upper and lower CIs 
# (this takes around 20 minutes to run)

tock <- Sys.time()
ci.list_F <- lapply(1:nrow(cov.f_noOil), function(x) {
  print(x)
  cov.sub.df <- cov.f_noOil[x,]
  return(CIreal(f.best.mod,covs=cov.sub.df)$gamma)
})
tick <- Sys.time()
tick-tock

save(ci.list_F, file = "Data_outputs/female_noOiltransition_CIs.R")
load("Data_outputs/female_noOiltransition_CIs.R")

# Extract means, upper and lower bounds and put into separate lists
means.f_noOil <- lapply(ci.list_F, '[[', 1) 
lb.f_noOil <- lapply(ci.list_F,'[[',3)
ub.f_noOil <- lapply(ci.list_F,'[[',4)



### MALES

# Get all alan*moon combinations
cov.alan <- seq(from = min(m.best.mod$data$rad), max(m.best.mod$data$rad), by = 0.2)
cov.moon <- c(min(m.best.mod$data$fraction), max(m.best.mod$data$fraction))
cov.comb <- expand.grid(cov.alan, cov.moon)

# Make the dataframe for predictions
cov.m_noOil <- data.frame(alan=cov.comb$Var1, moon = cov.comb$Var2, Oil = "No")
cov.m_noOil$Oil <- as.factor(cov.f_speed$Oil)

#cov.m_speed$WindSp <- as.numeric(as.character(cov.m_speed$WindSp)) Do I need this?
head(cov.m_noOil)


# CIreal only allows single row of covariates, so this function iterates through range of covariate values 
# and outputs list of predicted transition probabilities and upper and lower CIs 
# (this takes around 20 minutes to run)

tock <- Sys.time()
ci.list_M <- lapply(1:nrow(cov.m_noOil), function(x) {
  print(x)
  cov.sub.df <- cov.m_noOil[x,]
  return(CIreal(m.best.mod,covs=cov.sub.df)$gamma)
})
tick <- Sys.time()
tick-tock

save(ci.list_M, file = "Data_outputs/male_speedtransition_CIs.R")
load("Data_outputs/male_speedtransition_CIs.R")

# Extract means, upper and lower bounds and put into separate lists
means.m_speed <- lapply(ci.list_M, '[[', 1) 
lb.m_speed <- lapply(ci.list_M,'[[',3)
ub.m_speed <- lapply(ci.list_M,'[[',4)


# FIGURE 2: PLOT TRANSITIONS ESTIMATES BY SPEED FOR AVERAGE WIND DIRECTION --------------------------------------------

shy_col <- "#00DD2F"
bold_col <- "purple"

behaviour <- data.frame(state = c("Directed travel", "Search", "Rest"), filename = c("travel", "search", "rest"))

nb_states = 3
labels <- c(F = "Female", M = "Male")

for (i in 1:3) {
  
  for (j in 1:3) {
    
    state1 <- i; print(i)
    state2 <- j; print(j) 
    
    ### MALE COEFFECIENTS
    
    m.means <- unlist(lapply(means.m_speed,'[[',nb_states*(state2-1)+state1)) 
    m.lb <- unlist(lapply(lb.m_speed,'[[',nb_states*(state2-1)+state1))
    m.ub <- unlist(lapply(ub.m_speed,'[[',nb_states*(state2-1)+state1))
    df.m <- data.frame(sex = "M", pers = cov.m_speed$mean_BLUP_logit, 
                       wind=cov.m_speed$WindSp, mean = m.means, lower_bound = m.lb, upper_bound = m.ub)
    df.m$persCat <- ifelse(df.m$pers == unique(cov.m_speed$mean_BLUP_logit)[1], "shy", "bold")
    
    ### FEMALE COEFFICIENTS
    
    f.means <- unlist(lapply(means.f_speed,'[[',nb_states*(state2-1)+state1)) 
    f.lb <- unlist(lapply(lb.f_speed,'[[',nb_states*(state2-1)+state1))
    f.ub <- unlist(lapply(ub.f_speed,'[[',nb_states*(state2-1)+state1))
    df.f <- data.frame(sex = "F", pers = cov.f_speed$mean_BLUP_logit, 
                       wind=cov.f_speed$WindSp, mean = f.means, lower_bound = f.lb, upper_bound = f.ub)
    df.f$persCat <- ifelse(df.f$pers == unique(cov.f_speed$mean_BLUP_logit)[1], "shy", "bold")
    
    ### COMBINE DATAFRAMES
    
    all.df <- rbind(df.m, df.f)
    all.df$sex <- as.factor(as.character(all.df$sex))
    all.df$persCat <- as.factor(as.character(all.df$persCat))
    
    
    ### BUILD PLOT
    
    dirPlot <- ggplot(all.df, aes(x = wind, y=mean)) + facet_wrap(~sex, labeller=labeller(sex=labels)) +
      geom_ribbon(size = 1, linetype = "blank", aes(ymin=lower_bound, ymax=upper_bound, col = persCat), alpha=0.15) + 
      geom_line(size = 1, aes(col = persCat)) + 
      theme_bw() + ylab("Transition probability") +
      scale_x_continuous(limits=c(0, 23)) +
      #scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
      xlab("Wind speed (ms-1)") +
      theme(axis.text.x=element_text(size=15), 
            axis.text.y=element_text(size=15), 
            axis.title.x=element_text(size = 16),
            axis.title.y=element_text(size = 16),
            strip.text.x = element_text(size = 16),
            strip.placement = "outside",
            strip.background = element_blank(),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      scale_color_manual(name = "", labels = c("Bold", "Shy"), values = c(bold_col, shy_col)) +
      ggtitle(paste0(behaviour$state[i], " - > ", behaviour$state[j]))
    
    # Scale y as 0.5 - 1 for same behaviour -> same behaviour; scale 0 - 0.5 for same -> different 
    if (i == j) { dirPlot <- dirPlot + scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(0.5, 1)) } else {
      dirPlot <- dirPlot + scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(0, 0.5))  }
    
    assign(paste0("speedPlot_", i, "_", j), dirPlot)
    
    png(paste0("Figures/Transition Estimates/SPEED_", behaviour$filename[i], "_", behaviour$filename[j],".png"), 
        width = 9, height = 7, units = "in", res = 350)
    print(dirPlot)
    dev.off()
    
  }
  
}



# GET TRANSITION ESTIMATES BY alan FOR Oil/no Oil -------------------------
#moon对标wind direction; oil对标personality
## Transitions calculated for each of crosswind (90 degrees to trajectory), headwind (0 degrees),
## tailwind (180 degrees)
## Transition predictions are made separately for each sex and for the extremes of boldness score ('shy' and 'bold')

### FEMALES 

# Get all personality * wind combinations
cov.alan <- seq(from = min(f.best.mod$data$rad_log_5), max(f.best.mod$data$rad_log_5), by = 0.2)
cov.moon <- c(0.1)
cov.Oil <- c("No","Yes")
cov.comb <- expand.grid(cov.alan, cov.moon, cov.Oil)

# Construct dataset for predictions
cov.f <- data.frame(rad_log_5=cov.comb$Var1, fraction = cov.comb$Var2,
                          Oil = cov.comb$Var3)
cov.f$Oil <- as.factor(cov.f$Oil)
#cov.f$alan <- as.numeric(as.character(cov.f_speed$WindSp))
#cov.f$moon <- as.numeric(as.character(cov.f_speed$WindDir))
head(cov.f)

getTrProbs(cov.f,nbStates = 3,beta=f.best.mod$mle$beta,formula=f.best.mod$conditions$formula)

# Output list of predicted transition probabilities and upper and lower CIs 
tock <- Sys.time()
ci.list_F_bydir <- lapply(1:nrow(cov.f), function(x) {
  print(x)
  cov.sub.df <- cov.f[x,]
  return(CIreal(f.best.mod,covs=cov.sub.df)$gamma)
})
tick <- Sys.time()
tick-tock

save(ci.list_F_bydir, file = "Data_outputs/female_CIs.Rdata")
load("Data_outputs/female_CIs.Rdata")

# Extract means, upper and lower bounds and put into separate lists
means.f <- lapply(ci.list_F_bydir, '[[', 1) 
lb.f <- lapply(ci.list_F_bydir,'[[',3)
ub.f <- lapply(ci.list_F_bydir,'[[',4)


# MALES #

# Get all personality * wind combinations
cov.alan <- seq(from = min(m.best.mod$data$rad_log_5), max(m.best.mod$data$rad_log_5), by = 0.2)
cov.moon <- c(0.1)
cov.Oil <- c("No","Yes")
cov.comb <- expand.grid(cov.alan, cov.moon, cov.Oil)

# Construct dataset for predictions
cov.m <- data.frame(alan=cov.comb$Var1, moon = cov.comb$Var2,
                    Oil = cov.comb$Var3)
cov.m$Oil <- as.factor(cov.m$Oil)
#cov.f$alan <- as.numeric(as.character(cov.f_speed$WindSp))
#cov.f$moon <- as.numeric(as.character(cov.f_speed$WindDir))
head(cov.m)


# Output list of predicted transition probabilities and upper and lower CIs 
tock <- Sys.time()
ci.list_M_bydir <- lapply(1:nrow(cov.m), function(x) {
  print(x)
  cov.sub.df <- cov.m[x,]
  return(CIreal(m.best.mod,covs=cov.sub.df)$gamma)
})
tick <- Sys.time()
tick-tock

save(ci.list_M_bydir, file = "Data_outputs/male_CIs.Rdata")
load("Data_outputs/male_CIs.Rdata")


# extract means, upper and lower bounds and put into separate lists
means.m <- lapply(ci.list_M_bydir, '[[', 1) 
lb.m <- lapply(ci.list_M_bydir,'[[',3)
ub.m <- lapply(ci.list_M_bydir,'[[',4)






# PLOT TRANSITIONS ESTIMATES BY SPEED FOR CROSS/HEAD/TAILWIND (FIGURES S1-S3)  --------------------------------------------

shy_col <- "#00DD2F"
bold_col <- "purple"
mid_col <- "#a099a6"

behaviour <- data.frame(state = c("Raft", "Forage", "Travel"), filename = c("Raft", "Forage", "Travel"))

nb_states = 3
labels <- c(F = "Female",M = "Male")

for(k in 1:3){
  
  for (i in 1:3) {
    
    for (j in 1:3) {
      
      alan <- k; print(k)
      
      state1 <- i; print(i)
      state2 <- j; print(j) 
      
      ### MALE COEFFECIENTS
      m.means <- unlist(lapply(means.m,'[[',nb_states*(state2-1)+state1)) 
      m.lb <- unlist(lapply(lb.m,'[[',nb_states*(state2-1)+state1))
      m.ub <- unlist(lapply(ub.m,'[[',nb_states*(state2-1)+state1))
      df.m <- data.frame(sex = "M", moon = cov.m$moon, Oil = cov.m$Oil,
                         alan=cov.m$alan, mean = m.means, lower_bound = m.lb, upper_bound = m.ub)
      df.m$mooncat <- ifelse(df.m$moon == 0.1, "New", "Half")
      df.m$mooncat <- ifelse(df.m$moon == 0.9, "Full", as.character(df.m$mooncat))
      
      ### FEMALE COEFFICIENTS
      f.means <- unlist(lapply(means.f,'[[',nb_states*(state2-1)+state1)) 
      f.lb <- unlist(lapply(lb.f,'[[',nb_states*(state2-1)+state1))
      f.ub <- unlist(lapply(ub.f,'[[',nb_states*(state2-1)+state1))
      df.f <- data.frame(sex = "M", moon = cov.f$moon, Oil = cov.f$Oil,
                         alan=cov.f$alan, mean = f.means, lower_bound = f.lb, upper_bound = f.ub)
      df.f$mooncat <- ifelse(df.f$moon == 0.1, "New", "Half")
      df.f$mooncat <- ifelse(df.f$moon == 0.9, "Full", as.character(df.m$mooncat))
      
      ### COMBINE DATAFRAMES
      
      all.df <- rbind(df.m, df.f)
      all.df$sex <- as.factor(as.character(all.df$sex))
      all.df$Oil <- as.factor(as.character(all.df$Oil))
      all.df <- subset(all.df, mooncat == unique(all.df$mooncat)[k])
      
      ### BUILD PLOT
      
      alanPlot <- ggplot(all.df, aes(x = alan, y=mean)) + facet_wrap(~sex, labeller=labeller(sex=labels)) +
        geom_ribbon(size = 1, linetype = "blank", aes(ymin=lower_bound, ymax=upper_bound, col = Oil), alpha=0.15) + 
        geom_line(size = 1, aes(col = Oil, linetype = Oil)) + 
        theme_bw() + ylab("Transition probability") +
        xlab("Night Light Level (log transformed) ") +
        theme(axis.text.x=element_text(size=15), 
              axis.title.x=element_text(size = 16),
              axis.text.y=element_text(size=15), 
              axis.title.y=element_text(size = 16),
              strip.text.x = element_text(size = 16),
              strip.placement = "outside",
              strip.background = element_blank(),
              plot.title = element_text(hjust = 0.5),
              legend.position = "none") +
        scale_linetype_manual(name = "Presence of Oil Platform", values=c("solid", "dashed"), labels = c("No", "Yes")) +
        scale_color_manual(name = "Presence of Oil Platform", labels = c("No", "Yes"), values = c(bold_col, shy_col)) +
        ggtitle(paste0(behaviour$state[i], " - > ", behaviour$state[j]))
      
      assign(paste0("tpPlot_", i, "_", j), alanPlot)
      
      png(paste0("Figures/alan", behaviour$filename[i], " to ", behaviour$filename[j], " in ", unique(all.df$mooncat), "alan.png"), 
          width = 9, height = 7, units = "in", res = 350)
      print(alanPlot)
      dev.off()
      
    }
    
  }
  
}




