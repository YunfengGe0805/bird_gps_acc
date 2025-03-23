#install.packages('track2KBA')
library(track2KBA)
library(ctmm)
df<-boobies
df$timestamp<-as.POSIXct(paste0(df$date_gmt," ",df$time_gmt),tz="UTC",formats=c("%Y-%m-%d %H:%M:%OS"))
head(df)
Boobies<-as.telemetry(df)
head(Boobies)
names(Boobies)
###########################
# Home-range meta-analysis
###########################

#help("meta")

FITS <- list()
for(i in 1:length(Boobies))
{
  GUESS <- ctmm.guess(Boobies[[i]],interactive=FALSE)
  FITS[[i]] <- ctmm.select(Boobies[[i]],GUESS,trace=3)
}
names(FITS) <- names(Boobies)
summary(FITS)
save(FITS,file="C:/Movement Ecology/Boobies/BoobiesFITS.rda")
#load("C:/Movement Ecology/Boobies/BoobiesFITS.rda")

# calculate AKDES on a consistent grid
AKDES <- akde(Boobies,FITS,weights=TRUE)

# color to be spatially distinct
COL <- color(AKDES,by='individual')

# plot AKDEs
plot(AKDES,col.UD=COL,col.level=COL,col.grid=NA,level=NA,main="Boobies AKDEs")

# meta-analysis of Boobies home-range areas
meta(AKDES,col=c(COL,'black'),sort=TRUE)
# model selection: Dirac-delta > inverse-Gaussian
# force inverse-Gaussian population distribution
#meta(AKDES,plot=FALSE,IC=NA)
# since CoV isn't a selected feature, its underestimated here

#more general meta-analytic regressions
#help("Log")
#Log(FITS,variable="area")
#normalizing the data so the data can be applied to other package that only accept normal distribution

#look at individual difference and population mean of home range crossing interval
meta(FITS,variable="tau_position",sort=TRUE)

#########################
# Population density (PKDE)
#########################

# this is a straight mean of the individual densities that doesn't model population variance
#help("mean.UD")
# note the 'sample' argument for correct CIs

# straight mean - for a population of 6 buffalo
MEAN <- mean(AKDES,sample=FALSE)

plot(Boobies,MEAN,col=COL,main="Mean Boobies AKDE")

# this is a population kernel density estimate (paper coming)
#help("pkde")

PKDE <- pkde(Boobies,AKDES)

plot(Boobies,PKDE,col=COL,main="Boobies PKDE")
