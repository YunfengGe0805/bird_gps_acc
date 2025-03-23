my.options<-options(digits.secs = 3)  
library(ggplot2)
library(R.utils)
library(lubridate)
library(readxl)
library(dplyr)
library(tidyverse)
library(tagtools)
library(zoo)
#0 convert to binary-----
# binary to g reference table
dec_bin_g<-as.data.frame(matrix(ncol=5,nrow =  (32768*2)))
colnames(dec_bin_g)<-c("bin15","bin16","bin_1","bin","g")
dec_bin_g$bin<-c(seq(32767,0),seq(65535,32768))
which(dec_bin_g$bin==0)
dec_bin_g$bin16<-intToBin(dec_bin_g$bin)

unique(nchar(dec_bin_g$bin16))
dec_bin_g$bin1<-substr(dec_bin_g$bin16,1,1)
dec_bin_g$bin15<-substr(dec_bin_g$bin16,2,16)
dec_bin_g$g<-seq(1.99994,-2,length=nrow(dec_bin_g))

head(dec_bin_g)
tail(dec_bin_g)
dec_bin_g[32766:32770,]

temp.function<-function(x){
  x$Time<-as.POSIXct(x$Time,tz="UTC",formats=c("%Y-%m-%d %H:%M:%OS"))
  x<-x[order(x$Time),]
  x$Time.round<-lubridate::round_date(x$Time,unit="20 minutes")
  
  tmp<-dec_bin_g[,c("bin","g")]
  colnames(tmp)[1]<-"X"
  tmp<-dplyr::left_join(x,tmp,by="X")
  x$X.g<-tmp$g
  
  tmp<-dec_bin_g[,c("bin","g")]
  colnames(tmp)[1]<-"Y"
  tmp<-dplyr::left_join(x,tmp,by="Y")
  x$Y.g<-tmp$g
  
  tmp<-dec_bin_g[,c("bin","g")]
  colnames(tmp)[1]<-"Z"
  tmp<-dplyr::left_join(x,tmp,by="Z")
  x$Z.g<-tmp$g
  
  
  x<-split(x,f=x$Time.round)
  x<-lapply(x, function(y){
    y$timestamp<-paste0(as.character(y$Time),".",seq(0,nrow(y)-1,nrow(y)/10)*100+1)
    y$temp.index<-seq(0,nrow(y)-1,nrow(y)/10)
    y$timestamp[1]<-paste0(y$Time[1],".001")
    
    tmp<-y[,c("ID","IMEID","Time","Time.round","X.g","timestamp","temp.index","X")]
    colnames(tmp)[5]<-"g.value"
    colnames(tmp)[8]<-"value"
    tmp$axis<-"X"
    
    tmp2<-y[,c("ID","IMEID","Time","Time.round","Y.g","timestamp","temp.index","Y")]
    colnames(tmp2)[5]<-"g.value"
    colnames(tmp2)[8]<-"value"
    tmp2$axis<-"Y"
    
    tmp3<-y[,c("ID","IMEID","Time","Time.round","Z.g","timestamp","temp.index","Z")]
    colnames(tmp3)[5]<-"g.value"
    colnames(tmp3)[8]<-"value"
    tmp3$axis<-"Z"
    y<-rbind(tmp,tmp2,tmp3)
    
    return(y)
  })
  
  return(x)
}

data<-read.csv(file="C:/ALAN_StreakedShearwater/acc/M140/M140_202301filter.csv")
data<-temp.function(data)
x<-data[[1]]
data<-lapply(data, function(x){
  x$time_tmp<- c(seq(1:(nrow(x)/3)),seq(1:(nrow(x)/3)),seq(1:(nrow(x)/3)))
  return(x)
})

#ggplot(data = data[[8]], mapping = aes(time_tmp/10, y = g.value,group=axis,col=axis)) + geom_line()+ggtitle ("2021-08-31 04:00:01-04:00:07") +xlab("时间(s)")+ylab("加速度（g）")+labs(colour="轴")+coord_cartesian(ylim = c(-2, 2))+theme_classic()


# Extract the relevant columns from each data frame and combine them

df_list <- lapply(data, function(df) {
  df %>% 
    dplyr::select(ID, Time, `Time.round`, g.value, axis)
})

# Combine all data frames into a single data frame
final_df <- bind_rows(df_list)

# Print the first few rows of the resulting data frame
head(final_df)

reformat_dataframe <- function(df) {
  df <- df %>%
    pivot_wider(names_from = axis, values_from = `g.value`, values_fill = 0)
  
  return(df)
}

new_df <- reformat_dataframe(final_df)
new_df$Z<-new_df$Z*(-1)
new_df$X<-new_df$X*(-1)
###############################################################
#calculate pitch and roll
prdata<-as.matrix(new_df[,4:6])
prdata<-na.approx(prdata)
prlist<-a2pr(prdata)
prlist<-as.data.frame(prlist)
new_df$p<-prlist$p
new_df$r<-prlist$r

#calculate VEDBA
vedba<-odba(prdata, 10,0.5,"vedba",2)
new_df$vedba<-vedba

# Group by the rounded timestamp and calculate mean and variance of X
result <- new_df %>%
  group_by(Time.round) %>%
  summarise(
    mean_X = mean(X, na.rm = TRUE),
    sd_X = sd(X, na.rm = TRUE),
    mean_Y = mean(Y, na.rm = TRUE),
    sd_Y = sd(Y, na.rm = TRUE),
    mean_Z = mean(Z, na.rm = TRUE),
    sd_Z = sd(Z, na.rm = TRUE),
    mean_p = mean(p, na.rm = TRUE),
    sd_p = sd(p, na.rm = TRUE),
    mean_r = mean(r, na.rm = TRUE),
    sd_r = sd(r, na.rm = TRUE),
    mean_vedba = mean(vedba, na.rm = TRUE),
    sd_vedba = sd(vedba, na.rm = TRUE)
  )
colnames(result)[1]<-"t"

#save the file
save(result,file="C:/ALAN_StreakedShearwater/acc/M140/M140202301metrics.Rdata")
