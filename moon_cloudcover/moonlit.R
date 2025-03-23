library(lubridate)
library(moonlit)
library(lutz)

df <- read.csv("C:/alan_streakedshearwater/Rawdata_movebank/bird_interp20.csv")
df$t<-as.POSIXlt(df$t,tz="UTC")
df$tz<-tz_lookup_coords(df$lat,df$long,method = "accurate")
#  Determines timezone from Latitude and Longitude
table(df$tz)
unique(df$tz)
Asia_Ban<-filter(df,tz=="Asia/Bangkok")
Asia_Ban$t_local<-as.POSIXct(Asia_Ban$t,tz="Asia/Bangkok")
Asia_Banr<-calculateMoonlightIntensity(Asia_Ban$lat,Asia_Ban$long,Asia_Ban$t_local,e=0.28)
Asia_Ban$illumination<-Asia_Banr$illumination
Asia_Ban$moonlight<-Asia_Banr$moonlightModel
Asia_Ban$twilight<-Asia_Banr$twilightModel
Asia_Ban$night<-Asia_Banr$night
Asia_Ban<-Asia_Ban[,-1]
Asia_Ban<-Asia_Ban[,-7]
write.csv(Asia_Banr, file="C:/ALAN_StreakedShearwater/Rawdata_interp_env/moonlit_raw/Asia_Ban.csv")

Asia_Hoc<-filter(df,tz=="Asia/Ho_Chi_Minh")
Asia_Hoc$t_local<-as.POSIXct(Asia_Hoc$t,tz="Asia/Ho_Chi_Minh")
Asia_Hocr<-calculateMoonlightIntensity(Asia_Hoc$lat,Asia_Hoc$long,Asia_Hoc$t_local,e=0.28)
Asia_Hoc$illumination<-Asia_Hocr$illumination
Asia_Hoc$moonlight<-Asia_Hocr$moonlightModel
Asia_Hoc$twilight<-Asia_Hocr$twilightModel
Asia_Hoc$night<-Asia_Hocr$night
Asia_Hoc<-Asia_Hoc[,-1]
Asia_Hoc<-Asia_Hoc[,-7]
write.csv(Asia_Hocr, file="C:/ALAN_StreakedShearwater/Rawdata_interp_env/moonlit_raw/Asia_Hoc.csv")

Asia_HK<-filter(df,tz=="Asia/Hong_Kong")
Asia_HK$t_local<-as.POSIXct(Asia_HK$t,tz="Asia/Hong_Kong")
Asia_HKr<-calculateMoonlightIntensity(Asia_HK$lat,Asia_HK$long,Asia_HK$t_local,e=0.28)
Asia_HK$illumination<-Asia_HKr$illumination
Asia_HK$moonlight<-Asia_HKr$moonlightModel
Asia_HK$twilight<-Asia_HKr$twilightModel
Asia_HK$night<-Asia_HKr$night
Asia_HK<-Asia_HK[,-1]
Asia_HK<-Asia_HK[,-7]
write.csv(Asia_HKr, file="C:/ALAN_StreakedShearwater/Rawdata_interp_env/moonlit_raw/Asia_HK.csv")

Asia_Jak<-filter(df,tz=="Asia/Jakarta")
Asia_Jak$t_local<-as.POSIXct(Asia_Jak$t,tz="Asia/Jakarta")
Asia_Jakr<-calculateMoonlightIntensity(Asia_Jak$lat,Asia_Jak$long,Asia_Jak$t_local,e=0.28)
Asia_Jak$illumination<-Asia_Jakr$illumination
Asia_Jak$moonlight<-Asia_Jakr$moonlightModel
Asia_Jak$twilight<-Asia_Jakr$twilightModel
Asia_Jak$night<-Asia_Jakr$night
Asia_Jak<-Asia_Jak[,-1]
Asia_Jak<-Asia_Jak[,-7]
write.csv(Asia_Jakr, file="C:/ALAN_StreakedShearwater/Rawdata_interp_env/moonlit_raw/Asia_Jak.csv")

Asia_Kua<-filter(df,tz=="Asia/Kuala_Lumpur")
Asia_Kua$t_local<-as.POSIXct(Asia_Kua$t,tz="Asia/Kuala_Lumpur")
Asia_Kuar<-calculateMoonlightIntensity(Asia_Kua$lat,Asia_Kua$long,Asia_Kua$t_local,e=0.28)
Asia_Kua$illumination<-Asia_Kuar$illumination
Asia_Kua$moonlight<-Asia_Kuar$moonlightModel
Asia_Kua$twilight<-Asia_Kuar$twilightModel
Asia_Kua$night<-Asia_Kuar$night
Asia_Kua<-Asia_Kua[,-1]
Asia_Kua<-Asia_Kua[,-7]
write.csv(Asia_Kuar, file="C:/ALAN_StreakedShearwater/Rawdata_interp_env/moonlit_raw/Asia_Kua.csv")

Asia_Kuc<-filter(df,tz=="Asia/Kuching")
Asia_Kuc$t_local<-as.POSIXct(Asia_Kuc$t,tz="Asia/Kuching")
Asia_Kucr<-calculateMoonlightIntensity(Asia_Kuc$lat,Asia_Kuc$long,Asia_Kuc$t_local,e=0.28)
Asia_Kuc$illumination<-Asia_Kucr$illumination
Asia_Kuc$moonlight<-Asia_Kucr$moonlightModel
Asia_Kuc$twilight<-Asia_Kucr$twilightModel
Asia_Kuc$night<-Asia_Kucr$night
Asia_Kuc<-Asia_Kuc[,-1]
Asia_Kuc<-Asia_Kuc[,-7]
write.csv(Asia_Kucr, file="C:/ALAN_StreakedShearwater/Rawdata_interp_env/moonlit_raw/Asia_Kuc.csv")

Asia_Mak<-filter(df,tz=="Asia/Makassar")
Asia_Mak$t_local<-as.POSIXct(Asia_Mak$t,tz="Asia/Makassar")
Asia_Makr<-calculateMoonlightIntensity(Asia_Mak$lat,Asia_Mak$long,Asia_Mak$t_local,e=0.28)
Asia_Mak$illumination<-Asia_Makr$illumination
Asia_Mak$moonlight<-Asia_Makr$moonlightModel
Asia_Mak$twilight<-Asia_Makr$twilightModel
Asia_Mak$night<-Asia_Makr$night
Asia_Mak<-Asia_Mak[,-1]
Asia_Mak<-Asia_Mak[,-7]
write.csv(Asia_Makr, file="C:/ALAN_StreakedShearwater/Rawdata_interp_env/moonlit_raw/Asia_Mak.csv")

Asia_Man<-filter(df,tz=="Asia/Manila")
Asia_Man$t_local<-as.POSIXct(Asia_Man$t,tz="Asia/Manila")
Asia_Manr<-calculateMoonlightIntensity(Asia_Man$lat,Asia_Man$long,Asia_Man$t_local,e=0.28)
Asia_Man$illumination<-Asia_Manr$illumination
Asia_Man$moonlight<-Asia_Manr$moonlightModel
Asia_Man$twilight<-Asia_Manr$twilightModel
Asia_Man$night<-Asia_Manr$night
Asia_Man<-Asia_Man[,-1]
Asia_Man<-Asia_Man[,-7]
write.csv(Asia_Manr, file="C:/ALAN_StreakedShearwater/Rawdata_interp_env/moonlit_raw/Asia_Man.csv")

Asia_Pyo<-filter(df,tz=="Asia/Pyongyang")
Asia_Pyo$t_local<-as.POSIXct(Asia_Pyo$t,tz="Asia/Pyongyang")
Asia_Pyor<-calculateMoonlightIntensity(Asia_Pyo$lat,Asia_Pyo$long,Asia_Pyo$t_local,e=0.28)
Asia_Pyo$illumination<-Asia_Pyor$illumination
Asia_Pyo$moonlight<-Asia_Pyor$moonlightModel
Asia_Pyo$twilight<-Asia_Pyor$twilightModel
Asia_Pyo$night<-Asia_Pyor$night
Asia_Pyo<-Asia_Pyo[,-1]
Asia_Pyo<-Asia_Pyo[,-7]
write.csv(Asia_Pyor, file="C:/ALAN_StreakedShearwater/Rawdata_interp_env/moonlit_raw/Asia_Pyo.csv")

Asia_Seo<-filter(df,tz=="Asia/Seoul")
Asia_Seo$t_local<-as.POSIXct(Asia_Seo$t,tz="Asia/Seoul")
Asia_Seor<-calculateMoonlightIntensity(Asia_Seo$lat,Asia_Seo$long,Asia_Seo$t_local,e=0.28)
Asia_Seo$illumination<-Asia_Seor$illumination
Asia_Seo$moonlight<-Asia_Seor$moonlightModel
Asia_Seo$twilight<-Asia_Seor$twilightModel
Asia_Seo$night<-Asia_Seor$night
Asia_Seo<-Asia_Seo[,-1]
Asia_Seo<-Asia_Seo[,-7]
write.csv(Asia_Seor, file="C:/ALAN_StreakedShearwater/Rawdata_interp_env/moonlit_raw/Asia_Seo.csv")

Asia_Sha<-filter(df,tz=="Asia/Shanghai")
Asia_Sha$t_local<-as.POSIXct(Asia_Sha$t,tz="Asia/Shanghai")
Asia_Shar<-calculateMoonlightIntensity(Asia_Sha$lat,Asia_Sha$long,Asia_Sha$t_local,e=0.28)
Asia_Sha$illumination<-Asia_Shar$illumination
Asia_Sha$moonlight<-Asia_Shar$moonlightModel
Asia_Sha$twilight<-Asia_Shar$twilightModel
Asia_Sha$night<-Asia_Shar$night
Asia_Sha<-Asia_Sha[,-1]
Asia_Sha<-Asia_Sha[,-7]
write.csv(Asia_Shar, file="C:/ALAN_StreakedShearwater/Rawdata_interp_env/moonlit_raw/Asia_Sha.csv")

Asia_Sin<-filter(df,tz=="Asia/Singapore")
Asia_Sin$t_local<-as.POSIXct(Asia_Sin$t,tz="Asia/Singapore")
Asia_Sinr<-calculateMoonlightIntensity(Asia_Sin$lat,Asia_Sin$long,Asia_Sin$t_local,e=0.28)
Asia_Sin$illumination<-Asia_Sinr$illumination
Asia_Sin$moonlight<-Asia_Sinr$moonlightModel
Asia_Sin$twilight<-Asia_Sinr$twilightModel
Asia_Sin$night<-Asia_Sinr$night
Asia_Sin<-Asia_Sin[,-1]
Asia_Sin<-Asia_Sin[,-7]
write.csv(Asia_Sinr, file="C:/ALAN_StreakedShearwater/Rawdata_interp_env/moonlit_raw/Asia_Sin.csv")

Asia_Tai<-filter(df,tz=="Asia/Taipei")
Asia_Tai$t_local<-as.POSIXct(Asia_Tai$t,tz="Asia/Taipei")
Asia_Tair<-calculateMoonlightIntensity(Asia_Tai$lat,Asia_Tai$long,Asia_Tai$t_local,e=0.28)
Asia_Tai$illumination<-Asia_Tair$illumination
Asia_Tai$moonlight<-Asia_Tair$moonlightModel
Asia_Tai$twilight<-Asia_Tair$twilightModel
Asia_Tai$night<-Asia_Tair$night
Asia_Tai<-Asia_Tai[,-1]
Asia_Tai<-Asia_Tai[,-7]
write.csv(Asia_Tair, file="C:/ALAN_StreakedShearwater/Rawdata_interp_env/moonlit_raw/Asia_Tai.csv")

Asia_Tok<-filter(df,tz=="Asia/Tokyo")
Asia_Tok$t_local<-as.POSIXct(Asia_Tok$t,tz="Asia/Tokyo")
Asia_Tokr<-calculateMoonlightIntensity(Asia_Tok$lat,Asia_Tok$long,Asia_Tok$t_local,e=0.28)
Asia_Tok$illumination<-Asia_Tokr$illumination
Asia_Tok$moonlight<-Asia_Tokr$moonlightModel
Asia_Tok$twilight<-Asia_Tokr$twilightModel
Asia_Tok$night<-Asia_Tokr$night
Asia_Tok<-Asia_Tok[,-1]
Asia_Tok<-Asia_Tok[,-7]
write.csv(Asia_Tokr, file="C:/ALAN_StreakedShearwater/Rawdata_interp_env/moonlit_raw/Asia_Tok.csv")

EG7<-filter(df,tz=="Etc/GMT-7")
EG7$t_local<-as.POSIXct(EG7$t,tz="Etc/GMT-7")
EG7r<-calculateMoonlightIntensity(EG7$lat,EG7$long,EG7$t_local,e=0.28)
EG7$illumination<-EG7r$illumination
EG7$moonlight<-EG7r$moonlightModel
EG7$twilight<-EG7r$twilightModel
EG7$night<-EG7r$night
EG7<-EG7[,-1]
EG7<-EG7[,-7]
write.csv(EG7r, file="C:/ALAN_StreakedShearwater/Rawdata_interp_env/moonlit_raw/EG7.csv")

EG8<-filter(df,tz=="Etc/GMT-8")
EG8$t_local<-as.POSIXct(EG8$t,tz="Etc/GMT-8")
EG8r<-calculateMoonlightIntensity(EG8$lat,EG8$long,EG8$t_local,e=0.28)
EG8$illumination<-EG8r$illumination
EG8$moonlight<-EG8r$moonlightModel
EG8$twilight<-EG8r$twilightModel
EG8$night<-EG8r$night
EG8<-EG8[,-1]
EG8<-EG8[,-7]
write.csv(EG8r, file="C:/ALAN_StreakedShearwater/Rawdata_interp_env/moonlit_raw/EG8.csv")

EG9<-filter(df,tz=="Etc/GMT-9")
EG9$t_local<-as.POSIXct(EG9$t,tz="Etc/GMT-9")
EG9r<-calculateMoonlightIntensity(EG9$lat,EG9$long,EG9$t_local,e=0.28)
EG9$illumination<-EG9r$illumination
EG9$moonlight<-EG9r$moonlightModel
EG9$twilight<-EG9r$twilightModel
EG9$night<-EG9r$night
EG9<-EG9[,-1]
EG9<-EG9[,-7]
write.csv(EG9r, file="C:/ALAN_StreakedShearwater/Rawdata_interp_env/moonlit_raw/EG9.csv")
########################################################
bird_interp_moonlit<-rbind(Asia_Ban,Asia_Hoc,Asia_HK,Asia_Jak,Asia_Kua,Asia_Kuc,Asia_Mak,
                                  Asia_Man,Asia_Pyo,Asia_Seo,Asia_Sha,Asia_Sin,Asia_Tai,Asia_Tok,EG7,EG8,EG9)
save(bird_interp_moonlit,file="C:/ALAN_StreakedShearwater/Rawdata_interp_env/bird_interp_moonlit.Rdata")
write.csv(bird_interp_moonlit,file="C:/ALAN_StreakedShearwater/Rawdata_interp_env/bird_interp_moonlit.csv")
