#install.packages("devtools")
#devtools::install_github("rmendels/xtractomatic")
library(xtractomatic)
library(tidyverse)
library(oce)
library(lubridate)
library(gganimate)
library(sf)
## zonal component
wind_x = xtracto_3D(dtype = "qsux101day", 
                    xpos = c(94, 159), 
                    ypos = c(-15, 42.8), 
                    tpos = c("2023-01-01", "2023-01-01"))
# meridional component
wind_y = xtracto_3D(dtype = "qsuy101day", 
                    xpos = c(94, 159), 
                    ypos = c(-15, 42.8), 
                    tpos = c("2023-01-01", "2023-01-01"))
## Extract
longitude = wind_x$longitude
latitude = wind_x$latitude
time = wind_x$time %>% as.Date()
# eastward velocity (zonal)
u = wind_x$data
# northward velocity (meridional)
v = wind_y$data

# calculate wind velocity
velocity = sqrt(u^2 + v^2)
n.lon = length(longitude)
n.lat = length(latitude)+1

u.all = NULL

## zonal from array to data frame

for (i in 1:length(time)){
  u.df = data.frame(longitude, u[,,i] %>% as.data.frame()) %>% 
    gather(key = "key" , value = "u", 2:n.lat) %>% 
    mutate(latitude = rep(latitude, each = n.lon), date = time[i]) %>% 
    select(date,longitude, latitude, u)%>% 
    as_tibble()
  
  u.all = u.all %>% bind_rows(u.df)
}

## zonal from array to data frame
v.all = NULL

for (i in 1:length(time)){
  v.df = data.frame(longitude, v[,,i] %>% as.data.frame()) %>% 
    gather(key = "key" , value = "v", 2:n.lat) %>% 
    mutate(latitude = rep(latitude, each = n.lon), date = time[i]) %>% 
    select(date,longitude, latitude, v)%>% 
    as_tibble()
  
  v.all = v.all %>% bind_rows(v.df)
}


## velocity from array to data frame
velocity.all = NULL

for (i in 1:length(time)){
  velocity.df = data.frame(longitude, velocity[,,i] %>% as.data.frame()) %>% 
    gather(key = "key" , value = "velocity", 2:n.lat) %>% 
    mutate(latitude = rep(latitude, each = n.lon), date = time[i]) %>% 
    select(date,longitude, latitude, velocity)%>% 
    as_tibble()
  
  velocity.all = velocity.all %>% bind_rows(velocity.df)
}
wind = data.frame(u.all, v.all$v, velocity.all$velocity) %>%
  mutate(day = yday(date) %>%as.integer(), 
         week = week(date) %>%as.integer(),  month = month(date) %>%as.integer(), 
         year = year(date) %>%as.integer()) %>%
  select(date,day, week, month, year,longitude,latitude, u,
         v = v.all.v, velocity = velocity.all.velocity)
wind.month = wind %>% 
  group_by(longitude, latitude, month) %>% 
  summarise(u = median(u, na.rm = TRUE),
            v = median(v, na.rm = TRUE), 
            velocity = median(velocity, na.rm = TRUE))

wind.vector = ggplot() +
  geom_raster(data = wind.month, aes(x = longitude, y = latitude, fill = velocity))+
  geom_segment(data = wind.month, 
               aes(x = longitude, xend = longitude+u/60, y = latitude, 
                   yend = latitude+v/60), arrow = arrow(length = unit(0.25, "cm")))+
  geom_sf(data = tz.ke, fill = "grey85", col = 1)+
  coord_sf(xlim = c(38.7, 40), ylim =  c(-6.7, -4.7))+
  scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,12), 
                       na.value = "white", name = "Speed\n (m/s)")+
  scale_x_continuous(breaks = c(38.8,40))+
  theme_bw()+
  theme(axis.text = element_text(size = 14, colour = 1),
        legend.text = element_text(size = 14, colour = 1), 
        legend.title = element_text(size = 14, colour = 1),
        legend.position.inside = c(.12,.17),
        legend.background = element_rect(colour = 1, fill = "white"))+
  labs(x = NULL, y = NULL, title = "Month of : {frame_time}")+
  transition_time(month) +
  ease_aes("linear")

animate(wind.vector)
