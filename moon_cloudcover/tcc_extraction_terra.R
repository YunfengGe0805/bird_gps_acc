library(terra)
library(tidyverse)
library(lubridate)
library(ncdf4)

###########202301
nc_file <- "C:/alan_streakedshearwater/lcc_bymonth/lcc_2023_01.nc"
tcc_rast <- rast(nc_file)

nc <- nc_open(nc_file)
time <- ncvar_get(nc, "valid_time")
time_units <- ncatt_get(nc, "valid_time", "units")$value
time_origin <- strsplit(time_units, "seconds since ")[[1]][2]
time(tcc_rast) <- as.POSIXct(time, origin = time_origin, tz = "UTC")
nc_close(nc)

# Load GPS data
data <- read.csv("C:/alan_streakedshearwater/Rawdata_interp_env/bird_interp_moonlit.csv")
data$t<-as.POSIXlt(data$t,tz="UTC")
location<-filter(data,t >= ymd_hms("2023-01-01 00:00:00") & t <=ymd_hms ("2023-01-31 23:59:59"))
location<-filter(location,ID=="SYSUL140")
# Convert timestamps to POSIXct format for comparison
location$t <- as.POSIXct(location$t, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extract cloud cover values at corresponding timestamps
results <- location %>%
  rowwise() %>% # Process each row individually
  mutate(
    lcc = {
      # Find the closest time in the NetCDF file
      nc_time <- time(tcc_rast)
      closest_time <- nc_time[which.min(abs(nc_time - t))]
      
      # Subset raster to the closest time slice
      tcc_slice <- tcc_rast[[which(nc_time == closest_time)]]
      
      # Perform bilinear extraction for this specific time slice
      terra::extract(tcc_slice, cbind(long, lat), method = "bilinear")[1,1]
    }
  ) %>%
  ungroup()
results<-results[,-1]
write.csv(results,file="C:/ALAN_StreakedShearwater/Rawdata_interp_env/M140_interp_moonlit_lcc_202301.csv")
###########202302
nc_file <- "C:/alan_streakedshearwater/tcc_bymonth/cloud_cover_2023_02.nc"
tcc_rast <- rast(nc_file)

nc <- nc_open(nc_file)
time <- ncvar_get(nc, "valid_time")
time_units <- ncatt_get(nc, "valid_time", "units")$value
time_origin <- strsplit(time_units, "seconds since ")[[1]][2]
time(tcc_rast) <- as.POSIXct(time, origin = time_origin, tz = "UTC")
nc_close(nc)

# Load GPS data
location<-filter(data,t >= ymd_hms("2023-02-01 00:00:00") & t <=ymd_hms ("2023-02-31 23:59:59"))

# Convert timestamps to POSIXct format for comparison
location$t <- as.POSIXct(location$t, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extract cloud cover values at corresponding timestamps
results <- location %>%
  rowwise() %>% # Process each row individually
  mutate(
    cloud_cover = {
      # Find the closest time in the NetCDF file
      nc_time <- time(tcc_rast)
      closest_time <- nc_time[which.min(abs(nc_time - t))]
      
      # Subset raster to the closest time slice
      tcc_slice <- tcc_rast[[which(nc_time == closest_time)]]
      
      # Perform bilinear extraction for this specific time slice
      terra::extract(tcc_slice, cbind(long, lat), method = "bilinear")[1,1]
    }
  ) %>%
  ungroup()
###########202301
nc_file <- "C:/alan_streakedshearwater/tcc_bymonth/cloud_cover_2023_01.nc"
tcc_rast <- rast(nc_file)

nc <- nc_open(nc_file)
time <- ncvar_get(nc, "valid_time")
time_units <- ncatt_get(nc, "valid_time", "units")$value
time_origin <- strsplit(time_units, "seconds since ")[[1]][2]
time(tcc_rast) <- as.POSIXct(time, origin = time_origin, tz = "UTC")
nc_close(nc)

# Load GPS data
data <- read.csv("C:/alan_streakedshearwater/Rawdata_interp_env/bird_interp_moonlit.csv")
data$t<-as.POSIXlt(data$t,tz="UTC")
location<-filter(data,t >= ymd_hms("2023-01-01 00:00:00") & t <=ymd_hms ("2023-01-31 23:59:59"))

# Convert timestamps to POSIXct format for comparison
location$t <- as.POSIXct(location$t, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extract cloud cover values at corresponding timestamps
results <- location %>%
  rowwise() %>% # Process each row individually
  mutate(
    cloud_cover = {
      # Find the closest time in the NetCDF file
      nc_time <- time(tcc_rast)
      closest_time <- nc_time[which.min(abs(nc_time - t))]
      
      # Subset raster to the closest time slice
      tcc_slice <- tcc_rast[[which(nc_time == closest_time)]]
      
      # Perform bilinear extraction for this specific time slice
      terra::extract(tcc_slice, cbind(long, lat), method = "bilinear")[1,1]
    }
  ) %>%
  ungroup()
###########202301
nc_file <- "C:/alan_streakedshearwater/tcc_bymonth/cloud_cover_2023_01.nc"
tcc_rast <- rast(nc_file)

nc <- nc_open(nc_file)
time <- ncvar_get(nc, "valid_time")
time_units <- ncatt_get(nc, "valid_time", "units")$value
time_origin <- strsplit(time_units, "seconds since ")[[1]][2]
time(tcc_rast) <- as.POSIXct(time, origin = time_origin, tz = "UTC")
nc_close(nc)

# Load GPS data
data <- read.csv("C:/alan_streakedshearwater/Rawdata_interp_env/bird_interp_moonlit.csv")
data$t<-as.POSIXlt(data$t,tz="UTC")
location<-filter(data,t >= ymd_hms("2023-01-01 00:00:00") & t <=ymd_hms ("2023-01-31 23:59:59"))

# Convert timestamps to POSIXct format for comparison
location$t <- as.POSIXct(location$t, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extract cloud cover values at corresponding timestamps
results <- location %>%
  rowwise() %>% # Process each row individually
  mutate(
    cloud_cover = {
      # Find the closest time in the NetCDF file
      nc_time <- time(tcc_rast)
      closest_time <- nc_time[which.min(abs(nc_time - t))]
      
      # Subset raster to the closest time slice
      tcc_slice <- tcc_rast[[which(nc_time == closest_time)]]
      
      # Perform bilinear extraction for this specific time slice
      terra::extract(tcc_slice, cbind(long, lat), method = "bilinear")[1,1]
    }
  ) %>%
  ungroup()
###########202301
nc_file <- "C:/alan_streakedshearwater/tcc_bymonth/cloud_cover_2023_01.nc"
tcc_rast <- rast(nc_file)

nc <- nc_open(nc_file)
time <- ncvar_get(nc, "valid_time")
time_units <- ncatt_get(nc, "valid_time", "units")$value
time_origin <- strsplit(time_units, "seconds since ")[[1]][2]
time(tcc_rast) <- as.POSIXct(time, origin = time_origin, tz = "UTC")
nc_close(nc)

# Load GPS data
data <- read.csv("C:/alan_streakedshearwater/Rawdata_interp_env/bird_interp_moonlit.csv")
data$t<-as.POSIXlt(data$t,tz="UTC")
location<-filter(data,t >= ymd_hms("2023-01-01 00:00:00") & t <=ymd_hms ("2023-01-31 23:59:59"))

# Convert timestamps to POSIXct format for comparison
location$t <- as.POSIXct(location$t, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extract cloud cover values at corresponding timestamps
results <- location %>%
  rowwise() %>% # Process each row individually
  mutate(
    cloud_cover = {
      # Find the closest time in the NetCDF file
      nc_time <- time(tcc_rast)
      closest_time <- nc_time[which.min(abs(nc_time - t))]
      
      # Subset raster to the closest time slice
      tcc_slice <- tcc_rast[[which(nc_time == closest_time)]]
      
      # Perform bilinear extraction for this specific time slice
      terra::extract(tcc_slice, cbind(long, lat), method = "bilinear")[1,1]
    }
  ) %>%
  ungroup()
###########202301
nc_file <- "C:/alan_streakedshearwater/tcc_bymonth/cloud_cover_2023_01.nc"
tcc_rast <- rast(nc_file)

nc <- nc_open(nc_file)
time <- ncvar_get(nc, "valid_time")
time_units <- ncatt_get(nc, "valid_time", "units")$value
time_origin <- strsplit(time_units, "seconds since ")[[1]][2]
time(tcc_rast) <- as.POSIXct(time, origin = time_origin, tz = "UTC")
nc_close(nc)

# Load GPS data
data <- read.csv("C:/alan_streakedshearwater/Rawdata_interp_env/bird_interp_moonlit.csv")
data$t<-as.POSIXlt(data$t,tz="UTC")
location<-filter(data,t >= ymd_hms("2023-01-01 00:00:00") & t <=ymd_hms ("2023-01-31 23:59:59"))

# Convert timestamps to POSIXct format for comparison
location$t <- as.POSIXct(location$t, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extract cloud cover values at corresponding timestamps
results <- location %>%
  rowwise() %>% # Process each row individually
  mutate(
    cloud_cover = {
      # Find the closest time in the NetCDF file
      nc_time <- time(tcc_rast)
      closest_time <- nc_time[which.min(abs(nc_time - t))]
      
      # Subset raster to the closest time slice
      tcc_slice <- tcc_rast[[which(nc_time == closest_time)]]
      
      # Perform bilinear extraction for this specific time slice
      terra::extract(tcc_slice, cbind(long, lat), method = "bilinear")[1,1]
    }
  ) %>%
  ungroup()
###########202301
nc_file <- "C:/alan_streakedshearwater/tcc_bymonth/cloud_cover_2023_01.nc"
tcc_rast <- rast(nc_file)

nc <- nc_open(nc_file)
time <- ncvar_get(nc, "valid_time")
time_units <- ncatt_get(nc, "valid_time", "units")$value
time_origin <- strsplit(time_units, "seconds since ")[[1]][2]
time(tcc_rast) <- as.POSIXct(time, origin = time_origin, tz = "UTC")
nc_close(nc)

# Load GPS data
data <- read.csv("C:/alan_streakedshearwater/Rawdata_interp_env/bird_interp_moonlit.csv")
data$t<-as.POSIXlt(data$t,tz="UTC")
location<-filter(data,t >= ymd_hms("2023-01-01 00:00:00") & t <=ymd_hms ("2023-01-31 23:59:59"))

# Convert timestamps to POSIXct format for comparison
location$t <- as.POSIXct(location$t, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extract cloud cover values at corresponding timestamps
results <- location %>%
  rowwise() %>% # Process each row individually
  mutate(
    cloud_cover = {
      # Find the closest time in the NetCDF file
      nc_time <- time(tcc_rast)
      closest_time <- nc_time[which.min(abs(nc_time - t))]
      
      # Subset raster to the closest time slice
      tcc_slice <- tcc_rast[[which(nc_time == closest_time)]]
      
      # Perform bilinear extraction for this specific time slice
      terra::extract(tcc_slice, cbind(long, lat), method = "bilinear")[1,1]
    }
  ) %>%
  ungroup()
###########202301
nc_file <- "C:/alan_streakedshearwater/tcc_bymonth/cloud_cover_2023_01.nc"
tcc_rast <- rast(nc_file)

nc <- nc_open(nc_file)
time <- ncvar_get(nc, "valid_time")
time_units <- ncatt_get(nc, "valid_time", "units")$value
time_origin <- strsplit(time_units, "seconds since ")[[1]][2]
time(tcc_rast) <- as.POSIXct(time, origin = time_origin, tz = "UTC")
nc_close(nc)

# Load GPS data
data <- read.csv("C:/alan_streakedshearwater/Rawdata_interp_env/bird_interp_moonlit.csv")
data$t<-as.POSIXlt(data$t,tz="UTC")
location<-filter(data,t >= ymd_hms("2023-01-01 00:00:00") & t <=ymd_hms ("2023-01-31 23:59:59"))

# Convert timestamps to POSIXct format for comparison
location$t <- as.POSIXct(location$t, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extract cloud cover values at corresponding timestamps
results <- location %>%
  rowwise() %>% # Process each row individually
  mutate(
    cloud_cover = {
      # Find the closest time in the NetCDF file
      nc_time <- time(tcc_rast)
      closest_time <- nc_time[which.min(abs(nc_time - t))]
      
      # Subset raster to the closest time slice
      tcc_slice <- tcc_rast[[which(nc_time == closest_time)]]
      
      # Perform bilinear extraction for this specific time slice
      terra::extract(tcc_slice, cbind(long, lat), method = "bilinear")[1,1]
    }
  ) %>%
  ungroup()
###########202301
nc_file <- "C:/alan_streakedshearwater/tcc_bymonth/cloud_cover_2023_01.nc"
tcc_rast <- rast(nc_file)

nc <- nc_open(nc_file)
time <- ncvar_get(nc, "valid_time")
time_units <- ncatt_get(nc, "valid_time", "units")$value
time_origin <- strsplit(time_units, "seconds since ")[[1]][2]
time(tcc_rast) <- as.POSIXct(time, origin = time_origin, tz = "UTC")
nc_close(nc)

# Load GPS data
data <- read.csv("C:/alan_streakedshearwater/Rawdata_interp_env/bird_interp_moonlit.csv")
data$t<-as.POSIXlt(data$t,tz="UTC")
location<-filter(data,t >= ymd_hms("2023-01-01 00:00:00") & t <=ymd_hms ("2023-01-31 23:59:59"))

# Convert timestamps to POSIXct format for comparison
location$t <- as.POSIXct(location$t, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extract cloud cover values at corresponding timestamps
results <- location %>%
  rowwise() %>% # Process each row individually
  mutate(
    cloud_cover = {
      # Find the closest time in the NetCDF file
      nc_time <- time(tcc_rast)
      closest_time <- nc_time[which.min(abs(nc_time - t))]
      
      # Subset raster to the closest time slice
      tcc_slice <- tcc_rast[[which(nc_time == closest_time)]]
      
      # Perform bilinear extraction for this specific time slice
      terra::extract(tcc_slice, cbind(long, lat), method = "bilinear")[1,1]
    }
  ) %>%
  ungroup()
###########202301
nc_file <- "C:/alan_streakedshearwater/tcc_bymonth/cloud_cover_2023_01.nc"
tcc_rast <- rast(nc_file)

nc <- nc_open(nc_file)
time <- ncvar_get(nc, "valid_time")
time_units <- ncatt_get(nc, "valid_time", "units")$value
time_origin <- strsplit(time_units, "seconds since ")[[1]][2]
time(tcc_rast) <- as.POSIXct(time, origin = time_origin, tz = "UTC")
nc_close(nc)

# Load GPS data
data <- read.csv("C:/alan_streakedshearwater/Rawdata_interp_env/bird_interp_moonlit.csv")
data$t<-as.POSIXlt(data$t,tz="UTC")
location<-filter(data,t >= ymd_hms("2023-01-01 00:00:00") & t <=ymd_hms ("2023-01-31 23:59:59"))

# Convert timestamps to POSIXct format for comparison
location$t <- as.POSIXct(location$t, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extract cloud cover values at corresponding timestamps
results <- location %>%
  rowwise() %>% # Process each row individually
  mutate(
    cloud_cover = {
      # Find the closest time in the NetCDF file
      nc_time <- time(tcc_rast)
      closest_time <- nc_time[which.min(abs(nc_time - t))]
      
      # Subset raster to the closest time slice
      tcc_slice <- tcc_rast[[which(nc_time == closest_time)]]
      
      # Perform bilinear extraction for this specific time slice
      terra::extract(tcc_slice, cbind(long, lat), method = "bilinear")[1,1]
    }
  ) %>%
  ungroup()
###########202301
nc_file <- "C:/alan_streakedshearwater/tcc_bymonth/cloud_cover_2023_01.nc"
tcc_rast <- rast(nc_file)

nc <- nc_open(nc_file)
time <- ncvar_get(nc, "valid_time")
time_units <- ncatt_get(nc, "valid_time", "units")$value
time_origin <- strsplit(time_units, "seconds since ")[[1]][2]
time(tcc_rast) <- as.POSIXct(time, origin = time_origin, tz = "UTC")
nc_close(nc)

# Load GPS data
data <- read.csv("C:/alan_streakedshearwater/Rawdata_interp_env/bird_interp_moonlit.csv")
data$t<-as.POSIXlt(data$t,tz="UTC")
location<-filter(data,t >= ymd_hms("2023-01-01 00:00:00") & t <=ymd_hms ("2023-01-31 23:59:59"))

# Convert timestamps to POSIXct format for comparison
location$t <- as.POSIXct(location$t, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extract cloud cover values at corresponding timestamps
results <- location %>%
  rowwise() %>% # Process each row individually
  mutate(
    cloud_cover = {
      # Find the closest time in the NetCDF file
      nc_time <- time(tcc_rast)
      closest_time <- nc_time[which.min(abs(nc_time - t))]
      
      # Subset raster to the closest time slice
      tcc_slice <- tcc_rast[[which(nc_time == closest_time)]]
      
      # Perform bilinear extraction for this specific time slice
      terra::extract(tcc_slice, cbind(long, lat), method = "bilinear")[1,1]
    }
  ) %>%
  ungroup()
###########202301
nc_file <- "C:/alan_streakedshearwater/tcc_bymonth/cloud_cover_2023_01.nc"
tcc_rast <- rast(nc_file)

nc <- nc_open(nc_file)
time <- ncvar_get(nc, "valid_time")
time_units <- ncatt_get(nc, "valid_time", "units")$value
time_origin <- strsplit(time_units, "seconds since ")[[1]][2]
time(tcc_rast) <- as.POSIXct(time, origin = time_origin, tz = "UTC")
nc_close(nc)

# Load GPS data
data <- read.csv("C:/alan_streakedshearwater/Rawdata_interp_env/bird_interp_moonlit.csv")
data$t<-as.POSIXlt(data$t,tz="UTC")
location<-filter(data,t >= ymd_hms("2023-01-01 00:00:00") & t <=ymd_hms ("2023-01-31 23:59:59"))

# Convert timestamps to POSIXct format for comparison
location$t <- as.POSIXct(location$t, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extract cloud cover values at corresponding timestamps
results <- location %>%
  rowwise() %>% # Process each row individually
  mutate(
    cloud_cover = {
      # Find the closest time in the NetCDF file
      nc_time <- time(tcc_rast)
      closest_time <- nc_time[which.min(abs(nc_time - t))]
      
      # Subset raster to the closest time slice
      tcc_slice <- tcc_rast[[which(nc_time == closest_time)]]
      
      # Perform bilinear extraction for this specific time slice
      terra::extract(tcc_slice, cbind(long, lat), method = "bilinear")[1,1]
    }
  ) %>%
  ungroup()
