# Load necessary libraries
library(ncdf4)
library(dplyr)
library(lubridate)
library(akima)

# 1. Load your GPS data
gps_data <- read.csv("C:/alan_streakedshearwater/Rawdata_movebank/birddf.csv")

# 2. Load ERA5 cloud cover data (.nc file)
# Replace "path/to/your/era5_data.nc" with the actual path to your .nc file
nc_file <- nc_open("C:/alan_streakedshearwater/Total_cloud_cover/2024totalcloudcover.nc")

# 3. Extract cloud cover data and relevant dimensions
cloud_cover <- ncvar_get(nc_file, "tcc") # Assuming "tcc" is the variable name for total cloud cover
lat <- ncvar_get(nc_file, "latitude")
lon <- ncvar_get(nc_file, "longitude")

time <- ncvar_get(nc_file, "valid_time")
time_units <- ncatt_get(nc_file, "valid_time", "units")$value
time_origin <- strsplit(time_units, "seconds since ")[[1]][2]
date_times <- as.POSIXct(time, origin = time_origin, tz = "UTC")
nc_close(nc_file)

# 4. Function to find the nearest cloud cover value for a given GPS point and timestamp using bilinear interpolation
get_cloud_cover <- function(gps_time, gps_lat, gps_lon) {
  # Find the nearest time index
  time_diff <- abs(difftime(gps_time, date_times, units = "hours"))
  time_index <- which.min(time_diff)
  
  # Extract the cloud cover data for the nearest time
  cloud_cover_slice <- cloud_cover[, , time_index]
  
  # Perform bilinear interpolation
  interpolated_value <- interp(x = lon, y = lat, z = cloud_cover_slice, 
                               xo = gps_lon, yo = gps_lat, 
                               extrap = TRUE)
  
  return(interpolated_value$z)
}

# 5. Apply the function to your GPS data
gps_data$cloud_cover <- mapply(get_cloud_cover, gps_data$t, gps_data$lat, gps_data$long)

