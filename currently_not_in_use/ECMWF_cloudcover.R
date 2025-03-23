#install.packages("ecmwfr")
#install.packages("ncdf4")
#Clear environment and set working directory
rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")
library(dplyr)
library(ecmwfr)
library(tidyverse)
#{
#  "url"   : "https://api.ecmwf.int/v1",
#  "key"   : "01b34f4c-b725-4791-9c48-c55cb9276c23",
#  "email" : "yunfengge0805@gmail.com"
#  "userID": "a575df32-b627-4d71-8f9f-42d48c561172"
#}

# Set up the API key in your environment
wf_set_key(
  key = "01b34f4c-b725-4791-9c48-c55cb9276c23",
  user = "a575df32-b627-4d71-8f9f-42d48c561172"
)

# Load the data
load(file = "RawData/sw_NB_22to24_envall_2times.Rdata")
data <- as.data.frame(sw_NB_22to24_envall_2times)

# Extract relevant columns (latitude, longitude, and timestamps)
locations <- data %>%
  select(lat, long, timeUTC)

#process csv file for python tcc extraction in Colab(not useful here)
#gps_data <- data %>%
#  select(lat, long, timeUTC)
#colnames(gps_data)<-c("latitude","longitude","timestamp")
#write.csv(gps_data, "gps_data.csv", row.names = FALSE)
# Create a folder for the cloud cover data
output_dir <- "cloud_cover_data"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  message("Directory created: ", output_dir)
}

setwd("C:/ALAN_StreakedShearwater/cloud_cover_data")

# Define a function to retrieve cloud cover data
fetch_cloud_cover <- function(lat, long, timeUTC,index) {
  # Extract year, month, and day
  year <- substr(timeUTC, 1, 4)
  month <- substr(timeUTC, 6, 7)
  day <- substr(timeUTC, 9, 10)
  
  # Set all hourly times for the request
  time <- paste0(substr(timeUTC, 12, 13), ":00")
  
  # Define a small bounding box around the given latitude and longitude
  area <- c(lat + 0.05, long - 0.05, lat - 0.05, long + 0.05)
  
  # Construct the request
  request <- list(
    dataset_short_name = "reanalysis-era5-single-levels",
    product_type = "reanalysis",
    variable = "total_cloud_cover",
    year = year,
    month = month,
    day = day,
    time = time,  # Include all hours for the given day
    data_format = "netcdf",
    download_format = "unarchived",
    area = area,
    target =  paste0(
      "cloud_cover_",index, ".nc"
    )
  )
  
  # Use tryCatch to handle API request errors gracefully
  result <- tryCatch(
    wf_request(request = request, transfer = TRUE, path = ".",time_out = 43200),
    error = function(e) {
      message("Error for lat: ", lat, ", long: ", long, ", timestamps: ", timeUTC, " - ", e$message)
      return(NULL)
    }
  )
  
  return(result)
}

# Iterate over the locations and timestamps
output_files <- mapply(
  fetch_cloud_cover,
  lat = locations$lat,
  long = locations$long,
  timeUTC = locations$timeUTC,
  index = seq_along(locations$lat)
)

# Print the results
message("Data download completed")

#process downloaded data into dataframe
library(ncdf4)
# Set the working directory containing the .nc files
setwd("C:/ALAN_StreakedShearwater/cloud_cover_data")

# List all .nc files in the directory and sort them numerically
nc_files <- list.files(
  path = "C:/ALAN_StreakedShearwater/cloud_cover_data",
  pattern = "\\.nc$",      # Select only .nc files
  full.names = TRUE        # Include full file path
)

# Sort files numerically by their names
nc_files <- nc_files[order(as.numeric(gsub("\\D", "", basename(nc_files))))]

# Initialize an empty dataframe to store results
cloud_cover_data <- data.frame()

# Function to extract data from a single .nc file
extract_nc_data <- function(file) {
  # Open the NetCDF file
  nc_file <- nc_open(file)
  
  # Extract cloud cover data
  cloud_cover <- ncvar_get(nc_file, "tcc")  # "tcc" is total cloud cover
  
  # Close the NetCDF file
  nc_close(nc_file)
  
  # extract and calculate the mean total cloud cover
  tcc <- as.vector(cloud_cover)
  tcc <- mean(tcc)
  tcc <- as.data.frame(tcc)
  # Add file name as a new column
  tcc$file_name <- basename(file)
  
  return(tcc)
}

# Iterate through all .nc files and combine their data
for (file in nc_files) {
  message("Processing file: ", file)
  file_data <- extract_nc_data(file)
  cloud_cover_data <- bind_rows(cloud_cover_data, file_data)
}

# Print the first few rows of the combined dataframe
head(cloud_cover_data)

# Save the combined dataframe as a CSV for later use
write.csv(cloud_cover_data, "cloud_cover_combined.csv", row.names = FALSE)