# Ensure we have a clean environment to avoid variable conflicts
rm(list = ls())

# Install and load required libraries
library(ncdf4)
library(dplyr)

# Set the working directory containing the .nc files
setwd("C:/ALAN_StreakedShearwater/cloud_cover_data")

# List all .nc files in the directory
nc_files <- list.files(pattern = "\\.nc$")

# Initialize an empty dataframe to store results
cloud_cover_data <- data.frame()

# Function to extract data from a single .nc file
extract_nc_data <- function(file) {
  # Open the NetCDF file
  nc_data <- nc_open(file)  # Avoid shadowing 'nc'
  
  # Extract latitude, longitude, time, and cloud cover data
  lat <- ncvar_get(nc_data, "latitude")
  long <- ncvar_get(nc_data, "longitude")
  
  # Extract cloud cover data
  cloud_cover <- ncvar_get(nc_data, "tcc")  # Replace "tcc" with the actual variable name
  
  # Close the NetCDF file
  nc_close(nc_data)
  
  # Combine into a dataframe
  data <- expand.grid(lat = lat, long = long)
  data$cloud_cover <- as.vector(cloud_cover)
  
  return(data)
}

# Iterate through all .nc files and combine their data
for (file in nc_files) {
  message("Processing file: ", file)
  file_data <- tryCatch(
    {
      extract_nc_data(file)
    },
    error = function(e) {
      message("Error processing file: ", file, " - ", e$message)
      return(NULL)
    }
  )
  if (!is.null(file_data)) {
    cloud_cover_data <- bind_rows(cloud_cover_data, file_data)
  }
}

# Print the first few rows of the combined dataframe
head(cloud_cover_data)

# Save the combined dataframe as a CSV for later use
write.csv(cloud_cover_data, "cloud_cover_combined.csv", row.names = FALSE)
