library(ncdf4)
library(dplyr)

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
  cloud_cover <- ncvar_get(nc_file, "tcc")  # Replace "tcc" with the variable name in your .nc file
  
  # Close the NetCDF file
  nc_close(nc_file)
  
  # Combine into a dataframe
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
