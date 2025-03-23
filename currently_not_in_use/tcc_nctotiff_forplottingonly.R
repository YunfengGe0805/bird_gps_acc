library(ncdf4)
library(raster)
library(lubridate) # for easier date/time handling

# 1. Open the NetCDF file
nc_file <- nc_open("C:/alan_streakedshearwater/Total_cloud_cover/2024totalcloudcover.nc") # Use the correct path
print(nc_file)

# 2. Extract the tcc variable
tcc <- ncvar_get(nc_file, "tcc")

# 3. Extract dimensions
latitude <- ncvar_get(nc_file, "latitude")
longitude <- ncvar_get(nc_file, "longitude")
time <- ncvar_get(nc_file, "valid_time")

# 4. Extract the time origin and calculate the dates
time_units <- ncatt_get(nc_file, "valid_time", "units")$value
time_origin <- strsplit(time_units, "seconds since ")[[1]][2]
date_times <- as.POSIXct(time, origin = time_origin, tz = "UTC")

# 5. Close the NetCDF file
nc_close(nc_file)

# 6. Loop through each time step and create a raster and save as TIFF
output_dir <- "C:/alan_streakedshearwater/Total_cloud_cover/tcc_tiff_files" # Directory to save the TIFF files
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

for (i in 1:length(time)) {
  # Extract the tcc data for the current time step
  tcc_slice <- tcc[,,i] # Assuming dimensions are lon, lat, time
  
  # Create a raster object
  r <- raster(tcc_slice,
              xmn = min(longitude), xmx = max(longitude),
              ymn = min(latitude), ymx = max(latitude),
              crs = "+proj=longlat +datum=WGS84 +no_defs") #Define the CRS
  
  # Rotate if necessary
  if (min(longitude) > max(longitude)) {
    r <- rotate(r)
  }
  
  # Create a filename for the TIFF file
  file_name <- paste0(output_dir, "/tcc_", format(date_times[i], "%Y%m%d_%H%M"), ".tif")
  
  # Save the raster as a TIFF file
  writeRaster(r, filename = file_name, format = "GTiff", overwrite = TRUE)
  
  print(paste("Saved:", file_name))
}

print("Finished processing all time steps.")
