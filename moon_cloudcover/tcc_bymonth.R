library(lubridate)
library(ecmwfr)

# Set working directory for output files
setwd("C:/ALAN_StreakedShearwater/tcc_bymonth")

# Configure ECMWF API access (replace with your credentials)
wf_set_key(
  user = "a575df32-b627-4d71-8f9f-42d48c561172",
  key = "01b34f4c-b725-4791-9c48-c55cb9276c23"
)

download_monthly_data <- function(year, month) {
  # Generate filename with year-month format
  nc_filename <- sprintf("cloud_cover_%d_%02d.nc", year, month)
  
  if (file.exists(nc_filename)) {
    message("Skipping existing file: ", nc_filename)
    return()
  }
  
  # Calculate number of days in month
  target_date <- ymd(paste(year, month, "01", sep = "-"))
  days_in_month <- days_in_month(target_date)
  
  # Build API request parameters[1][4][7]
  request <- list(
    dataset_short_name = "reanalysis-era5-single-levels",
    product_type = "reanalysis",
    variable = "total_cloud_cover",
    year = as.character(year),
    month = formatC(month, width = 2, flag = "0"),
    day = formatC(1:days_in_month, width = 2, flag = "0"),
    time = sprintf("%02d:00", 0:23),  # All hours[7][10]
    area = c(42, 88, -15, 155),       # N/W/S/E coordinates
    data_format = "netcdf",
    target = nc_filename
  )
  
  # Execute request with error handling[1][4]
  tryCatch({
    wf_request(
      request = request,
      transfer = TRUE,
      path = ".",
      time_out = 172800  # 48-hour timeout
    )
    message("Successfully downloaded: ", nc_filename)
  }, error = function(e) {
    message("Failed to download ", nc_filename, ": ", e$message)
  })
}

# Download data for 2021-2024 with nested loops
for (year in 2021:2024) {
  for (month in 1:12) {
    download_monthly_data(year, month)
    Sys.sleep(10)  # Add delay between requests[1][4]
  }
}
###################################################################
#low cloud cover
# Set working directory for output files
setwd("C:/ALAN_StreakedShearwater/lcc_bymonth")

# Configure ECMWF API access (replace with your credentials)
wf_set_key(
  user = "a575df32-b627-4d71-8f9f-42d48c561172",
  key = "01b34f4c-b725-4791-9c48-c55cb9276c23"
)

download_monthly_data <- function(year, month) {
  # Generate filename with year-month format
  nc_filename <- sprintf("lcc_%d_%02d.nc", year, month)
  
  if (file.exists(nc_filename)) {
    message("Skipping existing file: ", nc_filename)
    return()
  }
  
  # Calculate number of days in month
  target_date <- ymd(paste(year, month, "01", sep = "-"))
  days_in_month <- days_in_month(target_date)
  
  # Build API request parameters[1][4][7]
  request <- list(
    dataset_short_name = "reanalysis-era5-single-levels",
    product_type = "reanalysis",
    variable = "low_cloud_cover",
    year = as.character(year),
    month = formatC(month, width = 2, flag = "0"),
    day = formatC(1:days_in_month, width = 2, flag = "0"),
    time = sprintf("%02d:00", 0:23),  # All hours[7][10]
    area = c(42, 88, -15, 155),       # N/W/S/E coordinates
    data_format = "netcdf",
    target = nc_filename
  )
  
  # Execute request with error handling[1][4]
  tryCatch({
    wf_request(
      request = request,
      transfer = TRUE,
      path = ".",
      time_out = 172800  # 48-hour timeout
    )
    message("Successfully downloaded: ", nc_filename)
  }, error = function(e) {
    message("Failed to download ", nc_filename, ": ", e$message)
  })
}

# Download data for 2021-2024 with nested loops
for (year in 2021:2024) {
  for (month in 1:12) {
    download_monthly_data(year, month)
    Sys.sleep(10)  # Add delay between requests[1][4]
  }
}