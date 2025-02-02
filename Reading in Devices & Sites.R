library(tidyverse)
library(lubridate)

# Function to read all CSV files for a single device and store it as an object
read_device_data <- function(device_path) {
  files <- list.files(device_path, pattern = "*.csv", full.names = TRUE)
  
  device_data <- files %>%
    map_dfr(read_csv, .id = "file_id") 
  
  return(device_data)
}

# Define the main directory
getwd
main_dir <- "/Users/christianching/Documents/Projects/cardamoms-riparian-acoustics/clean_data"
dir.exists(main_dir)

# Get all device directories
device_dirs <- list.dirs(main_dir, recursive = TRUE, full.names = TRUE)

# Read data from all devices and store as named list of data frames
device_data_list <- set_names(device_dirs, basename(device_dirs)) %>%
  map(read_device_data)


# Create Single Device 
list2env(device_data_list, envir = .GlobalEnv)





# Function to filter data by device, date range, and time range
filter_device_data <- function(device_name, start_date, end_date, start_time, end_time) {
  if (!(device_name %in% names(device_data_list))) {
    stop("Device not found.")
  }
  
  data <- device_data_list[[device_name]]
  
  # Convert time column if present
  if ("Time" %in% colnames(data)) {
    data <- data %>% mutate(Time = hms::as_hms(Time))
  }
  
  # Apply filtering
  filtered_data <- data %>%
    filter(Date >= as.Date(start_date) & Date <= as.Date(end_date)) %>%
    filter(if ("Time" %in% colnames(.)) Time >= hms::as_hms(start_time) & Time <= hms::as_hms(end_time) else TRUE)
  
  return(filtered_data)
}

# Example: Extract data for device "CC07" from "2023-11-20" to "2023-11-25" between "06:00:00" and "18:00:00"
filtered_data <- filter_device_data("CC07", "2023-11-20", "2023-11-25", "04:00:00", "9:00:00")

# View results
print(filtered_data)
