library(tidyverse)

#### Setup & Read in Data -----------------------------------------------------------------

# Define the main directory
main_dir <- "/Users/christianching/Documents/Projects/cardamoms-riparian-acoustics/clean_data"
dir.exists(main_dir) # Checks the existence of directory

# Get all device directories (only those containing CSV files)
device_dirs <- list.dirs(main_dir, recursive = TRUE, full.names = TRUE) %>%
  keep(~ length(list.files(.x, pattern = "*.csv", full.names = TRUE)) > 0)

# Read data from all devices and store as named list of data frames
device_data_list <- set_names(device_dirs, basename(device_dirs)) %>%
  map(read_device_data)

# Assign each device dataset to its own object in the global environment
list2env(device_data_list, envir = .GlobalEnv)

head(unique(CI07$Date)) # Check available dates
head(unique(CI07$Time)) # Check available times




#### Function to Subset Data -----------------------------------------------------------------

# Define the function to subset the data based on time and date
subset_data_by_time_and_date <- function(device_data, start_date, end_date, start_time, end_time) {
  # Ensure that the Date and Time columns are in the correct format
  device_data$Date <- as.character(device_data$Date)  # Ensure Date is in character format
  device_data$Time <- format(strptime(as.character(device_data$Time), format = "%H%M%S"), "%H%M")
  
  # Subset data by the provided date range and time range
  subset <- device_data %>%
    filter(Date >= as.character(start_date) & Date <= as.character(end_date)) %>%
    filter(Time >= start_time & Time <= end_time)
  
  return(subset)
}

#### November 2023 Dawn Chorus Subset -----------------------------------------------------------------

# Arai 
nov_dawn_AraiCI02 <- subset_data_by_time_and_date(CI02, "20231123", "20231129", "053000", "083000")
nov_dawn_AraiCI14 <- subset_data_by_time_and_date(CI14, "20231123", "20231129", "053000", "083000")

# Oda
nov_dawn_OdaCI01 <- subset_data_by_time_and_date(CI01, "20231123", "20231129", "053000", "083000")
nov_dawn_OdaCI13 <- subset_data_by_time_and_date(CI13, "20231123", "20231129", "053000", "083000")

# Tang Rang Bridge
nov_dawn_TRangCC07 <- subset_data_by_time_and_date(CI07, "20231123", "20231129", "053000", "083000")
nov_dawn_TRangCI07 <- subset_data_by_time_and_date(CI07, "20231123", "20231129", "053000", "083000")

# Kravanh
nov_dawn_KraCC08 <- subset_data_by_time_and_date(CC08, "20231123", "20231129", "053000", "083000")
nov_dawn_KraCI08 <- subset_data_by_time_and_date(CI08, "20231123", "20231129", "053000", "083000")

# Pursat
nov_dawn_PurCC09 <- subset_data_by_time_and_date(CC09, "20231123", "20231129", "053000", "083000")
nov_dawn_PurCI09 <- subset_data_by_time_and_date(CI09, "20231123", "20231129", "053000", "083000")

# Combine Dataframe for November 2023 Dawn Choruses
Nov23_dawn_list <- list(nov_dawn_AraiCI02, nov_dawn_AraiCI14,
                        nov_dawn_OdaCI01, nov_dawn_OdaCI13, 
                        nov_dawn_TRangCC07, nov_dawn_TRangCI07, 
                        nov_dawn_KraCC08, nov_dawn_KraCI08, 
                        nov_dawn_PurCC09, nov_dawn_PurCI09)
Nov23_dawn_data <- bind_rows(Nov23_dawn_list)
 
#### November 2023 Full Day Subsets  -----------------------------------------------------------------

# Arai
nov_AraiCI02 <- subset_data_by_time_and_date(CI02, "20231123", "20231129", "000000", "235999")
nov_AraiCI14 <- subset_data_by_time_and_date(CI14, "20231123", "20231129", "000000", "235999")

# Oda
nov_OdaCI01 <- subset_data_by_time_and_date(CI01, "20231123", "20231129", "000000", "235999")
nov_OdaCI13 <- subset_data_by_time_and_date(CI13, "20231123", "20231129", "000000", "235999")

# Tang Rang Bridge
nov_TRangCC07 <- subset_data_by_time_and_date(CI07, "20231123", "20231129", "000000", "235999")
nov_TRangCI07 <- subset_data_by_time_and_date(CI07, "20231123", "20231129", "000000", "235999")

# Kravanh
nov_KraCC08 <- subset_data_by_time_and_date(CC08, "20231123", "20231129", "000000", "235999")
nov_KraCI08 <- subset_data_by_time_and_date(CI08, "20231123", "20231129", "000000", "235999")

# Pursat
nov_PurCC09 <- subset_data_by_time_and_date(CC09, "20231123", "20231129", "000000", "235999")
nov_PurCI09 <- subset_data_by_time_and_date(CI09, "20231123", "20231129", "000000", "235999")

# Combine Dataframe for November 2023 Full Days
Nov23_fullday_list <- list(nov_AraiCI02, nov_AraiCI14, nov_OdaCI01, nov_OdaCI13, nov_TRangCC07, nov_TRangCI07, nov_KraCC08, nov_KraCI08, nov_PurCC09, nov_PurCI09)
Nov23_fullday_data <- bind_rows(Nov23_fullday_list)
