library(tidyverse) # General data organisation 
library(lubridate) # For use of timings to subset data

#### Setup & Read in Data -----------------------------------------------------------------


# Function to read all CSV files for a single device and store it as an object
read_device_data <- function(device_path) {
  files <- list.files(device_path, pattern = "*.csv", full.names = TRUE)
  device_data <- files %>%
    map_dfr(read_csv, .id = "file_id") 
  return(device_data)
  }


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

#### November 2023 Dawn Chorus Subset -----------------------------------------------------------------

# Arai 
nov_dawn_AraiCI02 <- subset_data_by_time_and_date(CI02, "20231123", "20231129", "050000", "090000")
nov_dawn_AraiCI14 <- subset_data_by_time_and_date(CI14, "20231123", "20231129", "050000", "090000")

# Oda
nov_dawn_OdaCI01 <- subset_data_by_time_and_date(CI01, "20231123", "20231129", "050000", "090000")
nov_dawn_OdaCI13 <- subset_data_by_time_and_date(CI13, "20231123", "20231129", "050000", "090000")

# Tang Rang Bridge
nov_dawn_TRangCC07 <- subset_data_by_time_and_date(CI07, "20231123", "20231129", "050000", "090000")
nov_dawn_TRangCI07 <- subset_data_by_time_and_date(CI07, "20231123", "20231129", "050000", "090000")

# Kravanh
nov_dawn_KraCC08 <- subset_data_by_time_and_date(CC08, "20231123", "20231129", "050000", "090000")
nov_dawn_KraCI08 <- subset_data_by_time_and_date(CI08, "20231123", "20231129", "050000", "090000")

# Pursat
nov_dawn_PurCC09 <- subset_data_by_time_and_date(CC09, "20231123", "20231129", "050000", "090000")
nov_dawn_PurCI09 <- subset_data_by_time_and_date(CI09, "20231123", "20231129", "050000", "090000")

# Combine Dataframe for November 2023 Dawn Choruses
Nov23_dawn_list <- list(nov_dawn_AraiCI02, nov_dawn_AraiCI14,
                        nov_dawn_OdaCI01, nov_dawn_OdaCI13, 
                        nov_dawn_TRangCC07, nov_dawn_TRangCI07, 
                        nov_dawn_KraCC08, nov_dawn_KraCI08, 
                        nov_dawn_PurCC09, nov_dawn_PurCI09)
Nov23_dawn_data <- bind_rows(Nov23_dawn_list)

#### November Midday Subset ---------------------------------------------
# Arai 
nov_midd_AraiCI02 <- subset_data_by_time_and_date(CI02, "20231123", "20231129", "103000", "143000")
nov_midd_AraiCI14 <- subset_data_by_time_and_date(CI14, "20231123", "20231129", "103000", "143000")

# Oda
nov_midd_OdaCI01 <- subset_data_by_time_and_date(CI01, "20231123", "20231129", "103000", "143000")
nov_midd_OdaCI13 <- subset_data_by_time_and_date(CI13, "20231123", "20231129", "103000", "143000")

# Tang Rang Bridge
nov_midd_TRangCC07 <- subset_data_by_time_and_date(CI07, "20231123", "20231129", "103000", "143000")
nov_midd_TRangCI07 <- subset_data_by_time_and_date(CI07, "20231123", "20231129", "103000", "143000")

# Kravanh
nov_midd_KraCC08 <- subset_data_by_time_and_date(CC08, "20231123", "20231129", "103000", "143000")
nov_midd_KraCI08 <- subset_data_by_time_and_date(CI08, "20231123", "20231129", "103000", "143000")

# Pursat
nov_midd_PurCC09 <- subset_data_by_time_and_date(CC09, "20231123", "20231129", "103000", "143000")
nov_midd_PurCI09 <- subset_data_by_time_and_date(CI09, "20231123", "20231129", "103000", "143000")

# Combine Dataframe for November 2023 Dawn Choruses
Nov23_midd_list <- list(nov_midd_AraiCI02, nov_midd_AraiCI14,
                        nov_midd_OdaCI01, nov_midd_OdaCI13, 
                        nov_midd_TRangCC07, nov_midd_TRangCI07, 
                        nov_midd_KraCC08, nov_midd_KraCI08, 
                        nov_midd_PurCC09, nov_midd_PurCI09)
Nov23_midd_data <- bind_rows(Nov23_midd_list)

#### November Dusk Chorus Subset ---------------------------------------------

# Arai 
nov_dusk_AraiCI02 <- subset_data_by_time_and_date(CI02, "20231123", "20231129", "153000", "193000")
nov_dusk_AraiCI14 <- subset_data_by_time_and_date(CI14, "20231123", "20231129", "153000", "193000")

# Oda
nov_dusk_OdaCI01 <- subset_data_by_time_and_date(CI01, "20231123", "20231129", "153000", "193000")
nov_dusk_OdaCI13 <- subset_data_by_time_and_date(CI13, "20231123", "20231129", "153000", "193000")

# Tang Rang Bridge
nov_dusk_TRangCC07 <- subset_data_by_time_and_date(CI07, "20231123", "20231129", "153000", "193000")
nov_dusk_TRangCI07 <- subset_data_by_time_and_date(CI07, "20231123", "20231129", "153000", "193000")

# Kravanh
nov_dusk_KraCC08 <- subset_data_by_time_and_date(CC08, "20231123", "20231129", "153000", "193000")
nov_dusk_KraCI08 <- subset_data_by_time_and_date(CI08, "20231123", "20231129", "153000", "193000")

# Pursat
nov_dusk_PurCC09 <- subset_data_by_time_and_date(CC09, "20231123", "20231129", "153000", "193000")
nov_dusk_PurCI09 <- subset_data_by_time_and_date(CI09, "20231123", "20231129", "153000", "193000")

# Combine Dataframe for November 2023 Dawn Choruses
Nov23_dusk_list <- list(nov_dusk_AraiCI02, nov_dusk_AraiCI14,
                        nov_dusk_OdaCI01, nov_dusk_OdaCI13, 
                        nov_dusk_TRangCC07, nov_dusk_TRangCI07, 
                        nov_dusk_KraCC08, nov_dusk_KraCI08, 
                        nov_dusk_PurCC09, nov_dusk_PurCI09)
Nov23_dusk_data <- bind_rows(Nov23_dusk_list)

#### November Midnight Subset ---------------------------------------------

# Arai 
nov_midn1_AraiCI02 <- subset_data_by_time_and_date(CI02, "20231123", "20231129", "000000", "020000")
nov_midn2_AraiCI02 <- subset_data_by_time_and_date(CI02, "20231123", "20231129", "220000", "235900")
nov_midn1_AraiCI14 <- subset_data_by_time_and_date(CI14, "20231123", "20231129", "000000", "020000")
nov_midn2_AraiCI14 <- subset_data_by_time_and_date(CI14, "20231123", "20231129", "220000", "235900")

# Oda
nov_midn1_OdaCI01 <- subset_data_by_time_and_date(CI01, "20231123", "20231129", "000000", "020000")
nov_midn2_OdaCI01 <- subset_data_by_time_and_date(CI01, "20231123", "20231129", "220000", "235900")
nov_midn1_OdaCI13 <- subset_data_by_time_and_date(CI13, "20231123", "20231129", "000000", "020000")
nov_midn2_OdaCI13 <- subset_data_by_time_and_date(CI13, "20231123", "20231129", "220000", "235900")


# Tang Rang Bridge
nov_midn1_TRangCC07 <- subset_data_by_time_and_date(CC07, "20231123", "20231129", "000000", "020000")
nov_midn2_TRangCC07 <- subset_data_by_time_and_date(CC07, "20231123", "20231129", "220000", "235900")
nov_midn1_TRangCI07 <- subset_data_by_time_and_date(CI07, "20231123", "20231129", "000000", "020000")
nov_midn2_TRangCI07 <- subset_data_by_time_and_date(CI07, "20231123", "20231129", "220000", "235900")


# Kravanh
nov_midn1_KraCC08 <- subset_data_by_time_and_date(CC08, "20231123", "20231129", "000000", "020000")
nov_midn2_KraCC08 <- subset_data_by_time_and_date(CC08, "20231123", "20231129", "220000", "235900")
nov_midn1_KraCI08 <- subset_data_by_time_and_date(CI08, "20231123", "20231129", "000000", "020000")
nov_midn2_KraCI08 <- subset_data_by_time_and_date(CI08, "20231123", "20231129", "220000", "235900")


# Pursat
nov_midn1_PurCC09 <- subset_data_by_time_and_date(CC09, "20231123", "20231129", "000000", "020000")
nov_midn2_PurCC09 <- subset_data_by_time_and_date(CC09, "20231123", "20231129", "220000", "235900")
nov_midn1_PurCI09 <- subset_data_by_time_and_date(CI09, "20231123", "20231129", "000000", "020000")
nov_midn2_PurCI09 <- subset_data_by_time_and_date(CI09, "20231123", "20231129", "220000", "235900")


# Combine Dataframe for November 2023 Dawn Choruses
Nov23_midn_list <- list(nov_midn1_AraiCI02, nov_midn2_AraiCI02, nov_midn1_AraiCI14, nov_midn2_AraiCI14,
                        nov_midn1_OdaCI01,  nov_midn2_OdaCI01, nov_midn1_OdaCI13, nov_midn2_OdaCI13,
                        nov_midn1_TRangCC07, nov_midn2_TRangCC07, nov_midn1_TRangCI07, nov_midn2_TRangCI07,
                        nov_midn1_KraCC08, nov_midn2_KraCC08, nov_midn1_KraCI08, nov_midn2_KraCI08,
                        nov_midn1_PurCC09, nov_midn2_PurCC09, nov_midn1_PurCI09, nov_midn2_PurCI09)
Nov23_midn_data <- bind_rows(Nov23_midn_list)



#### January 2024 Full Day Subset ------------------------------------------------------------------------------------------

# Ta Chey
jan_TachCI10 <- subset_data_by_time_and_date(CI10, "20240111", "20240120", "000000", "235999")
jan_TachCC01 <- subset_data_by_time_and_date(CC01, "20240111", "20240120", "000000", "235999")

# Arai
jan_AraiCI02 <- subset_data_by_time_and_date(CI02, "20240112", "20240121", "000000", "235999")
jan_AraiCI14 <- subset_data_by_time_and_date(CI14, "20231231", "20240109", "000000", "235999")

# Oda
jan_OdaCI01 <- subset_data_by_time_and_date(CI01, "20240112", "20240121", "000000", "235999")
jan_OdaCI13 <- subset_data_by_time_and_date(CI13, "20240112", "20240121", "000000", "235999")

# Knaong Bat Sa
jan_BatCI05 <- subset_data_by_time_and_date(CI05, "20240117", "20240126", "000000", "235999")
jan_BatCI17 <-  subset_data_by_time_and_date(CI17, "20240117", "20240126", "000000", "235999")

# Ta Say
jan_TasCC12 <- subset_data_by_time_and_date(CC12, "20240119", "20240128", "000000", "235999")
jan_TasCI12 <- subset_data_by_time_and_date(CI12, "20240119", "20240128", "000000", "235999")

# Dam Five
jan_DamCC11 <- subset_data_by_time_and_date(CC11, "20240113", "20240122", "000000", "235999")
jan_DamCI11 <- subset_data_by_time_and_date(CI11, "20240113", "20240122", "000000", "235999")

# Tang Rang Bridge
jan_TRangCC07 <- subset_data_by_time_and_date(CI07, "20240110", "20240119", "000000", "235999")
jan_TRangCI07 <- subset_data_by_time_and_date(CI07, "20240110", "20240119", "000000", "235999")

# Pursat
jan_PurCC09 <- subset_data_by_time_and_date(CC09, "20240120", "20240129", "000000", "235999")
jan_PurCI09 <- subset_data_by_time_and_date(CI09, "20240120", "20240129", "000000", "235999")

# Combine Dataframe for janember 2024 Full Days
jan24_fullday_list <- list(jan_TachCI10, jan_TachCC01,
                           jan_AraiCI02, jan_AraiCI14, 
                           jan_OdaCI01, jan_OdaCI13, 
                           jan_BatCI05, jan_BatCI17,
                           jan_TasCC12, jan_TasCI12,
                           jan_DamCC11, jan_DamCI11,
                           jan_TRangCC07, jan_TRangCI07, 
                           jan_PurCC09, jan_PurCI09)

jan24_fullday_data <- bind_rows(jan24_fullday_list)


#### January 2024 Dawn Chorus Subset ------------------------------------------------------------------------------------------

# Ta Chey
jan_dawn_TachCI10 <- subset_data_by_time_and_date(CI10, "20240111", "20240120", "050000", "090000")
jan_dawn_TachCC01 <- subset_data_by_time_and_date(CC01, "20240111", "20240120", "050000", "090000")

# Arai
jan_dawn_AraiCI02 <- subset_data_by_time_and_date(CI02, "20240112", "20240121", "050000", "090000")
jan_dawn_AraiCI14 <- subset_data_by_time_and_date(CI14, "20241241", "20240109", "050000", "090000")

# Oda
jan_dawn_OdaCI01 <- subset_data_by_time_and_date(CI01, "20240112", "20240121", "050000", "090000")
jan_dawn_OdaCI13 <- subset_data_by_time_and_date(CI13, "20240112", "20240121", "050000", "090000")

# Knaong Bat Sa
jan_dawn_BatCI05 <- subset_data_by_time_and_date(CI05, "20240117", "20240126", "050000", "090000")
jan_dawn_BatCI17 <-  subset_data_by_time_and_date(CI17, "20240117", "20240126", "050000", "090000")

# Ta Say
jan_dawn_TasCC12 <- subset_data_by_time_and_date(CC12, "20240119", "20240128", "050000", "090000")
jan_dawn_TasCI12 <- subset_data_by_time_and_date(CI12, "20240119", "20240128", "050000", "090000")

# Dam Five
jan_dawn_DamCC11 <- subset_data_by_time_and_date(CC11, "20240113", "20240122", "050000", "090000")
jan_dawn_DamCI11 <- subset_data_by_time_and_date(CI11, "20240113", "20240122", "050000", "090000")

# Tang Rang Bridge
jan_dawn_TRangCC07 <- subset_data_by_time_and_date(CC07, "20240110", "20240119", "050000", "090000")
jan_dawn_TRangCI07 <- subset_data_by_time_and_date(CI07, "20240110", "20240119", "050000", "090000")

# Pursat
jan_dawn_PurCC09 <- subset_data_by_time_and_date(CC09, "20240120", "20240129", "050000", "090000")
jan_dawn_PurCI09 <- subset_data_by_time_and_date(CI09, "20240120", "20240129", "050000", "090000")

# Combine Dataframe for Januray 2024 Dawn
jan24_dawn_list <- list(jan_dawn_TachCI10, jan_dawn_TachCC01,
                        jan_dawn_AraiCI02, jan_dawn_AraiCI14, 
                        jan_dawn_OdaCI01, jan_dawn_OdaCI13, 
                        jan_dawn_BatCI05, jan_dawn_BatCI17,
                        jan_dawn_TasCC12, jan_dawn_TasCI12,
                        jan_dawn_DamCC11, jan_dawn_DamCI11,
                        jan_dawn_TRangCC07, jan_dawn_TRangCI07, 
                        jan_dawn_PurCC09, jan_dawn_PurCI09)

jan24_dawn_data <- bind_rows(jan24_dawn_list)

#### January 2024 Midday Subset ------------------------------------------------------------------------------------------

# Ta Chey
jan_midd_TachCI10 <- subset_data_by_time_and_date(CI10, "20240111", "20240120", "103000", "143000")
jan_midd_TachCC01 <- subset_data_by_time_and_date(CC01, "20240111", "20240120", "103000", "143000")

# Arai
jan_midd_AraiCI02 <- subset_data_by_time_and_date(CI02, "20240112", "20240121", "103000", "143000")
jan_midd_AraiCI14 <- subset_data_by_time_and_date(CI14, "20241241", "20240109", "103000", "143000")

# Oda
jan_midd_OdaCI01 <- subset_data_by_time_and_date(CI01, "20240112", "20240121", "103000", "143000")
jan_midd_OdaCI13 <- subset_data_by_time_and_date(CI13, "20240112", "20240121", "103000", "143000")

# Knaong Bat Sa
jan_midd_BatCI05 <- subset_data_by_time_and_date(CI05, "20240117", "20240126", "103000", "143000")
jan_midd_BatCI17 <-  subset_data_by_time_and_date(CI17, "20240117", "20240126", "103000", "143000")

# Ta Say
jan_midd_TasCC12 <- subset_data_by_time_and_date(CC12, "20240119", "20240128", "103000", "143000")
jan_midd_TasCI12 <- subset_data_by_time_and_date(CI12, "20240119", "20240128", "103000", "143000")

# Dam Five
jan_midd_DamCC11 <- subset_data_by_time_and_date(CC11, "20240113", "20240122", "103000", "143000")
jan_midd_DamCI11 <- subset_data_by_time_and_date(CI11, "20240113", "20240122", "103000", "143000")

# Tang Rang Bridge
jan_midd_TRangCC07 <- subset_data_by_time_and_date(CC07, "20240110", "20240119", "103000", "143000")
jan_midd_TRangCI07 <- subset_data_by_time_and_date(CI07, "20240110", "20240119", "103000", "143000")

# Pursat
jan_midd_PurCC09 <- subset_data_by_time_and_date(CC09, "20240120", "20240129", "103000", "143000")
jan_midd_PurCI09 <- subset_data_by_time_and_date(CI09, "20240120", "20240129", "103000", "143000")

# Combine Dataframe for January 2024 Midday
jan24_midd_list <- list(jan_midd_TachCI10, jan_midd_TachCC01,
                        jan_midd_AraiCI02, jan_midd_AraiCI14, 
                        jan_midd_OdaCI01, jan_midd_OdaCI13, 
                        jan_midd_BatCI05, jan_midd_BatCI17,
                        jan_midd_TasCC12, jan_midd_TasCI12,
                        jan_midd_DamCC11, jan_midd_DamCI11,
                        jan_midd_TRangCC07, jan_midd_TRangCI07, 
                        jan_midd_PurCC09, jan_midd_PurCI09)
jan24_midd_data <- bind_rows(jan24_midd_list)

#### January 2024 Dusk Chorus Subset ------------------------------------------------------------------------------------------


# Ta Chey
jan_dusk_TachCI10 <- subset_data_by_time_and_date(CI10, "20240111", "20240120", "153000", "193000")
jan_dusk_TachCC01 <- subset_data_by_time_and_date(CC01, "20240111", "20240120", "153000", "193000")

# Arai
jan_dusk_AraiCI02 <- subset_data_by_time_and_date(CI02, "20240112", "20240121", "153000", "193000")
jan_dusk_AraiCI14 <- subset_data_by_time_and_date(CI14, "20241241", "20240109", "153000", "193000")

# Oda
jan_dusk_OdaCI01 <- subset_data_by_time_and_date(CI01, "20240112", "20240121", "153000", "193000")
jan_dusk_OdaCI13 <- subset_data_by_time_and_date(CI13, "20240112", "20240121", "153000", "193000")

# Knaong Bat Sa
jan_dusk_BatCI05 <- subset_data_by_time_and_date(CI05, "20240117", "20240126", "153000", "193000")
jan_dusk_BatCI17 <-  subset_data_by_time_and_date(CI17, "20240117", "20240126", "153000", "193000")

# Ta Say
jan_dusk_TasCC12 <- subset_data_by_time_and_date(CC12, "20240119", "20240128", "153000", "193000")
jan_dusk_TasCI12 <- subset_data_by_time_and_date(CI12, "20240119", "20240128", "153000", "193000")

# Dam Five
jan_dusk_DamCC11 <- subset_data_by_time_and_date(CC11, "20240113", "20240122", "153000", "193000")
jan_dusk_DamCI11 <- subset_data_by_time_and_date(CI11, "20240113", "20240122", "153000", "193000")

# Tang Rang Bridge
jan_dusk_TRangCC07 <- subset_data_by_time_and_date(CC07, "20240110", "20240119", "153000", "193000")
jan_dusk_TRangCI07 <- subset_data_by_time_and_date(CI07, "20240110", "20240119", "153000", "193000")

# Pursat
jan_dusk_PurCC09 <- subset_data_by_time_and_date(CC09, "20240120", "20240129", "153000", "193000")
jan_dusk_PurCI09 <- subset_data_by_time_and_date(CI09, "20240120", "20240129", "153000", "193000")

# Combine Dataframe for Jan 2024 Dusk
jan24_dusk_list <- list(jan_dusk_TachCI10, jan_dusk_TachCC01,
                        jan_dusk_AraiCI02, jan_dusk_AraiCI14, 
                        jan_dusk_OdaCI01, jan_dusk_OdaCI13, 
                        jan_dusk_BatCI05, jan_dusk_BatCI17,
                        jan_dusk_TasCC12, jan_dusk_TasCI12,
                        jan_dusk_DamCC11, jan_dusk_DamCI11,
                        jan_dusk_TRangCC07, jan_dusk_TRangCI07, 
                        jan_dusk_PurCC09, jan_dusk_PurCI09)
jan24_dusk_data <- bind_rows(jan24_dusk_list)

#### January 2024 Midnight Subset ------------------------------------------------------------------------------------------

# Ta Chey
jan_midn1_TachCI10 <- subset_data_by_time_and_date(CI10, "20240111", "20240120", "000000", "020000")
jan_midn2_TachCI10 <- subset_data_by_time_and_date(CI10, "20240111", "20240120", "220000", "245900")
jan_midn1_TachCC01 <- subset_data_by_time_and_date(CC01, "20240111", "20240120", "000000", "020000")
jan_midn2_TachCC01 <- subset_data_by_time_and_date(CC01, "20240111", "20240120", "220000", "245900")

# Arai
jan_midn1_AraiCI02 <- subset_data_by_time_and_date(CI02, "20240112", "20240121", "000000", "020000")
jan_midn2_AraiCI02 <- subset_data_by_time_and_date(CI02, "20240112", "20240121", "220000", "245900")
jan_midn1_AraiCI14 <- subset_data_by_time_and_date(CI14, "20241241", "20240109", "000000", "020000")
jan_midn2_AraiCI14 <- subset_data_by_time_and_date(CI14, "20241241", "20240109", "220000", "245900")

# Oda
jan_midn1_OdaCI01 <- subset_data_by_time_and_date(CI01, "20240112", "20240121", "000000", "020000")
jan_midn2_OdaCI01 <- subset_data_by_time_and_date(CI01, "20240112", "20240121", "220000", "245900")
jan_midn1_OdaCI13 <- subset_data_by_time_and_date(CI13, "20240112", "20240121", "000000", "020000")
jan_midn2_OdaCI13 <- subset_data_by_time_and_date(CI13, "20240112", "20240121", "220000", "245900")

# Knaong Bat Sa
jan_midn1_BatCI05 <- subset_data_by_time_and_date(CI05, "20240117", "20240126", "000000", "020000")
jan_midn2_BatCI05 <- subset_data_by_time_and_date(CI05, "20240117", "20240126", "220000", "245900")
jan_midn1_BatCI17 <- subset_data_by_time_and_date(CI17, "20240117", "20240126", "000000", "020000")
jan_midn2_BatCI17 <- subset_data_by_time_and_date(CI17, "20240117", "20240126", "220000", "245900")

# Ta Say
jan_midn1_TasCC12 <- subset_data_by_time_and_date(CC12, "20240119", "20240128", "000000", "020000")
jan_midn2_TasCC12 <- subset_data_by_time_and_date(CC12, "20240119", "20240128", "220000", "245900")
jan_midn1_TasCI12 <- subset_data_by_time_and_date(CI12, "20240119", "20240128", "000000", "020000")
jan_midn2_TasCI12 <- subset_data_by_time_and_date(CI12, "20240119", "20240128", "220000", "245900")

# Dam Five
jan_midn1_DamCC11 <- subset_data_by_time_and_date(CC11, "20240113", "20240122", "000000", "020000")
jan_midn2_DamCC11 <- subset_data_by_time_and_date(CC11, "20240113", "20240122", "220000", "245900")
jan_midn1_DamCI11 <- subset_data_by_time_and_date(CI11, "20240113", "20240122", "000000", "020000")
jan_midn2_DamCI11 <- subset_data_by_time_and_date(CI11, "20240113", "20240122", "220000", "245900")

# Tang Rang Bridge
jan_midn1_TRangCC07 <- subset_data_by_time_and_date(CC07, "20240110", "20240119", "000000", "020000")
jan_midn2_TRangCC07 <- subset_data_by_time_and_date(CC07, "20240110", "20240119", "000000", "020000")
jan_midn1_TRangCI07 <- subset_data_by_time_and_date(CI07, "20240110", "20240119", "000000", "020000")
jan_midn2_TRangCI07 <- subset_data_by_time_and_date(CI07, "20240110", "20240119", "000000", "020000")

# Pursat
jan_midn1_PurCC09 <- subset_data_by_time_and_date(CC09, "20240120", "20240129", "000000", "020000")
jan_midn2_PurCC09 <- subset_data_by_time_and_date(CC09, "20240120", "20240129", "220000", "245900")
jan_midn1_PurCI09 <- subset_data_by_time_and_date(CI09, "20240120", "20240129", "000000", "020000")
jan_midn2_PurCI09 <- subset_data_by_time_and_date(CI09, "20240120", "20240129", "220000", "245900")

# Combine Dataframe for Jan 2024 midn
jan24_midn_list <- list(jan_midn1_TachCI10, jan_midn1_TachCC01,
                        jan_midn2_TachCI10, jan_midn2_TachCC01,
                        jan_midn1_AraiCI02, jan_midn1_AraiCI14, 
                        jan_midn2_AraiCI02, jan_midn2_AraiCI14, 
                        jan_midn1_OdaCI01, jan_midn1_OdaCI13,
                        jan_midn2_OdaCI01, jan_midn2_OdaCI13, 
                        jan_midn1_BatCI05, jan_midn1_BatCI17,
                        jan_midn2_BatCI05, jan_midn2_BatCI17,
                        jan_midn1_TasCC12, jan_midn1_TasCI12,
                        jan_midn2_TasCC12, jan_midn2_TasCI12,
                        jan_midn1_DamCC11, jan_midn1_DamCI11,
                        jan_midn2_DamCC11, jan_midn2_DamCI11,
                        jan_midn1_TRangCC07, jan_midn1_TRangCI07, 
                        jan_midn2_TRangCC07, jan_midn2_TRangCI07, 
                        jan_midn1_PurCC09, jan_midn1_PurCI09,
                        jan_midn2_PurCC09, jan_midn2_PurCI09)

jan24_midn_data <- bind_rows(jan24_midn_list)


##### ODA Site Temporal Subset

# Oda
apr_OdaCI01 <- subset_data_by_time_and_date(CI01, "20240405", "20240414", "000000", "235999")
apr_OdaCI13 <- subset_data_by_time_and_date(CI13, "20240405", "20240414", "000000", "235999")
jun_OdaCI01 <- subset_data_by_time_and_date(CI01, "20240616", "20240625", "000000", "235999")
jun_OdaCI13 <- subset_data_by_time_and_date(CI13, "20240616", "20240625", "000000", "235999")

oda_season_list <- list(nov_OdaCI01, nov_OdaCI13, 
                        jan_OdaCI01, jan_OdaCI13, 
                        apr_OdaCI01, apr_OdaCI13, 
                        jun_OdaCI01, jun_OdaCI13)

oda_season_data <- bind_rows(oda_season_list)


# Oda Dawn
apr_dawn_OdaCI01 <- subset_data_by_time_and_date(CI01, "20240405", "20240414", "050000", "090000")
apr_dawn_OdaCI13 <- subset_data_by_time_and_date(CI13, "20240405", "20240414", "050000", "090000")
jun_dawn_OdaCI01 <- subset_data_by_time_and_date(CI01, "20240616", "20240625", "050000", "090000")
jun_dawn_OdaCI13 <- subset_data_by_time_and_date(CI13, "20240616", "20240625", "050000", "090000")

oda_dawn_list <- list(nov_dawn_OdaCI01, nov_dawn_OdaCI13, 
                        jan_dawn_OdaCI01, jan_dawn_OdaCI13, 
                        apr_dawn_OdaCI01, apr_dawn_OdaCI13, 
                        jun_dawn_OdaCI01, jun_dawn_OdaCI13)

oda_dawn_data <- bind_rows(oda_season_list)

