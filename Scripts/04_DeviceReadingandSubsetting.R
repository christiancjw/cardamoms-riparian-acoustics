# This script creates month based datasets combining a single device from each site 
## (no replication due to recorder losses in. other sites)
# 

library(tidyverse) # General data organisation 
library(lubridate) # For use of timings to subset data
#### Setup  -----------------------------------------------------------------

main_dir <- "/Users/christianching/Documents/Projects/cardamoms-riparian-acoustics/clean_data/daily_indices/"

dir.exists(main_dir) 

# -------------------------
# Function to read all CSV files for a single device
# -------------------------
read_device_data <- function(device_path) {
  files <- list.files(device_path, pattern = "*.csv", full.names = TRUE)
  device_data <- files %>%
    map_dfr(read_csv, .id = "file_id") 
  return(device_data)
}

# -------------------------
# Function to Subset Data (now requires site + device)
# -------------------------
subset_data_by_time_and_date <- function(site, device, start_date, end_date, start_time, end_time) {
  key <- paste(site, device, sep = "_")
  
  if (!key %in% names(device_data_list)) {
    stop(paste("No data found for", site, device))
  }
  
  device_data <- device_data_list[[key]]
  
  device_data$Date <- as.character(device_data$Date)
  device_data$Time <- sprintf("%06d", as.numeric(device_data$Time))
  
  subset <- device_data %>%
    filter(Date >= as.character(start_date) & Date <= as.character(end_date)) %>%
    filter(Time >= start_time & Time <= end_time)
  
  return(subset)
}

# -------------------------
# Data Read-In
# -------------------------
# Get all device directories that contain CSVs
device_dirs <- list.dirs(main_dir, recursive = TRUE, full.names = TRUE) %>%
  keep(~ length(list.files(.x, pattern = "*.csv", full.names = TRUE)) > 0)

# Remove macOS hidden "._" directories if they slip in
device_dirs <- device_dirs[!basename(device_dirs) %in% c("._", ".DS_Store")]
device_dirs <- device_dirs[!grepl("^\\._", basename(device_dirs))]

# Read the data
device_data_list <- map(device_dirs, read_device_data)

# Name each entry Site_Device
names(device_data_list) <- map_chr(device_dirs, function(path) {
  device <- basename(path)        # e.g. CI01
  site <- basename(dirname(path)) # e.g. TaChey
  paste(site, device, sep = "_")  # -> "TaChey_CI01"
})

# Check naming
names(device_data_list)

# Assign each device dataset to its own object in the global environment
list2env(device_data_list, envir = .GlobalEnv)
head(unique(Arai_CI02$Date)) # Check available dates
head(unique(Arai_CI02$Time)) # Check available times



#### Global Single-Device Subset -----------------------------------------------------------------

# Ta Chey
jan_TachCC01 <- subset_data_by_time_and_date("TaChey", "CC01", "20240111", "20240120", "000000", "235999")
apr_TachCC01 <- subset_data_by_time_and_date("TaChey", "CC01", "20240405", "20240414", "000000", "235999")
jun_TachCC01 <- subset_data_by_time_and_date("TaChey", "CC01", "20240610", "20240619", "000000", "235999")

# Arai
nov_AraiCI02 <- subset_data_by_time_and_date("Arai","CI02", "20231123", "20231202", "000000", "235999")
jan_AraiCI02 <- subset_data_by_time_and_date("Arai","CI02", "20240112", "20240121", "000000", "235999")
apr_AraiCI02 <- subset_data_by_time_and_date("Arai","CI02", "20240405", "20240414", "000000", "235999")
jun_AraiCI02 <- subset_data_by_time_and_date("Arai","CI02", "20240612", "20240621", "000000", "235999")

# Oda
nov_OdaCI13 <- subset_data_by_time_and_date("Oda", "CI13", "20231123", "20231202", "000000", "235999")
jan_OdaCI13 <- subset_data_by_time_and_date("Oda", "CI13", "20240112", "20240121", "000000", "235999")
apr_OdaCI13 <- subset_data_by_time_and_date("Oda", "CI13", "20240405", "20240414", "000000", "235999")
jun_OdaCI01 <- subset_data_by_time_and_date("Oda", "CI01", "20240610", "20240619", "000000", "235999")

# Knaong Bat Sa
jan_BatCI05 <- subset_data_by_time_and_date("KnaongBatSa", "CI05", "20240117", "20240126", "000000", "235999")
apr_BatCI17 <- subset_data_by_time_and_date("KnaongBatSa", "CI05", "20240408", "20240417", "000000", "235999")
jun_BatCI17 <- subset_data_by_time_and_date("KnaongBatSa", "CI05", "20240616", "20240625", "000000", "235999")

# Ta Say
jan_TasCC12 <- subset_data_by_time_and_date("TaSay", "CC12", "20240119", "20240128", "000000", "235999")
apr_TasCI12 <- subset_data_by_time_and_date("TaSay", "CI12", "20240407", "20240416", "000000", "235999")
jun_TasCI12 <- subset_data_by_time_and_date("TaSay", "CI12", "20240608", "20240617", "000000", "235999")

# Kronomh
apr_KroCI19 <- subset_data_by_time_and_date("Kronomh", "CI19", "20240406", "20240415", "000000", "235999")
jun_KroCI19 <- subset_data_by_time_and_date("Kronomh", "CI19", "20240614", "20240623", "000000", "235999")

# Dam Five
jan_DamCC11 <- subset_data_by_time_and_date("DamFive", "CC11", "20240113", "20240122", "000000", "235999")
apr_DamCC11 <- subset_data_by_time_and_date("DamFive", "CC11", "20240407", "20240416", "000000", "235999")
jun_DamCC11 <- subset_data_by_time_and_date("DamFive", "CC11", "20240614", "20240623", "000000", "235999")

# Tang Rang Bridge
nov_TRangCI07 <- subset_data_by_time_and_date("TangRang" ,"CI07", "20231123", "20231202", "000000", "235999")
jan_TRangCI07 <- subset_data_by_time_and_date("TangRang" ,"CI07", "20240111", "20240120", "000000", "235999")
apr_TRangCI07 <- subset_data_by_time_and_date("TangRang" ,"CI07", "20240405", "20240414", "000000", "235999")
jun_TRangCI07 <- subset_data_by_time_and_date("TangRang" ,"CI07", "20240614", "20240623", "000000", "235999")

# Pursat
nov_PurCI09 <- subset_data_by_time_and_date("PursatTown", "CI09", "20231123", "20231202", "000000", "235999")
jan_PurCI09 <- subset_data_by_time_and_date("PursatTown", "CI09", "20240120", "20240129", "000000", "235999")
apr_PurCI09 <- subset_data_by_time_and_date("PursatTown", "CI09", "20240404", "20240413", "000000", "235999")
jun_PurCI09 <- subset_data_by_time_and_date("PursatTown", "CI09", "20240614", "20240623", "000000", "235999")


# Combine Dataframe for November 2023 Single-Device Susbset
global_singleD_list <- list(jan_TachCC01, apr_TachCC01, jun_TachCC01, 
                           nov_AraiCI02, jan_AraiCI02, apr_AraiCI02, jun_AraiCI02, 
                           nov_OdaCI13, jan_OdaCI13, apr_OdaCI13, jun_OdaCI01,
                           jan_BatCI05, apr_BatCI17, jun_BatCI17,
                           jan_TasCC12, apr_TasCC12, jun_TasCC12,
                           apr_KroCI19, jun_KroCI19,
                           jan_DamCC11, apr_DamCC11, jun_DamCC11,
                           nov_TRangCI07, jan_TRangCI07, apr_TRangCI07, jun_TRangCI07,
                           nov_PurCI09, jan_PurCI09, apr_PurCI09, jun_PurCI09)

global_singleD_data <- bind_rows(global_singleD_list)

write.csv(global_singleD_data, "clean_data/datasets/single_device_indices/global_singleD_data.csv", row.names = FALSE)


#### Global Continuous Subset ------------
# Arai
cont_nov23_AraiCI02 <- subset_data_by_time_and_date("Arai", "CI02", "20231123", "20231229", "000000", "235999")
cont_jan24_AraiCI02 <- subset_data_by_time_and_date("Arai", "CI02", "20240112", "20240121", "000000", "235999")
cont_apr24_AraiCI02 <- subset_data_by_time_and_date("Arai", "CI02", "20240405", "20240414", "000000", "235999")
cont_jun24_AraiCI02 <- subset_data_by_time_and_date("Arai", "CI02", "20240612", "20240621", "000000", "235999")

# Oda
cont_nov23_OdaCI13 <- subset_data_by_time_and_date("Oda", "CI13", "20231123", "20231229", "000000", "235999")
cont_jan24_OdaCI13 <- subset_data_by_time_and_date("Oda", "CI13", "20240112", "20240121", "000000", "235999")
cont_apr24_OdaCI13 <- subset_data_by_time_and_date("Oda", "CI13", "20240405", "20240414", "000000", "235999")
cont_jun24_OdaCI01 <- subset_data_by_time_and_date("Oda", "CI01", "20240610", "20240619", "000000", "235999")

# Tang Rang Bridge
cont_nov23_TRangCI07 <- subset_data_by_time_and_date("TangRang", "CI07", "20231123", "20231229", "000000", "235999")
cont_jan24_TRangCI07 <- subset_data_by_time_and_date("TangRang", "CI07", "20240111", "20240120", "000000", "235999")
cont_apr24_TRangCI07 <- subset_data_by_time_and_date("TangRang", "CI07", "20240405", "20240414", "000000", "235999")
cont_jun24_TRangCI07 <- subset_data_by_time_and_date("TangRang", "CI07", "20240614", "20240623", "000000", "235999")

# Pursat
cont_nov23_PurCI09 <- subset_data_by_time_and_date("Pursat", "CI09", "20231123", "20231229", "000000", "235999")
cont_jan24_PurCI09 <- subset_data_by_time_and_date("Pursat", "CI09", "20240120", "20240129", "000000", "235999")
cont_apr24_PurCI09 <- subset_data_by_time_and_date("Pursat", "CI09", "20240404", "20240413", "000000", "235999")
cont_jun24_PurCI09 <- subset_data_by_time_and_date("Pursat", "CI09", "20240614", "20240623", "000000", "235999")

# Combine Dataframe for Continuous Single-Device Susbset
continuous_list <- list(cont_nov23_AraiCI02, cont_jan24_AraiCI02, cont_apr24_AraiCI02, cont_jun24_AraiCI02, 
                        cont_nov23_OdaCI13, cont_jan24_OdaCI13, cont_apr24_OdaCI13, cont_jun24_OdaCI13,
                        cont_nov23_TRangCI07, cont_jan24_TRangCI07, cont_apr24_TRangCI07, cont_jun24_TRangCI07,
                        cont_nov23_PurCI09, cont_jan24_PurCI09, cont_apr24_PurCI09, cont_jun24_PurCI09)

continuous_data <- bind_rows(continuous_list)

write.csv(continuous_data, "clean_data/datasets/single_device_indices/continuous_data.csv", row.names = FALSE)


#### Multiple Device Verification Dataset -------------
# The Data here is full. Need to firstly make it 1 in 5 - or a variant dataset

# Ta Chey Hill Forest - loaded 2 days for this subset. Full Days
mult_tachhillCI07 <- subset_data_by_time_and_date("TaCheyHill", "CI07", "20250609", "20250610", "000000", "235999")
mult_tachhillCC05 <- subset_data_by_time_and_date("TaCheyHill", "CC05", "20250609", "20250610", "000000", "235999")
mult_tachhillCI12 <- subset_data_by_time_and_date("TaCheyHill", "CI12", "20250609", "20250610", "000000", "235999")

# Ta Chey 100
mult_tachCI01 <- subset_data_by_time_and_date("Tachey", "CI01", "20250608", "20250610", "000000", "235999")
mult_tachCI13 <- subset_data_by_time_and_date("Tachey", "CI13", "20250608", "20250610", "000000", "235999")
mult_tachCC13 <- subset_data_by_time_and_date("Tachey", "CC13", "20250608", "20250610", "000000", "235999")

# Arai
mult_araiCI10 <- subset_data_by_time_and_date("Arai", "CI10", "20250608", "20250610", "000000", "235999")
mult_araiCI15 <- subset_data_by_time_and_date("Arai", "CI15", "20250608", "20250610", "000000", "235999")
mult_araiCC06 <- subset_data_by_time_and_date("Arai", "CC06", "20250608", "20250610", "000000", "235999")

# Oda
mult_odaCI09 <- subset_data_by_time_and_date("Oda", "CI09", "20250608", "20250610", "000000", "235999")
mult_odaCI19 <- subset_data_by_time_and_date("Oda", "CI19", "20250608", "20250610", "000000", "235999")
mult_odaCC07 <- subset_data_by_time_and_date("Oda", "CC07", "20250608", "20250610", "000000", "235999")

# Bat Sa
mult_batsaCI07 <- subset_data_by_time_and_date("KnaongBatSa", "CI07", "20250705", "20250707", "000000", "235999")
mult_batsaCI01 <- subset_data_by_time_and_date("KnaongBatSa", "CI01", "20250705", "20250707", "000000", "235999")
mult_batsaCC05 <- subset_data_by_time_and_date("KnaongBatSa", "CC05", "20250705", "20250707", "000000", "235999")

# Ta Say
mult_tasayCI19 <- subset_data_by_time_and_date("TaSay", "CI19", "20250705", "20250707", "000000", "235999")
mult_tasayCI09 <- subset_data_by_time_and_date("TaSay", "CI09", "20250705", "20250707", "000000", "235999")
mult_tasayCI15 <- subset_data_by_time_and_date("TaSay", "CI15", "20250705", "20250707", "000000", "235999")

# Kronomh
mult_kronCI13 <- subset_data_by_time_and_date("Kronomh", "CI13", "20250702", "20250704", "000000", "235999")
mult_kronCI10 <- subset_data_by_time_and_date("Kronomh", "CI10", "20250702", "20250704", "000000", "235999")

# Tang Rang
mult_tangrangCC01 <- subset_data_by_time_and_date("TangRang", "CC01", "20250606", "20250608", "000000", "235999")
mult_tangrangCC02 <- subset_data_by_time_and_date("TangRang", "CC02", "20250606", "20250608", "000000", "235999")

# Combine Dataframe for Multi-Device Susbset
multi_constant_list <- list(mult_tachhillCI07, mult_tachhillCC05, mult_tachhillCI12,
                          mult_tachCI01, mult_tachCI13, mult_tachCC13,
                          mult_araiCI10, mult_araiCI15, mult_araiCC06, 
                          mult_odaCI09, mult_odaCI19, mult_odaCC07,
                          mult_batsaCI07, mult_batsaCI01, mult_batsaCC05, 
                          mult_tasayCI19, mult_tasayCI09, mult_tasayCI15, 
                          mult_kronCI13, mult_kronCI10,
                          mult_tangrangCC01, mult_tangrangCC02)

multi_data <- bind_rows(multi_list)

multi_1in5_list

write.csv(continuous_data, "clean_data/datasets/single_device_indices/multi_data.csv", row.names = FALSE)


# -------------------------
# Subset 1 in 5 minutes per day
# -------------------------
subset_one_in_five <- function(df) {
  df %>%
    arrange(Date, Time) %>%
    group_by(Date) %>%
    mutate(row_num = row_number()) %>%
    filter((row_num - 1) %% 5 == 0) %>%   # keep every 5th row
    select(-row_num) %>%
    ungroup()
}

# Test:Run the function on just one object
mult_1in5_tachhillCI07 <- subset_one_in_five(mult_tachhillCI07)

# Check the size difference
nrow(mult_tachhillCI07)       # original row count
nrow(mult_1in5_tachhillCI07) 

# Apply to each element in the list
multi_1in5_list<- lapply(multi_constant_list, subset_one_in_five)

multi_1in5_data <- bind_rows(multi_1in5_list)

write.csv(continuous_data, "clean_data/datasets/single_device_indices/multi_data.csv", row.names = FALSE)

