# This script creates datasets from read in by site, device, and dates
## (no replication due to recorder losses in. other sites)

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
  
  # Ensure Date is character
  device_data$Date <- as.character(device_data$Date)
  
  # If Time isn’t already in HHMMSS format, this formats it so
  if (!all(grepl("^\\d{6}$", device_data$Time))) {
    device_data$Time <- sprintf("%06d", as.integer(device_data$Time))
  }
  
  subset <- device_data %>%
    filter(Date >= as.character(start_date) & Date <= as.character(end_date)) %>%
    filter(Time >= start_time & Time <= end_time)
  
  return(subset)
}

# -------------------------
# Function to take subset and further subset One in Five minutes of Data 
# takes dataframe and groups each date, then takes every 5th row within that date. 
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

# -------------------------
# Data Read-In
# -------------------------

# Get all device directories that contain CSVs
device_dirs <- list.dirs(main_dir, recursive = TRUE, full.names = TRUE) %>%
  keep(~ length(list.files(.x, pattern = "*.csv", full.names = TRUE)) > 0)

# Remove macOS hidden "._" directories if they slip in
device_dirs <- device_dirs[!basename(device_dirs) %in% c("._", ".DS_Store")]
device_dirs <- device_dirs[!grepl("^\\._", basename(device_dirs))]

# Read in the data - will process through all the cleaned .csv files
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
head(unique(Arai_CI02$Month))


#### Global Single-Device Subset (2023-2025) -----------------------------------------------------------------

# Ta Chey
  
#This one is a full day recording - needs subsetting into 1in5
nov_TachCI15 <- subset_data_by_time_and_date("TaChey", "CI15", "20231117", "20231220", "000000", "235999")
  # Use the subset function
      nov_TachCI15 <- subset_one_in_five(nov_TachCI15)
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
apr_BatCI17 <- subset_data_by_time_and_date("KnaongBatSa", "CI17", "20240408", "20240417", "000000", "235999")
jun_BatCI17 <- subset_data_by_time_and_date("KnaongBatSa", "CI17", "20240616", "20240625", "000000", "235999")

# Ta Say
jan_TasCC12 <- subset_data_by_time_and_date("TaSay", "CC12", "20240119", "20240128", "000000", "235999")
apr_TasCI12 <- subset_data_by_time_and_date("TaSay", "CI12", "20240407", "20240416", "000000", "235999")
jun_TasCI12 <- subset_data_by_time_and_date("TaSay", "CI12", "20240616", "20240625", "000000", "235999")

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


# Combine Dataframe for Global Single-Device Susbset
global_singleD_list <- list(nov_TachCI15, jan_TachCC01, apr_TachCC01, jun_TachCC01, 
                           nov_AraiCI02, jan_AraiCI02, apr_AraiCI02, jun_AraiCI02, 
                           nov_OdaCI13, jan_OdaCI13, apr_OdaCI13, jun_OdaCI01,
                           jan_BatCI05, apr_BatCI17, jun_BatCI17,
                           jan_TasCC12, apr_TasCI12, jun_TasCI12,
                           apr_KroCI19, jun_KroCI19,
                           jan_DamCC11, apr_DamCC11, jun_DamCC11,
                           nov_TRangCI07, jan_TRangCI07, apr_TRangCI07, jun_TRangCI07,
                           nov_PurCI09, jan_PurCI09, apr_PurCI09, jun_PurCI09)

global_singleD_data <- bind_rows(global_singleD_list)

write.csv(global_singleD_data, "clean_data/datasets/indices_datasets/2023_2024_singledevice_data.csv.csv", row.names = FALSE)


### Adding 2025
sum25_tachCI01 <- subset_data_by_time_and_date("TaChey", "CI01", "20250608", "20250612", "000000", "235999")
sum25_araiCI10 <- subset_data_by_time_and_date("Arai", "CI10", "20250608", "20250612", "000000", "235999")          
sum25_odaCI09 <- subset_data_by_time_and_date("Oda", "CI09", "20250607", "20250611", "000000", "235999")

#fix oda's QBR in 2025
sum25_odaCI09 <- sum25_odaCI09 %>%
  mutate(QBR_Score = 40)

sum25_batsaCI07 <- subset_data_by_time_and_date("KnaongBatSa", "CI01", "20250704", "20250708", "000000", "235999")
sum25_tasayCI19 <- subset_data_by_time_and_date("TaSay", "CI19", "20250705", "20250709", "000000", "235999")
sum25_kronCI13 <- subset_data_by_time_and_date("Kronomh", "CI13", "20250702", "20250706", "000000", "235999")

sum25_tangrangCC02 <- subset_data_by_time_and_date("TangRang", "CC02", "20250606", "20250609", "000000", "235999")          

# Ensure that subset from 2025 is given 1 in 5 minute subsetting
summer25_singledeivces <- list(sum25_tachCI01, sum25_araiCI10, sum25_odaCI09, sum25_batsaCI07, sum25_tasayCI19, sum25_kronCI13, sum25_tangrangCC02)
summer25_singledeivces_1in5 <- lapply(summer25_singledeivces, subset_one_in_five)

# Add the 2025 1 in 5 data to the 2023-2024 global
global2023_2025_singleD_list <- list(global_singleD_list, summer25_singledeivces_1in5)

global2023_2025_singleDataset <- bind_rows(global2023_2025_singleD_list)

head(global2023_2025_singleDataset)

write.csv(global2023_2025_singleDataset, "clean_data/datasets/indices_datasets/global_singledevice_data.csv", row.names = FALSE)

#### Global Continuous Subset ------------
# Ta Chey

#This one is a full day recording - needs subsetting into 1in5
nov_TachCI15 <- subset_data_by_time_and_date("TaChey", "CI15", "20231117", "20231220", "000000", "235999")
    # Use the subset function
    nov_TachCI15 <- subset_one_in_five(nov_TachCI15)
jan_TachCC01 <- subset_data_by_time_and_date("TaChey", "CC01", "20240111", "20240120", "000000", "235999")
apr_TachCC01 <- subset_data_by_time_and_date("TaChey", "CC01", "20240405", "20240414", "000000", "235999")
jun_TachCC01 <- subset_data_by_time_and_date("TaChey", "CC01", "20240610", "20240619", "000000", "235999")

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
cont_nov23_PurCI09 <- subset_data_by_time_and_date("PursatTown", "CI09", "20231123", "20231229", "000000", "235999")
cont_jan24_PurCI09 <- subset_data_by_time_and_date("PursatTown", "CI09", "20240120", "20240129", "000000", "235999")
cont_apr24_PurCI09 <- subset_data_by_time_and_date("PursatTown", "CI09", "20240404", "20240413", "000000", "235999")
cont_jun24_PurCI09 <- subset_data_by_time_and_date("PursatTown", "CI09", "20240614", "20240623", "000000", "235999")

# Combine Dataframe for Continuous Single-Device Susbset
continuous_list <- list(nov_TachCI15, jan_TachCC01, apr_TachCC01, jun_TachCC01, 
                        cont_nov23_AraiCI02, cont_jan24_AraiCI02, cont_apr24_AraiCI02, cont_jun24_AraiCI02, 
                        cont_nov23_OdaCI13, cont_jan24_OdaCI13, cont_apr24_OdaCI13, cont_jun24_OdaCI01,
                        cont_nov23_TRangCI07, cont_jan24_TRangCI07, cont_apr24_TRangCI07, cont_jun24_TRangCI07,
                        cont_nov23_PurCI09, cont_jan24_PurCI09, cont_apr24_PurCI09, cont_jun24_PurCI09,
                        sum25_araiCI10, sum25_odaCI09, sum25_tangrangCC02)

continuous_data <- bind_rows(continuous_list)


### Adding 2025
sum25_tachCI01 <- subset_data_by_time_and_date("TaChey", "CI01", "20250608", "20250612", "000000", "235999")
sum25_araiCI10 <- subset_data_by_time_and_date("Arai", "CI10", "20250608", "20250612", "000000", "235999")          
sum25_odaCI09 <- subset_data_by_time_and_date("Oda", "CI09", "20250607", "20250611", "000000", "235999")
sum25_tangrangCC02 <- subset_data_by_time_and_date("TangRang", "CC02", "20250606", "20250609", "000000", "235999")        
    #fix oda's QBR in 2025
    sum25_odaCI09 <- sum25_odaCI09 %>%
      mutate(QBR_Score = 40)
  # Ensure that subset from 2025 is given 1 in 5 minute subsetting
  summer25_cont <- list(sum25_tachCI01, sum25_araiCI10, sum25_odaCI09, sum25_tangrangCC02)
  summer25_cont_1in5 <- lapply(summer25_cont, subset_one_in_five)
    
# Add the 2025 1 in 5 data to the 2023-2024 continuous
cont_2023_2025_singleD_list <- list(continuous_list, summer25_cont_1in5)
  
# Write Dataset csv  
cont_2023_2025_singleDataset <- bind_rows(cont_2023_2025_singleD_list)
write.csv(cont_2023_2025_singleDataset, "clean_data/datasets/indices_datasets/continuous_data.csv", row.names = FALSE)



#### Multiple Device Verification Dataset -----------------------------------------------------------------------------

# Ta Chey Hill Forest - loaded 2 days for this subset. Full Days
mult_tachhillCI07 <- subset_data_by_time_and_date("TaCheyHill", "CI07", "20250609", "20250610", "000000", "235999")
mult_tachhillCC05 <- subset_data_by_time_and_date("TaCheyHill", "CC05", "20250609", "20250610", "000000", "235999")
mult_tachhillCI12 <- subset_data_by_time_and_date("TaCheyHill", "CI12", "20250609", "20250610", "000000", "235999")

# Ta Chey 100
mult_tachCI01 <- subset_data_by_time_and_date("TaChey", "CI01", "20250608", "20250610", "000000", "235999")
mult_tachCI13 <- subset_data_by_time_and_date("TaChey", "CI13", "20250608", "20250610", "000000", "235999")
mult_tachCC13 <- subset_data_by_time_and_date("TaChey", "CC13", "20250608", "20250610", "000000", "235999")

# Arai
mult_araiCI10 <- subset_data_by_time_and_date("Arai", "CI10", "20250608", "20250610", "000000", "235999")
mult_araiCI15 <- subset_data_by_time_and_date("Arai", "CI15", "20250608", "20250610", "000000", "235999")
mult_araiCC06 <- subset_data_by_time_and_date("Arai", "CC06", "20250608", "20250610", "000000", "235999")

# Oda
mult_odaCI09 <- subset_data_by_time_and_date("Oda", "CI09", "20250608", "20250610", "000000", "235999")
mult_odaCI19 <- subset_data_by_time_and_date("Oda", "CI19", "20250608", "20250610", "000000", "235999")
mult_odaCC07 <- subset_data_by_time_and_date("Oda", "CC07", "20250608", "20250610", "000000", "235999")
  #fix oda's QBR in 2025
  mult_odaCI09 <- mult_odaCI09 %>% mutate(QBR_Score = 40)
  mult_odaCI19 <- mult_odaCI19 %>% mutate(QBR_Score = 40)
  mult_odaCC07 <- mult_odaCC07 %>% mutate(QBR_Score = 40)

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


# Combine Dataframe for Multi-Device Susbset
multi_full_list <- list(mult_tachhillCI07, mult_tachhillCC05, mult_tachhillCI12,
                          mult_tachCI01, mult_tachCI13, mult_tachCC13,
                          mult_araiCI10, mult_araiCI15, mult_araiCC06, 
                          mult_odaCI09, mult_odaCI19, mult_odaCC07,
                          mult_batsaCI07, mult_batsaCI01, mult_batsaCC05, 
                          mult_tasayCI19, mult_tasayCI09, mult_tasayCI15, 
                          mult_kronCI13, mult_kronCI10)

multi_data <- bind_rows(multi_full_list)


write.csv(multi_data, "clean_data/datasets/indices_datasets/multi25_data.csv", row.names = FALSE)



# Subset 1 in 5 minutes per day for this subset
  # Apply to each element in the list
  multi_1in5_list<- lapply(multi_full_list, subset_one_in_five)
  multi_1in5_data <- bind_rows(multi_1in5_list)
          
  # Check the size difference
  nrow(multi_data)
  nrow(multi_1in5_data)
          
  write.csv(multi_1in5_data, "clean_data/datasets/indices_datasets/multi25_1in5_data.csv", row.names = FALSE)
  
  
          
### Combined Full Riparia Dataset (Global) ------------------------------------------------


# Ta Chey (Done)

novdec_TachCI15 <- subset_data_by_time_and_date("TaChey", "CI15", "20231117", "20231220", "000000", "235999")
    ## This one is a full day recording - needs subsetting into 1in5 
      # Use the function found below in 2025 subset
    nov_TachCI15 <- subset_one_in_five(nov_TachCI15)
    
jan_tachCI10 <- subset_data_by_time_and_date("TaChey", "CI10", "20240111", "20240121", "000000", "235999")
jan_TachCC01 <- subset_data_by_time_and_date("TaChey", "CC01", "20240111", "20240120", "000000", "235999")

apr_TachCI10 <- subset_data_by_time_and_date("TaChey", "CI10", "20240404", "20240414", "000000", "235999")
apr_TachCC01 <- subset_data_by_time_and_date("TaChey", "CC01", "20240404", "20240414", "000000", "235999")

jun_TachCI10 <- subset_data_by_time_and_date("TaChey", "CI10", "20240609", "20240619", "000000", "235999")
jun_TachCC01 <- subset_data_by_time_and_date("TaChey", "CC01", "20240609", "20240619", "000000", "235999")

# Arai (Done)

nov_AraiCI02 <- subset_data_by_time_and_date("Arai","CI02", "20231122", "20231202", "000000", "235999")
nov_AraiCI14 <- subset_data_by_time_and_date("Arai","CI14", "20231122", "20231202", "000000", "235999")

jan_AraiCI02 <- subset_data_by_time_and_date("Arai","CI02", "20240112", "20240121", "000000", "235999")
dec_AraiCI14 <- subset_data_by_time_and_date("Arai","CI14", "20231209", "20240121", "000000", "235999")

apr_AraiCI02 <- subset_data_by_time_and_date("Arai","CI02", "20240405", "20240415", "000000", "235999")

jun_AraiCI02 <- subset_data_by_time_and_date("Arai","CI02", "20240612", "20240627", "000000", "235999")



# Oda (Done) 

nov_OdaCI01 <- subset_data_by_time_and_date("Oda", "CI01", "20231122", "20231202", "000000", "235999")
nov_OdaCI13 <- subset_data_by_time_and_date("Oda", "CI13", "20231122", "20231202", "000000", "235999")

jan_OdaCI01 <- subset_data_by_time_and_date("Oda", "CI01", "20240112", "20240207", "000000", "235999")
jan_OdaCI13 <- subset_data_by_time_and_date("Oda", "CI13", "20240112", "20240207", "000000", "235999")

apr_OdaCI01 <- subset_data_by_time_and_date("Oda", "CI01", "20240405", "20240416", "000000", "235999")
apr_OdaCI13 <- subset_data_by_time_and_date("Oda", "CI13", "20240405", "20240415", "000000", "235999")

jun_OdaCI01 <- subset_data_by_time_and_date("Oda", "CI01", "20240609", "20240706", "000000", "235999")
jun_OdaCI13 <- subset_data_by_time_and_date("Oda", "CI01", "20240609", "20240706", "000000", "235999")



# Knaong Bat Sa (Done)

jan_BatCI05 <- subset_data_by_time_and_date("KnaongBatSa", "CI05", "20240117", "20240127", "000000", "235999")
jan_BatCI17 <- subset_data_by_time_and_date("KnaongBatSa", "CI17", "20240117", "20240127", "000000", "235999")

apr_BatCI17 <- subset_data_by_time_and_date("KnaongBatSa", "CI17", "20240408", "20240501", "000000", "235999")

jun_BatCI17 <- subset_data_by_time_and_date("KnaongBatSa", "CI17", "20240616", "20240626", "000000", "235999")

# Ta Say (Done)

jan_TasCC12 <- subset_data_by_time_and_date("TaSay", "CC12", "20240119", "20240128", "000000", "235999")
jan_TasCI12 <- subset_data_by_time_and_date("TaSay", "CI12", "20240119", "20240129", "000000", "235999")

apr_TasCI12 <- subset_data_by_time_and_date("TaSay", "CI12", "20240407", "20240504", "000000", "235999")

jun_TasCI12 <- subset_data_by_time_and_date("TaSay", "CI12", "20240616", "20240626", "000000", "235999")


# Kronomh (Done)

apr_KroCI19 <- subset_data_by_time_and_date("Kronomh", "CI19", "20240406", "20240416", "000000", "235999")
apr_KroCI20 <- subset_data_by_time_and_date("Kronomh", "CI20", "20240406", "20240417", "000000", "235999")


jun_KroCI19 <- subset_data_by_time_and_date("Kronomh", "CI19", "20240614", "20240625", "000000", "235999")
jun_KroCI20 <- subset_data_by_time_and_date("Kronomh", "CI20", "20240614", "20240625", "000000", "235999")


# Dam Five (Done) 
jan_DamCC11 <- subset_data_by_time_and_date("DamFive", "CC11", "20240113", "20240122", "000000", "235999")

apr_DamCC11 <- subset_data_by_time_and_date("DamFive", "CC11", "20240405", "20240428", "000000", "235999")

jun_DamCC11 <- subset_data_by_time_and_date("DamFive", "CC11", "20240614", "20240623", "000000", "235999")

# Tang Rang Bridge (Done)
nov_TRangCI07 <- subset_data_by_time_and_date("TangRang" ,"CI07", "20231123", "20231203", "000000", "235999")
jan_TRangCI07 <- subset_data_by_time_and_date("TangRang" ,"CI07", "20240110", "20240205", "000000", "235999")
apr_TRangCI07 <- subset_data_by_time_and_date("TangRang" ,"CI07", "20240405", "20240414", "000000", "235999")
jun_TRangCI07 <- subset_data_by_time_and_date("TangRang" ,"CI07", "20240608", "20240705", "000000", "235999")

# Pursat
nov_PurCI09 <- subset_data_by_time_and_date("PursatTown", "CI09", "20231123", "20231202", "000000", "235999")
jan_PurCI09 <- subset_data_by_time_and_date("PursatTown", "CI09", "20240120", "20240129", "000000", "235999")
apr_PurCI09 <- subset_data_by_time_and_date("PursatTown", "CI09", "20240404", "20240413", "000000", "235999")
jun_PurCI09 <- subset_data_by_time_and_date("PursatTown", "CI09", "20240614", "20240623", "000000", "235999")


# Combine Dataframe for Global Multi-Device Susbset
global_list <- list(nov_TachCI15, jan_tachCI10, jan_TachCC01, apr_TachCI10, apr_TachCC01, jun_TachCI10, jun_TachCC01,
                            nov_AraiCI02, nov_AraiCI14, dec_AraiCI14, jan_AraiCI02, apr_AraiCI02, jun_AraiCI02,
                            nov_OdaCI01, nov_OdaCI13, jan_OdaCI01, jan_OdaCI13, apr_OdaCI01, apr_OdaCI13, jun_OdaCI01, jun_OdaCI13,
                            jan_BatCI05, jan_BatCI17, apr_BatCI17, jun_BatCI17,
                            jan_TasCC12, jan_TasCI12, apr_TasCI12, jun_TasCI12,
                            apr_KroCI19, apr_KroCI20, jun_KroCI19, jun_KroCI20,
                            jan_DamCC11, apr_DamCC11, jun_DamCC11,
                            nov_TRangCI07,jan_TRangCI07, apr_TRangCI07, jun_TRangCI07,
                            jan_PurCI09, jan_PurCI09, apr_PurCI09, jun_PurCI09)

global_data <- bind_rows(global_list)

# Add Summer 2025 for global dataset
### Adding 2025
sum25_tachhillCI07 <- subset_data_by_time_and_date("TaCheyHill", "CI07", "20250608", "20250611", "000000", "235999")
sum25_tachhillCC05 <- subset_data_by_time_and_date("TaCheyHill", "CC05", "20250608", "20250611", "000000", "235999")
sum25_tachhillCI12 <- subset_data_by_time_and_date("TaCheyHill", "CI12", "20250608", "20250611", "000000", "235999")

sum25_tachCI01 <- subset_data_by_time_and_date("TaChey", "CI01", "20250608", "20250612", "000000", "235999")
sum25_tachCI13 <- subset_data_by_time_and_date("TaChey", "CI13", "20250608", "20250612", "000000", "235999")
sum25_tachCC13 <- subset_data_by_time_and_date("TaChey", "CC13", "20250608", "20250611", "000000", "235999")

sum25_araiCI10 <- subset_data_by_time_and_date("Arai", "CI10", "20250608", "20250612", "000000", "235999")  
sum25_araiCI15 <- subset_data_by_time_and_date("Arai", "CI15", "20250608", "20250612", "000000", "235999")          
sum25_araiCC06 <- subset_data_by_time_and_date("Arai", "CC06", "20250608", "20250612", "000000", "235999")          

sum25_odaCI09 <- subset_data_by_time_and_date("Oda", "CI09", "20250607", "20250611", "000000", "235999")
sum25_odaCI19 <- subset_data_by_time_and_date("Oda", "CI19", "20250607", "20250611", "000000", "235999")
sum25_odaCC07 <- subset_data_by_time_and_date("Oda", "CC07", "20250607", "20250610", "000000", "120000")
  #fix oda's QBR in 2025
sum25_odaCI09 <- sum25_odaCI09 %>% mutate(QBR_Score = 40)
sum25_odaCI19 <- sum25_odaCI19 %>% mutate(QBR_Score = 40)
sum25_odaCC07 <- sum25_odaCC07 %>% mutate(QBR_Score = 40)

# Bat Sa
mult_batsaCI07 <- subset_data_by_time_and_date("KnaongBatSa", "CI07", "20250704", "20250708", "000000", "235999")
mult_batsaCI01 <- subset_data_by_time_and_date("KnaongBatSa", "CI01", "20250704", "20250708", "000000", "235999")
mult_batsaCC05 <- subset_data_by_time_and_date("KnaongBatSa", "CC05", "20250704", "20250707", "000000", "235999")

# Ta Say
mult_tasayCI19 <- subset_data_by_time_and_date("TaSay", "CI19", "20250705", "20250709", "000000", "235999")
mult_tasayCI09 <- subset_data_by_time_and_date("TaSay", "CI09", "20250705", "20250709", "000000", "235999")
mult_tasayCI15 <- subset_data_by_time_and_date("TaSay", "CI15", "20250705", "20250709", "000000", "235999")

# Kronomh
mult_kronCI13 <- subset_data_by_time_and_date("Kronomh", "CI13", "20250702", "20250706", "000000", "235999")
mult_kronCI10 <- subset_data_by_time_and_date("Kronomh", "CI10", "20250702", "20250706", "000000", "235999")

# Tang Rang
sum25_tangrangCC02 <- subset_data_by_time_and_date("TangRang", "CC02", "20250606", "20250609", "000000", "235999")        

# Ensure that subset from 2025 is given 1 in 5 minute subsetting
summer25_global <- list(sum25_tachhillCI07, sum25_tachhillCC05, sum25_tachhillCI12, 
                      sum25_tachCI01, sum25_tachCI13, sum25_tachCC13,
                      sum25_araiCI10, sum25_araiCI15, sum25_araiCC06,
                      sum25_odaCI09, sum25_odaCI19, sum25_odaCC07,
                      mult_batsaCI07, mult_batsaCI01, mult_batsaCC05,
                      mult_tasayCI19, mult_tasayCI09, mult_tasayCI15,
                      mult_kronCI13, mult_kronCI10,
                      sum25_tangrangCC02)
                      
summer25_global_1in5 <- lapply(summer25_global, subset_one_in_five)

global_list_23to25 <- list(summer25_global_1in5, global_list)

global_data_23to25 <- bind_rows(global_list_23to25)

write.csv(global_data_23to25, "clean_data/datasets/indices_datasets/global2325_data.csv", row.names = FALSE)


### Songmeter Subset (2025) ----------------------------------------------------
sum25_SM_tachill <- subset_data_by_time_and_date("TaCheyHill", "SM3352", "20250608", "20250611", "000000", "235999")
sum25_SM_tach <- subset_data_by_time_and_date("TaChey", "SM3293", "20250608", "20250612", "000000", "235999")
sum25_SM_arai <- subset_data_by_time_and_date("Arai", "SM2967", "20250608", "20250612", "000000", "235999")

sum25_SM_ts <- subset_data_by_time_and_date("TaSay", "SM3293", "20250704", "20250709", "000000", "235999")
sum25_SM_krn <- subset_data_by_time_and_date("Kronomh", "SM3352", "20250701", "20250706", "000000", "235999")

sum25_songmeters <- list(sum25_SM_tachill, sum25_SM_tach, sum25_SM_arai, 
                         sum25_SM_ts, sum25_SM_krn)

sum25_songmeters_dataset <- bind_rows(sum25_songmeters)
head(sum25_songmeters_dataset)

write.csv(sum25_songmeters_dataset, "clean_data/datasets/indices_datasets/songmeters25_data.csv", row.names = FALSE)




