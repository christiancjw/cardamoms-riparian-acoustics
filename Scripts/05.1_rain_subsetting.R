library(dplyr)
library(stringr)
library(lubridate)

## Manual Rain Removal 
# to use this, list the rain periods by date (YYYYMMDD), and time selections in HH:MM:SS
# Go through LDFCS to find rain periods, then list them per site

# Function: Extract rain-related audio files for a specific site and device ---------------
# This works by finding filenames for timeperiods called to be masked
# List all site folders under the base path
extract_rain_files_by_date <- function(base_path, site, device, rain_periods) {
  
  # Capture unquoted variable names as strings
  site <- deparse(substitute(site))
  device <- deparse(substitute(device))
  
  # Create an empty data frame to store matching files
  result <- data.frame(Site = character(),
                       Device = character(),
                       Date = character(),
                       FileName = character(),
                       stringsAsFactors = FALSE)
  
  # Loop through each date provided in the rain_periods list
  for (date in names(rain_periods)) {    
    
    # Construct the full path to that day's folder
    folder_path <- file.path(base_path, site, device, date)
    
    # Check if that folder exists
    if (dir.exists(folder_path)) {
      
      # List all .wav files in the folder
      files <- list.files(folder_path, pattern = "\\.WAV$", full.names = TRUE)
      
      # Loop through each file in the folder
      for (file in files) {
        # Extract the time string from the end of the fileName (last 6 digits)
        time_str <- stringr::str_extract(file, "(?<=_)\\d{6}")
        
        print(paste("Checking file:", file, "— time:", time_str))
        
        # If we successfully extract a time...
        if (!is.na(time_str)) {
          
          # Convert that time string into a proper HH:MM:SS format
          time <- hms::as_hms(sprintf("%02d:%02d:%02d",
                                      as.integer(substr(time_str, 1, 2)),
                                      as.integer(substr(time_str, 3, 4)),
                                      as.integer(substr(time_str, 5, 6))))
          
          # Check if the time falls into any rain periods for this date
          in_range <- any(sapply(rain_periods[[date]], function(range) {
            hms::as_hms(range[1]) <= time & time <= hms::as_hms(range[2])
          }))
          
          # If the time is in range, save the file's info to the results table
          if (in_range) {
            result <- dplyr::bind_rows(result, data.frame(
              Site = site,
              Device = device,
              Date = date,
              FileName = basename(file),
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    } else {
      # If the folder doesn't exist, print a message
      message("Folder not found: ", folder_path)
    }
  }
  
  # Return the data frame (no CSV writing)
  return(result)
}

## Usage: site_device_rain <- extract_rain_files_by_date(base_path, 
#                                       site, device, rain_periods, 
#                                       output_csv = "clean_data/datasets/rain/__.csv")

# Set Base Path to Data ------------------------
raw_audio_files <- "/Volumes/CChing SSD/Acoustics/Pursat/2023-2024/"

# Variation for 2025 Data
raw_audio_files2025 <- "/Volumes/C Media SSD/2025 Acoustics/"


## Stung Sar: Tachey  ------------------------------------------------------------------------------
# Done 

# 2024 Devices:
"CI10" # Once April starts, heavy interference
"CC01" 

# List the time periods rain found in false colour long duration spectrograms
tachey_rain_periods <- list(
  "20240404" = list(c("14:45:00", "17:25:00")),
  "20240407" = list(c("16:45:00", "17:40:00")),
  "20240610" = list(c("06:25:00", "09:40:00")),
  "20240612" = list(c("14:20:00", "16:30:00")),
  "20240614" = list(c("16:00:00", "18:30:00")),
  "20240616" = list(c("16:50:00", "18:35:00")),
  "20240617" = list(c("14:45:00", "19:00:00"))
  )

tachey_CC01_interference <- list(
  "20240111" = list(c("10:55:00", "10:59:59"), c("13:45:00", "13:49:59"), c("15:40:00", "15:44:59")),
  "20240112" = list(c("09:00:00", "09:30:00"), c("13:05:00", "13:14:59"), c("16:15:00", "16:19:59")),
  "20240113" = list(c("11:10:00", "11:14:59"), c("14:45:00", "14:49:59")),
  "20240118" = list(c("16:20:00", "16:24:59"), c("17:00:00", "17:25:00"))
) # Lots of self noise from mic after 20240618

# Has some interference sporadically  # BIG POP 
tachey_CI10_interference <- list(
  "20240404" = list(c("00:00:00", "15:00:00")),
  "20240405" = list(c("09:00:00", "21:40:00")),
  "20240406" = list(c("09:30:00", "19:15:00")),
  "20240407" = list(c("15:25:00", "15:50:00"), c("16:35:00", "16:39:59")),
  "20240609" = list(c("00:10:00", "00:40:00"), c("11:50:00", "14:40:00")),
  "20240610" = list(c("00:40:00", "06:30:00"), c("18:00:00", "23:40:00")),
  "20240611" = list(c("08:00:00", "14:10:00"))
)

# write CSV
tachey_CI10_rain <- extract_rain_files_by_date(raw_audio_files, TaChey, CI10, tachey_rain_periods)
tachey_CI10_noise <- extract_rain_files_by_date(raw_audio_files, Tachey, CI10, tachey_CI10_interference)

tachey_CC01_rain <- extract_rain_files_by_date(raw_audio_files, Tachey, CC01, tachey_rain_periods)
tachey_CC01_noise <- extract_rain_files_by_date(raw_audio_files, Tachey, CC01, tachey_CC01_interference)


## Stung Sar: Arai ------------------------------------------------------------------------------
# Complete dataset finished

# 2024 Devices:
"CI02"
"CI14"

# List the time periods rain found in false colour long duration spectrograms
# Rain periods listed separately due to different natures of rain pick-up by the different devices

# CI02
arai_CI02_rain_periods <- list(
  "20231126" = list(c("17:15:00", "23:59:59")), 
  "20231127" = list(c("00:00:00", "03:00:00")), # Notably less rain pickup
  "20231130" = list(c("21:15:00", "21:25:01"), c("23:10:00", "23:20:01")),
  "20231201" = list(c("02:00:00", "03:00:00")), # Less rain again
  # Apr
  "20240407" = list(c("16:35:00", "17:30:00")),
  # June
  "20240614" = list(c("16:00:00", "18:49:00")),
  "20240616" = list(c("15:35:00", "21:10:00")),
  "20240617" = list(c("11:10:00", "12:00:00"), c("15:15:00", "23:30:00")), 
  "20240620" = list(c("12:55:00", "22:00:00")),
  "20240621" = list(c("15:35:00", "18:40:00")), #initial bit is insect interested in membrane
  "20240622" = list(c("11:55:00", "15:00:00"), c("21:50:00", "23:59:00")),
  "20240623" = list(c("00:00:00", "10:30:00"), c("18:35:00", "19:15:00"), c("22:30:00", "23:59:00")),
  "20240624" = list(c("00:00:00", "08:30:00"), c("10:15:00", "13:30:00"), c("19:45:00", "23:59:00")),
  "20240625" = list(c("09:00:00", "08:00:00"), c("12:15:00", "17:30:00")),
  "20240626" = list(c("19:00:00", "22:00:00"))
)

arai_CI14_rain_periods  <- list( # Stolen after March deployment ): no rain there anyway
  "20231126" = list(c("17:15:00", "23:59:59")),
  "20231127" = list(c("00:00:00", "08:00:00")), # Notably more rain pickup
  "20231130" = list(c("21:15:00", "21:25:01"), c("23:10:00", "23:20:01")),
  "20231201" = list(c("00:00:00", "06:30:00")), # Same here
  "20231221" = list(c("01:30:00", "03:00:00"))  # This day didn't get recorded on CI02 - bad batteries.
)

# write CSV
arai_CI02_rain <- extract_rain_files_by_date(raw_audio_files, Arai, CI02, arai_CI02_rain_periods)

arai_CI14_rain <- extract_rain_files_by_date(raw_audio_files, Arai, CI14, arai_CI14_rain_periods)

## Stung Sar: Stung Oda  ------------------------------------------------------------------------------
# Dataset Complete

# 2024 Devices:
"CI01" ## substantial masking from river
"CI13" 

# List the time periods rain found in false colour long duration spectrograms
oda_rain_periods <- list(
  "20231126" = list(c("15:40:00", "23:59:59")),
  "20231127" = list(c("00:00:00", "04:00:00")),
  "20231130" = list(c("21:10:00", "23:59:59")),  
  "20231201" = list(c("00:00:00", "02:10:00")),  
  "20231202" = list(c("05:00:00", "07:10:00")),
  "20240206" = list(c("19:00:00", "23:59:59")),
  "20240207" = list(c("00:00:00", "07:00:00"), c("17:35:00", "18:25:00")),
  "20240206" = list(c("19:00:00", "23:59:59")),
  "20240407" = list(c("16:25:00", "17:30:00")),
  "20240610" = list(c("07:00:00", "09:15:00")),
  "20240614" = list(c("15:55:00", "17:05:00")),
  "20240616" = list(c("15:55:00", "20:35:00")),
  "20240617" = list(c("11:55:00", "12:00:00"), c("15:35:00", "18:25:00")),
  "20240619" = list(c("11:00:00", "11:25:00")),
  "20240620" = list(c("13:05:00", "15:25:00"), c("16:55:00", "18:35:00")),
  "20240622" = list(c("12:45:00", "14:10:00"), c("21:45:00", "23:59:00")),
  "20240623" = list(c("00:00:00", "06:15:00"), c("11:25:00", "19:25:00")),
  "20240624" = list(c("00:00:00", "07:00:00"), c("09:20:00", "13:00:00"), c("19:15:00", "23:59:59")),
  "20240625" = list(c("00:00:00", "03:35:00"), c("14:05:00", "16:15:00")),
  "20240626" = list(c("03:45:00", "05:00:00"), c("16:40:00", "23:59:59")),
  "20240627" = list(c("00:00:00", "05:00:00")),
  "20240628" = list(c("13:10:00", "13:35:00")), 
  "20240629" = list(c("12:50:00", "17:15:00"), c("18:45:00", "20:20:00")), #DONE til here
  "20240630" = list(c("00:00:00", "05:00:00"), c("12:40:00", "14:05:00")),
  "20240701" = list(c("21:40:00", "22:30:00")),
  "20240702" = list(c("14:00:00", "17:05:00")),
  "20240703" = list(c("13:45:00", "17:00:00")),
  "20240704" = list(c("12:50:00", "23:59:00")),
  "20240705" = list(c("00:00:00", "01:00:00"), c("12:45:00", "18:00:00")),
  "20240706" = list(c("14:40:00", "15:55:00"), c("18:30:00", "19:15:00"))
)

oda_CI13_scratching <- list(
  "20231124" = list(c("12:00:00", "12:30:00")),
  "20231125" = list(c("11:10:00", "11:25:00"), c("14:50:00", "14:54:00")), # Second one is definitely a mmammal scratching away
  "20231126" = list(c("10:15:00", "11:45:00")),
  "20240609" = list(c("11:55:00", "13:00:00")),
  "20240610" = list(c("06:45:00", "07:00:00"))
)  

oda_CI13_insect_interference <- list(
  "20240405" = list(c("18:45:00", "21:25:00")),
  "20240406" = list(c("01:10:00", "01:30:00"), c("19:00:00", "19:15:00")),
  "20240408" = list(c("18:55:00", "23:59:59")),
  "20240409" = list(c("19:00:00", "20:30:00"), c("22:00:00", "23:59:59")),
  "20240410" = list(c("00:00:00", "02:00:00"), c("19:00:00", "22:30:00"))
)

# write CSVs
oda_CI01_rain <- extract_rain_files_by_date(raw_audio_files, Oda, CI01, oda_rain_periods)

oda_CI13_rain <- extract_rain_files_by_date(raw_audio_files, Oda, CI13, oda_rain_periods)

oda_CI13_interference <- extract_rain_files_by_date(raw_audio_files, Oda, CI13, oda_CI13_scratching)

## Stung Ta Say: KnaongBatSa  ------------------------------------------------------------------------------
# Complete dataset finished

# 2024 Devices:
"CI05"
"CI17"

# List the time periods rain found in false colour long duration spectrograms
KnaongBatSa_rain_periods <- list(
  "20240417" = list(c("17:10:00", "17:35:00")), 
  "20240419" = list(c("16:05:00", "16:15:00")), 
  "20240617" = list(c("10:35:00", "11:05:00"), c("14:55:00", "17:35:00")), #lots of masking via stream geophony 
  "20240618" = list(c("14:25:00", "14:35:00")),
  "20240619" = list(c("13:35:00", "15:25:00"), c("17:35:00", "18:50:00")),
  "20240620" = list(c("11:10:00", "13:05:00")), # So much more masking here now
  "20240622" = list(c("20:30:00", "23:59:59")),
  "20240623" = list(c("00:00:00", "05:00:00"), c("10:30:00", "10:40:00"), c("12:15:00", "13:05:00")),
  "20240624" = list(c("01:15:00", "03:10:00"), c("07:45:00", "13:40:00"), c("19:30:00", "22:15:00"),
                    c("23:40:00", "23:59:59")),
  "20240625" = list(c("00:00:00", "06:15:00"), c("12:20:00", "13:00:00"), c("15:30:00", "18:15:00")),
  "20240625" = list(c("15:25:00", "17:00:00"), c("19:00:00", "21:00:00"), c("22:30:00", "23:00:00"))
)

# write CSV
knaongbatsa_CI05_rain <- extract_rain_files_by_date(raw_audio_files, KnaongBatSa, CI05, KnaongBatSa_rain_periods)


knaongbatsa_CI17_rain <- extract_rain_files_by_date(raw_audio_files, KnaongBatSa, CI17, KnaongBatSa_rain_periods)

## Stung Ta Say: Ta Say   ------------------------------------------------------------------------------

# 2024 Devices
"CI12" # This should be used for Apr & June
"CC12" # Use this for Jan - CI12 is too close to river so lots masked

# List the time periods rain found in false colour long duration spectrograms
tasay_rain_periods <- list(
  "20240122" = list(c("21:15:00", "22:50:00")),
  "20240421" = list(c("11:05:00", "13:10:00")),
  "20240504" = list(c("12:30:00", "15:20:00")),
  "20240616" = list(c("17:20:00", "19:00:00")),
  "20240617" = list(c("11:10:00", "11:20:00"), c("15:30:00", "18:25:00")),
  "20240619" = list(c("14:00:00", "14:35:00"), c("17:50:00", "18:45:00")),
  "20240621" = list(c("17:15:00", "18:00:00")),
  "20240622" = list(c("13:00:00", "14:10:00"), c("21:30:00", "23:59:59")),
  "20240623" = list(c("00:00:00", "06:00:00"), c("08:10:00", "08:55:00"), c("22:20:00", "23:59:59")),
  "20240624" = list(c("00:00:00", "03:00:00"), c("07:10:00", "09:00:00"), c("19:20:00", "23:59:59")),
  "20240625" = list(c("00:00:00", "04:00:00"), c("15:20:00", "18:30:00")),
  "20240626" = list(c("05:00:00", "07:00:00"), c("14:55:00", "16:30:00"))
)

# write CSV
tasay_CI12_rain <- extract_rain_files_by_date(raw_audio_files, TaSay, CI12, tasay_rain_periods)

tasay_CC12_rain <- extract_rain_files_by_date(raw_audio_files, TaSay, CC12, tasay_rain_periods)

## Stung Ta Say: Kronomh Village  ------------------------------------------------------------------------------
# Complete dataset finished

kronomh <- "Kronomh"
kronomhCI19 <- "CI19"
kronomhCI20 <- "CI20" # CI20 has auditory masking from stream

# List the time periods rain found in false colour long duration spectrograms
# CI02
kronomh_rain_periods <- list(
  "20240614" = list(c("16:45:00", "18:35:00")),
  "20240616" = list(c("14:55:00", "15:15:00"), c("18:30:00", "19:00:00")),
  "20240617" = list(c("11:30:00", "12:30:00")), 
  "20240618" = list(c("13:45:00", "14:20:00")), 
  "20240619" = list(c("15:40:00", "16:25:00")),
  "20240620" = list(c("13:45:00", "19:00:00")),
  "20240621" = list(c("11:55:00", "12:45:00"), c("17:00:00", "17:35:00")),
  "20240622" = list(c("12:10:00", "13:25:00"), c("22:00:00", "23:59:59")),
  "20240623" = list(c("01:20:00", "02:35:00"), c("16:30:00", "17:25:00"), c("23:05:00", "23:15:59")),
  "20240624" = list(c("00:20:00", "02:35:00"), c("06:00:00", "07:25:00"), c("09:30:00", "12:00:00"), c("16:35:00",  "23:59:59")),
  "20240625" = list(c("00:00:00", "02:30:00"), c("14:45:00", "18:40:00"))
)

kronomhCI19_wonky_periods <- list( #CI19 Scratching Wave. Seems to occur at night via interference from an insect
  "20240411" = list(c("20:10:00", "23:59:00")),
  "20240412" = list(c("00:00:00", "03:30:00"), c("20:15:00", "23:59:59")),
  "20240413" = list(c("00:00:00", "02:40:00"), c("21:05:00", "23:59:59")),
  "20240414" = list(c("00:00:00", "02:15:00"), c("20:40:00", "23:59:59")),
  "20240415" = list(c("00:00:00", "02:25:00"), c("19:35:00", "23:59:59")),
  "20240416" = list(c("00:00:00", "02:50:00"), c("21:30:00", "23:59:59")),
  "20240616" = list(c("20:20:00", "23:45:00")), # transition from scratch to 'CLOK' sound
  "20240617" = list(c("11:10:00", "18:00:00")),
  "20240618" = list(c("01:50:00", "02:09:00")) # fluttering - most likely a termite alate
)


# write CSV
kronomh_CI19_rain <- extract_rain_files_by_date(raw_audio_files, Kronomh, CI19, kronomh_rain_periods)

kronomh_CI19_noise <- extract_rain_files_by_date(raw_audio_files, Kronomh, CI19, kronomhCI19_wonky_periods)

kronomh_CI20_rain <- extract_rain_files_by_date(raw_audio_files, Kronomh, CI20, kronomh_rain_periods)

## Stung Ta Say: Dam 5 ------------------------------------------------------------------------------
# Complete dataset finished

# 2024 Devices
"CC11"
"CI11"

# List the time periods rain found in false colour long duration spectrograms
# CI02
damfive_rain_periods <- list(
  "20240405" = list(c("17:45:00", "18:30:00")),
  "20240407" = list(c("16:30:00", "18:25:00")),
  "20240409" = list(c("16:20:00", "17:05:00")),
  "20240415" = list(c("16:50:00", "17:45:00")),
  "20240417" = list(c("15:10:00", "18:00:00")),
  "20240421" = list(c("16:50:00", "19:30:00")),
  "20240422" = list(c("12:25:00", "16:15:00")),
  "20240422" = list(c("12:30:00", "13:10:00")),
  "20240428" = list(c("14:00:00", "18:15:00")),
  "20240430" = list(c("14:00:00", "18:30:00")),
  "20240614" = list(c("15:55:00", "17:35:00")),
  "20240615" = list(c("14:35:00", "16:35:00")),
  "20240616" = list(c("16:25:00", "20:40:00")),
  "20240617" = list(c("15:25:00", "17:25:00")),
  "20240618" = list(c("13:50:00", "14:45:00")),
  "20240619" = list(c("11:20:00", "12:45:00"), c("15:05:00", "17:45:00"), c("21:20:00", "23:15:00")),
  "20240620" = list(c("13:25:00", "15:05:01"), c("16:10:00", "18:33:00")),
  "20240621" = list(c("12:45:00", "14:20:00")),
  "20240622" = list(c("12:45:00", "14:45:00"), c("16:45:00", "18:20:00"), c("22:45:00", "23:59:00")),
  "20240623" = list(c("00:00:00", "06:10:00")),
  "20240624" = list(c("00:45:00", "05:45:00"), c("19:25:00", "23:59:00"))
)

# write CSV
damfive_cc11_rain <- extract_rain_files_by_date(raw_audio_files, DamFive, CC11, damfive_rain_periods)

damfive_cI11_rain <- extract_rain_files_by_date(raw_audio_files, DamFive, CI11, damfive_rain_periods)

## Stung Pursat: Tang Rang   ------------------------------------------------------------------------------
# up to 20240624
tangrang <- "TangRang"
tangrangCI07 <- "CI07" 
tangrangCC07 <- "CC07" ## Huge amount of noise - very low pickup

# List the time periods rain found in false colour long duration spectrograms
tangrang_rain_periods <- list(
  "20231127" = list(c("01:20:00", "03:00:00"), c("04:45:00", "05:20:00")),
  "20231130" = list(c("23:00:00", "23:59:00")),
  "20231201" = list(c("00:00:00", "01:00:00")),
  "20240101" = list(c("15:10:00", "17:30:00")),
  "20240608" = list(c("14:10:00", "15:05:00")),
  "20240609" = list(c("12:10:00", "13:10:00")),
  "20240610" = list(c("07:25:00", "08:50:00")),
  "20240614" = list(c("15:40:00", "17:05:00")),
  "20240616" = list(c("15:45:00", "21:30:00")),
  "20240617" = list(c("15:10:00", "18:10:00")),
  "20240619" = list(c("16:05:00", "17:40:00")),
  "20240620" = list(c("13:20:00", "18:30:00")),
  "20240621" = list(c("12:00:00", "14:00:00")),
  "20240622" = list(c("13:00:00", "18:40:00"), c("21:20:00", "23:59:59")),
  "20240623" = list(c("00:00:00", "06:35:00"))
  
)

# CI 07 Has some interference sporadically  # BIG POP 
# I think a mammal came into the tree in june as well - loads of interference for that deployment
tangrang_CI07_interference <- list(
  "20240110" = list(c("00:00:00", "01:10:00"), c("09:00:00", "23:30:00")),
  "20240111" = list(c("11:00:00", "13:10:00"), c("16:20:00", "16:45:00"), c("18:20:00", "18:24:59")),
  "20240112" = list(c("11:40:00", "20:15:00")),
  "20240113" = list(c("11:40:00", "20:15:00")),
  "20240115" = list(c("11:40:00", "20:15:00"))
)


# write CSV
tangrang_CI07_rain <- extract_rain_files_by_date(raw_audio_files, TangRang, CI07, tangrang_rain_periods)


tangrang_CC07_rain <- extract_rain_files_by_date(raw_audio_files, TangRang, CC07, tangrang_rain_periods)


tangrang_CI07_noise <- extract_rain_files_by_date(raw_audio_files, TangRang, CI07, tangrang_CI07_interference)

## Stung Pursat: Kravanh Bridge  ------------------------------------------------------------------------------
# Complete dataset finished

kravanh <- "Kravanh Bridge"
kravanhCC08 <- "CC08"
kravanhCI08 <- "CI08"

# List the time periods rain found in false colour long duration spectrograms
# CI02
kravanh_rain_periods <- list(
  "20231127" = list(c("00:55:00", "03:30:00")), 
  "20231130" = list(c("21:35:00", "23:59:59")),
  "20231131" = list(c("00:00:00", "02:30:00"))
)

# write CSV
kravanh_CC08_rain <- extract_rain_files_by_date(raw_audio_files, Kravanh, CC08, kravanh_rain_periods)


kravanh_CI08_rain <- extract_rain_files_by_date(raw_audio_files, Kravanh, CI08, kravanh_rain_periods)




## Stung Pursat: Pursat Suburb  ------------------------------------------------------------------------------
# Done up to 2024 06 27
pursat <- "PursatTown"
pursatCC09 <- "CC09" ## Huge amount of noise
pursatCI09 <- "CI09" 

# List the time periods rain found in false colour long duration spectrograms
pursat_rain_periods <- list(
  "20231127" = list(c("01:20:00", "03:00:00"), c("04:45:00", "05:20:00")),
  "20231130" = list(c("21:00:00", "23:59:00")),
  "20231201" = list(c("00:00:00", "01:30:00")),
  "20240403" = list(c("15:45:00", "17:05:00")),
  "20240614" = list(c("16:25:00", "19:25:00")),
  "20240616" = list(c("18:10:00", "20:25:00")),
  "20240617" = list(c("16:05:00", "17:50:00")),
  "20240618" = list(c("20:00:00", "20:45:00")),
  "20240620" = list(c("14:10:00", "17:10:00")),
  "20240622" = list(c("13:30:00", "15:25:00")),
  "20240623" = list(c("02:40:00", "04:50:00"), c("09:35:00", "10:15:00")),
  "20240625" = list(c("03:55:00", "06:25:00"), c("13:35:00", "14:45:00")),
  "20240626" = list(c("19:20:00", "23:59:00")),
  "20240627" = list(c("00:00:00", "02:15:00"))
)

# write CSV
pursat_CC09_rain <- extract_rain_files_by_date(raw_audio_files, PursatTown, CC09, pursat_rain_periods)


pursat_CI09_rain <- extract_rain_files_by_date(raw_audio_files, PursatTown, CI09, pursat_rain_periods)



#### Global 2023 - 2024 Dataset Lists --------------------------------------------------------------------------------------
initialrainlist <- list(tachey_CI10_rain, tachey_CC01_rain, 
                 arai_CI02_rain , arai_CI14_rain, 
                 oda_CI01_rain, oda_CI13_rain,
                 knaongbatsa_CI05_rain, knaongbatsa_CI17_rain,
                 tasay_CI12_rain, tasay_CC12_rain,
                 kronomh_CI19_rain, kronomh_CI20_rain,
                 damfive_cc11_rain, damfive_cI11_rain,
                 tangrang_CI07_rain, tangrang_CC07_rain,
                 kravanh_CC08_rain, kravanh_CI08_rain,
                 pursat_CC09_rain, pursat_CI09_rain)

fullraindata <- bind_rows(initialrainlist)
head(fullraindata)

write.csv(fullraindata, "clean_data/datasets/interference/2324rain.csv", row.names = FALSE)

noiselist <- list(tachey_CI10_noise, tachey_CC01_noise, 
                  oda_CI13_interference,
                  kronomh_CI19_noise,
                  tangrang_CI07_noise)

noisedata <- bind_rows(noiselist)

write.csv(noisedata, "clean_data/datasets/interference/2324noise.csv", row.names = FALSE)


#### 2025 Summer Rains  --------------------------------------------------------------------------------------------
raw_audio_files <- "/Volumes/C Media SSD/2025 Acoustics/"

# Ta Chey Hill
# Devices
"CC05" # Clean and lovely recordings
"CI07" # Scratching evening of the 9th & some of the 10th
"CI12" # Small amount of scratching by insect / mammal
"SM3352" # Some scratching sporadically
# No rain within this dataset

tacheyhillC107_sum25_interference_periods <- list(
  "20250609" = list(c("19:15:00", "22:25:00"))
)
tacheyhillC107_interference <- extract_rain_files_by_date(raw_audio_files, TaCheyHill, CI07, tacheyhillC107_sum25_interference_periods)


# Ta Chey 
# Devices:
"CC13" # Great
"CI01" # Great
"CI13" # Great 
"SM3293" # Gr8

tachey_sum25_rain_periods <- list(
  "20250607" = list(c("14:45:00", "23:59:00")),
  "20250611" = list(c("10:30:00", "15:25:00")),
  "20250612" = list(c("12:20:00", "14:15:00"), c("16:30:00", "19:12:00"))
)

tacheyCI01_rain <- extract_rain_files_by_date(raw_audio_files, TaChey, CI01, tachey_sum25_rain_periods)

tacheyCI13_rain <- extract_rain_files_by_date(raw_audio_files, TaChey, CI13, tachey_sum25_rain_periods)

tacheyCC13_rain <- extract_rain_files_by_date(raw_audio_files, TaChey, CC13, tachey_sum25_rain_periods)

tacheySM3293_rain <- extract_rain_files_by_date(raw_audio_files, TaChey, SM3293, tachey_sum25_rain_periods)

# Arai
# Devices
"CC06" # Good
"CI15" # 
"CI10" # Good
"SM2967"

arai_sum25_rain_periods <- list(
  "20250607" = list(c("14:45:00", "23:59:00")),
  "20250608" = list(c("13:06:00", "13:24:00")),
  "20250611" = list(c("10:30:00", "15:05:00")),
  "20250612" = list(c("07:54:00", "08:30:00"), c("12:20:00", "14:15:00"), c("15:30:00", "17:30:00"))
)

araiCC06_rain <- extract_rain_files_by_date(raw_audio_files, Arai, CC06, arai_sum25_rain_periods)

araiCI15_rain <- extract_rain_files_by_date(raw_audio_files, Arai, CI15, arai_sum25_rain_periods)

araiCI10_rain <- extract_rain_files_by_date(raw_audio_files, Arai, CI10, arai_sum25_rain_periods)

araiSM2967_rain <- extract_rain_files_by_date(raw_audio_files, Arai, SM2967, arai_sum25_rain_periods)



# Oda
# Devices
"CC07"
"CI19"
"CI09"

oda_sum25_rain_periods <- list(
  "20250607" = list(c("14:35:00", "23:59:00")),
  "20250611" = list(c("10:30:00", "15:25:00"))
)

odaCC07_rain <- extract_rain_files_by_date(raw_audio_files, Oda, CC07, oda_sum25_rain_periods)

odaCI19_rain <- extract_rain_files_by_date(raw_audio_files, Oda, CI19, oda_sum25_rain_periods)

odaCI09_rain <- extract_rain_files_by_date(raw_audio_files, Oda, CI09, oda_sum25_rain_periods)


# Knaong Bat Sa
# Devices
"CC05"
"CI01"
"CI07"

kbs_sum25_rain_periods <- list(
  "20250703" = list(c("13:20:00", "14:00:00"), c("16:00:00", "18:30:00")),
  "20250705" = list(c("12:40:00", "14:00:00")),
  "20250706" = list(c("12:30:00", "13:30:00"))
)

kbsCC05_rain <- extract_rain_files_by_date(raw_audio_files, KnaongBatSa, CC05, kbs_sum25_rain_periods)

kbsCI01_rain <- extract_rain_files_by_date(raw_audio_files, KnaongBatSa, CI01, kbs_sum25_rain_periods)

kbsCI07_rain <- extract_rain_files_by_date(raw_audio_files, KnaongBatSa, CI07, kbs_sum25_rain_periods)

# Ta Say
# Devices
"CI19"
"CI09"
"CI15"
"SM3293"

ts_sum25_rain_periods <- list(
  "20250704" = list(c("14:10:00", "14:00:00")),
  "20250705" = list(c("13:30:00", "15:45:00")),
  "20250706" = list(c("12:35:00", "14:30:00")),
  "20250707" = list(c("13:35:00", "15:30:00")),
  "20250708" = list(c("10:45:00", "23:59:00"))
)

tsCI19_rain <- extract_rain_files_by_date(raw_audio_files, TaSay, CI19, ts_sum25_rain_periods)

tsCI09_rain <- extract_rain_files_by_date(raw_audio_files, TaSay, CI09, ts_sum25_rain_periods)

tsCI15_rain <- extract_rain_files_by_date(raw_audio_files, TaSay, CI15, ts_sum25_rain_periods)

tsSM3293_rain <- extract_rain_files_by_date(raw_audio_files, TaSay, SM3293, ts_sum25_rain_periods)

# Kronomh
# Devices
"CI13"
"CI10"
"SM3352"

krn_sum25_rain_periods <- list(
  "20250702" = list(c("00:00:00", "05:30:00"), c("15:45:00", "23:30:00")),
  "20250703" = list(c("17:10:00", "19:00:00")),
  "20250704" = list(c("14:20:00", "16:25:00")),
  "20250705" = list(c("12:00:00", "14:45:00")),
  "20250706" = list(c("12:45:00", "13:00:00"), c("14:00:00", "16:35:00"))
)

krnCI13_rain <- extract_rain_files_by_date(raw_audio_files, Kronomh, CI13, krn_sum25_rain_periods)

krnCI10_rain <- extract_rain_files_by_date(raw_audio_files, Kronomh, CI10, krn_sum25_rain_periods)

krnSM3352_rain <- extract_rain_files_by_date(raw_audio_files, Kronomh, SM3352, krn_sum25_rain_periods)

# Tang Rang 
# Devices
trCC01 <- "CC01"
trCC02 <- "CC02"
trCI17 <- "CI17"

tr_sum25_rain_periods <- list(
  "20250604" = list(c("14:10:00", "14:00:00")),
  "20250605" = list(c("13:30:00", "15:45:00")),
  "20250606" = list(c("12:35:00", "14:30:00")),
  "20250607" = list(c("13:35:00", "15:30:00")),
  "20250608" = list(c("10:45:00", "23:59:00"))
)


trCC01_rain <- extract_rain_files_by_date(raw_audio_files, TangRang, CC01, tr_sum25_rain_periods)
trCC02_rain <- extract_rain_files_by_date(raw_audio_files, TangRang, CC02, tr_sum25_rain_periods)
trCI17_rain <- extract_rain_files_by_date(raw_audio_files, TangRang, CI17, tr_sum25_rain_periods)


# Summer 2025 Full
rainlist2025 <- list(
                     tacheyCI01_rain, tacheyCI13_rain, tacheyCC13_rain, tacheySM3293_rain,
                     araiCC06_rain, araiCI15_rain, araiCI10_rain, araiSM2967_rain,
                     odaCC07_rain, odaCI19_rain, odaCI09_rain,
                     kbsCC05_rain, kbsCI01_rain, kbsCI07_rain,
                     tsCI19_rain, tsCI09_rain, tsCI15_rain, tsSM3293_rain, 
                     krnCI13_rain, krnCI10_rain, krnSM3352_rain,
                     trCC01_rain, trCC02_rain, trCI17_rain)

fullraindata2025 <- bind_rows(rainlist2025)

head(fullraindata2025)

write.csv(fullraindata2025, "clean_data/datasets/interference/rain2025.csv", row.names = FALSE)


# Global 2023 - 2025 Rainfall and Interference Data ------------------------------------

globalinterferencelist20232025 <- list(tachey_CI10_rain, tachey_CC01_rain, 
                                        arai_CI02_rain , arai_CI14_rain, 
                                        oda_CI01_rain, oda_CI13_rain,
                                        knaongbatsa_CI05_rain, knaongbatsa_CI17_rain,
                                        tasay_CI12_rain, tasay_CC12_rain,
                                        kronomh_CI19_rain, kronomh_CI20_rain,
                                        damfive_cc11_rain, damfive_cI11_rain,
                                        tangrang_CI07_rain, tangrang_CC07_rain,
                                        kravanh_CC08_rain, kravanh_CI08_rain,
                                        pursat_CC09_rain, pursat_CI09_rain, tachey_CI10_noise, tachey_CC01_noise,
                                        oda_CI13_interference, kronomh_CI19_noise, tangrang_CI07_noise,
                                        tacheyhillC107_interference,
                                        tacheyCI01_rain, tacheyCI13_rain, tacheyCC13_rain, tacheySM3293_rain,
                                        araiCC06_rain, araiCI15_rain, araiCI10_rain, araiSM2967_rain,
                                        odaCC07_rain, odaCI19_rain, odaCI09_rain,
                                        kbsCC05_rain, kbsCI01_rain, kbsCI07_rain,
                                        tsCI19_rain, tsCI09_rain, tsCI15_rain, tsSM3293_rain, 
                                        krnCI13_rain, krnCI10_rain, krnSM3352_rain,
                                        trCC01_rain, trCC02_rain, trCI17_rain)

fullinterferencedata <- bind_rows(globalinterferencelist20232025)

write.csv(fullinterferencedata, "clean_data/datasets/interference/global_interference.csv", row.names = FALSE)
