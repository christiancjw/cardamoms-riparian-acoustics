library(dplyr)
library(stringr)
library(lubridate)

## Manual Rain Removal 

# Function: Extract rain-related audio files for a specific site and device ---------------
# This works by finding filenames for timeperiods called to be masked
extract_rain_files_by_date <- function(base_path, site, device, rain_periods, 
                                       output_csv = "rain_files.csv") {
  
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
        time_str <- str_extract(file, "(?<=_)\\d{6}")
        
        
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
            result <- bind_rows(result, data.frame(
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
  
  write.csv(result, output_csv, row.names = FALSE)
  message("CSV written to: ", output_csv)
  # Also return the data getframe in case the user wants to use it in R
  return(result)
}

## Usage: site_device_rain <- extract_rain_files_by_date(base_path, 
#                                       site, device, rain_periods, 
#                                       output_csv = "clean_data/datasets/rain/__.csv")

# Set Base Path to Data ------------------------
raw_audio_files <- "/Volumes/CChing SSD/Acoustics/Pursat/2023-2024/"

## Stung Sar: Tachey  ------------------------------------------------------------------------------
# Done 

tachey <- "TaChey"
tacheyCI10 <- "CI10" # Once April starts, heavy interference
tacheyCC01 <- "CC01" 

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
tachey_CI10_rain <- extract_rain_files_by_date(raw_audio_files, tachey, tacheyCI10, tachey_rain_periods, 
                                               output_csv = "clean_data/datasets/rain/rain_tachey_CI10.csv")


tachey_CC01_rain <- extract_rain_files_by_date(raw_audio_files, tachey, tacheyCC01, tachey_rain_periods, 
                                               output_csv = "clean_data/datasets/rain/rain_tachey_CC01.csv")

tachey_CC01_noise <- extract_rain_files_by_date(raw_audio_files, tachey, tacheyCC01, tachey_CI10_interference, 
                                               output_csv = "clean_data/datasets/rain/noise_tachey_CC01.csv")

## Stung Sar: Arai ------------------------------------------------------------------------------
# Complete dataset finished

arai <- "Arai"
ARAICI02 <- "CI02"
ARAICI14 <- "CI14"

# List the time periods rain found in false colour long duration spectrograms
# CI02
arai_CI02_rain_periods <- list(
  "20231126" = list(c("17:15:00", "10:30:00")),
  "20231127" = list(c("00:00:00", "03:00:00")),
  "20231130" = list(c("21:15:00", "21:25:01"), c("23:10:00", "23:20:01")),
  "20231201" = list(c("02:00:00", "03:00:00")),
  "20240407" = list(c("16:35:00", "17:30:00")),
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

arai_CI14_rain_periods  <- list(
  "20231126" = list(c("17:15:00", "23:59:59")),
  "20231127" = list(c("00:00:00", "08:00:00")),
  "20231130" = list(c("21:15:00", "21:25:01"), c("23:10:00", "23:20:01")),
  "20231201" = list(c("00:00:00", "06:30:00")),
  "20231221" = list(c("01:30:00", "03:00:00"))
)


# write CSV
arai_CI02_rain <- extract_rain_files_by_date(raw_audio_files, arai, ARAICI02, arai_CI02_rain_periods, 
                                         output_csv = "clean_data/datasets/rain/rain_arai_CI02.csv")


arai_CI14_rain <- extract_rain_files_by_date(raw_audio_files, arai, ARAICI14, arai_CI14_rain_periods, 
                                             output_csv = "clean_data/datasets/rain/rain_arai_CI15.csv")

## Stung Sar: Stung Oda  ------------------------------------------------------------------------------
# Dataset Complete
oda <- "Oda"
odaCI01 <- "CI01" ## substantial masking from river
odaCI13 <- "CI13" 

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

oda_odaCI13_scratchy <- list(
  "20231124" = list(c("12:00:00", "12:30:00")),
  "20231125" = list(c("11:10:00", "11:25:00"), c("14:50:00", "14:54:00")), # Second one is definitely a mmammal scratching away
  "20231126" = list(c("10:15:00", "11:45:00")),
  "20240609" = list(c("11:55:00", "13:00:00")),
  "20240610" = list(c("06:45:00", "07:00:00"))
)  

oda_odaCI13_insect_interference <- list(
  "20240405" = list(c("18:45:00", "21:25:00")),
  "20240406" = list(c("01:10:00", "01:30:00"), c("19:00:00", "19:15:00")),
  "20240408" = list(c("18:55:00", "23:59:59")),
  "20240409" = list(c("19:00:00", "20:30:00"), c("22:00:00", "23:59:59")),
  "20240410" = list(c("00:00:00", "02:00:00"), c("19:00:00", "22:30:00"))
)



# write CSV
oda_CI01_rain <- extract_rain_files_by_date(raw_audio_files, oda, odaCI01, oda_rain_periods, 
                                            output_csv = "clean_data/datasets/rain/rain_oda_CI01.csv")


oda_CI13_rain <- extract_rain_files_by_date(raw_audio_files, oda, odaCI13, oda_rain_periods, 
                                            output_csv = "clean_data/datasets/rain/rain_pursat_CI09.csv")


## Stung Ta Say: KnaongBatSa  ------------------------------------------------------------------------------
# Complete dataset finished

KnaongBatSa <- "KnaongBatSa"
KnaongBatSaCI05 <- "CI05"
KnaongBatSaCI17 <- "CI17"

# List the time periods rain found in false colour long duration spectrograms
# CI02
KnaongBatSa_rain_periods <- list(
  "20240417" = list(c("17:10:00", "17:35:00")), 
  "20240419" = list(c("16:05:00", "16:15:00")), 
  "20240617" = list(c("10:35:00", "11:05:00"), c("14:55:00", "17:35:00")), #lots of masking via stream geophony 
  "20240618" = list(c("14:25:00", "14:35:00")),
  "20240619" = list(c("13:35:00", "15:25:00"), c("17:35:00", "18:50:00")),
  "20240620" = list(c("11:10:00", "13:05:00")), #So much more masking here now
  "20240622" = list(c("20:30:00", "23:59:59")),
  "20240623" = list(c("00:00:00", "05:00:00"), c("10:30:00", "10:40:00"), c("12:15:00", "13:05:00")),
  "20240624" = list(c("01:15:00", "03:10:00"), c("07:45:00", "13:40:00"), c("19:30:00", "22:15:00"),
                    c("23:40:00", "23:59:59")),
  "20240625" = list(c("00:00:00", "06:15:00"), c("12:20:00", "13:00:00"), c("15:30:00", "18:15:00")),
  "20240625" = list(c("15:25:00", "17:00:00"), c("19:00:00", "21:00:00"), c("22:30:00", "23:00:00"))
)

# write CSV
knaongbatsa_CI05_rain <- extract_rain_files_by_date(raw_audio_files, KnaongBatSa, KnaongBatSaCI05, KnaongBatSa_rain_periods, 
                                                output_csv = "clean_data/datasets/rain/rain_knaongbatsa_CI05.csv")


knaongbatsa_CI17_rain <- extract_rain_files_by_date(raw_audio_files, KnaongBatSa, KnaongBatSaCI17, KnaongBatSa_rain_periods, 
                                                output_csv = "clean_data/datasets/rain/rain_knaongbatsa_CI17.csv")


## Stung Ta Say: Ta Say   ------------------------------------------------------------------------------

tasay <- "TaSay"
tasayCI12 <- "CI12" # This should be used for Apr & June
tasayCC12 <- "CC12" # Use this for Jan - CI12 is too close to river so lots masked

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
tasay_CI12_rain <- extract_rain_files_by_date(raw_audio_files, tasay, tasayCI12, tasay_rain_periods, 
                                              output_csv = "clean_data/datasets/rain/rain_tasay_CI12.csv")


tasay_CC12_rain <- extract_rain_files_by_date(raw_audio_files, tasay, tasayCC12, tasay_rain_periods, 
                                              output_csv = "clean_data/datasets/rain/rain_tasay_CC12.csv")



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
kronomh_CI19_rain <- extract_rain_files_by_date(raw_audio_files, kronomh, kronomhCI19, kronomh_rain_periods, 
                                                output_csv = "clean_data/datasets/rain/rain_kronomh_CI19.csv")


kronomh_CI19_noise <- extract_rain_files_by_date(raw_audio_files, kronomh, kronomhCI19, kronomhCI19_wonky_periods, 
                                                 output_csv = "clean_data/datasets/rain/noise_kronomh_CI19.csv")


kronomh_CI20_rain <- extract_rain_files_by_date(raw_audio_files, kronomh, kronomhCI20, kronomh_rain_periods, 
                                                output_csv = "clean_data/datasets/rain/rain_kronomh_CI20.csv")




## Stung Ta Say: Dam 5 ------------------------------------------------------------------------------
# Complete dataset finished

damfive <- "DamFive"
damfiveCC11 <- "CC11"
damfiveCI11 <- "CI11"

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
damfive_cc11_rain <- extract_rain_files_by_date(raw_audio_files, damfive, damfiveCC11, damfive_rain_periods, 
                                                output_csv = "clean_data/datasets/rain/rain_damfive_CI02.csv")


damfive_cI11_rain <- extract_rain_files_by_date(raw_audio_files, damfive, damfiveCI11, damfive_rain_periods, 
                                                output_csv = "clean_data/datasets/rain/rain_damfive_CI15.csv")




## Stung Pursat: Tang Rang   ------------------------------------------------------------------------------
# up to 20240624
tangrang <- "TangRang"
tangrangCI07 <- "CI07" ## Huge amount of noise
tangrangCC07 <- "CC07" 

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

# Has some interference sporadically  # BIG POP 
tangrang_CI07_interference <- list(
  "20240110" = list(c("00:00:00", "01:10:00"), c("09:00:00", "23:30:00")),
  "20240111" = list(c("11:00:00", "13:10:00"), c("16:20:00", "16:45:00"), c("18:20:00", "18:24:59")),
  "20240112" = list(c("11:40:00", "20:15:00"))
)


# write CSV
tangrang_CI07_rain <- extract_rain_files_by_date(raw_audio_files, tangrang, tangrangCI07, tangrang_rain_periods, 
                                                 output_csv = "clean_data/datasets/rain/rain_tangrang_CI07.csv")


tangrang_CC07_rain <- extract_rain_files_by_date(raw_audio_files, tangrang, tangrangCC07, tangrang_rain_periods, 
                                                 output_csv = "clean_data/datasets/rain/rain_tangrang_CC07.csv")


tangrang_CI07_noise <- extract_rain_files_by_date(raw_audio_files, tangrang, tangrangCI07, tangrang_CI07_interference, 
                                                 output_csv = "clean_data/datasets/rain/noise_tangrang_CI07.csv")

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
kravanh_CC08_rain <- extract_rain_files_by_date(raw_audio_files, kravanh, kravanhCC08, kravanh_rain_periods, 
                                                    output_csv = "clean_data/datasets/rain/rain_kravanh_CC08.csv")


kravanh_CI08_rain <- extract_rain_files_by_date(raw_audio_files, kravanh, kravanhCI08, kravanh_rain_periods, 
                                                    output_csv = "clean_data/datasets/rain/rain_kravanh_CI08.csv")




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
pursat_CC09_rain <- extract_rain_files_by_date(raw_audio_files, pursat, pursatCC09, pursat_rain_periods, 
                                                output_csv = "clean_data/datasets/rain/rain_pursat_CC09.csv")


pursat_CI09_rain <- extract_rain_files_by_date(raw_audio_files, pursat, pursatCI09, pursat_rain_periods, 
                                                output_csv = "clean_data/datasets/rain/rain_pursat_CI09.csv")



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

noiselist <- list(kronomh_CI19_noise, tachey_CC01_noise, tangrang_CI07_noise)

noisedata <- bind_rows(noiselist)

write.csv(noisedata, "clean_data/datasets/interference/2324noise.csv", row.names = FALSE)


#### 2025 Summer Rains  --------------------------------------------------------------------------------------------

# Define new site: Ta Chey Hill
tacheyhill <- "TaCheyHill"
tacheyhillCC05 <- "CC05"
tacheyhillCI07 <- "CI07"
tacheyhillCI12 <- "CI12"
tacheyhillSM3352 <- "SM3352"

tachey_sum25_rain_periods <- list(
  "20250607" = list(c("14:45:00", "23:59:00")),
  "20250611" = list(c("10:30:00", "15:25:00")),
  "20250612" = list(c("12:20:00", "14:15:00"), c("16:30:00", "19:12:00"))
)

tacheyhillCC05_rain <- extract_rain_files_by_date(raw_audio_files, tacheyhill, tacheyhillCC05, tachey_sum25_rain_periods, 
                                               output_csv = "clean_data/datasets/rain/tacheyhillCC05_rain.csv")
tacheyhillCI07_rain <- extract_rain_files_by_date(raw_audio_files, tacheyhill, tacheyhillCI07, tachey_sum25_rain_periods, 
                                                  output_csv = "clean_data/datasets/rain/tacheyhillCI07_rain.csv")
tacheyhillCI12_rain <- extract_rain_files_by_date(raw_audio_files, tacheyhill, tacheyhillCI12, tachey_sum25_rain_periods, 
                                                  output_csv = "clean_data/datasets/rain/tacheyhillCC05_rain.csv")
tacheyhillSM3352_rain <- extract_rain_files_by_date(raw_audio_files, tacheyhill, tacheyhillSM3352, tachey_sum25_rain_periods, 
                                                  output_csv = "clean_data/datasets/rain/tacheyhillCC05_rain.csv")

# Arai
araiCC06 <- "CC06"
araiCI15 <- "CI15"
araiCI10 <- "CI10"
araiSM2967 <- "SM2967"

arai_sum25_rain_periods <- list(
  "20250607" = list(c("14:45:00", "23:59:00")),
  "20250608" = list(c("13:06:00", "13:24:00")),
  "20250611" = list(c("10:30:00", "15:05:00")),
  "20250612" = list(c("07:54:00", "08:30:00"), c("12:20:00", "14:15:00"), c("15:30:00", "17:30:00")),
)

araiCC06_rain <- extract_rain_files_by_date(raw_audio_files, arai, araiCC06, arai_sum25_rain_periods, 
                                                    output_csv = "clean_data/datasets/rain/araiCC06_rain.csv")
araiCI15_rain <- extract_rain_files_by_date(raw_audio_files, arai, araiCI15, arai_sum25_rain_periods, 
                                                    output_csv = "clean_data/datasets/rain/araiCI15_rain.csv")
araiCI10_rain <- extract_rain_files_by_date(raw_audio_files, arai, araiCI10, arai_sum25_rain_periods, 
                                                    output_csv = "clean_data/datasets/rain/araiCI10_rain.csv")
araiSM2967_rain <- extract_rain_files_by_date(raw_audio_files, arai, araiSM2967, arai_sum25_rain_periods, 
                                                    output_csv = "clean_data/datasets/rain/araiSM2967_rain.csv")


# Oda
odaCC07 <- "CC07"
odaCI19 <- "CI19"
odaCI09 <- "CI09"

oda_sum25_rain_periods <- list(
  "20250607" = list(c("14:35:00", "23:59:00")),
  "20250611" = list(c("10:30:00", "15:25:00"))
)

odaCC07_rain <- extract_rain_files_by_date(raw_audio_files, oda, odaCC07, oda_sum25_rain_periods, 
                                            output_csv = "clean_data/datasets/rain/odaCC07_rain.csv")
odaCI19_rain <- extract_rain_files_by_date(raw_audio_files, oda, odaCI19, oda_sum25_rain_periods, 
                                           output_csv = "clean_data/datasets/rain/odaCI19_rain.csv")
odaCI09_rain <- extract_rain_files_by_date(raw_audio_files, oda, odaCI09, oda_sum25_rain_periods, 
                                           output_csv = "clean_data/datasets/rain/odaCI09_rain.csv")



# Knaong Bat Sa
kbsCC05 <- "CC05"
kbsCI01 <- "CI01"
kbsCI07 <- "CI07"

kbs_sum25_rain_periods <- list(
  "20250703" = list(c("13:20:00", "14:00:00"), c("16:00:00", "18:30:00")),
  "20250705" = list(c("12:40:00", "14:00:00")),
  "20250706" = list(c("12:30:00", "13:30:00"))
)

kbsCC05_rain <- extract_rain_files_by_date(raw_audio_files, KnaongBatSa, kbsCC05, kbs_sum25_rain_periods, 
                                           output_csv = "clean_data/datasets/rain/kbsCC05_rain.csv")
kbsCI01_rain <- extract_rain_files_by_date(raw_audio_files, KnaongBatSa, kbsCI01, kbs_sum25_rain_periods, 
                                           output_csv = "clean_data/datasets/rain/odaCC07_rain.csv")
kbsCI07_rain <- extract_rain_files_by_date(raw_audio_files, KnaongBatSa, kbsCI07, kbs_sum25_rain_periods, 
                                           output_csv = "clean_data/datasets/rain/odaCC07_rain.csv")



# Ta Say
tsCI19 <- "CI19"
tsCI09 <- "CI09"
tsCI15 <- "CI15"
tsSM3293 <- "SM3293"

ts_sum25_rain_periods <- list(
  "20250704" = list(c("14:10:00", "14:00:00")),
  "20250705" = list(c("13:30:00", "15:45:00")),
  "20250706" = list(c("12:35:00", "14:30:00")),
  "20250707" = list(c("13:35:00", "15:30:00")),
  "20250708" = list(c("10:45:00", "23:59:00"))
)

tsCI19_rain <- extract_rain_files_by_date(raw_audio_files, tasay, tsCI19, ts_sum25_rain_periods, 
                                           output_csv = "clean_data/datasets/rain/tsCI19_rain.csv")
tsCI09_rain <- extract_rain_files_by_date(raw_audio_files, tasay, tsCI09, ts_sum25_rain_periods, 
                                          output_csv = "clean_data/datasets/rain/tsCI09_rain.csv")
tsCI15_rain <- extract_rain_files_by_date(raw_audio_files, tasay, tsCI15, ts_sum25_rain_periods, 
                                          output_csv = "clean_data/datasets/rain/tsCI15_rain.csv")
tsSM3293_rain <- extract_rain_files_by_date(raw_audio_files, tasay, tsSM3293, ts_sum25_rain_periods, 
                                          output_csv = "clean_data/datasets/rain/tsSM3293_rain.csv")





# Kronomh
krnCI13 <- "CI13"
krnCI10 <- "CI10"
krnSM3352 <- "SM3352"

krn_sum25_rain_periods <- list(
  "20250702" = list(c("00:00:00", "05:30:00"), c("15:45:00", "23:30:00")),
  "20250703" = list(c("17:10:00", "19:00:00")),
  "20250704" = list(c("14:20:00", "16:25:00")),
  "20250705" = list(c("12:00:00", "14:45:00")),
  "20250706" = list(c("12:45:00", "13:00:00"), c("14:00:00", "16:35:00")),
)

krnCI13_rain <- extract_rain_files_by_date(raw_audio_files, kronomh, krnCI13, krn_sum25_rain_periods, 
                                          output_csv = "clean_data/datasets/rain/krnCI13_rain.csv")
krnCI10_rain <- extract_rain_files_by_date(raw_audio_files, kronomh, krnCI10, krn_sum25_rain_periods, 
                                           output_csv = "clean_data/datasets/rain/krnCI10_rain.csv")
krnSM3352_rain <- extract_rain_files_by_date(raw_audio_files, kronomh, krnSM3352, krn_sum25_rain_periods, 
                                           output_csv = "clean_data/datasets/rain/krnSM3352_rain.csv")



# Tang Rang 
trCC01 <- "CC01"
trCC02 <- "CC02"
trCI17 <- "CI17"

tr_sum25_rain_periods <- list(
  "20250704" = list(c("14:10:00", "14:00:00")),
  "20250705" = list(c("13:30:00", "15:45:00")),
  "20250706" = list(c("12:35:00", "14:30:00")),
  "20250707" = list(c("13:35:00", "15:30:00")),
  "20250708" = list(c("10:45:00", "23:59:00"))
)


trCC01_rain <- extract_rain_files_by_date(raw_audio_files, tangrang, trCC01, tr_sum25_rain_periods, 
                                           output_csv = "clean_data/datasets/rain/trCC01_rain.csv")
trCC02_rain <- extract_rain_files_by_date(raw_audio_files, tangrang, trCC02, tr_sum25_rain_periods, 
                                          output_csv = "clean_data/datasets/rain/trCC02_rain.csv")
trCI17_rain <- extract_rain_files_by_date(raw_audio_files, tangrang, trCI17, tr_sum25_rain_periods, 
                                          output_csv = "clean_data/datasets/rain/trCI17_rain.csv")





# Summer 2025 Full
rainlist2025 <- list(tacheyhillCC05_rain, tacheyhillCI07_rain, tacheyhillCI12_rain, tacheyhillSM3352_rain,
                                 araiCC06_rain, araiCI15_rain, araiCI10_rain, araiSM2967_rain,
                                 odaCC07_rain, odaCI19_rain, odaCI09_rain,
                                 kbsCC05_rain, kbsCI01_rain, kbsCI07_rain,
                                 tsCI19_rain, tsCI09_rain, tsCI15_rain, tsSM3293_rain, 
                                 krnCI13_rain, krnCI10_rain, krnSM3352_rain,
                                 trCC01_rain, trCC02_rain, trCI17_rain)

fullraindata2025 <- bind_rows(rainlist2025)
head(fullraindata2025)

write.csv(fullraindata2025, "clean_data/datasets/interference/rain2025.csv", row.names = FALSE)
 