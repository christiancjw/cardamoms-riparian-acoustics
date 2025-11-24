library(dplyr)
library(tidyverse) # General data organisation 

setwd("/Users/christianching/Documents/Projects/cardamoms-riparian-acoustics")

# Data Read In 
global_singledevice     <- read.csv("clean_data/datasets/indices_datasets/global_singledevice_data.csv")
continuous_singledevice <- read.csv("clean_data/datasets/indices_datasets/continuous_data.csv")
multi25                 <- read.csv("clean_data/datasets/indices_datasets/multi25_data.csv")
multi25_1in5            <- read.csv("clean_data/datasets/indices_datasets/multi25_1in5_data.csv")
global_data             <- read.csv("clean_data/datasets/indices_datasets/global2325_data.csv")
songmeters25_data       <- read.csv("clean_data/datasets/indices_datasets/songmeters25_data.csv")
 

# Read in Rain and Noise Datasets 
fullraindata              <- read.csv("clean_data/datasets/interference/rain.csv")
fullnoisedata             <- read.csv("clean_data/datasets/interference/noise.csv")

fullinterferencedata      <- read.csv("clean_data/datasets/interference/global_interference.csv")


# Clean interference datasets to ensure column names for joining
fullinterferencedata_clean <-fullinterferencedata %>%
  mutate(FileName = str_remove(FileName, "\\.WAV$"))

head(fullinterferencedata_clean)

# Ensure common column names for joining
fullinterference_trimmed <- fullinterferencedata_clean %>%
  select(Site, Device, Date, FileName)


# Remove rain-affected files --------------------------

# Global SingleDevice Dataset
global_single_clean <- global_singledevice %>%
  anti_join(fullinterference_trimmed, by = c("Site", "Device", "Date", "FileName")) 

write.csv(global_single_clean, "clean_data/datasets/indices_datasets/globalRL_singledevice_data.csv", row.names = FALSE)


# Coninuous Dataset
continuous_singledevice_clean <- continuous_singledevice %>%
  anti_join(fullinterference_trimmed, by = c("Site", "Device", "Date", "FileName")) 

write.csv(global_single_clean, "clean_data/datasets/indices_datasets/continuousRL_data.csv", row.names = FALSE)


# Multi Full Dataset
multi25_clean <- multi25 %>%
  anti_join(fullinterference_trimmed, by = c("Site", "Device", "Date", "FileName"))

write.csv(multi25_clean, "clean_data/datasets/indices_datasets/multi25RL_data.csv", row.names = FALSE)


# Multi 1 in 5 Dataset
multi25_1in5_clean <- multi25_1in5 %>%
  anti_join(fullinterference_trimmed, by = c("Site", "Device", "Date", "FileName"))

write.csv(multi25_1in5_clean, "clean_data/datasets/indices_datasets/multi25_1in5RL_data.csv", row.names = FALSE)


# Global 23 25 Dataset
global_data_clean <- global_data %>%
  anti_join(fullinterference_trimmed, by = c("Site", "Device", "Date", "FileName"))

write.csv(global_data_clean, "clean_data/datasets/indices_datasets/global2325RL_data.csv", row.names = FALSE)


# Songmeter Dataset
songmeters25_data_clean <- songmeters25_data %>%
  anti_join(fullinterference_trimmed, by = c("Site", "Device", "Date", "FileName"))

write.csv(global_data_clean, "clean_data/datasets/indices_datasets/songmeters25RL_data.csv", row.names = FALSE)


# Check number of rows before and after filtering
rows_before <- nrow(global_singledevice)
rows_after<- nrow(global_single_clean)
rows_removed <- rows_before - rows_after

rows_before <- nrow(global_data)
rows_after<- nrow(global_data_clean)
rows_removed <- rows_before - rows_after
rows_removed
# Check number of rows before and after filtering (june)
rows_before <- nrow(jun24_single_data)
rows_after_rain <- nrow(jun24_single_rainless)
rows_after_noise <- nrow(jun24_single_clean)
rows_removed_rain <- rows_before - rows_after_rain
rows_removed_noise<- rows_after_rain - rows_after_noise
rows_removed <- rows_before - rows_after_noise


# Print the results
cat("Rows before filtering:", rows_before, "\n")
cat("Rows after filtering:", rows_after_noise, "\n")
cat("Rows removed due to rain:", rows_removed_rain, "\n")
cat("Rows removed due to noise:", rows_removed_noise, "\n")
cat("Rows removed (total):", rows_removed, "\n")

