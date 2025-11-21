library(dplyr)
library(tidyverse) # General data organisation 

setwd("/Users/christianching/Documents/Projects/cardamoms-riparian-acoustics")

# Data Read In 
global_single_data <- read.csv("clean_data/datasets/indices_datasets/single_global_data.csv")
continuous_single_data <- read.csv("clean_data/datasets/indices_datasets/single_continuous_data.csv")
multi25 <- read.csv("clean_data/datasets/indices_datasets/multi25_data.csv")
multi25_1in5 <- read.csv("clean_data/datasets/indices_datasets/multi25_1in5_data.csv")

global23to25_single_data <- read.csv("clean_data/datasets/indices_datasets/global2023_2025_data.csv")

# Read in Rain and Noise Datasets 
fullraindata <- read.csv("clean_data/datasets/interference/rain.csv")
fullnoisedata <- read.csv("clean_data/datasets/interference/noise.csv")

summmer25raindata <- read.csv("clean_data/datasets/interference/summmer25rain.csv")
allrainnoise <- list(fullraindata, fullnoisedata, summmer25raindata)
allrainnoise <- bind_rows(allrainnoise)


# Clean interference datasets to ensure column names for joining
fullraindata_clean <- fullraindata %>%
  mutate(FileName = str_remove(FileName, "\\.WAV$"))
head(fullraindata_clean)

fullnoisedata_clean <- fullnoisedata %>%
  mutate(FileName = str_remove(FileName, "\\.WAV$"))
head(fullnoisedata_clean)

summmer25rain_clean <- summmer25raindata %>%
  mutate(FileName = str_remove(FileName, "\\.WAV$"))
head(summmer25rain_clean)

allrainnoise_clean <-allrainnoise %>%
  mutate(FileName = str_remove(FileName, "\\.WAV$"))
head(allrainnoise_clean)

# Ensure common column names for joining
rain_trimmed <- fullraindata_clean %>%
  select(Site, Device, Date, FileName)

noise_trimmed <- fullnoisedata_clean %>%
  select(Site, Device, Date, FileName)

rains25_trimmed <- summmer25rain_clean %>%
  select(Site, Device, Date, FileName)

allrainnoise_trimmed <- allrainnoise_clean %>%
  select(Site, Device, Date, FileName)


# Remove rain-affected files --------------------------

# Global Dataset
global_single_rainless <- global_single_data %>%
  anti_join(rain_trimmed, by = c("Site", "Device", "Date", "FileName"))

global_single_clean <- global_single_rainless %>%
  anti_join(noise_trimmed, by = c("Site", "Device", "Date", "FileName"))

write.csv(global_single_clean, "clean_data/datasets/indices_datasets/single_global_rainless_data.csv", row.names = FALSE)


# Coninuous Dataset
continuous_single_rainless <- continuous_single_data %>%
  anti_join(rain_trimmed, by = c("Site", "Device", "Date", "FileName"))

continuous_single_clean <- continuous_single_rainless %>%
  anti_join(noise_trimmed, by = c("Site", "Device", "Date", "FileName"))

write.csv(continuous_single_clean, "clean_data/datasets/indices_datasets/single_continuous_rainless_data.csv", row.names = FALSE)


# Multi Full Dataset
multi25_clean <- multi25 %>%
  anti_join(rains25_trimmed, by = c("Site", "Device", "Date", "FileName"))

write.csv(multi25_clean, "clean_data/datasets/indices_datasets/multi25_rainless_data.csv", row.names = FALSE)

# Multi 1 in 5 Dataset
multi25_1in5_clean <- multi25_1in5 %>%
  anti_join(rains25_trimmed, by = c("Site", "Device", "Date", "FileName"))

nrow(multi25_1in5)
nrow(multi25_1in5_clean)

write.csv(multi25_1in5_clean, "clean_data/datasets/indices_datasets/multi25_1in5_rainless_data.csv", row.names = FALSE)

# Global 23 25 Dataset
global23to25_single_data_clean <- global23to25_single_data %>%
  anti_join(allrainnoise_trimmed, by = c("Site", "Device", "Date", "FileName"))

write.csv(global23to25_single_data_clean, "clean_data/datasets/indices_datasets/global2023_2025_rainless_data.csv", row.names = FALSE)

nrow(global23to25_single_data)
nrow(global23to25_single_data_clean)

# Check number of rows before and after filtering
rows_before <- nrow(global_single_data)
rows_after_rain <- nrow(global_single_rainless)
rows_after_noise <- nrow(global_single_clean)
rows_removed_rain <- rows_before - rows_after_rain
rows_removed_noise<- rows_after_rain - rows_after_noise
rows_removed <- rows_before - rows_after_noise

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

