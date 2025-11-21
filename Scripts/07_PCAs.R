library(hms)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)

setwd("/Users/christianching/Documents/Projects/cardamoms-riparian-acoustics")

#### Data Read in ------------------
global_singledevice     <- read.csv("clean_data/datasets/indices_datasets/global_singledevice_data.csv")
continuous_singledevice <- read.csv("clean_data/datasets/indices_datasets/continuous_data.csv")
multi25                 <- read.csv("clean_data/datasets/indices_datasets/multi25_data.csv")
multi25_1in5            <- read.csv("clean_data/datasets/indices_datasets/multi25_1in5_data.csv")
global_data             <- read.csv("clean_data/datasets/indices_datasets/global2325_data.csv")
sum25_songmeters        <- read.csv("clean_data/datasets/indices_datasets/songmeters25_data.csv")

global_singledevice_RL    <- read.csv("clean_data/datasets/indices_datasets/globalRL_singledevice_data.csv")
continuous_singledevice_RL <- read.csv("clean_data/datasets/indices_datasets/continuousRL_data.csv")
multi25_RL                <- read.csv("clean_data/datasets/indices_datasets/multi25RL_data.csv")
multi25_1in5_RL           <- read.csv("clean_data/datasets/indices_datasets/multi25_1in5RL_data.csv")
global_data_RL            <- read.csv("clean_data/datasets/indices_datasets/global2325RL_data.csv")
sum25_songmeters_RL       <- read.csv("clean_data/datasets/indices_datasets/songmeters25RL_data.csv")


#### Function to run subset PCAs - allowing correct timing --------------------

run_pca_subset <- function(data_subset, start_time = NULL, end_time = NULL) {
  # Fix the Time format (e.g., 3 -> "00:00:03", 500 -> "00:05:00")
  data_subset <- data_subset %>%
    mutate(Time_fixed = sprintf("%06d", as.integer(Time)),
           Time_fixed = stringr::str_replace_all(Time_fixed, "(.{2})(.{2})(.{2})", "\\1:\\2:\\3"))
  
  # Convert to proper time format
  data_subset <- data_subset %>% mutate(Time_parsed = lubridate::hms(Time_fixed))
  
  # Handle the midnight wrap-around case
  if (!is.null(start_time) & !is.null(end_time)) {
    start_time_parsed <- lubridate::hms(start_time)
    end_time_parsed <- lubridate::hms(end_time)
    
    if (end_time_parsed < start_time_parsed) {
      # If end time is earlier, split the filter into two parts
      data_subset <- data_subset %>%
        filter((Time_parsed >= start_time_parsed) | (Time_parsed <= end_time_parsed))
    } else {
      data_subset <- data_subset %>%
        filter(Time_parsed >= start_time_parsed & Time_parsed <= end_time_parsed)
    }
  }
  
  # Select only the acoustic indices
  acoustic_indices <- c("AcousticComplexity", "TemporalEntropy", "Ndsi", "EventsPerSecond",
                        "LowFreqCover", "MidFreqCover", "HighFreqCover", "ClusterCount", "ThreeGramCount")
  
  # Ensure required indices exist
  data_subset <- data_subset %>% select(any_of(acoustic_indices)) %>% drop_na()
  
  # Remove constant columns if any (to avoid PCA errors)
  constant_cols <- sapply(data_subset, function(x) length(unique(x)) <= 1)
  if (any(constant_cols)) {
    warning("Dropped constant columns: ", paste(names(data_subset)[constant_cols], collapse = ", "))
    data_subset <- data_subset[, !constant_cols]
  }
  
  # Ensure at least two variable columns for PCA
  if (ncol(data_subset) < 2) {
    stop("Not enough variable columns to perform PCA.")
  }
  
  # Standardize the data and run PCA
  data_scaled <- scale(data_subset)
  pca_result <- prcomp(data_scaled, center = TRUE, scale. = TRUE)
  
  return(pca_result)
}


# usage: data_pca <- run_pca_subset(dataset, "HH:MM:SS", "HH:MM:SS")


#### Function to bind metadata to PCA subsets --------

bind_metadata <- function(pca_result, original_data, start_time = NULL, end_time = NULL) {
  # Extract PCA scores as a dataframe
  pca_scores <- as.data.frame(pca_result$x)
  
  # Fix the Time format in the original data
  original_data <- original_data %>%
    mutate(Time_fixed = sprintf("%06d", as.integer(Time)),
           Time_fixed = stringr::str_replace_all(Time_fixed, "(.{2})(.{2})(.{2})", "\\1:\\2:\\3"))
  
  # Convert to proper time format
  original_data <- original_data %>%
    mutate(Time_parsed = lubridate::hms(Time_fixed))
  
  # Filter original data based on the time range (same logic as in run_pca_subset)
  if (!is.null(start_time) & !is.null(end_time)) {
    start_time_parsed <- lubridate::hms(start_time)
    end_time_parsed <- lubridate::hms(end_time)
    
    if (end_time_parsed < start_time_parsed) {
      # If end time is earlier, split the filter into two parts
      original_data <- original_data %>%
        filter((Time_parsed >= start_time_parsed) | (Time_parsed <= end_time_parsed))
    } else {
      original_data <- original_data %>%
        filter(Time_parsed >= start_time_parsed & Time_parsed <= end_time_parsed)
    }
  }

  # Bind metadata to PCA scores
  pca_scores_with_metadata <- pca_scores %>%
    mutate(Device = original_data$Device, 
           Site = original_data$Site, 
           Date = original_data$Date, 
           Time = original_data$Time,
           Strahler = original_data$Strahler, 
           Strahler_Class = original_data$Strahler_Class,
           QBR = original_data$QBR_Score,
           QBR_Class = original_data$QBR_Class,
           FileName = original_data$FileName, 
           Branch = original_data$Branch,
           Deployment_Season = original_data$Deployment_Season
           )
  
  return(pca_scores_with_metadata)
}

### ================================
### RUN + SAVE RAW PCA RESULTS
### ================================

# ---- Run PCAs ----
global_single_pca      <- run_pca_subset(global_singledevice,     "00:00:00", "23:59:00")
continuous_single_pca  <- run_pca_subset(continuous_singledevice, "00:00:00", "23:59:00")
multi25_pca            <- run_pca_subset(multi25,                 "00:00:00", "23:59:00")
multi25_1in5_pca       <- run_pca_subset(multi25_1in5,            "00:00:00", "23:59:00")
global_data_pca        <- run_pca_subset(global_data,             "00:00:00", "23:59:00")
songmeter25_pca        <- run_pca_subset(sum25_songmeters,        "00:00:00", "23:59:00")

# ---- Bind Metadata ----
global_single_scores      <- bind_metadata(global_single_pca,     global_singledevice,     "00:00:00", "23:59:00")
continuous_single_scores  <- bind_metadata(continuous_single_pca, continuous_singledevice, "00:00:00", "23:59:00")
multi25_scores            <- bind_metadata(multi25_pca,           multi25,                 "00:00:00", "23:59:00")
multi25_1in5_scores       <- bind_metadata(multi25_1in5_pca,      multi25_1in5,            "00:00:00", "23:59:00")
global_data_scores        <- bind_metadata(global_data_pca,       global_data,             "00:00:00", "23:59:00")
songmeter25_scores        <- bind_metadata(songmeter25_pca,       sum25_songmeters,        "00:00:00", "23:59:00")

# ---- Save Files ----
write.csv(global_single_scores,     "clean_data/datasets/PCAs/global_single_pca.csv",       row.names = FALSE)
write.csv(continuous_single_scores, "clean_data/datasets/PCAs/continuous_single_pca.csv",   row.names = FALSE)
write.csv(multi25_scores,           "clean_data/datasets/PCAs/multi25_pca.csv",             row.names = FALSE)
write.csv(multi25_1in5_scores,      "clean_data/datasets/PCAs/multi25_1in5_pca.csv",        row.names = FALSE)
write.csv(global_data_scores,       "clean_data/datasets/PCAs/global2325_pca.csv",          row.names = FALSE)
write.csv(songmeter25_scores,       "clean_data/datasets/PCAs/songmeters25_pca.csv",        row.names = FALSE)

### ================================
### RUN + SAVE RAINLESS PCA RESULTS
### ================================

# ---- Run PCAs ----
rl_global_single_pca      <- run_pca_subset(global_singledevice_RL,     "00:00:00", "23:59:00")
summary(rl_global_single_pca)
rl_global_single_pca$rotation

rl_continuous_single_pca  <- run_pca_subset(continuous_singledevice_RL, "00:00:00", "23:59:00")
summary(continuous_singledevice_RL)
continuous_singledevice_RL$rotation

rl_multi25_pca            <- run_pca_subset(multi25_RL,                 "00:00:00", "23:59:00")
summary(multi25_RL)
multi25_RL$rotation

rl_multi25_1in5_pca       <- run_pca_subset(multi25_1in5_RL,            "00:00:00", "23:59:00")
summary(multi25_1in5_RL)
multi25_1in5_RL$rotation

rl_global_data_pca        <- run_pca_subset(global_data_RL,             "00:00:00", "23:59:00")
summary(global_data_RL)
global_data_RL$rotation

rl_songmeter25_pca        <- run_pca_subset(sum25_songmeters_RL,        "00:00:00", "23:59:00")
summary(sum25_songmeters_RL)
sum25_songmeters_RL$rotation

# ---- Bind Metadata ----
rl_global_single_scores      <- bind_metadata(rl_global_single_pca,     global_singledevice_RL,     "00:00:00", "23:59:00")
rl_continuous_single_scores  <- bind_metadata(rl_continuous_single_pca, continuous_singledevice_RL, "00:00:00", "23:59:00")
rl_multi25_scores            <- bind_metadata(rl_multi25_pca,           multi25_RL,                 "00:00:00", "23:59:00")
rl_multi25_1in5_scores       <- bind_metadata(rl_multi25_1in5_pca,      multi25_1in5_RL,            "00:00:00", "23:59:00")
rl_global_data_scores        <- bind_metadata(rl_global_data_pca,       global_data_RL,             "00:00:00", "23:59:00")
rl_songmeter25_scores        <- bind_metadata(rl_songmeter25_pca,       sum25_songmeters_RL,        "00:00:00", "23:59:00")

# ---- Save Files ----
write.csv(rl_global_single_scores,     "clean_data/datasets/PCAs/rainless_global_single_pca.csv",       row.names = FALSE)
write.csv(rl_continuous_single_scores, "clean_data/datasets/PCAs/rainless_continuous_single_pca.csv",   row.names = FALSE)
write.csv(rl_multi25_scores,           "clean_data/datasets/PCAs/rainless_multi25_pca.csv",             row.names = FALSE)
write.csv(rl_multi25_1in5_scores,      "clean_data/datasets/PCAs/rainless_multi25_1in5_pca.csv",        row.names = FALSE)
write.csv(rl_global_data_scores,       "clean_data/datasets/PCAs/rainless_global2325_pca.csv",          row.names = FALSE)
write.csv(rl_songmeter25_scores,       "clean_data/datasets/PCAs/rainless_songmeters25_pca.csv",        row.names = FALSE)


