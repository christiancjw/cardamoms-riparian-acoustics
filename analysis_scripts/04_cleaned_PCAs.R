library(dplyr)
library(hms)
library(dplyr)
library(stringr)
library(lubridate)

#### Data Read in ------------------
nov23_fullday_data <- read.csv("clean_data/Datasets/nov23_fullday_data.csv")


#### Function to run subset PCAs --------------------

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
           QBR = original_data$QBR_Score, 
           FileName = original_data$FileName, 
           Branch = original_data$Branch)
  
  return(pca_scores_with_metadata)
}


# November 2023 PCAs ------------------------------------------------------------------------
nov23_fullday_pca <- run_pca_subset(nov23_fullday_data, "00:00:00", "23:59:00")
summary(nov23_fullday_pca)
nov23_fullday_pca$rotation

nov23_dawn_pca <- run_pca_subset(nov23_fullday_data, "05:00:00", "09:00:00")
summary(nov23_dawn_pca)
nov23_dawn_pca$rotation

nov23_midday_pca <- run_pca_subset(nov23_fullday_data, "10:30:00", "14:30:00")
summary(nov23_midday_pca)
nov23_midday_pca$rotation

nov23_dusk_pca <- run_pca_subset(nov23_fullday_data, "15:30:00", "19:30:00")
summary(nov23_dusk_pca)
nov23_dusk_pca$rotation

nov23_midnight_pca <- run_pca_subset(nov23_fullday_data, "22:00:00", "02:00:00")
summary(nov23_midnight_pca)
nov23_midnight_pca$rotation

# Create Data Frames and save files
nov23_pca_scores <- bind_metadata(nov23_fullday_pca, nov23_fullday_data, "00:00:00", "23:59:00")
write.csv(nov23_pca_scores, "clean_data/datasets/PCAs/nov23_fullday_pca.csv", row.names = FALSE)

nov23_dawn_pca_scores <- bind_metadata(nov23_dawn_pca, nov23_fullday_data, "05:00:00", "09:00:00")
write.csv(nov23_dawn_pca_scores, "clean_data/datasets/PCAs/nov23_dawn_pca.csv", row.names = FALSE)

nov23_midday_pca_scores <- bind_metadata(nov23_midday_pca, nov23_fullday_data, "10:30:00", "14:30:00")
write.csv(nov23_midday_pca_scores, "clean_data/datasets/PCAs/nov23_midday_pca.csv", row.names = FALSE)

nov23_dusk_pca_scores <- bind_metadata(nov23_dusk_pca, nov23_fullday_data, "15:30:00", "19:30:00")
write.csv(nov23_dusk_pca_scores, "clean_data/datasets/PCAs/nov23_dusk_pca.csv", row.names = FALSE)

nov23_midnight_pca_scores <- bind_metadata(nov23_midnight_pca, nov23_fullday_data, "22:00:00", "02:00:00")
write.csv(nov23_midnight_pca_scores, "clean_data/datasets/PCAs/nov23_midnight_pca.csv", row.names = FALSE)


# January 2024 PCAs ------------------------------------------------------------------------
jan24_fullday_pca <- run_pca_subset(jan24_fullday_data, "00:00:00", "23:59:00")
summary(jan24_fullday_pca)
jan24_fullday_pca$rotation

jan24_dawn_pca <- run_pca_subset(jan24_fullday_data, "05:00:00", "09:00:00")
summary(jan24_dawn_pca)
jan24_dawn_pca$rotation

jan24_midday_pca <- run_pca_subset(jan24_fullday_data, "10:30:00", "14:30:00")
summary(jan24_midday_pca)
jan24_midday_pca$rotation

jan24_dusk_pca <- run_pca_subset(jan24_fullday_data, "15:30:00", "19:30:00")
summary(jan24_dusk_pca)
jan24_dusk_pca$rotation

jan24_midnight_pca <- run_pca_subset(jan24_fullday_data, "22:00:00", "02:00:00")
summary(jan24_midnight_pca)
jan24_midnight_pca$rotation

# Create Data Frames and save files
jan24_pca_scores <- bind_metadata(jan24_fullday_pca, jan24_fullday_data, "00:00:00", "23:59:00")
write.csv(jan24_pca_scores, "clean_data/datasets/PCAs/jan24_fullday_pca.csv", row.names = FALSE)

jan24_dawn_pca_scores <- bind_metadata(jan24_dawn_pca, jan24_fullday_data, "05:00:00", "09:00:00")
write.csv(jan24_dawn_pca_scores, "clean_data/datasets/PCAs/jan24_dawn_pca.csv", row.names = FALSE)

jan24_midday_pca_scores <- bind_metadata(jan24_midday_pca, jan24_fullday_data, "10:30:00", "14:30:00")
write.csv(jan24_midday_pca_scores, "clean_data/datasets/PCAs/jan24_midday_pca.csv", row.names = FALSE)

jan24_dusk_pca_scores <- bind_metadata(jan24_dusk_pca, jan24_fullday_data, "15:30:00", "19:30:00")
write.csv(jan24_dusk_pca_scores, "clean_data/datasets/PCAs/jan24_dusk_pca.csv", row.names = FALSE)

jan24_midnight_pca_scores <- bind_metadata(jan24_midnight_pca, jan24_fullday_data, "22:00:00", "02:00:00")
write.csv(jan24_midnight_pca_scores, "clean_data/datasets/PCAs/jan24_midnight_pca.csv", row.names = FALSE)


# April 2024 PCAs ------------------------------------------------------------------------
apr24_fullday_pca <- run_pca_subset(apr24_fullday_data, "00:00:00", "23:59:00")
summary(apr24_fullday_pca)
apr24_fullday_pca$rotation

apr24_dawn_pca <- run_pca_subset(apr24_fullday_data, "05:00:00", "09:00:00")
summary(apr24_dawn_pca)
apr24_dawn_pca$rotation

apr24_midday_pca <- run_pca_subset(apr24_fullday_data, "10:30:00", "14:30:00")
summary(apr24_midday_pca)
apr24_midday_pca$rotation

apr24_dusk_pca <- run_pca_subset(apr24_fullday_data, "15:30:00", "19:30:00")
summary(apr24_dusk_pca)
apr24_dusk_pca$rotation

apr24_midnight_pca <- run_pca_subset(apr24_fullday_data, "22:00:00", "02:00:00")
summary(apr24_midnight_pca)
apr24_midnight_pca$rotation

# Create Data Frames and save files
apr24_pca_scores <- bind_metadata(apr24_fullday_pca, apr24_fullday_data, "00:00:00", "23:59:00")
write.csv(apr24_pca_scores, "clean_data/datasets/PCAs/apr24_fullday_pca.csv", row.names = FALSE)

apr24_dawn_pca_scores <- bind_metadata(apr24_dawn_pca, apr24_fullday_data, "05:00:00", "09:00:00")
write.csv(apr24_dawn_pca_scores, "clean_data/datasets/PCAs/apr24_dawn_pca.csv", row.names = FALSE)

apr24_midday_pca_scores <- bind_metadata(apr24_midday_pca, apr24_fullday_data, "10:30:00", "14:30:00")
write.csv(apr24_midday_pca_scores, "clean_data/datasets/PCAs/apr24_midday_pca.csv", row.names = FALSE)

apr24_dusk_pca_scores <- bind_metadata(apr24_dusk_pca, apr24_fullday_data, "15:30:00", "19:30:00")
write.csv(apr24_dusk_pca_scores, "clean_data/datasets/PCAs/apr24_dusk_pca.csv", row.names = FALSE)

apr24_midnight_pca_scores <- bind_metadata(apr24_midnight_pca, apr24_fullday_data, "22:00:00", "02:00:00")
write.csv(apr24_midnight_pca_scores, "clean_data/datasets/PCAs/apr24_midnight_pca.csv", row.names = FALSE)



# June 2024 PCAs ------------------------------------------------------------------------
jun24_fullday_pca <- run_pca_subset(jun24_fullday_data, "00:00:00", "23:59:00")
summary(jun24_fullday_pca)
jun24_fullday_pca$rotation

jun24_dawn_pca <- run_pca_subset(jun24_fullday_data, "05:00:00", "09:00:00")
summary(jun24_dawn_pca)
jun24_dawn_pca$rotation

jun24_midday_pca <- run_pca_subset(jun24_fullday_data, "10:30:00", "14:30:00")
summary(jun24_midday_pca)
jun24_midday_pca$rotation

jun24_dusk_pca <- run_pca_subset(jun24_fullday_data, "15:30:00", "19:30:00")
summary(jun24_dusk_pca)
jun24_dusk_pca$rotation

jun24_midnight_pca <- run_pca_subset(jun24_fullday_data, "22:00:00", "02:00:00")
summary(jun24_midnight_pca)
jun24_midnight_pca$rotation

# Create Data Frames and save files
jun24_pca_scores <- bind_metadata(jun24_fullday_pca, jun24_fullday_data, "00:00:00", "23:59:00")
write.csv(jun24_pca_scores, "clean_data/datasets/PCAs/jun24_fullday_pca.csv", row.names = FALSE)

jun24_dawn_pca_scores <- bind_metadata(jun24_dawn_pca, jun24_fullday_data, "05:00:00", "09:00:00")
write.csv(jun24_dawn_pca_scores, "clean_data/datasets/PCAs/jun24_dawn_pca.csv", row.names = FALSE)

jun24_midday_pca_scores <- bind_metadata(jun24_midday_pca, jun24_fullday_data, "10:30:00", "14:30:00")
write.csv(jun24_midday_pca_scores, "clean_data/datasets/PCAs/jun24_midday_pca.csv", row.names = FALSE)

jun24_dusk_pca_scores <- bind_metadata(jun24_dusk_pca, jun24_fullday_data, "15:30:00", "19:30:00")
write.csv(jun24_dusk_pca_scores, "clean_data/datasets/PCAs/jun24_dusk_pca.csv", row.names = FALSE)

jun24_midnight_pca_scores <- bind_metadata(jun24_midnight_pca, jun24_fullday_data, "22:00:00", "02:00:00")
write.csv(jun24_midnight_pca_scores, "clean_data/datasets/PCAs/jun24_midnight_pca.csv", row.names = FALSE)

