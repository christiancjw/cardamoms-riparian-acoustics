# Effect Size Meta Analysis

library(broom)
library(metafor)
library(tidyverse) # for data manipulation and plotting
library(lme4) # for mixed models
library(vegan)
library(corrplot)

# Data Read-In
globalsingle_ds <- read.csv("clean_data/datasets/PCAs/rainless_global_single_pca.csv")
global_ds <- read.csv("clean_data/datasets/PCAs/rainless_global2325_pca.csv")

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

#### Run PCAs
rl_global_single_pca <- run_pca_subset(global_singledevice_RL,     "00:00:00", "23:59:00")
rl_global_single_scores <- bind_metadata(rl_global_single_pca,     global_singledevice_RL,     "00:00:00", "23:59:00")
summary(rl_global_single_pca)
rl_global_single_pca$rotation

rl_global_data_pca <- run_pca_subset(global_data_RL, "00:00:00", "23:59:00")
rl_global_data_scores <- bind_metadata(rl_global_data_pca, global_data_RL, "00:00:00", "23:59:00")
summary(rl_global_data_pca)
rl_global_data_pca$rotation

# Compare PCA Loadings
load1 <- rl_global_single_pca$rotation[, 1:3]
load2 <- rl_global_data_pca$rotation[, 1:3]

protest(load1, load2, permutations = 999)

# Plot
corrplot(cor(load1, load2), method="circle")

# Compare Variance
rl_global_single_pca$sdev^2 / sum(rl_global_single_pca$sdev^2)
rl_global_data_pca$sdev^2 / sum(rl_global_data_pca$sdev^2)

plot(load1[,1], load2[,1], xlab="Loadings PCA1 (Dataset A)",
     ylab="Loadings PCA1 (Dataset B)")
abline(0,1,col="red")
