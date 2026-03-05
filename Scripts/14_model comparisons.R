## Cross Comparison script - to understand if slopes across the datasets are the same.
library(broom)
library(metafor)
library(tidyverse) # for data manipulation and plotting
library(lme4) # for mixed models
library(vegan)
library(corrplot)
library(reshape2)
library(ggplot2)

# Read in and check data
setwd("/Users/christianching/Documents/Projects/cardamoms-riparian-acoustics")

single_ds <- read.csv("clean_data/datasets/PCAs/single_pca.csv")
single_rl_ds <- read.csv("clean_data/datasets/PCAs/rainless_single_pca.csv")
global_ds <- read.csv("clean_data/datasets/PCAs/global2325_pca.csv")
global_rl_ds <- read.csv("clean_data/datasets/PCAs/rainless_global2325_pca.csv")

head(global_ds)

str(global_ds)

# List data
datasets <- list(
  single_full      = single_ds,
  single_norain    = single_rl_ds,
  global_full      = global_ds,
  global_norain    = global_rl_ds
)

globaldatasets <- list(
  global_full      = global_ds,
  global_norain    = global_rl_ds
)


## Data Cleaning Functions: ---------------------------------------------------
# Create Timerange and Daterange
prep_dataset <- function(df) {
  
  df %>%
    mutate(
      Season = case_when(
        Date >= 20231116 & Date <= 20231203 ~ "Monsoon 2023",
        Date >= 20231230 & Date <= 20240208 ~ "Dry Transition 2023",
        Date >= 20240401 & Date <= 20240501 ~ "Dry 2024",
        Date >= 20240607 & Date <= 20240707 ~ "Monsoon Transition 2024",
        Date >= 20250605 & Date <= 20250716 ~ "Monsoon Transition 2025",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(Season)) %>%
    mutate(
      Time_num = as.numeric(Time),
      TimeHHMMSS = stringr::str_pad(Time_num, width = 6, pad = "0"),
      
      TimeRangeFactor = case_when(
        TimeHHMMSS >= "040001" & TimeHHMMSS <= "100000" ~ "Dawn",
        TimeHHMMSS >= "100001" & TimeHHMMSS <= "160000" ~ "Midday",
        TimeHHMMSS >= "160001" & TimeHHMMSS <= "220000" ~ "Dusk",
        TimeHHMMSS >= "220001" | TimeHHMMSS <= "040000" ~ "Midnight",
        TRUE ~ "Other"
      ),
      
      TimeRangeFactor = factor(
        TimeRangeFactor,
        levels = c("Dawn","Midday","Dusk","Midnight","Other")
      ),
      
      Site = factor(Site),
      Season = factor(Season),
      Device = factor(Device)
    ) %>%
    
    # Count devices per Site × Season
    group_by(Site, Season) %>%
    mutate(
      n_devices_site_season = n_distinct(Device)
    ) %>%
    ungroup()
}


# Function to run model (PCA)
run_PC1_m1 <- function(dat) {
  
  model <- lmer(
    PC1 ~ QBR * TimeRangeFactor +
      Strahler * TimeRangeFactor +
      (1 | Site) + 
      (1 | Season),
    data = dat,
    REML = TRUE
  )
  
  return(model)
}

# Updated Model - nested number of devices within site
run_PC1_m2 <- function(dat) {
  
  if(length(unique(dat$DeployID)) > length(unique(dat$Site))) {
    
    model <- lmer(
      PC1 ~ QBR * TimeRangeFactor +
        Strahler * TimeRangeFactor +
        (1 | Site) +
        (1 | Season) +
        (1 | DeployID),
      data = dat,
      REML = TRUE
    )
    
  } else {
    
    model <- lmer(
      PC1 ~ QBR * TimeRangeFactor +
        Strahler * TimeRangeFactor +
        (1 | Site) +
        (1 | Season),
      data = dat,
      REML = TRUE
    )
    
  }
  
  return(model)
}

# # Function to run model (PC2)
run_PC2_m1 <- function(dat) {
  
  model <- lmer(
    PC1 ~ QBR * TimeRangeFactor +
      Strahler * TimeRangeFactor +
      (1 | Site) + 
      (1 | Season),
    data = dat,
    REML = TRUE
  )
  
  return(model)
}

# PC2 M2
run_PC2_m2 <- function(dat) {
    
    model <- lmer(
      PC2 ~ QBR * TimeRangeFactor +
        Strahler * TimeRangeFactor +
        (1 | Site) +
        (1 | Season) +
        (1 | n_devices_site_season),
      data = dat,
      REML = TRUE
    )
  
  return(model)
}
  
# Extraction of model slopes :
extract_slopes <- function(model, dataset_name) {
    
    fixef <- broom.mixed::tidy(model, effects = "fixed")
    
    ci <- confint(model, method = "boot") |>
      as.data.frame() |>
      tibble::rownames_to_column("term") |>
      dplyr::rename(
        CI_low = `2.5 %`,
        CI_high = `97.5 %`
      )
    
    fixef |>
      dplyr::left_join(ci, by = "term") |>
      dplyr::mutate(
        dataset = dataset_name,
        direction = dplyr::case_when(
          CI_low > 0 ~ "+",
          CI_high < 0 ~ "-",
          TRUE ~ "0"
        ),
        supported = direction != "0"
      ) |>
      dplyr::select(
        dataset, term, estimate, CI_low, CI_high, direction, supported
      )
  }

# PC1 Analyses: ---------------------------------------------------#

# Model 1 
PC1_results_m1 <- purrr::imap_dfr(
  datasets,
  ~ extract_slopes(run_PC1_m1(.x), .y)
)

head(PC1_results_m1)

saveRDS(PC1_results_m1, "clean_data/datasets/PCAmodels/PC1_results_m1.rds")

# Model 2
PC1_results_m2 <- purrr::imap_dfr(
  globaldatasets,
  ~ extract_slopes(run_PC1_m2(.x), .y)
)

head(PC1_results_m2)

saveRDS(PC1_results_m2, "clean_data/datasets/PCAmodels/PC1_results_m2.rds")


# PC2 Analyses: ---------------------------------------------------
PC2_results_m1 <- purrr::imap_dfr(
  datasets,
  ~ extract_slopes(run_PC2_m1(.x), .y)
)

#Save Results 
head(PC2_results_m1)

saveRDS(PC2_results_m1, "clean_data/datasets/PCAmodels/PC2_results_m1.rds")

PC2_results_m1 <- readRDS("clean_data/datasets/PCAmodels/PC2_results_m1.rds")

PC2_results_m1


# Model 2
PC2_results_m2 <- purrr::imap_dfr(
  datasets,
  ~ extract_slopes(run_PC1_m2(.x), .y)
)

head(PC2_results_m2)

saveRDS(PC2_results_m2, "clean_data/datasets/PCAmodels/PC2_results_m2.rds")

