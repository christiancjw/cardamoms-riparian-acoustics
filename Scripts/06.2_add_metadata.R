
# Data Read-in
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

cardamoms_RL       <- read.csv("clean_data/datasets/indices_datasets/cardamomsRL_singledevice_data.csv")

# Fix Kronomh
# function to update QBR_Score for Kronomh
fix_qbr <- function(df) {
  df %>%
    mutate(QBR_Score = ifelse(Site == "Kronomh", 85, QBR_Score))
}

# apply to all datasets
global_singledevice      <- fix_qbr(global_singledevice)
continuous_singledevice  <- fix_qbr(continuous_singledevice)
multi25                  <- fix_qbr(multi25)
multi25_1in5             <- fix_qbr(multi25_1in5)
global_data              <- fix_qbr(global_data)
sum25_songmeters         <- fix_qbr(sum25_songmeters)

global_singledevice_RL     <- fix_qbr(global_singledevice_RL)
continuous_singledevice_RL <- fix_qbr(continuous_singledevice_RL)
multi25_RL                 <- fix_qbr(multi25_RL)
multi25_1in5_RL            <- fix_qbr(multi25_1in5_RL)
global_data_RL             <- fix_qbr(global_data_RL)
sum25_songmeters_RL        <- fix_qbr(sum25_songmeters_RL)

cardamoms_RL              <- fix_qbr(cardamoms_RL)

### Bin QBR and Strahler ------------------
bin_metadata <- function(df) {
  df %>%
    mutate(
      QBR_Class = case_when(
        QBR_Score >= 95 ~ "Natural (95–100)",
        QBR_Score >= 75 ~ "Good (75–90)",
        QBR_Score >= 55 ~ "Fair (55–70)",
        QBR_Score >= 30 ~ "Poor (30–50)",
        TRUE ~ "Bad (<25)"
      ),
      Strahler_Class = case_when(
        Strahler == 1 ~ "1st Order",
        Strahler == 2 ~ "2nd Order",
        Strahler == 3 ~ "3rd Order",
        Strahler == 4 ~ "4th Order",
        Strahler == 5 ~ "5th Order",
        TRUE ~ "Other"
      ),
      Deployment_Season = case_when(
        Date >= 20231116 & Date <= 20231203 ~ "Nov 2023",
        Date >= 20231216 & Date <= 20240208 ~ "Jan 2024",
        Date >= 20240401 & Date <= 20240501 ~ "Apr 2024",
        Date >= 20240607 & Date <= 20240707 ~ "Jun 2024",
        Date >= 20250605 & Date <= 20250716 ~ "Jun 2025",
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      QBR_Class = factor(QBR_Class,
                         levels = c("Natural (95–100)", "Good (75–90)", "Fair (55–70)", "Poor (30–50)", "Bad (<25)")
      ),
      Strahler_Class = factor(Strahler_Class,
                              levels = c("1st Order", "2nd Order", "3rd Order",
                                         "4th Order", "5th Order")
      )
    )
}




### Apply binning function to each dataset ------------------

# Non-RL datasets
global_singledevice      <- bin_metadata(global_singledevice)
continuous_singledevice  <- bin_metadata(continuous_singledevice)
multi25                  <- bin_metadata(multi25)
multi25_1in5             <- bin_metadata(multi25_1in5)
global_data              <- bin_metadata(global_data)
sum25_songmeters         <- bin_metadata(sum25_songmeters)

# RL datasets
global_singledevice_RL     <- bin_metadata(global_singledevice_RL)
continuous_singledevice_RL   <- bin_metadata(continuous_singledevice_RL)
multi25_RL                  <- bin_metadata(multi25_RL)
multi25_1in5_RL             <- bin_metadata(multi25_1in5_RL)
global_data_RL              <- bin_metadata(global_data_RL)
sum25_songmeters_RL         <- bin_metadata(sum25_songmeters_RL)

cardamoms_RL <- bin_metadata(cardamoms_RL)


### Save processed datasets back to CSV ----------------------

# Non-RL datasets
write.csv(global_singledevice,
          "clean_data/datasets/indices_datasets/global_singledevice_data.csv",
          row.names = FALSE)

write.csv(continuous_singledevice,
          "clean_data/datasets/indices_datasets/continuous_data.csv",
          row.names = FALSE)

write.csv(multi25,
          "clean_data/datasets/indices_datasets/multi25_data.csv",
          row.names = FALSE)

write.csv(multi25_1in5,
          "clean_data/datasets/indices_datasets/multi25_1in5_data.csv",
          row.names = FALSE)

write.csv(global_data,
          "clean_data/datasets/indices_datasets/global2325_data.csv",
          row.names = FALSE)

write.csv(sum25_songmeters,
          "clean_data/datasets/indices_datasets/songmeters25_data.csv",
          row.names = FALSE)


# RL datasets
write.csv(global_singledevice_RL,
          "clean_data/datasets/indices_datasets/globalRL_singledevice_data.csv",
          row.names = FALSE)

write.csv(continuous_singledevice_RL,
          "clean_data/datasets/indices_datasets/continuousRL_data.csv",
          row.names = FALSE)

write.csv(multi25_RL,
          "clean_data/datasets/indices_datasets/multi25RL_data.csv",
          row.names = FALSE)

write.csv(multi25_1in5_RL,
          "clean_data/datasets/indices_datasets/multi25_1in5RL_data.csv",
          row.names = FALSE)

write.csv(global_data_RL,
          "clean_data/datasets/indices_datasets/global2325RL_data.csv",
          row.names = FALSE)

write.csv(sum25_songmeters_RL,
          "clean_data/datasets/indices_datasets/songmeters25RL_data.csv",
          row.names = FALSE)


write.csv(cardamoms_RL,
          "clean_data/datasets/indices_datasets/cardamoms_RL_data.csv",
          row.names = FALSE)
