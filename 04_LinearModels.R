# Script to calculate medians of specific acoustic indices
# linear models to then be run onto the index values
# Multi-variate analyses to be run on linear models 
library(ggplot2)
library(tidyverse)


# November Data Reading --------------------------------------------------------
nov23_fullday_data <- read.csv("clean_data/Datasets/nov23_fullday_data.csv")
nov23_dawn_data <- read.csv("clean_data/Datasets/nov23_dawn_data.csv")
nov23_midd_data <- read.csv("clean_data/Datasets/nov23_midd_data.csv")
nov23_dusk_data <- read.csv("clean_data/Datasets/nov23_dusk_data.csv")
nov23_midn_data <- read.csv("clean_data/Datasets/nov23_midn_data.csv")

# January Data Reading ---------------------------------------------------------
jan24_fullday_data <- read.csv("clean_data/Datasets/jan24_fullday_data.csv")
jan24_dawn_data <- read.csv("clean_data/Datasets/jan24_dawn_data.csv")
jan24_midd_data <- read.csv("clean_data/Datasets/jan24_midd_data.csv")
jan24_dusk_data <- read.csv("clean_data/Datasets/jan24_dusk_data.csv")
jan24_midn_data <- read.csv("clean_data/Datasets/jan24_midn_data.csv")

# April Data Reading ---------------------------------------------------------
apr24_fullday_data <- read.csv("clean_data/Datasets/apr24_fullday_data.csv")
apr24_dawn_data <- read.csv("clean_data/Datasets/apr24_dawn_data.csv")
apr24_midd_data <- read.csv("clean_data/Datasets/apr24_midd_data.csv")
apr24_dusk_data <- read.csv("clean_data/Datasets/apr24_dusk_data.csv")
apr24_midn_data <- read.csv("clean_data/Datasets/apr24_midn_data.csv")


lm_AcousticComplexity <- lm(AcousticComplexity ~ Disturbance, data = nov23_fullday_data)
summary(lm_AcousticComplexity)
plot(lm_AcousticComplexity)


# January Single Season Analyses --------------------------------------------

# Acoustic Complexity: 

jan24_fullday_data$Disturbance <- as.numeric(as.character(jan24_fullday_data$Disturbance))
jan24_fullday_data$Branch <- as.factor(jan24_fullday_data$Branch)
jan24_fullday_data$Strahler <- as.numeric(as.character(jan24_fullday_data$Strahler))
jan24_fullday_data$Site <- as.factor(jan24_fullday_data$Site)

jan24_branch1_data <- jan24_fullday_data %>% filter(Branch == 1)
# Example: Linear model for 'AcousticComplexity' with disturbance, branch, and strahler order
lm_model <- lm(MidFreqCover ~ Disturbance * Branch * Strahler + Site, data = jan24_branch1_data)
# View summary of model
summary(lm_model)


lm_ACI_model <- lm(AcousticComplexity ~ Disturbance + Strahler, data = jan24_fullday_data)
lm_NDSI_model <- lm(Ndsi ~ Disturbance + Strahler, data = jan24_fullday_data)
lm_MFC_model <- lm(MidFreqCover ~ Disturbance + Strahler, data = jan24_fullday_data)
lm_Entrophy_model <- lm(TemporalEntropy ~ Disturbance + Strahler, data = jan24_fullday_data)


# View summary of model
summary(lm_ACI_model)
summary(lm_NDSI_model)
summary(lm_MFC_model)
summary(lm_Entrophy_model)



# Branches
jan24_branch1_data <- jan24_fullday_data %>% filter(Branch == 2)


  
  ggplot(jan24_fullday_data, aes(x = factor(Site, levels = c("TaChey", "Arai", "Stung Oda", "KnaongBatSa", "TaSay", "DamFive", "TangRang", "PursatTown")), 
                                 y = AcousticComplexity, 
                                 fill = as.factor(Disturbance))) +   
    geom_boxplot(outlier.shape = NA, fill = "darkgrey",alpha = 0.7) +  
    labs(x = "Site", y = "Acoustic Complexity", fill = "Disturbance", 
         title = "Acoustic Complexity Across Disturbance Levels, Split by Branch and Site") +  
    theme_minimal() +   
    theme(legend.position = "bottom") +  
    ylim(0.375, 0.55) +  
    scale_x_discrete(limits = c("TaChey", "Arai", "Stung Oda", "KnaongBatSa", "TaSay", "DamFive", "TangRang", "PursatTown"))  # Ensure order stays fixed

  # Example: Linear model for 'AcousticComplexity' with disturbance, branch, and strahler order
  jan24_AcousticComplexity_lm_model <- lm(AcousticComplexity ~ Disturbance * Branch * Strahler + Site, data = jan24_fullday_data)
  # View summary of model
  summary(jan24_AcousticComplexity_lm_model)
  
  jan24AcousticComplexity <- ggplot(jan24_fullday_data, aes(x = factor(Site, levels = c("TaChey", "Arai", "Stung Oda", "KnaongBatSa", "TaSay", "DamFive", "TangRang", "PursatTown")), 
                                 y = AcousticComplexity, 
                                 fill = as.factor(Disturbance))) +   
    geom_boxplot(outlier.shape = NA, fill = "darkgrey",alpha = 0.7) +  
    labs(x = "Site", y = "Mid Frequency Cover", fill = "Disturbance", 
         title = "Mid Frequency Cover Across Disturbance Levels, Split by Branch and Site") +  
    theme_minimal() +   
    theme(legend.position = "bottom") +  
    ylim(0.375, 0.55) +  
    scale_x_discrete(limits = c("TaChey", "Arai", "Stung Oda", "KnaongBatSa", "TaSay", "DamFive", "TangRang", "PursatTown"))  # Ensure order stays fixed
  
  ggsave("jan24AcousticComplexity.png", plot = jan24AcousticComplexity, 
         path = NULL, width = 4, height = 8, dpi = 950, bg='transparent')
  
  # Example: Linear model for 'Ndsi' with disturbance, branch, and strahler order
 jan24_NDSI_lm_model <- lm(Ndsi ~ Disturbance * Branch * Strahler + Site, data = jan24_fullday_data)
  # View summary of model
  summary(jan24_NDSI_lm_model)
  
jan24NDSI <- ggplot(jan24_fullday_data, aes(x = factor(Site, levels = c("TaChey", "Arai", "Stung Oda", "KnaongBatSa", "TaSay", "DamFive", "TangRang", "PursatTown")), 
                                 y = Ndsi, 
                                 fill = as.factor(Disturbance))) +   
    geom_boxplot(outlier.shape = NA, fill = "darkgrey",alpha = 0.7) +  
    labs(x = "Site", y = "NDSI", fill = "Disturbance", 
         title = "NDSI Across Disturbance Levels, Split by Branch and Site") +  
    theme_minimal() +   
    theme(legend.position = "bottom") +   ylim(-1, 1.1) +
    scale_x_discrete(limits = c("TaChey", "Arai", "Stung Oda", "KnaongBatSa", "TaSay", "DamFive", "TangRang", "PursatTown"))  # Ensure order stays fixed
  
  ggsave("jan24NDSI.png", plot = jan24NDSI, 
         path = NULL, width = 4, height = 8, dpi = 950, bg='transparent')
  
  
  # Example: Linear model for 'ThreeGramCount' with disturbance, branch, and strahler order
  jan24_ThreeGramCount_lm_model <- lm(ThreeGramCount ~ Disturbance * Branch * Strahler + Site, data = jan24_fullday_data)
  # View summary of model
  summary(jan24_ThreeGramCount_lm_model)
  
  jan24ThreeGramCount <- ggplot(jan24_fullday_data, aes(x = factor(Site, levels = c("TaChey", "Arai", "Stung Oda", "KnaongBatSa", "TaSay", "DamFive", "TangRang", "PursatTown")), 
                                              y = ThreeGramCount, 
                                              fill = as.factor(Disturbance))) +   
    geom_boxplot(outlier.shape = NA, fill = "darkgrey",alpha = 0.7) +  
    labs(x = "Site", y = "ThreeGramCount", fill = "Disturbance", 
         title = "ThreeGramCount Across Disturbance Levels, Split by Branch and Site") +  
    theme_minimal() +   
    theme(legend.position = "bottom") +  
    scale_x_discrete(limits = c("TaChey", "Arai", "Stung Oda", "KnaongBatSa", "TaSay", "DamFive", "TangRang", "PursatTown"))  # Ensure order stays fixed
  
  ggsave("jan24ThreeGramCount.png", plot = jan24ThreeGramCount, 
         path = NULL, width = 4, height = 8, dpi = 950, bg='transparent')

  
  TemporalEntropy  
  
  # Example: Linear model for 'AcousticComplexity' with disturbance, branch, and strahler order
  jan24_TemporalEntropy_lm_model <- lm(TemporalEntropy ~ Disturbance * Branch * Strahler + Site, data = jan24_fullday_data)
  # View summary of model
  summary(jan24_TemporalEntropy_lm_model)
  
  jan24TemporalEntropy <- ggplot(jan24_fullday_data, aes(x = factor(Site, levels = c("TaChey", "Arai", "Stung Oda", "KnaongBatSa", "TaSay", "DamFive", "TangRang", "PursatTown")), 
                                              y = TemporalEntropy, 
                                              fill = as.factor(Disturbance))) +   
    geom_boxplot(outlier.shape = NA, fill = "darkgrey",alpha = 0.7) +  
    labs(x = "Site", y = "TemporalEntropy", fill = "Disturbance", 
         title = "TemporalEntropy Across Disturbance Levels, Split by Branch and Site") +  
    theme_minimal() + ylim(0, 0.25) +
    theme(legend.position = "bottom") +  
    scale_x_discrete(limits = c("TaChey", "Arai", "Stung Oda", "KnaongBatSa", "TaSay", "DamFive", "TangRang", "PursatTown"))  # Ensure order stays fixed
  
  ggsave("jan24TemporalEntropy.png", plot = jan24TemporalEntropy, 
         path = NULL, width = 4, height = 8, dpi = 950, bg='transparent')
  