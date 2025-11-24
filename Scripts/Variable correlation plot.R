library(ggplot2)
library(tidyverse)
library(plotly)
library(gridExtra)
library(cowplot)
library(ggrepel)
library(GGally)

#### Data Read in ------------------
singledevice_ds <- read.csv("clean_data/datasets/indices_datasets/global_singledevice_data.csv")
global_ds <- read.csv("clean_data/datasets/indices_datasets/global2325_data.csv")

rl_singledevice_ds <- read.csv("clean_data/datasets/indices_datasets/globalRL_singledevice_data.csv")
rl_global_ds <- read.csv("clean_data/datasets/indices_datasets/global2325RL_data.csv")

#### Function to run Principal Component Analyses ---------------------------------------------------------------------------
# Selects specific indices, standardises data and runs prcomp() 

run_pca <- function(data_subset) { 
 # Select only the acoustic indices
 acoustic_indices <- c("AcousticComplexity", "TemporalEntropy", "Ndsi", "EventsPerSecond", 
 "LowFreqCover", "MidFreqCover", "HighFreqCover", "ClusterCount", "ThreeGramCount")
 
 # Ensure all required indices exist in the data
 data_subset <- data_subset %>% select(all_of(acoustic_indices))
 
 # Standardize the data
 data_scaled <- scale(data_subset)
 
 # Run PCA
 pca_result <- prcomp(data_scaled, center = TRUE, scale. = TRUE)
 
 return(pca_result)
}


#### Function to extract loadings --------------------







# Scale loadings for visualization (adjust multiplier if needed)
scale_factor <- 15 # Adjust this to change arrow length

# Add arrows for loadings - next two lines for adding to ggplots --------
geom_segment(data = nov23_loadings_df, aes(x = 0, y = 0, xend = PC1 * scale_factor, yend = PC2 * scale_factor),
 arrow = arrow(length = unit(0.2, "inches")),
 color = "#2d2d2d", linewidth = 1, alpha = 0.7) 
 
 # Add labels for loadings
 geom_text(data = nov23_loadings_df, aes(x = PC1 * scale_factor, y = PC2 * scale_factor, label = Index),
 color = "#2d2d2d", size = 3, vjust = -0.5) 

#### Plotting Variable Correlation Plot  ---------------------------------------------
  
  # Run PCA on the combined dataset
  nov23_pca <- run_pca(single_global_rainless_data)
  
  # View PCA summary
  summary(nov23_pca)
  nov23_loadings <- nov23_pca$rotation
  nov23_loadings 
  
  # Extract Loading Arrow Data
  nov23_loadings_df <- as.data.frame(nov23_pca$rotation)
  nov23_loadings_df$Index <- rownames(nov23_loadings_df) # Add names of indices
  
  # Add PCA scores to the dataset
  nov23_pca_scores <- as.data.frame(nov23_pca$x) %>%
   mutate(Device = single_global_rainless_data$Device, 
   Site = single_global_rainless_data$Site, 
   Date =single_global_rainless_data$Date, 
   Time = single_global_rainless_data$Time,
   Strahler = single_global_rainless_data$Strahler,
   Disturbance = single_global_rainless_data$Disturbance,
   Branch = single_global_rainless_data$Branch)
  
  
  # Define short labels
  short_labels <- c(
    "AcousticComplexity" = "ACI",
    "TemporalEntropy"    = "ENT",
    "Ndsi"               = "NDSI",
    "EventsPerSecond"    = "EVN",
    "LowFreqCover"       = "LFC",
    "MidFreqCover"       = "MFC",
    "HighFreqCover"      = "HFC",
    "ClusterCount"       = "CLS",
    "ThreeGramCount"     = "TGC"
  )
  
  # Apply short labels
  nov23_loadings_df$ShortIndex <- short_labels[nov23_loadings_df$Index]
  
  # PCA plot with site colors
  plot_nov23_full <- ggplot(nov23_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
   geom_point(size = 0.001, alpha = 0.00000001, show.legend = FALSE) +
   labs(title = "PCA of Full Day Indices (November 2023)") + 
   theme_minimal() + xlim(-8, 4) + ylim(-3, 6) + 
    geom_segment(data = nov23_loadings_df, aes(x = 0, y = 0, xend = PC1 * scale_factor, yend = PC2 * scale_factor),
                 arrow = arrow(length = unit(0.2, "inches")),
                 color = "#2d2d2d", linewidth = 1, alpha = 0.7) +
  geom_text_repel(data = nov23_loadings_df,
                  aes(x = PC1 * scale_factor, y = PC2 * scale_factor, 
                      label = ShortIndex),
                  color = "#2d2d2d", size = 3, hjust = 1, vjust = 1.5)
  scale_factor <- 9
  plot_nov23_full

#### Plotting correlation plots for raw indices
  # Select the acoustic indices you want to include
  indices_vars <- single_global_rainless_data %>%
    select(AcousticComplexity,
           TemporalEntropy,
           EventsPerSecond,
           HighFreqCover,
           MidFreqCover,
           LowFreqCover,
           Ndsi,
           ClusterCount,
           ThreeGramCount)
  
  colnames(indices_vars) <- short_labels[colnames(indices_vars)]
  
  indices_vars <- indices_vars %>% mutate(across(everything(), as.numeric))
  
  
  # Create the correlation matrix plot
  ggpairs(indices_vars,
          lower = list(continuous = wrap("points", alpha = 0.3, size = 0.5)),
          diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
          upper = list(continuous = wrap("cor", size = 3))) +
    theme_bw()
  
  
  
#### Heatmap Plot -------
  library(dplyr)
  library(plotly)
  
  # Run PCA (assuming indices_vars is your numeric dataset)
  pca_res <- prcomp(indices_vars, scale. = TRUE)
  
  # Extract loadings (rotation matrix)
  loadings <- as.data.frame(pca_res$rotation)
  
  # Keep only first few PCs (say 5)
  loadings <- loadings[, 1:9]
  
  # Add index names as a column
  loadings$Index <- rownames(loadings)
  
  # Use absolute values
  loadings_long$Loading <- abs(loadings_long$Loading)
  
  # Reshape to long format for plotly
  loadings_long <- loadings %>%
    tidyr::pivot_longer(cols = starts_with("PC"),
                        names_to = "PC",
                        values_to = "Loading")
  
  # Apply your short labels
  short_labels <- c(
    "AcousticComplexity" = "ACI",
    "TemporalEntropy"    = "ENT",
    "Ndsi"               = "NDSI",
    "EventsPerSecond"    = "EVN",
    "LowFreqCover"       = "LFC",
    "MidFreqCover"       = "MFC",
    "HighFreqCover"      = "HFC",
    "ClusterCount"       = "CLS",
    "ThreeGramCount"     = "TGC"
  )
  loadings_long$Index <- recode(loadings_long$Index, !!!short_labels)
  
  # Extract loadings
  loadings <- as.data.frame(rainless_global_single_pca$rotation)
  loadings$Index <- rownames(loadings)
  
  # Extract proportion of variance explained
  pve <- summary(rainless_global_single_pca)$importance[2, ]  # row 2 = Proportion of Variance
  
  # Convert to long format
  library(tidyr)
  loadings_long <- loadings %>%
    pivot_longer(cols = starts_with("PC"),
                 names_to = "PC",
                 values_to = "Loading")
  
  # Weight by PVE
  loadings_long <- loadings_long %>%
    mutate(WeightedLoading = abs(Loading) * pve[PC])
  
  # Apply short labels
  loadings_long$Index <- recode(loadings_long$Index, !!!short_labels)
  
  # Plot heatmap
  library(plotly)
  plot_ly(
    data = loadings_long,
    x = ~PC,
    y = ~Index,
    z = ~WeightedLoading,
    type = "heatmap",
    colors = colorRamp(c("white", "#3DA9C7")),
    zmin = 0
  ) %>%
    layout(
      title = "PCA Loadings Heatmap (Weighted by Variance Explained)",
      xaxis = list(title = "Principal Components"),
      yaxis = list(title = "Indices"),
      margin = list(l = 100, r = 20, t = 50, b = 50)
    )
  