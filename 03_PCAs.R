library(ggplot2)
library(plotly)
library(gridExtra)


#### Function to run Principal Component Analyses - Selecting specific indices, standardises data and runs prcomp() --------------------
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


#### Running PCA on November Full Day Dataset ---------------------------------------------

# Run PCA on the combined dataset
nov_pca <- run_pca(Nov23_fullday_data)

# View PCA summary
summary(nov_pca)
nov_loadings <- nov_pca$rotation
nov_loadings 

# Add PCA scores to the dataset
nov_pca_scores <- as.data.frame(nov_pca$x) %>%
  mutate(Device = Nov23_fullday_data$Device, Site = Nov23_fullday_data$Site)

# PCA plot with site colors
allnov <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.0001) +
  labs(title = "PCA of Full Day Indices (Nov)") + 
  stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()

#### Running PCA on November Dawn Chorus Dataset ---------------------------------------------
# Run PCA on the combined dataset
nov_dawn_pca <- run_pca(Nov23_dawn_data)

# View PCA summary
summary(nov_dawn_pca)
nov_dawn_loadings <- nov_dawn_pca$rotation
nov_dawn_loadings 

# Add PCA scores to the dataset
nov_dawn_pca_scores <- as.data.frame(nov_dawn_pca$x) %>%
  mutate(Device = Nov23_dawn_data$Device, Site = Nov23_dawn_data$Site)

# PCA plot with site colors
dawnnov <- ggplot(nov_dawn_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.3) +
  labs(title = "PCA of Dawn Chorus Indices (Nov)") + stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()


#### November Joint Plots 
grid.arrange(allnov, dawnnov, ncol = 2)



fig <- plot_ly(
  nov_dawn_pca_scores,
  x = ~PC1,
  y = ~PC2,
  z = ~PC3,
  color = ~Site,  # Color points by Site
  text = ~paste("Site:", Site, "<br>Order:", "<br>Device:", Device),  # Hover text
  type = 'scatter3d',
  mode = 'markers',
  marker = list(size = 1)