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


### November -------------------------------------------------------------------------------------------

#### November Full Day Dataset ---------------------------------------------

# Run PCA on the combined dataset
nov_pca <- run_pca(Nov23_fullday_data)

# View PCA summary
summary(nov_pca)
nov_loadings <- nov_pca$rotation
nov_loadings 

# Add PCA scores to the dataset
nov_pca_scores <- as.data.frame(nov_pca$x) %>%
  mutate(Device = Nov23_fullday_data$Device, 
         Site = Nov23_fullday_data$Site, 
         Date =Nov23_fullday_data$Date, 
         Time = Nov23_fullday_data$Time )

# PCA plot with site colors
plot_nov_full <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.0001) +
  labs(title = "PCA of Full Day Indices (Nov)") + 
  stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()

plot_nov_full

#### November Dawn Chorus Dataset ---------------------------------------------

# Run PCA on the combined dataset
nov_dawn_pca <- run_pca(Nov23_dawn_data)

# View PCA summary
summary(nov_dawn_pca)
nov_dawn_loadings <- nov_dawn_pca$rotation
nov_dawn_loadings 

# Add PCA scores to the dataset
nov_dawn_pca_scores <- as.data.frame(nov_dawn_pca$x) %>%
  mutate(Device = Nov23_dawn_data$Device, 
         Site = Nov23_dawn_data$Site, 
         Date = Nov23_dawn_data$Date, 
         Time = Nov23_dawn_data$Time)

# PCA plot with site colors
plot_nov_dawn <- ggplot(nov_dawn_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.3) +
  labs(title = "PCA of Dawn Chorus Indices (Nov)") + stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()

plot_nov_dawn <- ggplot(nov_dawn_pca_scores, aes(x = PC2, y = PC3, color = Site)) +
  geom_point(size = 0.3) +
  labs(title = "PCA of Dawn Chorus Indices (Nov)") + stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()

plot_nov_dawn

#### November Midday Dataset ---------------------------------------------

# Run PCA on the combined dataset
nov_midd_pca <- run_pca(Nov23_midd_data)

# View PCA summary
summary(nov_midd_pca)
nov_midd_loadings <- nov_midd_pca$rotation
nov_midd_loadings 

# Add PCA scores to the dataset
nov_midd_pca_scores <- as.data.frame(nov_midd_pca$x) %>%
  mutate(Device = Nov23_midd_data$Device, 
         Site = Nov23_midd_data$Site,
         Date = Nov23_midd_data$Date, 
         Time = Nov23_midd_data$Time)

# PCA plot with site colors
plot_nov_midd <- ggplot(nov_midd_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.3) +
  labs(title = "PCA of Midday Indices (Nov)") + stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()

plot_nov_midd

#### November Dusk Chorus Dataset ---------------------------------------------

# Run PCA on the combined dataset
nov_dusk_pca <- run_pca(Nov23_dusk_data)

# View PCA summary
summary(nov_dusk_pca)
nov_dusk_loadings <- nov_dusk_pca$rotation
nov_dusk_loadings 

# Add PCA scores to the dataset
nov_dusk_pca_scores <- as.data.frame(nov_dusk_pca$x) %>%
  mutate(Device = Nov23_dusk_data$Device, 
         Site = Nov23_dusk_data$Site,
         Date =Nov23_dusk_data$Date, 
         Time = Nov23_dusk_data$Time)

# PCA plot with site colors
plot_nov_dusk <- ggplot(nov_dusk_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.3) +
  labs(title = "PCA of Dusk Chorus Indices (Nov)") + stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()

plot_nov_dusk

#### November Midnight Dataset ---------------------------------------------

# Run PCA on the combined dataset
nov_midn_pca <- run_pca(Nov23_midn_data)

# View PCA summary
summary(nov_midn_pca)
nov_midn_loadings <- nov_midn_pca$rotation
nov_midn_loadings 

# Add PCA scores to the dataset
nov_midn_pca_scores <- as.data.frame(nov_midn_pca$x) %>%
  mutate(Device = Nov23_midn_data$Device, 
         Site = Nov23_midn_data$Site,
         Date =Nov23_midn_data$Date, 
         Time = Nov23_midn_data$Time)

# PCA plot with site colors
plot_nov_midn <- ggplot(nov_midn_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.3) +
  labs(title = "PCA of Midnight Indices (Nov)") + stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()

plot_nov_midn

#### November Joint Plots ---------------------------------------------
grid.arrange(plot_nov_full, plot_nov_dawn, plot_nov_midd, plot_nov_dusk, plot_nov_midn, ncol = 2)

#### Nov 3D Plots ---------------------------------------------

# Nov Full Day
threeD_nov<- plot_ly(nov_pca_scores,
               x = ~PC1,
               y = ~PC2,
               z = ~PC3,
               color = ~Site,  # Color points by Site
               text = ~paste("Site:", Site,
                             "<br>Time:", Time,
                             "<br>Date:", Date,
                             "<br>Device:", Device),  # Hover text
               type = 'scatter3d',
               mode = 'markers',
               marker = list(size = 2))
threeD_nov  

# Nov Dawn Chorus
threeD_nov_dawn<- plot_ly(nov_dawn_pca_scores,
                     x = ~PC1,
                     y = ~PC2,
                     z = ~PC3,
                     color = ~Site,  # Color points by Site
                     text = ~paste("Site:", Site,
                                   "<br>Time:", Time,
                                   "<br>Date:", Date,
                                   "<br>Device:", Device),  # Hover text
                     type = 'scatter3d',
                     mode = 'markers',
                     marker = list(size = 2))
threeD_nov_dawn  

# Nov Mid-Day
threeD_nov_midd<- plot_ly(nov_midd_pca_scores,
                          x = ~PC1,
                          y = ~PC2,
                          z = ~PC3,
                          color = ~Site,  # Color points by Site
                          text = ~paste("Site:", Site,
                                        "<br>Time:", Time,
                                        "<br>Date:", Date,
                                        "<br>Device:", Device),  # Hover text
                          type = 'scatter3d',
                          mode = 'markers',
                          marker = list(size = 2))
threeD_nov_midd 

# Nov Dusk
threeD_nov_dusk<- plot_ly(nov_dusk_pca_scores,
                          x = ~PC1,
                          y = ~PC2,
                          z = ~PC3,
                          color = ~Site,  # Color points by Site
                          text = ~paste("Site:", Site,
                                        "<br>Time:", Time,
                                        "<br>Date:", Date,
                                        "<br>Device:", Device),  # Hover text
                          type = 'scatter3d',
                          mode = 'markers',
                          marker = list(size = 2))
threeD_nov_dusk

# Nov Midnight
threeD_nov_midn<- plot_ly(nov_midn_pca_scores,
                          x = ~PC1,
                          y = ~PC2,
                          z = ~PC3,
                          color = ~Site,  # Color points by Site
                          text = ~paste("Site:", Site,
                                        "<br>Time:", Time,
                                        "<br>Date:", Date,
                                        "<br>Device:", Device),  # Hover text
                          type = 'scatter3d',
                          mode = 'markers',
                          marker = list(size = 2))
threeD_nov_midn

# Nov Dusk Chorus
threeD_nov_midn<- plot_ly(nov_midn_pca_scores,
                          x = ~PC1,
                          y = ~PC2,
                          z = ~PC3,
                          color = ~Site,  # Color points by Site
                          text = ~paste("Site:", Site,
                                        "<br>Time:", Time,
                                        "<br>Date:", Date,
                                        "<br>Device:", Device),  # Hover text
                          type = 'scatter3d',
                          mode = 'markers',
                          marker = list(size = 2))
threeD_nov_midn 




### January -------------------------------------------------------------------------------------------

#### January Full Day Dataset -------------------------------------------


# Run PCA on the combined dataset
jan_pca <- run_pca(jan24_fullday_data)

# View PCA summary
summary(jan_pca)
jan_loadings <- jan_pca$rotation
jan_loadings 

# Add PCA scores to the dataset
jan_pca_scores <- as.data.frame(jan_pca$x) %>%
  mutate(Device = jan24_fullday_data$Device, 
         Site = jan24_fullday_data$Site, 
         Date =jan24_fullday_data$Date, 
         Time = jan24_fullday_data$Time )

# PCA plot with site colors
plot_jan_full <- ggplot(jan_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.0001) +
  labs(title = "PCA of Full Day Indices (jan)") + 
  stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()

plot_jan_full

#### January Dawn Dataset -------------------------------------------

# Run PCA on the combined dataset
jan_dawn_pca <- run_pca(jan24_dawn_data)

# View PCA summary
summary(jan_dawn_pca)
jan_dawn_loadings <- jan_dawn_pca$rotation
jan_dawn_loadings 

# Add PCA scores to the dataset
jan_dawn_pca_scores <- as.data.frame(jan_dawn_pca$x) %>%
  mutate(Device = jan24_dawn_data$Device, 
         Site = jan24_dawn_data$Site, 
         Date = jan24_dawn_data$Date, 
         Time = jan24_dawn_data$Time)

# PCA plot with site colors
plot_jan_dawn <- ggplot(jan_dawn_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.3) +
  labs(title = "PCA of Dawn Chorus Indices (jan)") + stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()


plot_jan_dawn


#### January Midday Dataset -------------------------------------------

# Run PCA on the combined dataset
jan_midd_pca <- run_pca(jan24_midd_data)

# View PCA summary
summary(jan_midd_pca)
jan_midd_loadings <- jan_midd_pca$rotation
jan_midd_loadings 

# Add PCA scores to the dataset
jan_midd_pca_scores <- as.data.frame(jan_midd_pca$x) %>%
  mutate(Device = jan24_midd_data$Device, 
         Site = jan24_midd_data$Site, 
         Date = jan24_midd_data$Date, 
         Time = jan24_midd_data$Time)

# PCA plot with site colors
plot_jan_midd <- ggplot(jan_midd_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.3) +
  labs(title = "PCA of midd Chorus Indices (jan)") + stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()


plot_jan_midd



#### January Dusk Dataset -------------------------------------------

# Run PCA on the combined dataset
jan_dusk_pca <- run_pca(jan24_dusk_data)

# View PCA summary
summary(jan_dusk_pca)
jan_dusk_loadings <- jan_dusk_pca$rotation
jan_dusk_loadings 

# Add PCA scores to the dataset
jan_dusk_pca_scores <- as.data.frame(jan_dusk_pca$x) %>%
  mutate(Device = jan24_dusk_data$Device, 
         Site = jan24_dusk_data$Site, 
         Date = jan24_dusk_data$Date, 
         Time = jan24_dusk_data$Time)

# PCA plot with site colors
plot_jan_dusk <- ggplot(jan_dusk_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.3) +
  labs(title = "PCA of dusk Chorus Indices (jan)") + stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()


plot_jan_dusk

#### January Midnight Dataset -------------------------------------------

# Run PCA on the combined dataset
jan_midn_pca <- run_pca(jan24_midn_data)

# View PCA summary
summary(jan_midn_pca)
jan_midn_loadings <- jan_midn_pca$rotation
jan_midn_loadings 

# Add PCA scores to the dataset
jan_midn_pca_scores <- as.data.frame(jan_midn_pca$x) %>%
  mutate(Device = jan24_midn_data$Device, 
         Site = jan24_midn_data$Site, 
         Date = jan24_midn_data$Date, 
         Time = jan24_midn_data$Time)

# PCA plot with site colors
plot_jan_midn <- ggplot(jan_midn_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.3) +
  labs(title = "PCA of Midnight Indices (jan)") + stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()


plot_jan_midn



#### January Joint Plots -------------------------------------------
grid.arrange(plot_jan_full, plot_jan_dawn, plot_jan_midd, plot_jan_dusk, plot_jan_midn)


#### January 3D Plots -------------------------------------------

# jan Full Day
threeD_jan <- plot_ly(jan_pca_scores,
                     x = ~PC1,
                     y = ~PC2,
                     z = ~PC3,
                     color = ~Site,  # Color points by Site
                     text = ~paste("Site:", Site,
                                   "<br>Time:", Time,
                                   "<br>Date:", Date,
                                   "<br>Device:", Device),  # Hover text
                     type = 'scatter3d',
                     mode = 'markers',
                     marker = list(size = 2))
threeD_jan  

# jan Dawn Chorus
threeD_jan_dawn<- plot_ly(jan_dawn_pca_scores,
                          x = ~PC1,
                          y = ~PC2,
                          z = ~PC3,
                          color = ~Site,  # Color points by Site
                          text = ~paste("Site:", Site,
                                        "<br>Time:", Time,
                                        "<br>Date:", Date,
                                        "<br>Device:", Device),  # Hover text
                          type = 'scatter3d',
                          mode = 'markers',
                          marker = list(size = 2))
threeD_jan_dawn  

# jan Mid-Day
threeD_jan_midd<- plot_ly(jan_midd_pca_scores,
                          x = ~PC1,
                          y = ~PC2,
                          z = ~PC3,
                          color = ~Site,  # Color points by Site
                          text = ~paste("Site:", Site,
                                        "<br>Time:", Time,
                                        "<br>Date:", Date,
                                        "<br>Device:", Device),  # Hover text
                          type = 'scatter3d',
                          mode = 'markers',
                          marker = list(size = 2))
threeD_jan_midd 

# jan Dusk
threeD_jan_dusk<- plot_ly(jan_dusk_pca_scores,
                          x = ~PC1,
                          y = ~PC2,
                          z = ~PC3,
                          color = ~Site,  # Color points by Site
                          text = ~paste("Site:", Site,
                                        "<br>Time:", Time,
                                        "<br>Date:", Date,
                                        "<br>Device:", Device),  # Hover text
                          type = 'scatter3d',
                          mode = 'markers',
                          marker = list(size = 2))
threeD_jan_dusk

# jan Midnight
threeD_jan_midn<- plot_ly(jan_midn_pca_scores,
                          x = ~PC1,
                          y = ~PC2,
                          z = ~PC3,
                          color = ~Site,  # Color points by Site
                          text = ~paste("Site:", Site,
                                        "<br>Time:", Time,
                                        "<br>Date:", Date,
                                        "<br>Device:", Device),  # Hover text
                          type = 'scatter3d',
                          mode = 'markers',
                          marker = list(size = 2))
threeD_jan_midn

# jan Dusk Chorus
threeD_jan_midn<- plot_ly(jan_midn_pca_scores,
                          x = ~PC1,
                          y = ~PC2,
                          z = ~PC3,
                          color = ~Site,  # Color points by Site
                          text = ~paste("Site:", Site,
                                        "<br>Time:", Time,
                                        "<br>Date:", Date,
                                        "<br>Device:", Device),  # Hover text
                          type = 'scatter3d',
                          mode = 'markers',
                          marker = list(size = 2))
threeD_jan_midn 




q

#### Oda Temporal Series -------------------------------------------

# FULL
# Run PCA on the combined dataset
oda_pca <- run_pca(oda_season_data)

# View PCA summary
summary(oda_pca)
oda_loadings <- oda_pca$rotation
oda_loadings 

# Add PCA scores to the dataset
oda_pca_scores <- as.data.frame(oda_pca$x) %>%
  mutate(Device = oda_season_data$Device, 
         Site = oda_season_data$Site, 
         Date = oda_season_data$Date, 
         Month = oda_season_data$Month,
         Time = oda_season_data$Time )

# PCA plot with site colors
plot_oda_temporal <- ggplot(oda_pca_scores, aes(x = PC1, y = PC2, color = Month)) +
  geom_point(size = 0.0001) +
  labs(title = "PCA of Stung Oda indices across year") + 
  stat_ellipse(aes(colour = Month), type = "norm", level = 0.4) +
  theme_minimal()

plot_oda_temporal

# Dawn Choruses

# Run PCA on the combined dataset
oda_dawn_pca <- run_pca(oda_dawn_data)

# View PCA summary
summary(oda_dawn_pca)
oda_dawn_loadings <- oda_dawn_pca$rotation
oda_dawn_loadings 

# Add PCA scores to the dataset
oda_dawn_pca_scores <- as.data.frame(oda_dawn_pca$x) %>%
  mutate(Device = oda_dawn_data$Device, 
         Site = oda_dawn_data$Site, 
         Date = oda_dawn_data$Date, 
         Month = oda_dawn_data$Month,
         Time = oda_dawn_data$Time )

# PCA plot with site colors
plot_oda_dawn_temporal <- ggplot(oda_dawn_pca_scores, aes(x = PC1, y = PC2, color = Month)) +
  geom_point(size = 0.0001) +
  labs(title = "PCA of Stung Oda dawn indices") + 
  stat_ellipse(aes(colour = Month), type = "norm", level = 0.4) +
  theme_minimal()

plot_oda_dawn_temporal

