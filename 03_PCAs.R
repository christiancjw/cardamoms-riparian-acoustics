library(ggplot2)
library(plotly)
library(gridExtra)
library(cowplot)

#### Data Read in ------------------

# November Data Reading 
nov23_fullday_data <- read.csv("clean_data/Datasets/nov23_fullday_data.csv", colClasses = c("Time" = "character"))
nov23_dawn_data <- read.csv("clean_data/Datasets/nov23_dawn_data.csv", colClasses = c("Time" = "character"))

nov23_fullday_data <- read.csv("clean_data/Datasets/nov23_fullday_data.csv")
nov23_dawn_data <- read.csv("clean_data/Datasets/nov23_dawn_data.csv")
nov23_midd_data <- read.csv("clean_data/Datasets/nov23_midd_data.csv")
nov23_dusk_data <- read.csv("clean_data/Datasets/nov23_dusk_data.csv")
nov23_midn_data <- read.csv("clean_data/Datasets/nov23_midn_data.csv")

# January Data Reading 
jan24_fullday_data <- read.csv("clean_data/Datasets/jan24_fullday_data.csv")
jan24_dawn_data <- read.csv("clean_data/Datasets/jan24_dawn_data.csv")
jan24_midd_data <- read.csv("clean_data/Datasets/jan24_midd_data.csv")
jan24_dusk_data <- read.csv("clean_data/Datasets/jan24_dusk_data.csv")
jan24_midn_data <- read.csv("clean_data/Datasets/jan24_midn_data.csv")

# April Data Reading 
apr24_fullday_data <- read.csv("clean_data/Datasets/apr24_fullday_data.csv")
apr24_dawn_data <- read.csv("clean_data/Datasets/apr24_dawn_data.csv")
apr24_midd_data <- read.csv("clean_data/Datasets/apr24_midd_data.csv")
apr24_dusk_data <- read.csv("clean_data/Datasets/apr24_dusk_data.csv")
apr24_midn_data <- read.csv("clean_data/Datasets/apr24_midn_data.csv")

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


#### Function to extract loadings  --------------------
# Scale loadings for visualization (adjust multiplier if needed)
scale_factor <- 15  # Adjust this to change arrow length

# Add arrows for loadings  - next two lines for adding to ggplots
geom_segment(data = nov23_loadings_df, aes(x = 0, y = 0, xend = PC1 * scale_factor, yend = PC2 * scale_factor),
             arrow = arrow(length = unit(0.2, "inches")),
             color = "#2d2d2d", linewidth = 1, alpha = 0.7) 
  
  # Add labels for loadings
  geom_text(data = nov23_loadings_df, aes(x = PC1 * scale_factor, y = PC2 * scale_factor, label = Index),
            color = "#2d2d2d", size = 3, vjust = -0.5) 

#### November Full Day Dataset ---------------------------------------------

# Run PCA on the combined dataset
nov23_pca <- run_pca(nov23_fullday_data)

# View PCA summary
summary(nov23_pca)
nov23_loadings <- nov23_pca$rotation
nov23_loadings 

# Extract Loading Arrow Data
nov23_loadings_df <- as.data.frame(nov23_pca$rotation)
nov23_loadings_df$Index <- rownames(nov23_loadings_df)  # Add names of indices

# Add PCA scores to the dataset
nov23_pca_scores <- as.data.frame(nov23_pca$x) %>%
  mutate(Device = nov23_fullday_data$Device, 
         Site = nov23_fullday_data$Site, 
         Date =nov23_fullday_data$Date, 
         Time = nov23_fullday_data$Time,
         Strahler = nov23_fullday_data$Strahler,
         Disturbance = nov23_fullday_data$Disturbance,
         Branch = nov23_fullday_data$Branch)

# PCA plot with site colors
plot_nov23_full <- ggplot(nov23_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.0001) +
  labs(title = "PCA of Full Day Indices (November 2023)") + 
  stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal() + xlim(-5, 8) + ylim(-8, 6)

plot_nov23_full

#### November Dawn Chorus Dataset ---------------------------------------------

# Run PCA on the combined dataset
nov23_dawn_pca <- run_pca(nov23_dawn_data)

# View PCA summary
summary(nov23_dawn_pca)
nov23_dawn_loadings <- nov23_dawn_pca$rotation
nov23_dawn_loadings 

# Add PCA scores to the dataset
nov23_dawn_pca_scores <- as.data.frame(nov23_dawn_pca$x) %>%
  mutate(Device = nov23_dawn_data$Device, 
         Site = nov23_dawn_data$Site, 
         Date = nov23_dawn_data$Date, 
         Time = nov23_dawn_data$Time,
         Strahler = nov23_dawn_data$Strahler,
         Disturbance = nov23_dawn_data$Disturbance,
         Branch = nov23_dawn_data$Branch)

# PCA plot with site colors
plot_nov23_dawn <- ggplot(nov23_dawn_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.3) +
  labs(title = "PCA of Dawn Chorus Indices (November 2023)") + 
  stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) + 
  theme_minimal()

plot_nov23_dawn

#### November Midday Dataset ---------------------------------------------

# Run PCA on the combined dataset
nov23_midd_pca <- run_pca(nov23_midd_data)

# View PCA summary
summary(nov23_midd_pca)
nov_midd_loadings <- nov23_midd_pca$rotation
nov_midd_loadings 

# Add PCA scores to the dataset
nov23_midd_pca_scores <- as.data.frame(nov23_midd_pca$x) %>%
  mutate(Device = nov23_midd_data$Device, 
         Site = nov23_midd_data$Site,
         Date = nov23_midd_data$Date, 
         Time = nov23_midd_data$Time,
         Strahler = nov23_midd_data$Strahler,
         Disturbance = nov23_midd_data$Disturbance,
         Branch = nov23_midd_data$Branch)

# PCA plot with site colors
plot_nov23_midd <- ggplot(nov23_midd_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.3) +
  labs(title = "PCA of Midday Indices (November 2023)") + 
  stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()

plot_nov23_midd

#### November Dusk Chorus Dataset ---------------------------------------------

# Run PCA on the combined dataset
nov23_dusk_pca <- run_pca(nov23_dusk_data)

# View PCA summary
summary(nov23_dusk_pca)
nov23_dusk_loadings <- nov23_dusk_pca$rotation
nov23_dusk_loadings 

# Add PCA scores to the dataset
nov23_dusk_pca_scores <- as.data.frame(nov23_dusk_pca$x) %>%
  mutate(Device = nov23_dusk_data$Device, 
         Site = nov23_dusk_data$Site,
         Date = nov23_dusk_data$Date, 
         Time = nov23_dusk_data$Time,
         Strahler = nov23_dusk_data$Strahler,
         Disturbance = nov23_dusk_data$Disturbance,
         Branch = nov23_dusk_data$Branch)

# PCA plot with site colors
plot_nov23_dusk <- ggplot(nov23_dusk_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.3) +
  labs(title = "PCA of Dusk Chorus Indices (November 2023)") + 
  stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()

plot_nov23_dusk

#### November Midnight Dataset ---------------------------------------------

# Run PCA on the combined dataset
nov23_midn_pca <- run_pca(nov23_midn_data)

# View PCA summary
summary(nov23_midn_pca)
nov23_midn_loadings <- nov23_midn_pca$rotation
nov23_midn_loadings 

# Add PCA scores to the dataset
nov23_midn_pca_scores <- as.data.frame(nov23_midn_pca$x) %>%
  mutate(Device = nov23_midn_data$Device, 
         Site = nov23_midn_data$Site,
         Date = nov23_midn_data$Date, 
         Time = nov23_midn_data$Time,
         Strahler = nov23_midn_data$Strahler,
         Disturbance = nov23_midn_data$Disturbance,
         Branch = nov23_midn_data$Branch)

# PCA plot with site colors
plot_nov23_midn <- ggplot(nov23_midn_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.3) +
  labs(title = "PCA of Midnight Indices (November 2023)") + 
  stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()

plot_nov23_midn

#### November Joint Plots ---------------------------------------------
grid.arrange(plot_nov23_full, plot_nov23_dawn, plot_nov23_midd, plot_nov23_dusk, plot_nov23_midn, ncol = 2)

#### November 3D Plots ---------------------------------------------

# Nov Full Day
threeD_nov23<- plot_ly(nov23_pca_scores,
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
threeD_nov23  

# Nov Dawn Chorus
threeD_nov23_dawn<- plot_ly(nov23_dawn_pca_scores,
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
threeD_nov23_dawn  

# Nov Mid-Day
threeD_nov23_midd<- plot_ly(nov23_midd_pca_scores,
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
threeD_nov23_midd 

# Nov Dusk
threeD_nov23_dusk<- plot_ly(nov23_dusk_pca_scores,
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
threeD_nov23_dusk

# Nov Midnight
threeD_nov23_midn<- plot_ly(nov23_midn_pca_scores,
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
threeD_nov23_midn

# Nov Dusk Chorus
threeD_nov23_midn<- plot_ly(nov23_midn_pca_scores,
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
threeD_nov23_midn 




#### January Full Day Dataset -------------------------------------------


# Run PCA on the combined dataset
jan24_pca <- run_pca(jan24_fullday_data)

# View PCA summary
summary(jan24_pca)
jan24_loadings <- jan24_pca$rotation
jan24_loadings 

# Add PCA scores to the dataset
jan24_pca_scores <- as.data.frame(jan24_pca$x) %>%
  mutate(Device = jan24_fullday_data$Device, 
         Site = jan24_fullday_data$Site, 
         Date =jan24_fullday_data$Date, 
         Time = jan24_fullday_data$Time,
         Strahler = jan24_fullday_data$Strahler,
         Disturbance = jan24_fullday_data$Disturbance,
         Branch = jan24_fullday_data$Branch)

# Extract Loading Arrow Data
jan24_loadings_df <- as.data.frame(jan24_pca$rotation)
jan24_loadings_df$Index <- rownames(jan24_loadings_df)  # Add names of indices

# PCA plot with site colors
plot_jan24_full <- ggplot(jan24_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.1) +
  labs(title = "PCA of Full Day Indices (January 2024)") + 
  stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal() + xlim(-5, 10) + ylim(-6, 6) + 
  
  geom_segment(data = nov23_loadings_df, 
               aes(x = 0, y = 0, xend = PC1 * scale_factor, yend = PC2 * scale_factor),
               arrow = arrow(length = unit(0.2, "inches")),
              color = "#2d2d2d", linewidth = 1, alpha = 0.7) +

# Add labels for loadings
  geom_text(data = nov23_loadings_df, aes(x = PC1 * scale_factor, y = PC2 * scale_factor, label = Index),
          color = "#2d2d2d", size = 3, vjust = -0.5, hjust = -0.3) 

plot_jan24_full

ggsave("loadings.png", plot = plot_jan24_full, 
       path = NULL, width = 16, height = 8, dpi = 600)

#### January Dawn Dataset -------------------------------------------

# Run PCA on the combined dataset
jan24_dawn_pca <- run_pca(jan24_dawn_data)

# View PCA summary
summary(jan24_dawn_pca)
jan24_dawn_loadings <- jan24_dawn_pca$rotation
jan24_dawn_loadings 

# Add PCA scores to the dataset
jan24_dawn_pca_scores <- as.data.frame(jan24_dawn_pca$x) %>%
  mutate(Device = jan24_dawn_data$Device, 
         Site = jan24_dawn_data$Site, 
         Date = jan24_dawn_data$Date, 
         Time = jan24_dawn_data$Time,
         Strahler = jan24_dawn_data$Strahler,
         Disturbance = jan24_dawn_data$Disturbance,
         Branch = jan24_dawn_data$Branch)

# PCA plot with site colors
plot_jan24_dawn <- ggplot(jan24_dawn_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.25) +
  labs(title = "PCA of Dawn Chorus Indices (January 2024)") + 
  stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()

plot_jan24_dawn

#### January Midday Dataset -------------------------------------------

# Run PCA on the combined dataset
jan24_midd_pca <- run_pca(jan24_midd_data)

# View PCA summary
summary(jan24_midd_pca)
jan24_midd_loadings <- jan24_midd_pca$rotation
jan24_midd_loadings 

# Add PCA scores to the dataset
jan24_midd_pca_scores <- as.data.frame(jan24_midd_pca$x) %>%
  mutate(Device = jan24_midd_data$Device, 
         Site = jan24_midd_data$Site, 
         Date = jan24_midd_data$Date, 
         Time = jan24_midd_data$Time,
         Strahler = jan24_midd_data$Strahler,
         Disturbance = jan24_midd_data$Disturbance,
         Branch = jan24_midd_data$Branch)

# PCA plot with site colors
plot_jan24_midd <- ggplot(jan24_midd_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.25) +
  labs(title = "PCA of Midday Indices (January 2024)") +
  stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()

plot_jan24_midd

#### January Dusk Dataset -------------------------------------------

# Run PCA on the combined dataset
jan24_dusk_pca <- run_pca(jan24_dusk_data)

# View PCA summary
summary(jan24_dusk_pca)
jan24_dusk_loadings <- jan24_dusk_pca$rotation
jan24_dusk_loadings 

# Add PCA scores to the dataset
jan24_dusk_pca_scores <- as.data.frame(jan24_dusk_pca$x) %>%
  mutate(Device = jan24_dusk_data$Device, 
         Site = jan24_dusk_data$Site, 
         Date = jan24_dusk_data$Date, 
         Time = jan24_dusk_data$Time,
         Strahler = jan24_dusk_data$Strahler,
         Disturbance = jan24_dusk_data$Disturbance,
         Branch = jan24_dusk_data$Branch)

# PCA plot with site colors
plot_jan24_dusk <- ggplot(jan24_dusk_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.25) +
  labs(title = "PCA of Dusk Chorus Indices (January 2024)") + 
  stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()

plot_jan24_dusk

#### January Midnight Dataset -------------------------------------------

# Run PCA on the combined dataset
jan24_midn_pca <- run_pca(jan24_midn_data)

# View PCA summary
summary(jan24_midn_pca)
jan24_midn_loadings <- jan24_midn_pca$rotation
jan24_midn_loadings 

# Add PCA scores to the dataset
jan24_midn_pca_scores <- as.data.frame(jan24_midn_pca$x) %>%
  mutate(Device = jan24_midn_data$Device, 
         Site = jan24_midn_data$Site, 
         Date = jan24_midn_data$Date, 
         Time = jan24_midn_data$Time,
         Strahler = jan24_midn_data$Strahler,
         Disturbance = jan24_midn_data$Disturbance,
         Branch = jan24_midn_data$Branch)

# PCA plot with site colors
plot_jan24_midn <- ggplot(jan24_midn_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.25) +
  labs(title = "PCA of Midnight Indices (January 2024)") + 
  stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()


plot_jan24_midn

#### January Joint Plots -------------------------------------------
plot_jan24_dawn_grid <- plot_jan24_dawn + theme(legend.position = "none") 
plot_jan24_midd_grid <- plot_jan24_midd + theme(legend.position = "none")
plot_jan24_dusk_grid <- plot_jan24_dusk + theme(legend.position = "none")
plot_jan24_midn_grid <- plot_jan24_midn + theme(legend.position = "none")

jan24legend <- get_legend(plot_jan24_dawn)

plot(jan24legend)

jandiel <- grid.arrange(plot_jan24_dawn_grid, plot_jan24_midd_grid, plot_jan24_dusk_grid, plot_jan24_midn_grid)

ggsave("jandiel.png", plot = jandiel, 
       path = NULL, width = 16, height = 8, dpi = 1800)

ggsave("jandiellegend.png", plot = jan24legend, 
       path = NULL, width = 6, height = 2.5, dpi = 600)

#### January 3D Plots -------------------------------------------

# jan Full Day
threeD_jan <- plot_ly(jan24_pca_scores,
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
threeD_jan24_dawn<- plot_ly(jan24_dawn_pca_scores,
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
threeD_jan24_dawn  

# jan Mid-Day
threeD_jan24_midd<- plot_ly(jan24_midd_pca_scores,
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
threeD_jan24_midd 

# jan Dusk
threeD_jan24_dusk<- plot_ly(jan24_dusk_pca_scores,
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
threeD_jan24_dusk

# jan Midnight
threeD_jan24_midn<- plot_ly(jan24_midn_pca_scores,
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
threeD_jan24_midn

# jan Dusk Chorus
threeD_jan24_midn<- plot_ly(jan24_midn_pca_scores,
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
threeD_jan24_midn 


#### April Full Day Dataset -------------------------------------------


# Run PCA on the combined dataset
apr24_pca <- run_pca(apr24_fullday_data)

# View PCA summary
summary(apr24_pca)
apr24_loadings <- apr24_pca$rotation
apr24_loadings 

# Add PCA scores to the dataset
apr24_pca_scores <- as.data.frame(apr24_pca$x) %>%
  mutate(Device = apr24_fullday_data$Device, 
         Site = apr24_fullday_data$Site, 
         Date =apr24_fullday_data$Date, 
         Time = apr24_fullday_data$Time,
         Strahler = apr24_fullday_data$Strahler,
         Disturbance = apr24_fullday_data$Disturbance,
         Branch = apr24_fullday_data$Branch)

# PCA plot with site colors
plot_apr24_full <- ggplot(apr24_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.0001) +
  labs(title = "PCA of Full Day Indices (April 2024)") + 
  stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal() + xlim(-5, 8) + ylim(-8, 8)

plot_apr24_full

#### April Dawn Dataset -------------------------------------------

# Run PCA on the combined dataset
apr24_dawn_pca <- run_pca(apr24_dawn_data)

# View PCA summary
summary(apr24_dawn_pca)
apr24_dawn_loadings <- apr24_dawn_pca$rotation
apr24_dawn_loadings 

# Add PCA scores to the dataset
apr24_dawn_pca_scores <- as.data.frame(apr24_dawn_pca$x) %>%
  mutate(Device = apr24_dawn_data$Device, 
         Site = apr24_dawn_data$Site, 
         Date = apr24_dawn_data$Date, 
         Time = apr24_dawn_data$Time,
         Strahler = apr24_dawn_data$Strahler,
         Disturbance = apr24_dawn_data$Disturbance,
         Branch = apr24_dawn_data$Branch)

# PCA plot with site colors
plot_apr24_dawn <- ggplot(apr24_dawn_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.3) +
  labs(title = "PCA of Dawn Chorus Indices (April 2024)") + 
  stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()

plot_apr24_dawn

#### April Midday Dataset -------------------------------------------

# Run PCA on the combined dataset
apr24_midd_pca <- run_pca(apr24_midd_data)

# View PCA summary
summary(apr24_midd_pca)
apr24_midd_loadings <- apr24_midd_pca$rotation
apr24_midd_loadings 

# Add PCA scores to the dataset
apr24_midd_pca_scores <- as.data.frame(apr24_midd_pca$x) %>%
  mutate(Device = apr24_midd_data$Device, 
         Site = apr24_midd_data$Site, 
         Date = apr24_midd_data$Date, 
         Time = apr24_midd_data$Time,
         Strahler = apr24_midd_data$Strahler,
         Disturbance = apr24_midd_data$Disturbance,
         Branch = apr24_midd_data$Branch)

# PCA plot with site colors
plot_apr24_midd <- ggplot(apr24_midd_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.3) +
  labs(title = "PCA of Midday Chorus Indices (apr)") +
  stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()

plot_apr24_midd

#### April Dusk Dataset -------------------------------------------

# Run PCA on the combined dataset
apr24_dusk_pca <- run_pca(apr24_dusk_data)

# View PCA summary
summary(apr24_dusk_pca)
apr24_dusk_loadings <- apr24_dusk_pca$rotation
apr24_dusk_loadings 

# Add PCA scores to the dataset
apr24_dusk_pca_scores <- as.data.frame(apr24_dusk_pca$x) %>%
  mutate(Device = apr24_dusk_data$Device, 
         Site = apr24_dusk_data$Site, 
         Date = apr24_dusk_data$Date, 
         Time = apr24_dusk_data$Time,
         Strahler = apr24_dusk_data$Strahler,
         Disturbance = apr24_dusk_data$Disturbance,
         Branch = apr24_dusk_data$Branch)

# PCA plot with site colors
plot_apr24_dusk <- ggplot(apr24_dusk_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.3) +
  labs(title = "PCA of Dusk Chorus Indices (April 2024)") + 
  stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()

plot_apr24_dusk

#### April Midnight Dataset -------------------------------------------

# Run PCA on the combined dataset
apr24_midn_pca <- run_pca(apr24_midn_data)

# View PCA summary
summary(apr24_midn_pca)
apr24_midn_loadings <- apr24_midn_pca$rotation
apr24_midn_loadings 

# Add PCA scores to the dataset
apr24_midn_pca_scores <- as.data.frame(apr24_midn_pca$x) %>%
  mutate(Device = apr24_midn_data$Device, 
         Site = apr24_midn_data$Site, 
         Date = apr24_midn_data$Date, 
         Time = apr24_midn_data$Time,
         Strahler = apr24_midn_data$Strahler,
         Disturbance = apr24_midn_data$Disturbance,
         Branch = apr24_midn_data$Branch)

# PCA plot with site colors
plot_apr24_midn <- ggplot(apr24_midn_pca_scores, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(size = 0.3) +
  labs(title = "PCA of Midnight Indices (April 2024)") + 
  stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) +
  theme_minimal()

plot_apr24_midn

#### April Joint Plots -------------------------------------------
plot_apr24_full + theme(legend.position = "none")

grid.arrange(plot_apr24_dawn, plot_apr24_midd, plot_apr24_dusk, plot_apr24_midn)


#### April 3D Plots -------------------------------------------

# apr Full Day
threeD_apr <- plot_ly(apr24_pca_scores,
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
threeD_apr  

# apr Dawn Chorus
threeD_apr24_dawn<- plot_ly(apr24_dawn_pca_scores,
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
threeD_apr24_dawn  

# apr Mid-Day
threeD_apr24_midd<- plot_ly(apr24_midd_pca_scores,
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
threeD_apr24_midd 

# apr Dusk
threeD_apr24_dusk<- plot_ly(apr24_dusk_pca_scores,
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
threeD_apr24_dusk

# apr Midnight
threeD_apr24_midn<- plot_ly(apr24_midn_pca_scores,
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
threeD_apr24_midn

# apr Dusk Chorus
threeD_apr24_midn<- plot_ly(apr24_midn_pca_scores,
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
threeD_apr24_midn 


#### Presentation Plots -------------------------------------

# Joint FullDay Plots
plot_nov23_full_nolab <- plot_nov23_full + theme(legend.position = "none")
plot_jan24_full_nolab <- plot_jan24_full + theme(legend.position = "none")
Fullplots <- grid.arrange(plot_nov23_full_nolab, plot_jan24_full_nolab, plot_apr24_full, ncol = 3)

ggsave("pulling.png", plot = Fullplots, 
       path = NULL, width = 16, height = 8, dpi = 1800)



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

