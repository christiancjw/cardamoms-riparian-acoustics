# Load libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load dataset
df <- read_csv("clean_data/datasets/PCAs/rainless_global_single_pca.csv")

# Convert variables
df <- df %>%
  mutate(
    Site = as.factor(Site),
    QBR = as.factor(QBR),         # treat QBR as categorical (change to numeric if continuous)
    Time = sprintf("%06d", Time), # pad Time values (e.g., 500 -> "000500")
    Hour = as.numeric(substr(Time, 1, 2)) # extract hour for plotting
  )

# Basic PCA scatterplot by Site
ggplot(df, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(alpha = 0.4, size = 1) +
  theme_minimal() +
  labs(title = "PCA Scatterplot by Site")

# PCA scatterplot faceted by QBR
ggplot(df, aes(x = PC1, y = PC2, color = Site)) +
  geom_point(alpha = 0.4, size = 1) +
  facet_wrap(~QBR) +
  theme_minimal() +
  labs(title = "PCA by Site and QBR")

# PCA scatterplot colored by Hour of day
ggplot(df, aes(x = PC1, y = PC2, color = Hour)) +
  geom_point(alpha = 0.4, size = 1) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "PCA by Time of Day (Hour)", color = "Hour")

# Interaction: Site vs QBR on PCA space
ggplot(df, aes(x = PC1, y = PC2, color = Site, shape = QBR)) +
  geom_point(alpha = 0.5, size = 1.5) +
  theme_minimal() +
  labs(title = "Effect of Site and QBR on Acoustic Index PCA")

# Boxplot of PC1 across Sites and QBR
ggplot(df, aes(x = Site, y = PC1, fill = QBR)) +
  geom_boxplot(outlier.size = 0.5) +
  theme_minimal() +
  labs(title = "Distribution of PC1 across Sites and QBR")

site_colors <- c(
  "TaChey" = "#103F96",
  "Arai" = "#3DA9C7", 
  "Oda" = "#53D4FF",
  "KnaongBatSa" = "#1d601d",
  "TaSay" = "#7DBC62", 
  "Kronomh" = "#74EAA3", 
  "DamFive" = "#b2a539",  
  "TangRang" = "#e8d642",
  "Kravanh Bridge" = "#b5473a",  
  "PursatTown" = "#f87060"   
)

ggplot(df, aes(x = PC2, y = PC3, color = Site)) +
  geom_point(size = 0.0001, alpha = 0.5) +
  labs(title = "PCA of Global Indices") + 
  stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) + 
  scale_color_manual(values = site_colors) +
  theme_minimal() + xlim(-5, 3) + ylim(-8, 6)

ggplot(df, aes(x = PC1, y = PC2, color = QBR)) +
  geom_point(size = 0.0001, alpha = 0.5) +
  labs(title = "PCA of Global Indices") + 
  stat_ellipse(aes(colour = Site), type = "norm", level = 0.4) + 
  theme_minimal() + xlim(-5, 3) + ylim(-8, 6)

