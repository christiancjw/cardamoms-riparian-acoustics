library(ggplot2)
library(gridExtra)
library(dplyr)

## Individual Index Analysis

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


### November ACI  -------------------------------------------------------------------------------------------

# Single Day Subset
nov23_single_ACI <- as.data.frame(nov23_fullday_data) %>% 
  select(Site, Device, 
         Date, Time, 
         Strahler, Month, 
         AcousticComplexity)  %>% 
  filter(Date == 20231125) 

glimpse(nov23_single_ACI)

nov_23_single_ACI_plots <- nov23_single_ACI %>%
  split(.$Site) %>%
  lapply(function(data) {
    ggplot(data, aes(x = as.numeric(Time), y = AcousticComplexity, color = Device)) +
      geom_line() +
      labs(title = unique(data$Site), x = "Time (HHMM)", y = "Acoustic Index") +
      theme_minimal()
  })

grid.arrange(grobs = nov_23_single_ACI_plots, ncol = 2)

# Mean Average Subset
nov_mean_ACI_data <- nov23_fullday_data %>%
  mutate(Date = as.numeric(Date)) %>%
  group_by(Site, Time) %>%
  summarise(NovAcousticComplexMean = mean(AcousticComplexity, na.rm = TRUE), .groups = "drop")

# Mean Plotting
ggplot(nov_mean_ACI_data, aes(x = as.numeric(Time), y = NovAcousticComplexMean, color = Site)) +
  geom_line() +
  labs(title = "Average Acoustic Complexity November 2023", x = "Time (HHMM)", y = "Acoustic Index") +
  theme_minimal()

# Mean Plotting: Smoothed Plot - Shaded area shows 95% confidence
ggplot(nov_mean_ACI_data, aes(x = as.numeric(Time), y = NovAcousticComplexMean, color = Site)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +  # Smoothed curve with confidence bands
  labs(title = "Mean Mid-Frequency Cover November 2023", x = "Time (HHMM)", y = "MFC") +
  theme_minimal()

# Median Average Subset
nov_median_ACI_data <- nov23_fullday_data %>%
  mutate(Date = as.numeric(Date)) %>%
  group_by(Site, Time) %>%
  summarise(NovAcousticComplexMedian = median(AcousticComplexity, na.rm = TRUE), .groups = "drop")

# Median Plotting: Line Plot
ggplot(nov_median_ACI_data, aes(x = as.numeric(Time), y = NovAcousticComplexMedian, color = Site)) +
  geom_line() +
  labs(title = "Median Acoustic Complexity November 2023", x = "Time (HHMM)", y = "Acoustic Index") +
  theme_minimal()

# Median Plotting: Smoothed Plot - Shaded area shows 95% confidence
ggplot(nov_median_ACI_data, aes(x = as.numeric(Time), y = NovAcousticComplexMedian, color = Site)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +  # Smoothed curve with confidence bands
  labs(title = "Mean Mid-Frequency Cover November 2023", x = "Time (HHMM)", y = "MFC") +
  theme_minimal()



### November MFC  -------------------------------------------------------------------------------------------

# Single Day Subset
nov23_single_MFC <- as.data.frame(nov23_fullday_data) %>% 
  select(Site, Device, 
         Date, Time, 
         Strahler, Month, 
         MidFreqCover)  %>% 
  filter(Date == 20231125) 

# Check Data
glimpse(nov23_single_MFC)

# Plot Single Day MFC (20231125)
nov23_single_MFC_plots <- nov23_single_MFC %>%
  split(.$Site) %>%
  lapply(function(data) {
    ggplot(data, aes(x = as.numeric(Time), y = AcousticComplexity, color = Device)) +
      geom_line() +
      labs(title = unique(data$Site), x = "Time (HHMM)", y = "Acoustic Index") +
      theme_minimal()
  })

grid.arrange(grobs = nov23_single_MFC_plots, ncol = 2)

# Extract Mean MFC November 2023
nov_mean_MFC_data <- nov23_fullday_data %>%
  mutate(Date = as.numeric(Date)) %>%
  group_by(Site, Time) %>%
  summarise(NovMidFreqCoverMean = mean(NovMidFreqCover, na.rm = TRUE), .groups = "drop")

# Plot Mean MFC November 2023
ggplot(nov_mean_MFC_data, aes(x = as.numeric(Time), y = MidFreqCover, color = Site)) +
  geom_line() +
  labs(title = "Mean Mid-Frequency Cover November 2023", x = "Time (HHMM)", y = "Acoustic Index") +
  theme_minimal()

# Mean November 2023 MFC Plotting: Smoothed Plot - Shaded area shows 95% confidence
ggplot(nov_mean_MFC_data, aes(x = as.numeric(Time), y = MidFreqCover, color = Site)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +  # Smoothed curve with confidence bands
  labs(title = "Mean Mid-Frequency Cover November 2023", x = "Time (HHMM)", y = "MFC") +
  theme_minimal()

# Extract Median MFC November 2023
nov_median_MFC_data <- nov23_fullday_data %>%
  mutate(Date = as.numeric(Date)) %>%
  group_by(Site, Time) %>%
  summarise(NovMidFreqCoverMedian = median(MidFreqCover, na.rm = TRUE), .groups = "drop")

# Plot Median MFC November 2023
ggplot(nov_median_MFC_data, aes(x = as.numeric(Time), y = NovMidFreqCoverMedian, color = Site)) +
  geom_line() +
  labs(title = "Median Mid-Frequency Cover November 2023", x = "Time (HHMM)", y = "Acoustic Index") +
  theme_minimal()

# Median November 2023 MFC Plotting: Smoothed Plot - Shaded area shows 95% confidence
ggplot(nov_median_MFC_data, aes(x = as.numeric(Time), y = NovMidFreqCoverMedian, color = Site)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +  # Smoothed curve with confidence bands
  labs(title = "Mean Mid-Frequency Cover November 2023", x = "Time (HHMM)", y = "MFC") +
  theme_minimal()



### November LFC  -------------------------------------------------------------------------------------------

# Single Day Subset
nov23_single_LFC <- as.data.frame(nov23_fullday_data) %>% 
  select(Site, Device, 
         Date, Time, 
         Strahler, Month, 
         ThreeGramCount)  %>% 
  filter(Date == 20231125) 

glimpse(nov23_single_LFC)

nov23_single_LFC_plots <- nov23_single_LFC %>%
  split(.$Site) %>%
  lapply(function(data) {
    ggplot(data, aes(x = as.numeric(Time), y = ThreeGramCount, color = Device)) +
      geom_line() +
      labs(title = unique(data$Site), x = "Time (HHMM)", y = "Acoustic Index") +
      theme_minimal()
  })

grid.arrange(grobs = nov23_single_LFC_plots, ncol = 2)

# Mean Average Subset
nov23_mean_LFC_data <- nov23_fullday_data %>%
  mutate(Date = as.numeric(Date)) %>%
  group_by(Site, Time) %>%
  summarise(NovThreeGramCountMean = mean(ThreeGramCount, na.rm = TRUE), .groups = "drop")

# Mean Plotting
ggplot(nov23_mean_LFC_data, aes(x = as.numeric(Time), y = NovThreeGramCountMean, color = Site)) +
  geom_line() +
  labs(title = "Mean Low-Frequency Cover November 2023", x = "Time (HHMM)", y = "Acoustic Index") +
  theme_minimal()

# Mean Plotting: Smoothed Plot - Shaded area shows 95% confidence
ggplot(nov23_mean_LFC_data, aes(x = as.numeric(Time), y = NovThreeGramCountMean, color = Site)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +  # Smoothed curve with confidence bands
  labs(title = "Mean Low-Frequency Cover November 2023", x = "Time (HHMM)", y = "LFC") +
  theme_minimal()

# Median Average Subset
nov23_median_LFC_data <- nov23_fullday_data %>%
  mutate(Date = as.numeric(Date)) %>%
  group_by(Site, Time) %>%
  summarise(NovThreeGramCountMedian = median(ThreeGramCount, na.rm = TRUE), .groups = "drop")

# Median Plotting: Line Plot
ggplot(nov23_median_LFC_data, aes(x = as.numeric(Time), y = NovThreeGramCountMedian, color = Site)) +
  geom_line() +
  labs(title = "Median Low-Frequency Cover November 2023", x = "Time (HHMM)", y = "Acoustic Index") +
  theme_minimal()

# Median Plotting: Smoothed Plot - Shaded area shows 95% confidence
ggplot(nov23_median_LFC_data, aes(x = as.numeric(Time), y = NovThreeGramCountMedian, color = Site)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +  # Smoothed curve with confidence bands
  labs(title = "Median Low-Frequency Cover November 2023", x = "Time (HHMM)", y = "LFC") +
  theme_minimal()



### November TGC  -------------------------------------------------------------------------------------------


# Single Day Subset
nov23_single_TGC <- as.data.frame(nov23_fullday_data) %>% 
  select(Site, Device, 
         Date, Time, 
         Strahler, Month, 
         ThreeGramCount)  %>% 
  filter(Date == 20231125) 

glimpse(nov23_single_TGC)

nov_23_single_TGC_plots <- TGC_nov23_single_TGC %>%
  split(.$Site) %>%
  lapply(function(data) {
    ggplot(data, aes(x = as.numeric(Time), y = ThreeGramCount, color = Device)) +
      geom_line() +
      labs(title = unique(data$Site), x = "Time (HHMM)", y = "Three Gram Count") +
      theme_minimal()
  })

grid.arrange(grobs = nov_23_single_TGC_plots, ncol = 2)

# Mean Average Subset
nov23_mean_TGC_data <- nov23_fullday_data %>%
  mutate(Date = as.numeric(Date)) %>%
  group_by(Site, Time) %>%
  summarise(NovThreeGramCountMean = mean(ThreeGramCount, na.rm = TRUE), .groups = "drop")

# Mean Plotting
ggplot(nov23_mean_TGC_data, aes(x = as.numeric(Time), y = NovThreeGramCountMean, color = Site)) +
  geom_line() +
  labs(title = "Three Gram Count November 2023", x = "Time (HHMM)", y = "Acoustic Index") +
  theme_minimal()

# Mean Plotting: Smoothed Plot - Shaded area shows 95% confidence
ggplot(nov23_mean_TGC_data, aes(x = as.numeric(Time), y = NovThreeGramCountMean, color = Site)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +  # Smoothed curve with confidence bands
  labs(title = "Three Gram Count November 2023", x = "Time (HHMM)", y = "TGC") +
  theme_minimal()

# Median Average Subset
nov23_median_TGC_data <- nov23_fullday_data %>%
  mutate(Date = as.numeric(Date)) %>%
  group_by(Site, Time) %>%
  summarise(NovThreeGramCountMedian = median(ThreeGramCount, na.rm = TRUE), .groups = "drop")

# Median Plotting: Line Plot
ggplot(nov23_median_TGC_data, aes(x = as.numeric(Time), y = NovThreeGramCountMedian, color = Site)) +
  geom_line() +
  labs(title = "Three Gram Count November 2023", x = "Time (HHMM)", y = "Acoustic Index") +
  theme_minimal()

# Median Plotting: Smoothed Plot - Shaded area shows 95% confidence
ggplot(nov23_median_TGC_data, aes(x = as.numeric(Time), y = NovThreeGramCountMedian, color = Site)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +  # Smoothed curve with confidence bands
  labs(title = "Median Three Gram Count November 2023", x = "Time (HHMM)", y = "TGC") +
  theme_minimal()


### November NDSI  ----------------------------------------------------------------


# Single Day Subset
nov23_single_NDSI <- as.data.frame(nov23_fullday_data) %>% 
  select(Site, Device, 
         Date, Time, 
         Strahler, Month, 
         Ndsi)  %>% 
  filter(Date == 20231125) 

glimpse(nov23_single_NDSI)

nov_23_single_NDSI_plots <- nov23_single_NDSI %>%
  split(.$Site) %>%
  lapply(function(data) {
    ggplot(data, aes(x = as.numeric(Time), y = Ndsi, color = Device)) +
      geom_line() +
      labs(title = unique(data$Site), x = "Time (HHMM)", y = "Three Gram Count") +
      theme_minimal()
  })

grid.arrange(grobs = nov_23_single_NDSI_plots, ncol = 2)

# Mean Average Subset
nov23_mean_NDSI_data <- nov23_fullday_data %>%
  mutate(Date = as.numeric(Date)) %>%
  group_by(Site, Time) %>%
  summarise(NovNdsiMean = mean(Ndsi, na.rm = TRUE), .groups = "drop")

# Mean Plotting
ggplot(nov23_mean_NDSI_data, aes(x = as.numeric(Time), y = NovNdsiMean, color = Site)) +
  geom_line() +
  labs(title = "Normalized Difference Soundscape Index November 2023", x = "Time (HHMM)", y = "Acoustic Index") +
  theme_minimal()

# Mean Plotting: Smoothed Plot - Shaded area shows 95% confidence
ggplot(nov23_mean_NDSI_data, aes(x = as.numeric(Time), y = NovNdsiMean, color = Site)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +  # Smoothed curve with confidence bands
  labs(title = "Normalized Difference Soundscape Index November 2023", x = "Time (HHMM)", y = "NDSI") +
  theme_minimal()

# Median Average Subset
nov23_median_NDSI_data <- nov23_fullday_data %>%
  mutate(Date = as.numeric(Date)) %>%
  group_by(Site, Time) %>%
  summarise(NovNdsiMedian = median(Ndsi, na.rm = TRUE), .groups = "drop")

# Median Plotting: Line Plot
ggplot(nov23_median_NDSI_data, aes(x = as.numeric(Time), y = NovNdsiMedian, color = Site)) +
  geom_line() +
  labs(title = "Normalized Difference Soundscape Index November 2023", x = "Time (HHMM)", y = "Acoustic Index") +
  theme_minimal()

# Median Plotting: Smoothed Plot - Shaded area shows 95% confidence
ggplot(nov23_median_NDSI_data, aes(x = as.numeric(Time), y = NovNdsiMedian, color = Site)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +  # Smoothed curve with confidence bands
  labs(title = "Normalized Difference Soundscape Index November 2023", x = "Time (HHMM)", y = "NDSI") +
  theme_minimal()


### January ACI  -------------------------------------------------------------------------------------------

# Mean Average Subset
jan24_mean_ACI_data <- jan24_fullday_data %>%
  mutate(Date = as.numeric(Date)) %>%
  group_by(Site, Time) %>%
  summarise(JanAcousticComplexMean = mean(AcousticComplexity, na.rm = TRUE), .groups = "drop")

# Mean Plotting
ggplot(jan24_mean_ACI_data, aes(x = as.numeric(Time), y = JanAcousticComplexMean, color = Site)) +
  geom_line() +
  labs(title = "Average Acoustic Complexity January 2024", x = "Time (HHMM)", y = "Acoustic Index") +
  theme_minimal()

# Mean Plotting: Smoothed Plot - Shaded area shows 95% confidence
ggplot(jan24_mean_ACI_data, aes(x = as.numeric(Time), y = JanAcousticComplexMean, color = Site)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +  # Smoothed curve with confidence bands
  labs(title = "Mean Mid-Frequency Cover January 2024", x = "Time (HHMM)", y = "MFC") +
  theme_minimal()

# Median Average Subset
jan24_median_ACI_data <- jan24_fullday_data %>%
  mutate(Date = as.numeric(Date)) %>%
  group_by(Site, Time) %>%
  summarise(JanAcousticComplexMedian = median(AcousticComplexity, na.rm = TRUE), .groups = "drop")

# Median Plotting: Line Plot
ggplot(jan24_median_ACI_data, aes(x = as.numeric(Time), y = JanAcousticComplexMedian, color = Site)) +
  geom_line() +
  labs(title = "Median Acoustic Complexity January 2024", x = "Time (HHMM)", y = "Acoustic Index") +
  theme_minimal()

# Median Plotting: Smoothed Plot - Shaded area shows 95% confidence
ggplot(jan24_median_ACI_data, aes(x = as.numeric(Time), y = JanAcousticComplexMedian, color = Site)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +  # Smoothed curve with confidence bands
  labs(title = "Mean Mid-Frequency Cover January 2024", x = "Time (HHMM)", y = "MFC") +
  theme_minimal()



### January MFC  -------------------------------------------------------------------------------------------

# Mean Average Subset
jan24_mean_MFC_data <- jan24_fullday_data %>%
  mutate(Date = as.numeric(Date)) %>%
  group_by(Site, Time) %>%
  summarise(JanMidFreqCoverMean = mean(MidFreqCover, na.rm = TRUE), .groups = "drop")

# Mean Plotting
ggplot(jan24_mean_MFC_data, aes(x = as.numeric(Time), y = MidFreqCoverMean, color = Site)) +
  geom_line() +
  labs(title = "Mean Mid-Frequency Cover January 2024", x = "Time (HHMM)", y = "Acoustic Index") +
  theme_minimal()

# Mean Plotting: Smoothed Plot - Shaded area shows 95% confidence
ggplot(jan24_mean_MFC_data, aes(x = as.numeric(Time), y = MidFreqCoverMean, color = Site)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +  # Smoothed curve with confidence bands
  labs(title = "Mean Mid-Frequency Cover January 2024", x = "Time (HHMM)", y = "MFC") +
  theme_minimal()

# Median Average Subset
jan24_median_MFC_data <- jan24_fullday_data %>%
  mutate(Date = as.numeric(Date)) %>%
  group_by(Site, Time) %>%
  summarise(JanMidFreqCoverMedian = median(MidFreqCover, na.rm = TRUE), .groups = "drop")

# Median Plotting: Line Plot
ggplot(jan24_median_MFC_data, aes(x = as.numeric(Time), y = JanMidFreqCoverMedian, color = Site)) +
  geom_line() +
  labs(title = "Median Mid-Frequency Cover January 2024", x = "Time (HHMM)", y = "Acoustic Index") +
  theme_minimal()

# Median Plotting: Smoothed Plot - Shaded area shows 95% confidence
ggplot(jan24_median_MFC_data, aes(x = as.numeric(Time), y = JanMidFreqCoverMedian, color = Site)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +  # Smoothed curve with confidence bands
  labs(title = "Mean Mid-Frequency Cover January 2024", x = "Time (HHMM)", y = "MFC") +
  theme_minimal()



### January LFC  -------------------------------------------------------------------------------------------

# Mean Average Subset
jan24_mean_LFC_data <- jan24_fullday_data %>%
  mutate(Date = as.numeric(Date)) %>%
  group_by(Site, Time) %>%
  summarise(JanThreeGramCountMean = mean(ThreeGramCount, na.rm = TRUE), .groups = "drop")

# Mean Plotting
ggplot(jan24_mean_LFC_data, aes(x = as.numeric(Time), y = JanThreeGramCountMean, color = Site)) +
  geom_line() +
  labs(title = "Mean Low-Frequency Cover January 2024", x = "Time (HHMM)", y = "Acoustic Index") +
  theme_minimal()

# Mean Plotting: Smoothed Plot - Shaded area shows 95% confidence
ggplot(jan24_mean_LFC_data, aes(x = as.numeric(Time), y = JanThreeGramCountMean, color = Site)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +  # Smoothed curve with confidence bands
  labs(title = "Mean Low-Frequency Cover January 2024", x = "Time (HHMM)", y = "LFC") +
  theme_minimal()

# Median Average Subset
jan24_median_LFC_data <- jan24_fullday_data %>%
  mutate(Date = as.numeric(Date)) %>%
  group_by(Site, Time) %>%
  summarise(JanThreeGramCountMedian = median(ThreeGramCount, na.rm = TRUE), .groups = "drop")

# Median Plotting: Line Plot
ggplot(jan24_median_LFC_data, aes(x = as.numeric(Time), y = JanThreeGramCountMedian, color = Site)) +
  geom_line() +
  labs(title = "Median Low-Frequency Cover January 2024", x = "Time (HHMM)", y = "Acoustic Index") +
  theme_minimal()

# Median Plotting: Smoothed Plot - Shaded area shows 95% confidence
ggplot(jan24_median_LFC_data, aes(x = as.numeric(Time), y = JanThreeGramCountMedian, color = Site)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +  # Smoothed curve with confidence bands
  labs(title = "Median Low-Frequency Cover January 2024", x = "Time (HHMM)", y = "LFC") +
  theme_minimal()



### January TGC  -------------------------------------------------------------------------------------------

# Mean Average Subset
jan24_mean_TGC_data <- jan24_fullday_data %>%
  mutate(Date = as.numeric(Date)) %>%
  group_by(Site, Time) %>%
  summarise(JanThreeGramCountMean = mean(ThreeGramCount, na.rm = TRUE), .groups = "drop")

# Mean Plotting
ggplot(jan24_mean_TGC_data, aes(x = as.numeric(Time), y = JanThreeGramCountMean, color = Site)) +
  geom_line() +
  labs(title = "Mean Three Gram Count January 2024", x = "Time (HHMM)", y = "Acoustic Index") +
  theme_minimal()

# Mean Plotting: Smoothed Plot - Shaded area shows 95% confidence
ggplot(jan24_mean_TGC_data, aes(x = as.numeric(Time), y = JanThreeGramCountMean, color = Site)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +  # Smoothed curve with confidence bands
  labs(title = "Mean Three Gram Count January 2024", x = "Time (HHMM)", y = "TGC") +
  theme_minimal()

# Median Average Subset
jan24_median_TGC_data <- jan24_fullday_data %>%
  mutate(Date = as.numeric(Date)) %>%
  group_by(Site, Time) %>%
  summarise(JanThreeGramCountMedian = median(ThreeGramCount, na.rm = TRUE), .groups = "drop")

# Median Plotting: Line Plot
ggplot(jan24_median_TGC_data, aes(x = as.numeric(Time), y = JanThreeGramCountMedian, color = Site)) +
  geom_line() +
  labs(title = "Median Three Gram Count January 2024", x = "Time (HHMM)", y = "Acoustic Index") +
  theme_minimal()

# Median Plotting: Smoothed Plot - Shaded area shows 95% confidence
ggplot(jan24_median_TGC_data, aes(x = as.numeric(Time), y = JanThreeGramCountMedian, color = Site)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +  # Smoothed curve with confidence bands
  labs(title = "Median Three Gram CountJanuary 2024", x = "Time (HHMM)", y = "TGC") +
  theme_minimal()



### January NDSI ---------------------------------------------------------------

# Mean Average Subset
jan24_mean_NDSI_data <- jan24_fullday_data %>%
  mutate(Date = as.numeric(Date)) %>%
  group_by(Site, Time) %>%
  summarise(NovNdsiMean = mean(Ndsi, na.rm = TRUE), .groups = "drop")

# Mean Plotting
ggplot(jan24_mean_NDSI_data, aes(x = as.numeric(Time), y = NovNdsiMean, color = Site)) +
  geom_line() +
  labs(title = "Normalized Difference Soundscape Index November 2023", x = "Time (HHMM)", y = "Acoustic Index") +
  theme_minimal()

# Mean Plotting: Smoothed Plot - Shaded area shows 95% confidence
ggplot(jan24_mean_NDSI_data, aes(x = as.numeric(Time), y = NovNdsiMean, color = Site)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +  # Smoothed curve with confidence bands
  labs(title = "Normalized Difference Soundscape Index November 2023", x = "Time (HHMM)", y = "NDSI") +
  theme_minimal()

# Median Average Subset
jan24_median_NDSI_data <- jan24_fullday_data %>%
  mutate(Date = as.numeric(Date)) %>%
  group_by(Site, Time) %>%
  summarise(NovNdsiMedian = median(Ndsi, na.rm = TRUE), .groups = "drop")

# Median Plotting: Line Plot
ggplot(jan24_median_NDSI_data, aes(x = as.numeric(Time), y = NovNdsiMedian, color = Site)) +
  geom_line() +
  labs(title = "Normalized Difference Soundscape Index November 2023", x = "Time (HHMM)", y = "Acoustic Index") +
  theme_minimal()

# Median Plotting: Smoothed Plot - Shaded area shows 95% confidence
ggplot(jan24_median_NDSI_data, aes(x = as.numeric(Time), y = NovNdsiMedian, color = Site)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +  # Smoothed curve with confidence bands
  labs(title = "Normalized Difference Soundscape Index November 2023", x = "Time (HHMM)", y = "NDSI") +
  theme_minimal()

### Joint Plot Setup ----------------------------------------------------------------

# Selection of Acoustic Indices and Filtering
individual_acoustic_indices <- c("ACI", "NDSI", "MFC", "LFC", "TGC")

### Joint Plots Nov 23 ----------------------------------------------------------------

# Selection of Acoustic Indices and Filtering
individual_acoustic_indices <- c("ACI", "NDSI", "MFC", "LFC", "TGC")

# Process data: Compute mean, SD, and IQR for each site and time
nov_summary_data <- nov23_fullday_data %>%
  group_by(Site, Device, Time) %>%
  summarise(
    ACI_mean = mean(AcousticComplexity, na.rm = TRUE),
    ACI_sd = sd(AcousticComplexity, na.rm = TRUE),
    ACI_q25 = quantile(AcousticComplexity, 0.25, na.rm = TRUE),
    ACI_q75 = quantile(AcousticComplexity, 0.75, na.rm = TRUE),
    
    NDSI_mean = mean(Ndsi, na.rm = TRUE),
    NDSI_sd = sd(Ndsi, na.rm = TRUE),
    NDSI_q25 = quantile(Ndsi, 0.25, na.rm = TRUE),
    NDSI_q75 = quantile(Ndsi, 0.75, na.rm = TRUE),
    
    MFC_mean = mean(MidFreqCover, na.rm = TRUE),
    MFC_sd = sd(MidFreqCover, na.rm = TRUE),
    MFC_q25 = quantile(MidFreqCover, 0.25, na.rm = TRUE),
    MFC_q75 = quantile(MidFreqCover, 0.75, na.rm = TRUE),
    
    LFC_mean = mean(LowFreqCover, na.rm = TRUE),
    LFC_sd = sd(LowFreqCover, na.rm = TRUE),
    LFC_q25 = quantile(LowFreqCover, 0.25, na.rm = TRUE),
    LFC_q75 = quantile(LowFreqCover, 0.75, na.rm = TRUE),
    
    TGC_mean = mean(ThreeGramCount, na.rm = TRUE),
    TGC_sd = sd(ThreeGramCount, na.rm = TRUE),
    TGC_q25 = quantile(ThreeGramCount, 0.25, na.rm = TRUE),
    TGC_q75 = quantile(ThreeGramCount, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# Function to generate a plot for each acoustic index
nov_plot_acoustic_index <- function(index_name) {
  ggplot(nov_summary_data, aes(x = as.numeric(Time), color = Device, group = interaction(Site, Device))) +
    geom_ribbon(aes(
      ymin = get(paste0(index_name, "_mean")) - get(paste0(index_name, "_sd")), 
      ymax = get(paste0(index_name, "_mean")) + get(paste0(index_name, "_sd")), 
      fill = Device), alpha = 0.2) +  # Shaded area for mean ± SD
    geom_line(aes(y = get(paste0(index_name, "_mean"))), linewidth = 1) +  # Mean line
    geom_line(aes(y = get(paste0(index_name, "_q25"))), linetype = "dashed") +  # IQR (25th percentile)
    geom_line(aes(y = get(paste0(index_name, "_q75"))), linetype = "dashed") +  # IQR (75th percentile)
    facet_wrap(~Site, ncol = length(unique(nov_summary_data$Site))) +  # One column per site
    labs(title = index_name, x = "Time (HHMM)", y = paste(index_name, "Value")) +
    theme_minimal()
}

# Generate plots for each acoustic index
nov_individual_plots <- lapply(individual_acoustic_indices, nov_plot_acoustic_index)

# Arrange plots in a grid (rows = indices, columns = sites)
grid.arrange(grobs = nov_individual_plots, ncol = 1)





### Joint Plots Jan 24 ----------------------------------------------------------------

# Selection of Acoustic Indices and Filtering
individual_acoustic_indices <- c("ACI", "NDSI", "MFC", "LFC", "TGC")

# Process data: Compute mean, SD, and IQR for each site and time
jan24_summary_data <- jan24_fullday_data %>%
  group_by(Site, Device, Time) %>%
  summarise(
    ACI_mean = mean(AcousticComplexity, na.rm = TRUE),
    ACI_sd = sd(AcousticComplexity, na.rm = TRUE),
    ACI_q25 = quantile(AcousticComplexity, 0.25, na.rm = TRUE),
    ACI_q75 = quantile(AcousticComplexity, 0.75, na.rm = TRUE),
    
    NDSI_mean = mean(Ndsi, na.rm = TRUE),
    NDSI_sd = sd(Ndsi, na.rm = TRUE),
    NDSI_q25 = quantile(Ndsi, 0.25, na.rm = TRUE),
    NDSI_q75 = quantile(Ndsi, 0.75, na.rm = TRUE),
    
    MFC_mean = mean(MidFreqCover, na.rm = TRUE),
    MFC_sd = sd(MidFreqCover, na.rm = TRUE),
    MFC_q25 = quantile(MidFreqCover, 0.25, na.rm = TRUE),
    MFC_q75 = quantile(MidFreqCover, 0.75, na.rm = TRUE),
    
    LFC_mean = mean(LowFreqCover, na.rm = TRUE),
    LFC_sd = sd(LowFreqCover, na.rm = TRUE),
    LFC_q25 = quantile(LowFreqCover, 0.25, na.rm = TRUE),
    LFC_q75 = quantile(LowFreqCover, 0.75, na.rm = TRUE),
    
    TGC_mean = mean(ThreeGramCount, na.rm = TRUE),
    TGC_sd = sd(ThreeGramCount, na.rm = TRUE),
    TGC_q25 = quantile(ThreeGramCount, 0.25, na.rm = TRUE),
    TGC_q75 = quantile(ThreeGramCount, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# Function to generate a plot for each acoustic index
jan_plot_acoustic_index <- function(index_name) {
  ggplot(jan24_summary_data, aes(x = as.numeric(Time), color = Device, group = interaction(Site, Device))) +
    geom_ribbon(aes(
      ymin = get(paste0(index_name, "_mean")) - get(paste0(index_name, "_sd")), 
      ymax = get(paste0(index_name, "_mean")) + get(paste0(index_name, "_sd")), 
      fill = Device), alpha = 0.2) +  # Shaded area for mean ± SD
    geom_line(aes(y = get(paste0(index_name, "_mean"))), linewidth = 1) +  # Mean line
    geom_line(aes(y = get(paste0(index_name, "_q25"))), linetype = "dashed") +  # IQR (25th percentile)
    geom_line(aes(y = get(paste0(index_name, "_q75"))), linetype = "dashed") +  # IQR (75th percentile)
    facet_wrap(~Site, ncol = length(unique(jan_summary_data$Site))) +  # One column per site
    labs(title = index_name, x = "Time (HHMM)", y = paste(index_name, "Value")) +
    theme_minimal()
}

# Generate plots for each acoustic index
jan_individual_plots <- lapply(individual_acoustic_indices, jan_plot_acoustic_index)

# Arrange plots in a grid (rows = indices, columns = sites)
grid.arrange(grobs = jan_individual_plots, ncol = 1)


### Joint Plots Jan 24 MK2 ----------------------------------------------------------------

# Selection of Acoustic Indices and Filtering
individual_acoustic_indices <- c("ACI", "NDSI", "MFC", "LFC", "TGC")    

# Process data: Compute mean, SD, and IQR for each site and time
jan24_summary_data <- jan24_fullday_data %>%
  group_by(Site, Time) %>%
  summarise(
    ACI_mean = mean(AcousticComplexity, na.rm = TRUE),
    ACI_sd = sd(AcousticComplexity, na.rm = TRUE),
    
    NDSI_mean = mean(Ndsi, na.rm = TRUE),
    NDSI_sd = sd(Ndsi, na.rm = TRUE),
    
    MFC_mean = mean(MidFreqCover, na.rm = TRUE),
    MFC_sd = sd(MidFreqCover, na.rm = TRUE),
    
    LFC_mean = mean(LowFreqCover, na.rm = TRUE),
    LFC_sd = sd(LowFreqCover, na.rm = TRUE),
    
    TGC_mean = mean(ThreeGramCount, na.rm = TRUE),
    TGC_sd = sd(ThreeGramCount, na.rm = TRUE),
    .groups = "drop"
  )

# Function to generate a plot for each acoustic index
jan24_plot_acoustic_index <- function(index_name) {    jan24_summary_data$Site <- factor(
  jan24_summary_data$Site, 
  levels = c("TaChey", "Arai", "Stung Oda", "KnaongBatSa", "TaSay", "DamFive", "TangRang", "PursatTown")) 
  ggplot(jan24_summary_data, aes(x = as.numeric(Time), group = Site)) + 
    geom_ribbon(aes(
      ymin = get(paste0(index_name, "_mean")) - get(paste0(index_name, "_sd")), 
      ymax = get(paste0(index_name, "_mean")) + get(paste0(index_name, "_sd"))
    ), fill = "grey", alpha = 0.2, colour = NA) +  # Shaded area for mean ± SD in grey
    geom_line(aes(y = get(paste0(index_name, "_mean"))), linewidth = 0.4) +  # Mean line
    facet_wrap(~Site, ncol = length(unique(jan24_summary_data$Site))) +  # One column per site
    labs(title = index_name, x = "Time (HHMM)", y = paste(index_name)) + 
    theme_minimal() + 
    theme(legend.position = "none") +
    scale_x_continuous(
      labels = function(x) {
        # Remove the last two digits (assuming Time is in HHMMSS format)
        formatted_time <- sprintf("%04d", x / 100)
        return(formatted_time)
      }
    )
}

# Generate plots for each acoustic index
jan24_individual_plots <- lapply(individual_acoustic_indices, jan24_plot_acoustic_index)

# Arrange plots in a grid (rows = indices, columns = sites)
Janindicesplots <-grid.arrange(grobs = jan24_individual_plots, ncol = 1)

ggsave("individuals.png", plot = Janindicesplots, 
       path = NULL, width = 16, height = 8, dpi = 950, bg='transparent')

## MK3 

index_colors <- c(
  "ACI" = "#19bb43",    
  "NDSI" = "#025777",   
  "MFC" = "#dacf57",   
  "LFC" = "#6e0504",    
  "TGC" = "#f65a90"     
)

# Function to generate a plot for each acoustic index
jan_plot_acoustic_index <- function(index_name) {
  # Reorder Site factor levels to control the order of columns
  jan24_summary_data$Site <- factor(
    jan24_summary_data$Site, levels = 
      c("Arai", "Oda", "Trang")) # Customize with desired order
  
  ggplot(jan24_summary_data, aes(x = as.numeric(Time), color = Site, group = Site)) +
    geom_ribbon(aes(
      ymin = get(paste0(index_name, "_mean")) - get(paste0(index_name, "_sd")), 
      ymax = get(paste0(index_name, "_mean")) + get(paste0(index_name, "_sd")), 
      fill = index_name), alpha = 0.2, color = NA) +  # Shaded area with no border, fill by index
    geom_line(aes(y = get(paste0(index_name, "_mean"))), linewidth = 1) +  # Mean line
    facet_wrap(~Site, ncol = length(unique(jan24_summary_data$Site))) +  # One column per site
    labs(title = index_name, x = "Time (HHMM)", y = paste(index_name, "Value")) +
    scale_fill_manual(values = index_colors) +  # Color the ribbon by acoustic index
    scale_color_manual(values = index_colors) +  # Color the lines by acoustic index
    theme_minimal() +
    theme(legend.position = "none")  # Hide legend if not needed
}

# Generate plots for each acoustic index
jan_individual_plots <- lapply(individual_acoustic_indices, jan_plot_acoustic_index)

# Arrange plots in a grid (rows = indices, columns = sites)
grid.arrange(grobs = jan_individual_plots, ncol = 1)

