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


datasets <- list(
  single_full      = single_ds,
  single_norain    = single_rl_ds,
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
        TimeHHMMSS >= "050000" & TimeHHMMSS <= "090000" ~ "Dawn",
        TimeHHMMSS >= "103000" & TimeHHMMSS <= "143000" ~ "Midday",
        TimeHHMMSS >= "153000" & TimeHHMMSS <= "193000" ~ "Dusk",
        TimeHHMMSS >= "220000" | TimeHHMMSS <= "020000" ~ "Midnight",
        TRUE ~ "Other"
      ),
      
      TimeRangeFactor = factor(
        TimeRangeFactor,
        levels = c("Dawn","Midday","Dusk","Midnight","Other")
      )
    )
}

datasets <- purrr::map(datasets, prep_dataset)


# Function to run model (PC1)
run_PC1_model4 <- function(dat) {
  
  model <- lmer(
  PC1 ~ QBR * TimeRangeFactor + Strahler +
      (1 | Site) + (1 | Season),
    data = dat,
    REML = TRUE
  )
  
  return(model)
}

# # Function to run model (PC2)
run_PC2_model4 <- function(dat) {
  
  model <- lmer(
    PC2 ~ QBR * TimeRangeFactor + Strahler +
      (1 | Site) + (1 | Season),
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

# PC1 Analyses: ---------------------------------------------------
results_all <- purrr::imap_dfr(
  datasets,
  ~ extract_slopes(run_PC1_model4(.x), .y)
)

head(results_all)

# Build Table (PC1)
consistency_table <- results_all |>
  dplyr::group_by(term) |>
  dplyr::summarise(
    datasets_tested = n(),
    positive = sum(direction == "+"),
    negative = sum(direction == "-"),
    null = sum(direction == "0"),
    consistent =
      (positive == datasets_tested) |
      (negative == datasets_tested)
  )


consistency_table 



## PC1 Heatmap -----------------------------------------------------------------------

# Clean order and order terms
plot_df <- results_all %>%
  filter(term != "(Intercept)") %>%
  mutate(
    # nicer labels
    term = str_replace(term, "TimeRangeFactor", ""),
    term = str_replace(term, "QBR:", "QBR × "),
    
    # CI label
    CI_label = sprintf("%.3f\n[%.3f, %.3f]", estimate, CI_low, CI_high),
    
    # colour category
    effect_dir = case_when(
      direction == "+" ~ "Positive",
      direction == "-" ~ "Negative",
      TRUE ~ "Null"
    )
  )

# Order rows 
term_order <- c(
  "Midday",
  "Dusk",
  "Midnight",
  "Other",
  "Strahler",
  "QBR",
  "QBR × Midday",
  "QBR × Dusk",
  "QBR × Midnight",
  "QBR × Other"
)

plot_df <- plot_df %>%
  mutate(term = factor(term, levels = term_order))

# Create effect strength metric
plot_df <- plot_df %>%
  mutate(
    # how far the CI is from zero (strength)
    strength = case_when(
      direction == "+" ~ CI_low,      # lower bound > 0
      direction == "-" ~ abs(CI_high),# upper bound < 0
      TRUE ~ 0
    )
  )

# Order the datasets
plot_df$dataset <- factor(
  plot_df$dataset,
  levels = c("single_full", "single_norain",
             "global_full", "global_norain")
)

# Plot v1
ggplot(plot_df, aes(x = dataset, y = term, fill = effect_dir)) +
  geom_tile(color = "black") +
  geom_text(aes(label = CI_label), size = 3) +
  scale_fill_manual(
    values = c(
      "Positive" = "#4CAF50",
      "Negative" = "#e94f37",
      "Null"     = "grey80"
    )
  ) +
  labs(
    x = "Dataset",
    y = "Model Term",
    fill = "Effect Direction",
    title = "Consistency of Mixed Model Effects Across PCA Datasets"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Plot  v2
ggplot(plot_df, aes(x = dataset, y = term, fill = ifelse(direction == "-",
                                                  -strength,
                                                  strength))) +
  geom_tile(color = "black") +
  geom_text(aes(label = CI_label), size = 3) +
   scale_fill_gradient2(
    low = "#e94f37",     # red
    mid = "grey90",      # null
    high = "#4CAF50",    # green
    midpoint = 0,
    limits = c(
      -max(plot_df$strength),
      max(plot_df$strength)
    ),
    name = "Effect strength\n(CI distance from 0)"
  ) +
  labs(
    x = "Dataset",
    y = "Model Term",
    fill = "Effect Direction",
    title = "Consistency of Mixed Model Effects Across PCA Datasets"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


## PC1 Forest Plot ------------------------
forest_df <- results_all %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = gsub("TimeRangeFactor", "", term),
    term = gsub("QBR:", "QBR × ", term)
  )

# Ordering
term_order <- c(
  "Midday",
  "Dusk",
  "Midnight",
  "Other",
  "Strahler",
  "QBR",
  "QBR × Midday",
  "QBR × Dusk",
  "QBR × Midnight",
  "QBR × Other"
)

forest_df$term <- factor(forest_df$term, levels = term_order)

forest_df$dataset <- factor(
  forest_df$dataset,
  levels = c("single_full", "single_norain",
             "global_full", "global_norain")
)

# Plot 
dodge <- position_dodge(width = 0.6)

ggplot(forest_df,
       aes(y = term,
           x = estimate,
           xmin = CI_low,
           xmax = CI_high,
           colour = dataset)) +
  
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  geom_errorbarh(
    height = 0.15,
    size = 0.6,
    position = dodge
  ) +
  
  geom_point(
    size = 2.2,
    position = dodge
  ) +
  
  labs(
    x = "Effect size (PC1)",
    y = "Model term",
    colour = "Dataset",
    title = "Mixed Model Effect Sizes Across PCA Datasets"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank()
  )

# PC2 Analyses: ---------------------------------------------------
results_PC2_all <- purrr::imap_dfr(
  datasets,
  ~ extract_slopes(run_PC2_model4(.x), .y)
)

# Build Table (PC2)
consistency_table_pc2 <- results_PC2_all |>
  dplyr::group_by(term) |>
  dplyr::summarise(
    datasets_tested = n(),
    positive = sum(direction == "+"),
    negative = sum(direction == "-"),
    null = sum(direction == "0"),
    consistent =
      (positive == datasets_tested) |
      (negative == datasets_tested)
  )


consistency_table_pc2 

## PC2 Heatmap -----------------------------------------------------------------------

# Clean order and order terms
plot_df <- results_all %>%
  filter(term != "(Intercept)") %>%
  mutate(
    # nicer labels
    term = str_replace(term, "TimeRangeFactor", ""),
    term = str_replace(term, "QBR:", "QBR × "),
    
    # CI label
    CI_label = sprintf("%.3f\n[%.3f, %.3f]", estimate, CI_low, CI_high),
    
    # colour category
    effect_dir = case_when(
      direction == "+" ~ "Positive",
      direction == "-" ~ "Negative",
      TRUE ~ "Null"
    )
  )

# Order rows 
term_order <- c(
  "Midday",
  "Dusk",
  "Midnight",
  "Other",
  "Strahler",
  "QBR",
  "QBR × Midday",
  "QBR × Dusk",
  "QBR × Midnight",
  "QBR × Other"
)

plot_df <- plot_df %>%
  mutate(term = factor(term, levels = term_order))

# Create effect strength metric
plot_df <- plot_df %>%
  mutate(
    # how far the CI is from zero (strength)
    strength = case_when(
      direction == "+" ~ CI_low,      # lower bound > 0
      direction == "-" ~ abs(CI_high),# upper bound < 0
      TRUE ~ 0
    )
  )

# Order the datasets
plot_df$dataset <- factor(
  plot_df$dataset,
  levels = c("single_full", "single_norain",
             "global_full", "global_norain")
)

# Plot v1
ggplot(plot_df, aes(x = dataset, y = term, fill = effect_dir)) +
  geom_tile(color = "black") +
  geom_text(aes(label = CI_label), size = 3) +
  scale_fill_manual(
    values = c(
      "Positive" = "#4CAF50",
      "Negative" = "#e94f37",
      "Null"     = "grey80"
    )
  ) +
  labs(
    x = "Dataset",
    y = "Model Term",
    fill = "Effect Direction",
    title = "Consistency of Mixed Model Effects Across PCA Datasets"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Plot  v2
ggplot(plot_df, aes(x = dataset, y = term, fill = ifelse(direction == "-",
                                                         -strength,
                                                         strength))) +
  geom_tile(color = "black") +
  geom_text(aes(label = CI_label), size = 3) +
  scale_fill_gradient2(
    low = "#e94f37",     # red
    mid = "grey90",      # null
    high = "#4CAF50",    # green
    midpoint = 0,
    limits = c(
      -max(plot_df$strength),
      max(plot_df$strength)
    ),
    name = "Effect strength\n(CI distance from 0)"
  ) +
  labs(
    x = "Dataset",
    y = "Model Term",
    fill = "Effect Direction",
    title = "Consistency of Mixed Model Effects Across PCA Datasets"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


## PC2 Forest Plot ------------------------
forest_df <- results_all %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = gsub("TimeRangeFactor", "", term),
    term = gsub("QBR:", "QBR × ", term)
  )

# Ordering
term_order <- c(
  "Midday",
  "Dusk",
  "Midnight",
  "Other",
  "Strahler",
  "QBR",
  "QBR × Midday",
  "QBR × Dusk",
  "QBR × Midnight",
  "QBR × Other"
)

forest_df$term <- factor(forest_df$term, levels = term_order)

forest_df$dataset <- factor(
  forest_df$dataset,
  levels = c("single_full", "single_norain",
             "global_full", "global_norain")
)

# Plot 
dodge <- position_dodge(width = 0.6)

ggplot(forest_df,
       aes(y = term,
           x = estimate,
           xmin = CI_low,
           xmax = CI_high,
           colour = dataset)) +
  
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  geom_errorbarh(
    height = 0.15,
    size = 0.6,
    position = dodge
  ) +
  
  geom_point(
    size = 2.2,
    position = dodge
  ) +
  
  labs(
    x = "Effect size (PC2)",
    y = "Model term",
    colour = "Dataset",
    title = "Mixed Model Effect Sizes Across PCA Datasets"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank()
  )
