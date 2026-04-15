# Effect Size Meta Analysis

library(broom)
library(metafor)
library(tidyverse) # for data manipulation and plotting
library(lme4) # for mixed models
library(vegan)
library(corrplot)
library(reshape2)
library(ggplot2)

# Data Read-In
singledevice_ds <- read.csv("clean_data/datasets/indices_datasets/global_singledevice_data.csv")
global_ds <- read.csv("clean_data/datasets/indices_datasets/global2325_data.csv")

rl_singledevice_ds <- read.csv("clean_data/datasets/indices_datasets/globalRL_singledevice_data.csv")
rl_global_ds <- read.csv("clean_data/datasets/indices_datasets/global2325RL_data.csv")

#### Function to run subset PCAs - allowing correct timing --------------------

run_pca_subset <- function(data_subset, start_time = NULL, end_time = NULL) {
  # Fix the Time format (e.g., 3 -> "00:00:03", 500 -> "00:05:00")
  data_subset <- data_subset %>%
    mutate(Time_fixed = sprintf("%06d", as.integer(Time)),
           Time_fixed = stringr::str_replace_all(Time_fixed, "(.{2})(.{2})(.{2})", "\\1:\\2:\\3"))
  
  # Convert to proper time format
  data_subset <- data_subset %>% mutate(Time_parsed = lubridate::hms(Time_fixed))
  
  # Handle the midnight wrap-around case
  if (!is.null(start_time) & !is.null(end_time)) {
    start_time_parsed <- lubridate::hms(start_time)
    end_time_parsed <- lubridate::hms(end_time)
    
    if (end_time_parsed < start_time_parsed) {
      # If end time is earlier, split the filter into two parts
      data_subset <- data_subset %>%
        filter((Time_parsed >= start_time_parsed) | (Time_parsed <= end_time_parsed))
    } else {
      data_subset <- data_subset %>%
        filter(Time_parsed >= start_time_parsed & Time_parsed <= end_time_parsed)
    }
  }
  
  # Select only the acoustic indices
  acoustic_indices <- c("AcousticComplexity", "TemporalEntropy", "Ndsi", "EventsPerSecond",
                        "LowFreqCover", "MidFreqCover", "HighFreqCover", "ClusterCount", "ThreeGramCount")
  
  # Ensure required indices exist
  data_subset <- data_subset %>% select(any_of(acoustic_indices)) %>% drop_na()
  
  # Remove constant columns if any (to avoid PCA errors)
  constant_cols <- sapply(data_subset, function(x) length(unique(x)) <= 1)
  if (any(constant_cols)) {
    warning("Dropped constant columns: ", paste(names(data_subset)[constant_cols], collapse = ", "))
    data_subset <- data_subset[, !constant_cols]
  }
  
  # Ensure at least two variable columns for PCA
  if (ncol(data_subset) < 2) {
    stop("Not enough variable columns to perform PCA.")
  }
  
  # Standardize the data and run PCA
  data_scaled <- scale(data_subset)
  pca_result <- prcomp(data_scaled, center = TRUE, scale. = TRUE)
  
  return(pca_result)
}


# usage: data_pca <- run_pca_subset(dataset, "HH:MM:SS", "HH:MM:SS")


#### Function to bind metadata to PCA subsets --------

bind_metadata <- function(pca_result, original_data, start_time = NULL, end_time = NULL) {
  # Extract PCA scores as a dataframe
  pca_scores <- as.data.frame(pca_result$x)
  
  # Fix the Time format in the original data
  original_data <- original_data %>%
    mutate(Time_fixed = sprintf("%06d", as.integer(Time)),
           Time_fixed = stringr::str_replace_all(Time_fixed, "(.{2})(.{2})(.{2})", "\\1:\\2:\\3"))
  
  # Convert to proper time format
  original_data <- original_data %>%
    mutate(Time_parsed = lubridate::hms(Time_fixed))
  
  # Filter original data based on the time range (same logic as in run_pca_subset)
  if (!is.null(start_time) & !is.null(end_time)) {
    start_time_parsed <- lubridate::hms(start_time)
    end_time_parsed <- lubridate::hms(end_time)
    
    if (end_time_parsed < start_time_parsed) {
      # If end time is earlier, split the filter into two parts
      original_data <- original_data %>%
        filter((Time_parsed >= start_time_parsed) | (Time_parsed <= end_time_parsed))
    } else {
      original_data <- original_data %>%
        filter(Time_parsed >= start_time_parsed & Time_parsed <= end_time_parsed)
    }
  }
  
  # Bind metadata to PCA scores
  pca_scores_with_metadata <- pca_scores %>%
    mutate(Device = original_data$Device, 
           Site = original_data$Site, 
           Date = original_data$Date, 
           Time = original_data$Time,
           Strahler = original_data$Strahler, 
           Strahler_Class = original_data$Strahler_Class,
           QBR = original_data$QBR_Score,
           QBR_Class = original_data$QBR_Class,
           FileName = original_data$FileName, 
           Branch = original_data$Branch,
           Deployment_Season = original_data$Deployment_Season
    )
  
  return(pca_scores_with_metadata)
}

#### Run PCAs ---------------------------------

single_pca <- run_pca_subset(singledevice_ds, "00:00:00", "23:59:00")
single_scores <- bind_metadata(single_pca, singledevice_ds, "00:00:00", "23:59:00")
summary(single_pca)
single_pca$rotation

global_pca <- run_pca_subset(global_ds, "00:00:00", "23:59:00")
global_scores <- bind_metadata(global_pca, global_ds, "00:00:00", "23:59:00")
summary(global_pca)
global_pca$rotation

rl_single_pca <- run_pca_subset(rl_singledevice_ds,"00:00:00", "23:59:00")
rl_single_scores <- bind_metadata(rl_single_pca, rl_singledevice_ds,"00:00:00", "23:59:00")
summary(rl_single_pca)
rl_single_pca$rotation

rl_global_pca <- run_pca_subset(rl_global_ds, "00:00:00", "23:59:00")
rl_global_scores <- bind_metadata(rl_global_pca, rl_global_ds, "00:00:00", "23:59:00")
summary(rl_global_pca)
rl_global_pca$rotation

pca_models <- list(
  raw_single      = single_pca,
  raw_global      = global_pca,
  rainless_single = rl_single_pca,
  rainless_global = rl_global_pca
)
# Compare PCA Loading Function----------------------------

compare_loadings <- function(pca1, pca2, name1, name2) {
  # extract 3 PCs
  L1 <- pca1$rotation[, 1:3]
  L2 <- pca2$rotation[, 1:3]
  
  cat("\n==== Comparing:", name1, "vs", name2, "====\n")
  print(protest(L1, L2, permutations = 999))
  corrplot(cor(L1, L2), method = "circle", title = paste(name1, "vs", name2))
}

# Compare loadings across rainless and rain datasets -----------------------------

# 1) Rainless Single vs Raw Single
compare_loadings(rl_single_pca, single_pca, "Rainless Single", "Raw Single")

# 2) Rainless Global vs Raw Global
compare_loadings(rl_global_pca, global_pca, "Rainless Global", "Raw Global")

# 
EVs <- data.frame(
  PC = paste0("PC", 1:9),
  rainless_single = rl_single_pca$sdev^2 / sum(rl_single_pca$sdev^2),
  rainless_global = rl_global_pca$sdev^2 / sum(rl_global_pca$sdev^2),
  raw_single      = single_pca$sdev^2 / sum(single_pca$sdev^2),
  raw_global      = global_pca$sdev^2 / sum(global_pca$sdev^2)
)

EV_long <- melt(EVs, id.vars = "PC")

# Plot

p <- ggplot(EV_long, aes(x = PC, y = value, group = variable)) +
  geom_line(aes(linetype = variable), alpha = 0.5, color = "black", size = 1) +
  geom_point(aes(shape = variable), color = "black", size = 3) +
  labs(
    title = "Proportion of Variance Explained Across Datasets (PCA)",
    y = "Variance Explained",
    linetype = "Dataset",
    shape = "Dataset"
  ) +
  scale_linetype_manual(values = c("solid", "dashed", "dotdash", "dotted")) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  theme_minimal() +
  theme(
    # make all rectangles transparent
    plot.background      = element_rect(fill = "transparent", colour = NA),
    panel.background     = element_rect(fill = "transparent", colour = NA),
    legend.background    = element_rect(fill = "transparent", colour = NA),
    legend.key           = element_rect(fill = "transparent", colour = NA),
    panel.border         = element_rect(fill = "transparent", colour = NA),
    strip.background     = element_rect(fill = "transparent", colour = NA),
    
    # keep text visible (black here); change if you want different text color
    plot.title           = element_text(color = "black", face = "bold"),
    axis.title           = element_text(color = "black"),
    axis.text            = element_text(color = "black"),
    legend.title         = element_text(color = "black"),
    legend.text          = element_text(color = "black")
  )


# Compare PCA Loadings across the  datasets -------------------
loadA <- pcaA$rotation[, 1:nPC]
loadB <- pcaB$rotation[, 1:nPC]

# Store PCAs in a named list
pca_models <- list(
  raw_single      = single_pca,
  raw_global      = global_pca,
  rainless_single = rl_single_pca,
  rainless_global = rl_global_pca
)

### ---- FUNCTION: Compare PCA Loadings Between Two PCAs ----

compare_pca_loadings <- function(pcaA, pcaB, nameA, nameB, nPC = 3) {
  
  loadA <- as.matrix(pcaA$rotation[, 1:nPC]) %>% apply(2, as.numeric)
  loadB <- as.matrix(pcaB$rotation[, 1:nPC]) %>% apply(2, as.numeric)
  
  # Test similarity using PROTEST (rotation invariance)
  test <- protest(loadA, loadB, permutations = 999)
  
  # Visual correlation between loadings
  corrplot(cor(loadA, loadB),
           method = "circle",
           title = paste("Loading Correlation:", nameA, "vs", nameB))
  
  # Scatter diag for PC1
  plot(loadA[,1], loadB[,1],
       xlab = paste("PC1 Loadings:", nameA),
       ylab = paste("PC1 Loadings:", nameB),
       main = paste("PC1 Loading Match:", nameA, "vs", nameB))
  abline(0, 1, col = "red", lwd = 2)
  
  return(
    tibble(
      Comparison      = paste(nameA, "vs", nameB),
      Protest_pvalue  = test$signif,
      Protest_stat    = test$statistic
    )
  )
}

### ---- RUN COMPARISONS ACROSS DATASETS ----

comparison_results <- bind_rows(
  compare_pca_loadings(rl_single_pca, single_pca,  "Rainless Single", "Raw Single"),
  compare_pca_loadings(rl_global_pca, global_pca,  "Rainless Global", "Raw Global"),
  compare_pca_loadings(rl_single_pca, rl_global_pca, "Rainless Single", "Rainless Global"),
  compare_pca_loadings(single_pca, global_pca, "Raw Single", "Raw Global")
)

### ---- VIEW THE RESULTS ----
print(comparison_results)

# Compare Loadings:
compare_loadings <- function(pcaA, pcaB, nameA, nameB, nPC = 3) {
  
  L1 <- pcaA$rotation[, 1:nPC]
  L2 <- pcaB$rotation[, 1:nPC]
  
  test <- protest(L1, L2, permutations = 9999)
  
  tibble(
    Comparison = paste(nameA, "vs", nameB),
    Protest_statistic = test$statistic,
    P_value = test$signif
  )
}
  
  results <- bind_rows(
      compare_loadings(rl_single_pca, single_pca,  "Rainless Single", "Raw Single"),
      compare_loadings(rl_global_pca, global_pca,  "Rainless Global", "Raw Global"),
      compare_loadings(single_pca, global_pca,     "Raw Single", "Raw Global"),
      compare_loadings(rl_single_pca, rl_global_pca,"Rainless Single", "Rainless Global")
      )

print(results)

# Variance
var_explained <- function(pca, name) {
  tibble(
    Dataset = name,
    PC = paste0("PC", 1:length(pca$sdev)),
    Var = pca$sdev^2 / sum(pca$sdev^2)
  )
}


bind_rows(
  var_explained(single_pca, "Raw Primary"),
  var_explained(global_pca, "Raw Global"),
  var_explained(rl_single_pca, "Filtered Primary"),
  var_explained(rl_global_pca, "Rainless Global")
) %>%
  ggplot(aes(PC, Var, group = Dataset, linetype = Dataset)) +
  geom_line() +
  geom_point() +
  labs(x = "Compound Indices (PCs)", y = "Variance Explained") + 
  theme_minimal()

### Fixed - PCA Procrustes ---------------------------------
# ---- Load datasets ----
single_ds <- read.csv("clean_data/datasets/indices_datasets/global_singledevice_data.csv")
global_ds <- read.csv("clean_data/datasets/indices_datasets/global2325_data.csv")
single_rl_ds <- read.csv("clean_data/datasets/indices_datasets/globalRL_singledevice_data.csv")
global_rl_ds <- read.csv("clean_data/datasets/indices_datasets/global2325RL_data.csv")

datasets <- list(
  raw_single      = single_ds,
  rainless_single = single_rl_ds,
  raw_global      = global_ds,
  rainless_global = global_rl_ds
)

# ---- Acoustic variables to include ----
acoustic_vars <- c("AcousticComplexity", "TemporalEntropy", "Ndsi", "EventsPerSecond",
                   "LowFreqCover", "MidFreqCover", "HighFreqCover",
                   "ClusterCount", "ThreeGramCount")

# ---- Run PCA on each dataset ----
pca_list <- map(datasets, ~ prcomp(scale(.x[acoustic_vars]), center = TRUE))

# ---- Extract PC1–3 loadings ----
loadings <- map(pca_list, ~ .x$rotation[,1:3])

# ---- Compare all pairwise combinations and store results ----
dataset_names <- names(loadings)
comparison_results <- expand.grid(dataset1 = dataset_names,
                                  dataset2 = dataset_names,
                                  stringsAsFactors = FALSE) %>%
  filter(dataset1 != dataset2) %>%
  mutate(
    Procrustes_correlation = map2_dbl(dataset1, dataset2,
                                      ~ protest(loadings[[.x]], loadings[[.y]], permutations = 999)$t0),
    P_value = map2_dbl(dataset1, dataset2,
                       ~ protest(loadings[[.x]], loadings[[.y]], permutations = 999)$signif)
  )

# ---- Optional: remove duplicates (A vs B same as B vs A) ----
comparison_results <- comparison_results %>%
  rowwise() %>%
  mutate(pair = paste(sort(c(dataset1, dataset2)), collapse = "_vs_")) %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(Pair = pair, Procrustes_correlation, P_value)

# ---- View table ----
print(comparison_results)

# Matrix Table
res <- tibble(
  Pair = c(
    "rainless_single_vs_raw_single",
    "raw_global_vs_raw_single",
    "rainless_global_vs_raw_single",
    "rainless_single_vs_raw_global",
    "rainless_global_vs_rainless_single",
    "rainless_global_vs_raw_global"
  ),
  Procrustes_correlation = c(0.968, 0.854, 0.805, 0.939, 0.916, 0.991),
  P_value = c(0.001, 0.001, 0.002, 0.001, 0.001, 0.001)
)

# Split pair into rows (rainless) and columns (raw)
res_table <- res %>%
  separate(Pair, into = c("Row", "Col"), sep = "_vs_") %>%
  
  # Only keep combinations where one is rainless, one is raw
  filter(str_detect(Row, "rainless") & str_detect(Col, "raw")) %>%
  
  # Create a cell with correlation and p-value
  mutate(Cell = paste0(round(Procrustes_correlation, 3), " (p=", P_value, ")")) %>%
  
  select(Row, Col, Cell) %>%
  
  # Pivot into matrix style
  pivot_wider(names_from = Col, values_from = Cell)

res_table

