# This script:
# 1 Reads all four datasets
# 2 Runs random-subset PCA sensitivity tests on each
# 3 Runs bootstrapped PCA loading stability comparisons on each
# 4 Compares PCA loadings between datasets and tests whether they differ significantly

library(tidyverse)
library(purrr)
library(broom)
library(vegan)


singledevice_ds           <- read.csv("clean_data/datasets/indices_datasets/global_singledevice_data.csv")
global_ds                 <- read.csv("clean_data/datasets/indices_datasets/global2325_data.csv")

global_singledevice_RL    <- read.csv("clean_data/datasets/indices_datasets/globalRL_singledevice_data.csv")
global_data_RL            <- read.csv("clean_data/datasets/indices_datasets/global2325RL_data.csv")

datasets <- list(
  single        = singledevice_ds,
  global        = global_ds,
  rl_single     = global_singledevice_RL,
  rl_global     = global_data_RL
)
# Acoustic indices of interest
acoustic_vars <- c("AcousticComplexity", "TemporalEntropy", "Ndsi",
                   "EventsPerSecond", "LowFreqCover", "MidFreqCover",
                   "HighFreqCover", "ClusterCount", "ThreeGramCount")

##### 2) Function: Randomly choose 3 indices and run a PCA -------------
run_random_pca <- function(df, vars){
  
  # randomly sample 3 indices
  chosen <- sample(vars, 3)
  
  # subset and scale the data
  mat <- df %>% 
    select(all_of(chosen)) %>% 
    scale() %>% 
    as.matrix()
  
  # run PCA
  p <- prcomp(mat, center = TRUE, scale. = FALSE)
  # extract variance explained
  variance <- (p$sdev^2) / sum(p$sdev^2)
  
  # determine which variable loads most strongly on PC1, PC2, & PC3
  loadings <- abs(p$rotation[,1:3])
  pc1_top <- names(which.max(loadings[,1]))
  pc2_top <- names(which.max(loadings[,2]))
  tibble(
    var1 = chosen[1],
    var2 = chosen[2],
    var3 = chosen[3],
    pc1_top = pc1_top,
    pc2_top = pc2_top,
    var_exp_pc1 = variance[1],
    var_exp_pc2 = variance[2],
    var_exp_total = sum(variance[1:2])
  )
}


# 3) Run the simulation multiple times ---------------------------------
set.seed(123)   # for reproducibility

pca_sensitivity_results <- map_dfr(
  1:500, ~ run_random_pca(global_data_RL, acoustic_vars)
)


pca_sensitivity_results %>%
  count(pc1_top, sort = TRUE) %>%
  mutate(percent = 100 * n / sum(n))

pca_sensitivity_results %>%
  count(pc2_top, sort = TRUE) %>%
  mutate(percent = 100 * n / sum(n))


# 4) Bootstrapping Verification -------------------------------------------------
bootstrap_pca <- function(df, vars, B = 500){
  
  boot_results <- map(1:B, function(i){
    
    # Sample rows with replacement
    boot_df <- df[sample(1:nrow(df), replace = TRUE), ]
    
    # Run PCA on scaled data
    p <- prcomp(boot_df %>% select(all_of(vars)) %>% scale(), center = TRUE)
    
    # Extract loadings + PC rank order
    load_df <- as.data.frame(p$rotation[,1:3]) %>% 
      rownames_to_column("Index") %>% 
      mutate(bootstrap = i,
             PC1_rank = rank(-abs(PC1), ties.method = "first"),
             PC2_rank = rank(-abs(PC2), ties.method = "first"))
    
    load_df
  }) %>% bind_rows()
  
  boot_results
}

# Run on Dataset 
set.seed(123)
boot_data <- bootstrap_pca(global_data_RL, acoustic_vars, B = 500)

# Computer 95% Confidence Intervals for ladings:
ci_table <- boot_data %>%
  group_by(Index) %>%
  summarise(
    PC1_low = quantile(abs(PC1), 0.025),
    PC1_high = quantile(abs(PC1), 0.975),
    PC2_low = quantile(abs(PC2), 0.025),
    PC2_high = quantile(abs(PC2), 0.975)
  )

# Rank consistency
rank_consistency <- boot_data %>%
  group_by(Index) %>%
  summarise(
    PC1_ranked_top = mean(PC1_rank == 1),
    PC2_ranked_top = mean(PC2_rank == 1)
  )

# Stability Score
stability_scores <- boot_data %>%
  group_by(Index) %>%
  summarise(
    PC1_strength = mean(abs(PC1)),
    PC2_strength = mean(abs(PC2)),
    PC1_freq = mean(PC1_rank == 1),
    PC2_freq = mean(PC2_rank == 1),
    Stability_PC1 = PC1_strength * PC1_freq,
    Stability_PC2 = PC2_strength * PC2_freq
  )

# Join tables into Summary
full_stability_summary <- ci_table %>%
  left_join(rank_consistency, by = "Index") %>%
  left_join(stability_scores, by = "Index")

full_stability_summary


# Comparing loadings between datasets: ------------------------

pca_list <- map(datasets, ~ prcomp(scale(.x[acoustic_vars]), center = TRUE))

loadings <- lapply(pca_list, function(x) x$rotation[,1:2])  # use PC1 & PC2


procrustes_compare <- function(mat1, mat2){
  proc <- procrustes(mat1, mat2)
  list(
    correlation = protest(mat1, mat2)$t0,
    significance = protest(mat1, mat2)$p.value
  )
}

dataset_names <- names(loadings)

procrustes_results <- map2(
  rep(loadings, each=length(loadings)),
  rep(loadings, times=length(loadings)),
  ~ procrustes_compare(.x, .y)
) %>% 
  setNames(expand.grid(dataset_names, dataset_names, stringsAsFactors = FALSE) %>% 
             apply(1, paste, collapse="_vs_"))
