
# ========================== 1) DATA READ-IN =================================

library(tidyverse)
library(ggplot2)
library(cowplot)
library(GGally)
library(ggrepel)
library(plotly)
library(grid)   # for unit()

singledevice_ds    <- read.csv("clean_data/datasets/indices_datasets/global_singledevice_data.csv")
global_ds          <- read.csv("clean_data/datasets/indices_datasets/global2325_data.csv")

rl_singledevice_ds <- read.csv("clean_data/datasets/indices_datasets/globalRL_singledevice_data.csv")
rl_global_ds       <- read.csv("clean_data/datasets/indices_datasets/global2325RL_data.csv")

### Short plotting labels
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

acoustic_indices <- names(short_labels)

# ========================== 2) HELPER FUNCTIONS ====================================

# Run PCA
run_pca <- function(df, indices = acoustic_indices) {
  df_indices <- df %>% select(all_of(indices))
  df_scaled <- scale(df_indices)
  pca_res <- prcomp(df_scaled, center = TRUE, scale. = TRUE)
  return(list(pca = pca_res, scaled_data = df_scaled))
}

# Extract loadings into data.frame with short labels
extract_loadings_df <- function(pca_res) {
  df <- as.data.frame(pca_res$rotation)
  df$Index <- rownames(df)
  df$ShortIndex <- short_labels[df$Index]
  return(df)
}

# ==================== 3) RUN PCA ON EACH DATASET ============================

singledevice_pca    <- run_pca(singledevice_ds)
global_pca          <- run_pca(global_ds)
rl_singledevice_pca <- run_pca(rl_singledevice_ds)
rl_global_pca       <- run_pca(rl_global_ds)


# ================== 4) PCA ARROW PLOTS (NO FUNCTIONS) =======================

### 4A) SINGLE DEVICE ARROW PLOT
sd_loadings_df <- extract_loadings_df(singledevice_pca$pca)
sd_scores_df   <- as.data.frame(singledevice_pca$pca$x)

meta_cols <- c("Device", "Site", "Date", "Time", "Strahler", "Disturbance", "Branch")
meta_sd <- meta_cols[meta_cols %in% colnames(singledevice_ds)]
if(length(meta_sd) > 0) {
  sd_scores_df <- cbind(sd_scores_df, singledevice_ds[, meta_sd, drop = FALSE])
}

sd_arrow_scale <- 9
plot_singledevice_arrows <- ggplot() +
  theme_minimal() +
  labs(title = "PCA: Single Device") +
  geom_segment(data = sd_loadings_df,
               aes(x = 0, y = 0,
                   xend = PC1 * sd_arrow_scale,
                   yend = PC2 * sd_arrow_scale),
               arrow = arrow(length = unit(0.2, "inches")),
               color = "#2d2d2d", linewidth = 1, alpha = 0.7) +
  geom_text_repel(data = sd_loadings_df,
                  aes(x = PC1 * sd_arrow_scale, y = PC2 * sd_arrow_scale, label = ShortIndex),
                  color = "#2d2d2d", size = 3) +
  xlab("PC1") + ylab("PC2")

plot_singledevice_arrows

### 4B) GLOBAL ARROW PLOT
gl_loadings_df <- extract_loadings_df(global_pca$pca)
gl_scores_df   <- as.data.frame(global_pca$pca$x)

meta_gl <- meta_cols[meta_cols %in% colnames(global_ds)]
if(length(meta_gl) > 0) {
  gl_scores_df <- cbind(gl_scores_df, global_ds[, meta_gl, drop = FALSE])
}

gl_arrow_scale <- 9
plot_global_arrows <- ggplot() +
  theme_minimal() +
  labs(title = "PCA: Global Dataset") +
  geom_segment(data = gl_loadings_df,
               aes(x = 0, y = 0,
                   xend = PC1 * gl_arrow_scale,
                   yend = PC2 * gl_arrow_scale),
               arrow = arrow(length = unit(0.2, "inches")),
               color = "#2d2d2d", linewidth = 1, alpha = 0.7) +
  geom_text_repel(data = gl_loadings_df,
                  aes(x = PC1 * gl_arrow_scale, y = PC2 * gl_arrow_scale, label = ShortIndex),
                  color = "#2d2d2d", size = 3) +
  xlab("PC1") + ylab("PC2")

plot_global_arrows

### 4C) RL SINGLE DEVICE ARROW PLOT
rlds_loadings_df <- extract_loadings_df(rl_singledevice_pca$pca)
rlds_scores_df   <- as.data.frame(rl_singledevice_pca$pca$x)

meta_rlds <- meta_cols[meta_cols %in% colnames(rl_singledevice_ds)]
if(length(meta_rlds) > 0) {
  rlds_scores_df <- cbind(rlds_scores_df, rl_singledevice_ds[, meta_rlds, drop = FALSE])
}

rlds_arrow_scale <- 9
plot_rl_singledevice_arrows <- ggplot() +
  theme_minimal() +
  labs(title = "PCA: RL Single Device") +
  geom_segment(data = rlds_loadings_df,
               aes(x = 0, y = 0,
                   xend = PC1 * rlds_arrow_scale,
                   yend = PC2 * rlds_arrow_scale),
               arrow = arrow(length = unit(0.2, "inches")),
               color = "#2d2d2d", linewidth = 1, alpha = 0.7) +
  geom_text_repel(data = rlds_loadings_df,
                  aes(x = PC1 * rlds_arrow_scale, y = PC2 * rlds_arrow_scale, label = ShortIndex),
                  color = "#2d2d2d", size = 3) +
  xlab("PC1") + ylab("PC2")

plot_rl_singledevice_arrows

### 4D) RL GLOBAL ARROW PLOT
rlg_loadings_df <- extract_loadings_df(rl_global_pca$pca)
rlg_scores_df   <- as.data.frame(rl_global_pca$pca$x)

meta_rlg <- meta_cols[meta_cols %in% colnames(rl_global_ds)]
if(length(meta_rlg) > 0) {
  rlg_scores_df <- cbind(rlg_scores_df, rl_global_ds[, meta_rlg, drop = FALSE])
}

rlg_arrow_scale <- 9
plot_rl_global_arrows <- ggplot() +
  theme_minimal() +
  labs(title = "PCA: RL Global Dataset") +
  geom_segment(data = rlg_loadings_df,
               aes(x = 0, y = 0,
                   xend = PC1 * rlg_arrow_scale,
                   yend = PC2 * rlg_arrow_scale),
               arrow = arrow(length = unit(0.2, "inches")),
               color = "#2d2d2d", linewidth = 1, alpha = 0.7) +
  geom_text_repel(data = rlg_loadings_df,
                  aes(x = PC1 * rlg_arrow_scale, y = PC2 * rlg_arrow_scale, label = ShortIndex),
                  color = "#2d2d2d", size = 3) +
  xlab("PC1") + ylab("PC2")

plot_rl_global_arrows

arrow_plot_grid <- plot_grid(
  plot_singledevice_arrows,
  plot_global_arrows,
  plot_rl_singledevice_arrows,
  plot_rl_global_arrows,
  labels = c("A", "B", "C", "D"),
  label_size = 12,
  ncol = 2
)

arrow_plot_grid

###############################################################################
# ======================= 5) PCA HEATMAPS ====================================
###############################################################################

### 5A) HEATMAP: SINGLE DEVICE
sd_pve <- summary(singledevice_pca$pca)$importance[2, ]
sd_pve_vec <- setNames(sd_pve, names(sd_pve))

sd_loadings_long <- as.data.frame(singledevice_pca$pca$rotation) %>%
  rownames_to_column(var = "Index") %>%
  pivot_longer(cols = starts_with("PC"), names_to = "PC", values_to = "Loading") %>%
  mutate(WeightedLoading = abs(Loading) * sd_pve_vec[PC],
         IndexShort = short_labels[Index])

heatmap_singledevice <- plot_ly(
  data = sd_loadings_long,
  x = ~PC,
  y = ~IndexShort,
  z = ~WeightedLoading,
  type = "heatmap",
  colors = colorRamp(c("white", "#3DA9C7")),
  zmin = 0
) %>% layout(
  title = "Heatmap: Single Device",
  xaxis = list(title = "Principal Components"),
  yaxis = list(title = "Indices"),
  margin = list(l = 100, r = 20, t = 50, b = 50)
)

heatmap_singledevice

### 5B) HEATMAP: GLOBAL
gl_pve <- summary(global_pca$pca)$importance[2, ]
gl_pve_vec <- setNames(gl_pve, names(gl_pve))

gl_loadings_long <- as.data.frame(global_pca$pca$rotation) %>%
  rownames_to_column(var = "Index") %>%
  pivot_longer(cols = starts_with("PC"), names_to = "PC", values_to = "Loading") %>%
  mutate(WeightedLoading = abs(Loading) * gl_pve_vec[PC],
         IndexShort = short_labels[Index])

heatmap_global <- plot_ly(
  data = gl_loadings_long,
  x = ~PC,
  y = ~IndexShort,
  z = ~WeightedLoading,
  type = "heatmap",
  colors = colorRamp(c("white", "#3DA9C7")),
  zmin = 0
) %>% layout(
  title = "Heatmap: Global Dataset",
  xaxis = list(title = "Principal Components"),
  yaxis = list(title = "Indices"),
  margin = list(l = 100, r = 20, t = 50, b = 50)
)

heatmap_global

### 5C) HEATMAP: RL SINGLE DEVICE
rlds_pve <- summary(rl_singledevice_pca$pca)$importance[2, ]
rlds_pve_vec <- setNames(rlds_pve, names(rlds_pve))

rlds_loadings_long <- as.data.frame(rl_singledevice_pca$pca$rotation) %>%
  rownames_to_column(var = "Index") %>%
  pivot_longer(cols = starts_with("PC"), names_to = "PC", values_to = "Loading") %>%
  mutate(WeightedLoading = abs(Loading) * rlds_pve_vec[PC],
         IndexShort = short_labels[Index])

heatmap_rl_singledevice <- plot_ly(
  data = rlds_loadings_long,
  x = ~PC,
  y = ~IndexShort,
  z = ~WeightedLoading,
  type = "heatmap",
  colors = colorRamp(c("white", "#3DA9C7")),
  zmin = 0
) %>% layout(
  title = "Heatmap: RL Single Device",
  xaxis = list(title = "Principal Components"),
  yaxis = list(title = "Indices"),
  margin = list(l = 100, r = 20, t = 50, b = 50)
)

heatmap_rl_singledevice

### 5D) HEATMAP: RL GLOBAL
rlg_pve <- summary(rl_global_pca$pca)$importance[2, ]
rlg_pve_vec <- setNames(rlg_pve, names(rlg_pve))

rlg_loadings_long <- as.data.frame(rl_global_pca$pca$rotation) %>%
  rownames_to_column(var = "Index") %>%
  pivot_longer(cols = starts_with("PC"), names_to = "PC", values_to = "Loading") %>%
  mutate(WeightedLoading = abs(Loading) * rlg_pve_vec[PC],
         IndexShort = short_labels[Index])

heatmap_rl_global <- plot_ly(
  data = rlg_loadings_long,
  x = ~PC,
  y = ~IndexShort,
  z = ~WeightedLoading,
  type = "heatmap",
  colors = colorRamp(c("white", "#3DA9C7")),
  zmin = 0
) %>% layout(
  title = "Heatmap: RL Global Dataset",
  xaxis = list(title = "Principal Components"),
  yaxis = list(title = "Indices"),
  margin = list(l = 100, r = 20, t = 50, b = 50)
)

heatmap_rl_global

# Combine plots
# Helper to convert weighted loadings to ggplot heatmaps
make_heatmap_plot <- function(df, title) {
  ggplot(df, aes(x = PC, y = IndexShort, fill = WeightedLoading)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "#3DA9C7") +
    theme_minimal() +
    labs(title = title, x = "Principal Components", y = "Indices") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create ggplot heatmaps
heatmap_singledevice_gg    <- make_heatmap_plot(sd_loadings_long,    "Heatmap: Single Device")
heatmap_global_gg          <- make_heatmap_plot(gl_loadings_long,    "Heatmap: Global Dataset")
heatmap_rl_singledevice_gg <- make_heatmap_plot(rlds_loadings_long,  "Heatmap: RL Single Device")
heatmap_rl_global_gg       <- make_heatmap_plot(rlg_loadings_long,   "Heatmap: RL Global Dataset")


heatmap_grid <- plot_grid(
  heatmap_singledevice_gg,
  heatmap_global_gg,
  heatmap_rl_singledevice_gg,
  heatmap_rl_global_gg,
  labels = c("A", "B", "C", "D"),
  label_size = 12,
  ncol = 2
)

heatmap_grid

