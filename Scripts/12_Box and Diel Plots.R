library(tidyverse)
library(lubridate)
library(ggplot2)
library(cowplot)
library(dplyr)
# Data read in
structured_ds     <- read.csv("clean_data/datasets/PCAs/single_pca.csv")
global_ds         <- read.csv("clean_data/datasets/PCAs/global2325_pca.csv")

rl_structured_ds  <- read.csv("clean_data/datasets/PCAs/rainless_single_pca.csv")
rl_global_ds     <- read.csv("clean_data/datasets/PCAs/rainless_global2325_pca.csv")

head(structured_ds)
str(structured_ds)

# Setup: Colour Palettes & Ordering------------------------------------------------------
qbr_colors <- c(
  "Natural (95–100)" = "#006BA6",
  "Good (75–90)"     = "#22A122",
  "Fair (55–70)"     = "#DBCB43",
  "Poor (30–50)"     = "#FF7134",
  "Bad (<25)"        = "#AF3245"
)

strahler_colors <- c(
  "1st Order" = "#39A9DB",
  "2nd Order" = "#2EAB96",
  "3rd Order" = "#74A33F",
  "4th Order" = "#E3E351",
  "5th Order" = "#B6873A"
)

# Set the desired order for QBR_Class
structured_ds$QBR_Class <- factor(structured_ds$QBR_Class,
                                  levels = c("Natural (95–100)", 
                                             "Good (75–90)", 
                                             "Fair (55–70)", 
                                             "Poor (30–50)", 
                                             "Bad (<25)"))

rl_structured_ds$QBR_Class <- factor(rl_structured_ds$QBR_Class,
                                  levels = c("Natural (95–100)", 
                                             "Good (75–90)", 
                                             "Fair (55–70)", 
                                             "Poor (30–50)", 
                                             "Bad (<25)"))

global_ds$QBR_Class <- factor(global_ds$QBR_Class,
                                  levels = c("Natural (95–100)", 
                                             "Good (75–90)", 
                                             "Fair (55–70)", 
                                             "Poor (30–50)", 
                                             "Bad (<25)"))

rl_global_ds$QBR_Class <- factor(rl_global_ds$QBR_Class,
                                  levels = c("Natural (95–100)", 
                                             "Good (75–90)", 
                                             "Fair (55–70)", 
                                             "Poor (30–50)", 
                                             "Bad (<25)"))


# Plotting Individual PC1 Boxplots: QBR ----------------------------------------------------

# Single Device
strc_box_PC1 <- ggplot(structured_ds, aes(x = QBR_Class, y = PC1, fill = QBR_Class)) +
  geom_boxplot(outlier.size = 0.5) +
  scale_fill_manual(values = qbr_colors) +
  labs(x = "QBR Class", y = "PC1", fill = "QBR Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

strc_box_PC1


# Rainless Single Device
rl_strc_box_PC1 <- ggplot(rl_structured_ds, aes(x = QBR_Class, y = PC1, fill = QBR_Class)) +
  geom_boxplot(outlier.size = 0.5) +
  scale_fill_manual(values = qbr_colors) +
  labs(x = "QBR Class", y = "PC1", fill = "QBR Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

rl_strc_box_PC1


# Global
gl_box_PC1 <- ggplot(global_ds, aes(x = QBR_Class, y = PC1, fill = QBR_Class)) +
  geom_boxplot(outlier.size = 0.5) +
  scale_fill_manual(values = qbr_colors) +
  labs(x = "QBR Class", y = "PC1", fill = "QBR Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gl_box_PC1


# Rainless Global
rl_gl_box_PC1 <- ggplot(rl_global_ds, aes(x = QBR_Class, y = PC1, fill = QBR_Class)) +
  geom_boxplot(outlier.size = 0.5) +
  scale_fill_manual(values = qbr_colors) +
  labs(x = "QBR Class", y = "PC1", fill = "QBR Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

rl_gl_box_PC1


# PC1 Combined QBR Boxplots (4 Datasets) ------------------------------------------------

# helper theme to remove axis labels
no_xy_theme <- theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x  = element_blank(),
  axis.text.y  = element_blank(),
  legend.position = "none"
)

# Use already-created plots and strip labels/legend
strc_box_PC1_clean <- strc_box_PC1 + no_xy_theme
rl_strc_box_PC1_clean <- rl_strc_box_PC1 + no_xy_theme
gl_box_PC1_clean <- gl_box_PC1 + no_xy_theme
rl_gl_box_PC1_clean <- rl_gl_box_PC1 + no_xy_theme

# Extract ONE legend (from structured_ds plot)
legend_qbr <- get_legend( strc_box_PC1 + 
              theme( legend.position = "right", 
                     legend.title = element_text(size = 10), 
                     legend.margin = margin(0, 10, 0, 0), # ← GIVE RIGHT SPACE 
                     legend.box.margin = margin(0, 10, 0, 0) # ← EXTRA SPACE 
                                                ) )

legend_qbr_nudged <- plot_grid(legend_qbr, ncol = 1) +
  theme(plot.margin = margin(0, 10, 0, 0))   # ← nudges legend right

# Centered column titles
col_titles <- plot_grid(
  ggdraw() + draw_label("Structured", size = 14, hjust = -0.1),
  ggdraw() + draw_label("Global",     size = 14, hjust = -0.1),
  ncol = 2,
  align = "h"
)

# Row titles
row_titles <- plot_grid(
  ggdraw() + draw_label("Raw",      angle = 90, size = 14),
  ggdraw() + draw_label("Filtered", angle = 90, size = 14),
  ncol = 1
)

# main 2x2 plot grid
pc1_qbr_grid <- plot_grid(
  strc_box_PC1_clean, gl_box_PC1_clean,           # top: raw
  rl_strc_box_PC1_clean, rl_gl_box_PC1_clean,     # bottom: filtered
  ncol = 2,
  align = "hv"
)

# add row & column titles
pc1_qbr_panel <- plot_grid(
  col_titles, 
  plot_grid(row_titles, pc1_qbr_grid, ncol = 2, rel_widths = c(0.1, 1)),
  ncol = 1,
  rel_heights = c(0.1, 1)
)

# Add axis labels + legend 
final_pc1_qbr_plot <- plot_grid(
  pc1_qbr_panel, legend_qbr_nudged,
  ncol = 2, rel_widths = c(1, 0.35)
) +
  draw_label("PC1", x = 0.02, y = 0.5, angle = 90, size = 14)   # y-axis label

# View
final_pc1_qbr_plot

# Save
ggsave("PC1_QBR_boxplots.png", final_pc1_qbr_plot,
       width = 18, height = 10, dpi = 600, units = "cm")




# Plotting Individual PC1 Boxplots: Strahler  --------------------------

# Single Device
strc_box_PC1 <- ggplot(structured_ds, aes(x = Strahler_Class, y = PC1, fill = Strahler_Class)) +
  geom_boxplot(outlier.size = 0.5) +
  scale_fill_manual(values = strahler_colors) +
  labs(x = "Strahler Class", y = "PC1", fill = "Strahler Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

strc_box_PC1


# Rainless Single Device
rl_strc_box_PC1 <- ggplot(rl_structured_ds, aes(x = Strahler_Class, y = PC1, fill = Strahler_Class)) +
  geom_boxplot(outlier.size = 0.5) +
  scale_fill_manual(values = strahler_colors) +
  labs(x = "Strahler Class", y = "PC1", fill = "Strahler Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

rl_strc_box_PC1


# Global
gl_box_PC1 <- ggplot(global_ds, aes(x = Strahler_Class, y = PC1, fill = Strahler_Class)) +
  geom_boxplot(outlier.size = 0.5) +
  scale_fill_manual(values = strahler_colors) +
  labs(x = "Strahler Class", y = "PC1", fill = "Strahler Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gl_box_PC1


# Rainless Global
rl_gl_box_PC1 <- ggplot(rl_global_ds, aes(x = Strahler_Class, y = PC1, fill = Strahler_Class)) +
  geom_boxplot(outlier.size = 0.5) +
  scale_fill_manual(values = strahler_colors) +
  labs(x = "Strahler Class", y = "PC1", fill = "Strahler Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

rl_gl_box_PC1


# PC1 Combined Strahler Boxplots (4 Datasets) ### ------------------------------------------------------------

# helper theme to remove axis labels
no_xy_theme <- theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x  = element_blank(),
  axis.text.y  = element_blank(),
  legend.position = "none"
)

# Use already-created plots and strip labels/legend
strc_box_PC1_clean <- strc_box_PC1 + no_xy_theme
rl_strc_box_PC1_clean <- rl_strc_box_PC1 + no_xy_theme
gl_box_PC1_clean <- gl_box_PC1 + no_xy_theme
rl_gl_box_PC1_clean <- rl_gl_box_PC1 + no_xy_theme

# Extract ONE legend (from structured_ds plot)
legend_strahler <- get_legend( strc_box_PC1 + 
                            theme( legend.position = "right", 
                                   legend.title = element_text(size = 10), 
                                   legend.margin = margin(0, 10, 0, 0), # ← GIVE RIGHT SPACE 
                                   legend.box.margin = margin(0, 10, 0, 0) # ← EXTRA SPACE 
                            ) )
legend_strahler_nudged <- plot_grid(legend_strahler, ncol = 1) +
  theme(plot.margin = margin(0, 10, 0, 0))   # ← nudges legend right

# Build panel of 4 plots with row/column titles 
# Centered column titles
col_titles <- plot_grid(
  ggdraw() + draw_label("Structured", size = 14, hjust = -0.1),
  ggdraw() + draw_label("Global",     size = 14, hjust = -0.1),
  ncol = 2,
  align = "h"
)

# Row titles
row_titles <- plot_grid(
  ggdraw() + draw_label("Raw",      angle = 90, size = 14),
  ggdraw() + draw_label("Filtered", angle = 90, size = 14),
  ncol = 1
)

# main 2x2 plot grid
pc1_strahler_grid <- plot_grid(
  strc_box_PC1_clean, gl_box_PC1_clean,           # top: raw
  rl_strc_box_PC1_clean, rl_gl_box_PC1_clean,     # bottom: filtered
  ncol = 2,
  align = "hv"
)

# add row & column titles
pc1_strahler_panel <- plot_grid(
  col_titles, 
  plot_grid(row_titles, pc1_strahler_grid, ncol = 2, rel_widths = c(0.1, 1)),
  ncol = 1,
  rel_heights = c(0.1, 1)
)

# Add axis labels + legend 
final_pc1_strahler_plot <- plot_grid(
  pc1_strahler_panel, legend_strahler_nudged,
  ncol = 2, rel_widths = c(1, 0.35)
) +
  draw_label("PC1", x = 0.02, y = 0.5, angle = 90, size = 14)   # y-axis label

# View
final_pc1_strahler_plot

# Save
ggsave("PC1_Strahler_boxplots.png", final_pc1_strahler_plot,
       width = 18, height = 10, dpi = 600, units = "cm")




# Plotting Individual PC1 Diel Plots: QBR
structured_ds <- structured_ds %>%
  mutate(
    # Convert Time column to POSIXct
    Time_str = sprintf("%06d", Time),
    Hour = as.numeric(substr(Time_str, 1, 2)),
    Minute = as.numeric(substr(Time_str, 3, 4)),
    Time_POSIX = make_datetime(year = 2000, month = 1, day = 1, hour = Hour, min = Minute)
  )

# Bin into 30-min intervals
structured_ds <- structured_ds %>%
  mutate(Time_bin = floor_date(Time_POSIX, "30 minutes"))

# ------------------------
# 3. 30-min binned diel plot: PC1
# ------------------------
diel_PC1 <- structured_ds %>%
  group_by(Time_bin, QBR_Class) %>%
  summarise(
    PC1_mean = mean(PC1, na.rm = TRUE),
    PC1_se   = sd(PC1, na.rm = TRUE) / sqrt(n()),  # standard error
    .groups = "drop"
  )

plot_diel_PC1 <- ggplot(diel_PC1, aes(x = Time_bin, y = PC1_mean, color = QBR_Class)) +
  geom_ribbon(aes(ymin = PC1_mean - PC1_se, ymax = PC1_mean + PC1_se, fill = QBR_Class),
              alpha = 0.1, color = NA) +   # smaller shaded area
  geom_smooth(aes(group = QBR_Class), method = "loess", span = 0.2, se = FALSE, size = 1) +
  scale_color_manual(values = qbr_colors) +
  scale_fill_manual(values = qbr_colors) +
  labs(x = "Time of Day", y = "PC1", color = "QBR Class", fill = "QBR Class") +
  theme_minimal() +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hours")
plot_diel_PC1


# Diel Plots V2 - function enabled ------------------------
# Function to prepare and plot diel PC1
plot_diel_PC1_fun <- function(df) {
  df <- df %>%
    mutate(
      Time_str = sprintf("%06d", Time),
      Hour     = as.numeric(substr(Time_str, 1, 2)),
      Minute   = as.numeric(substr(Time_str, 3, 4)),
      Time_POSIX = make_datetime(year = 2000, month = 1, day = 1, hour = Hour, min = Minute),
      Time_bin   = floor_date(Time_POSIX, "30 minutes")
    )
  
  diel <- df %>%
    group_by(Time_bin, QBR_Class) %>%
    summarise(
      PC1_mean = mean(PC1, na.rm = TRUE),
      PC1_se   = sd(PC1, na.rm = TRUE)/sqrt(n()),
      .groups = "drop"
    )
  
  ggplot(diel, aes(x = Time_bin, y = PC1_mean, color = QBR_Class)) +
    geom_ribbon(aes(ymin = PC1_mean - PC1_se, ymax = PC1_mean + PC1_se, fill = QBR_Class),
                alpha = 0.1, color = NA) +
    geom_smooth(aes(group = QBR_Class), method = "loess", span = 0.2, se = FALSE, size = 1) +
    scale_color_manual(values = qbr_colors) +
    scale_fill_manual(values = qbr_colors) +
    labs(x = "Time of Day", y = "PC1", color = "QBR Class", fill = "QBR Class") +
    theme_minimal() +
    scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hours")
}

# Function for PC2
plot_diel_PC2_fun <- function(df) {
  df <- df %>%
    mutate(
      Time_str = sprintf("%06d", Time),
      Hour     = as.numeric(substr(Time_str, 1, 2)),
      Minute   = as.numeric(substr(Time_str, 3, 4)),
      Time_POSIX = make_datetime(year = 2000, month = 1, day = 1, hour = Hour, min = Minute),
      Time_bin   = floor_date(Time_POSIX, "30 minutes")
    )
  
  diel <- df %>%
    group_by(Time_bin, QBR_Class) %>%
    summarise(
      PC2_mean = mean(PC2, na.rm = TRUE),
      PC2_se   = sd(PC2, na.rm = TRUE)/sqrt(n()),
      .groups = "drop"
    )
  
  ggplot(diel, aes(x = Time_bin, y = PC2_mean, color = QBR_Class)) +
    geom_ribbon(aes(ymin = PC2_mean - PC2_se, ymax = PC1_mean + PC2_se, fill = QBR_Class),
                alpha = 0.1, color = NA) +
    geom_smooth(aes(group = QBR_Class), method = "loess", span = 0.2, se = FALSE, size = 1) +
    scale_color_manual(values = qbr_colors) +
    scale_fill_manual(values = qbr_colors) +
    labs(x = "Time of Day", y = "PC2", color = "QBR Class", fill = "QBR Class") +
    theme_minimal() +
    scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hours")
}

# Generate diel plots
plot_structured_PC1    <- plot_diel_PC1_fun(structured_ds)
plot_global_PC1        <- plot_diel_PC1_fun(global_ds)
plot_rl_structured_PC1 <- plot_diel_PC1_fun(rl_structured_ds)
plot_rl_global_PC1     <- plot_diel_PC1_fun(rl_global_ds)

# Column and row titles for cowplot
col_titles <- plot_grid(
  ggdraw() + draw_label("Structured", size = 14),
  ggdraw() + draw_label("Global", size = 14),
  ncol = 2
)

row_titles <- plot_grid(
  ggdraw() + draw_label("Raw", angle = 90, size = 14),
  ggdraw() + draw_label("Filtered", angle = 90, size = 14),
  ncol = 1
)

# Combine plots into 2x2 grid
diel_PC1_cow <- plot_grid(
  plot_structured_PC1, plot_global_PC1,
  plot_rl_structured_PC1, plot_rl_global_PC1,
  ncol = 2, nrow = 2
)

# Add titles
final_diel_PC1_plot <- plot_grid(
  col_titles, 
  plot_grid(row_titles, diel_PC1_cow, ncol = 2, rel_widths = c(0.05, 0.95)),
  ncol = 1, rel_heights = c(0.05, 0.95)
)

final_diel_PC1_plot

# Diel plot v3 no individual legends ----------------------------------------
# Function to prepare and plot diel PC1 without legend and 4-hour x-axis breaks
plot_diel_PC1_fun <- function(df) {
  df <- df %>%
    mutate(
      Time_str = sprintf("%06d", Time),
      Hour     = as.numeric(substr(Time_str, 1, 2)),
      Minute   = as.numeric(substr(Time_str, 3, 4)),
      Time_POSIX = make_datetime(year = 2000, month = 1, day = 1, hour = Hour, min = Minute),
      Time_bin   = floor_date(Time_POSIX, "30 minutes")
    )
  
  diel <- df %>%
    group_by(Time_bin, QBR_Class) %>%
    summarise(
      PC1_mean = mean(PC1, na.rm = TRUE),
      PC1_se   = sd(PC1, na.rm = TRUE)/sqrt(n()),
      .groups = "drop"
    )
  
  ggplot(diel, aes(x = Time_bin, y = PC1_mean, color = QBR_Class)) +
    geom_ribbon(aes(ymin = PC1_mean - PC1_se, ymax = PC1_mean + PC1_se, fill = QBR_Class),
                alpha = 0.1, color = NA) +
    geom_smooth(aes(group = QBR_Class), method = "loess", span = 0.2, se = FALSE, size = 1) +
    scale_color_manual(values = qbr_colors) +
    scale_fill_manual(values = qbr_colors) +
    labs(x = "Time of Day", y = "PC1") +   # remove color/fill labels from individual plots
    theme_minimal() +
    theme(legend.position = "none") +       # remove legend
    scale_x_datetime(date_labels = "%H:%M", date_breaks = "4 hours")
}

# Generate diel plots
plot_structured_PC1    <- plot_diel_PC1_fun(structured_ds)
plot_global_PC1        <- plot_diel_PC1_fun(global_ds)
plot_rl_structured_PC1 <- plot_diel_PC1_fun(rl_structured_ds)
plot_rl_global_PC1     <- plot_diel_PC1_fun(rl_global_ds)

# Extract a single legend from one plot
legend_PC1 <- get_legend(
  ggplot(structured_ds, aes(x = Time_POSIX, y = PC1, color = QBR_Class, fill = QBR_Class)) +
    geom_line() +
    scale_color_manual(values = qbr_colors) +
    scale_fill_manual(values = qbr_colors) +
    theme_minimal() +
    labs(color = "QBR Class", fill = "QBR Class")
)

# Column and row titles
col_titles <- plot_grid(
  ggdraw() + draw_label("Structured", size = 14),
  ggdraw() + draw_label("Global", size = 14),
  ncol = 2
)

row_titles <- plot_grid(
  ggdraw() + draw_label("Raw", angle = 90, size = 14),
  ggdraw() + draw_label("Filtered", angle = 90, size = 14),
  ncol = 1
)

# Combine plots into 2x2 grid
diel_PC1_cow <- plot_grid(
  plot_structured_PC1, plot_global_PC1,
  plot_rl_structured_PC1, plot_rl_global_PC1,
  ncol = 2, nrow = 2
)

# Add titles and legend on the side
final_diel_PC1_plot <- plot_grid(
  col_titles,
  plot_grid(
    row_titles, diel_PC1_cow, legend_PC1,
    ncol = 3,
    rel_widths = c(0.05, 0.85, 0.25)  # increase right margin for legend
  ),
  ncol = 1,
  rel_heights = c(0.05, 0.95)
)

final_diel_PC1_plot

ggsave("PC1_QBR_dielplots.png", final_diel_PC1_plot,
       width = 36, height = 20, dpi = 600, units = "cm")


