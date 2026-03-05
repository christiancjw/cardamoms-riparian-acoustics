library(lme4)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

# Setup -----------------------------------------------------------------------

# Read data
global_ds <- read.csv("clean_data/datasets/PCAs/global2325_pca.csv")

# Clean Data Tables - adding values to use for filtering 
global_ds <- global_ds %>%
  mutate(
    Season = case_when(
      Date >= 20231116 & Date <= 20231203 ~ "Monsoon ",
      Date >= 20231230 & Date <= 20240208 ~ "Dry",
      Date >= 20240401 & Date <= 20240501 ~ "Dry",
      Date >= 20240607 & Date <= 20240707 ~ "Monsoon",
      Date >= 20250605 & Date <= 20250716 ~ "Monsoon",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Season)) %>%
  mutate(
    Time_num = as.numeric(Time),
    TimeHHMMSS = stringr::str_pad(Time_num, width = 6, pad = "0"),
    
    TimeRangeFactor = case_when(
      TimeHHMMSS >= "040001" & TimeHHMMSS <= "100000" ~ "Dawn",
      TimeHHMMSS >= "100001" & TimeHHMMSS <= "160000" ~ "Midday",
      TimeHHMMSS >= "160001" & TimeHHMMSS <= "220000" ~ "Dusk",
      TimeHHMMSS >= "220001" | TimeHHMMSS <= "040000" ~ "Midnight",
      TRUE ~ "Other"
    ),
    
    TimeRangeFactor = factor(
      TimeRangeFactor,
      levels = c("Dawn","Midday","Dusk","Midnight","Other")
    ),
    
    Site = factor(Site),
    Season = factor(Season),
    Device = factor(Device)
  ) %>%  # Create QBR_Bin
  mutate(QBR = case_when(
    QBR >= 95 & QBR <= 100 ~ 1,            # Natural
    QBR >= 75 & QBR < 95 ~ 2,              # Good
    QBR >= 55 & QBR < 75 ~ 3,              # Fair
    QBR >= 30 & QBR < 55 ~ 4,              # Poor
    QBR < 30 ~ 5,                           # Bad
    TRUE ~ NA_real_
  )
  )

# Functions -----------------------------------------

# Function to Subsample Data Randomly
sample_one_device_per_site_season <- function(dat) {
  
  chosen_devices <- dat %>%
    distinct(Site, Season, Device) %>%
    group_by(Site, Season) %>%
    slice_sample(n = 1) %>%
    ungroup()
  
  dat %>%
    semi_join(chosen_devices, by = c("Site", "Season", "Device"))
}



# Function to fit model & extract CIs (PC1)
fit_model_and_extract_ci_pc1 <- function(dat) {
      model <- lmer(
        PC1 ~ QBR * TimeRangeFactor +
          Strahler * TimeRangeFactor +
          (1 | Site) +
          (1 | Season),
        data = dat,
        REML = TRUE
      )
      # Profile confidence intervals (fixed effects only)
      ci <- confint(model, method = "profile", parm = "beta_")
      # Extract fixed effects
      estimates <- fixef(model)
      # Convert to tidy dataframe
      results <- tibble(
        term = names(estimates),
        estimate = as.numeric(estimates),
        lower = ci[, 1],
        upper = ci[, 2]
      )
      return(results)
}

# Function to fit model & extract CIs (PC2)
fit_model_and_extract_ci_pc2 <- function(dat) {
  model <- lmer(
    PC2 ~ QBR * TimeRangeFactor +
      Strahler * TimeRangeFactor +
      (1 | Site) +
      (1 | Season),
    data = dat,
    REML = TRUE
  )
  # Profile confidence intervals (fixed effects only)
  ci <- confint(model, method = "profile", parm = "beta_")
  # Extract fixed effects
  estimates <- fixef(model)
  # Convert to tidy dataframe
  results <- tibble(
    term = names(estimates),
    estimate = as.numeric(estimates),
    lower = ci[, 1],
    upper = ci[, 2]
  )
  return(results)
}






# Run PC1 Iterations -------------------------------
set.seed(123)

n_runs <- 50

results_pc1 <- map_dfr(1:n_runs, function(i) {
  message("Running iteration ", i)
  sampled_data <- sample_one_device_per_site_season(global_ds)
  ci_results <- fit_model_and_extract_ci_pc1(sampled_data)
  ci_results$iteration <- i
  return(ci_results)
})

saveRDS(results_pc1, "clean_data/datasets/modelconsistency/model4mk2resultsPC1.rds")

# Evaluation of model 
resultsPC1 <- readRDS("clean_data/datasets/modelconsistency/resultsPC1.rds")

resultsPC1 <- resultsPC1 %>%
  mutate(significant = ifelse(lower > 0 | upper < 0, TRUE, FALSE))

mean(resultsPC1$significant)


resultsPC1 <- resultsPC1 %>%
  mutate(sign = sign(estimate))

table(resultsPC1$sign)


# Quick plot
summary_resultsPC1 <- resultsPC1 %>%
  group_by(term) %>%
  summarise(
    mean_est = mean(estimate),
    lower_mean = mean(lower),
    upper_mean = mean(upper),
    sd_est = sd(estimate)
  )

ggplot(summary_resultsPC1, aes(x = reorder(term, mean_est), y = mean_est)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_mean, ymax = upper_mean), width = 0) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Average Profile CIs Across 100 Subsamples",
    x = "Model Term",
    y = "Estimate"
  )

# Sign stability
sign_stabilityPC1 <- resultsPC1 %>%
  mutate(significant = ifelse(lower > 0 | upper < 0, 1, 0)) %>%
  group_by(term) %>%
  summarise(prop_significant = mean(significant))

ggplot(sign_stabilityPC1, aes(x = reorder(term, prop_significant), 
                           y = prop_significant)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Proportion of Subsamples Where Term is Significant (PC1)",
    x = "Model Term",
    y = "Proportion Significant"
  )



# Run PC2 Iterations -------------------------------
set.seed(123)

n_runs <- 50

results_PC2 <- map_dfr(1:n_runs, function(i) {
  message("Running iteration ", i)
  sampled_data <- sample_one_device_per_site_season(global_ds)
  ci_results <- fit_model_and_extract_ci_pc2(sampled_data)
  ci_results$iteration <- i
  return(ci_results)
})

saveRDS(results_PC2, "clean_data/datasets/modelconsistency/model4mk2resultsPC2.rds")

# Evaluation of model
resultsPC2 <- readRDS("clean_data/datasets/modelconsistency/resultsPC2.rds")


resultsPC2 <- resultsPC2 %>%
  mutate(significant = ifelse(lower > 0 | upper < 0, TRUE, FALSE))

mean(resultsPC2$significant)


resultsPC2 <- resultsPC2 %>%
  mutate(sign = sign(estimate))

table(resultsPC2$sign)


# Quick plot
summary_resultPC2 <- resultsPC2 %>%
  group_by(term) %>%
  summarise(
    mean_est = mean(estimate),
    lower_mean = mean(lower),
    upper_mean = mean(upper),
    sd_est = sd(estimate)
  )

ggplot(summary_resultsPC2, aes(x = reorder(term, mean_est), y = mean_est)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_mean, ymax = upper_mean), width = 0) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Average Profile CIs Across 100 Subsamples",
    x = "Model Term",
    y = "Estimate"
  )

# Sign stability
sign_stabilityPC2 <- resultsPC2 %>%
  mutate(significant = ifelse(lower > 0 | upper < 0, 1, 0)) %>%
  group_by(term) %>%
  summarise(prop_significant = mean(significant))

ggplot(sign_stabilityPC2, aes(x = reorder(term, prop_significant), 
                              y = prop_significant)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Proportion of Subsamples Where Term is Significant (PC2)",
    x = "Model Term",
    y = "Proportion Significant"
  )

# Run PC3 Iterations -------------------------------
set.seed(123)
n_runs <- 100

results_PC3 <- map_dfr(1:n_runs, function(i) {
  message("Running iteration ", i)
  sampled_data <- sample_one_device_per_site_season(global_ds)
  ci_results <- fit_model_and_extract_ci_pc3(sampled_data)
  ci_results$iteration <- i
  return(ci_results)
})

saveRDS(results_PC3, "clean_data/datasets/modelconsistency/resultsPC3.rds")

# Evaluation of model 
resultsPC3 <- readRDS("clean_data/datasets/modelconsistency/resultsPC2.rds")

resultsPC3 <- resultsPC3 %>%
  mutate(significant = ifelse(lower > 0 | upper < 0, TRUE, FALSE))

mean(resultsPC3$significant)


resultsPC3 <- resultsPC3 %>%
  mutate(sign = sign(estimate))

table(resultsPC3$sign)


# Quick plot
summary_resultsPC3 <- resultsPC3 %>%
  group_by(term) %>%
  summarise(
    mean_est = mean(estimate),
    lower_mean = mean(lower),
    upper_mean = mean(upper),
    sd_est = sd(estimate)
  )

ggplot(summary_resultsPC3, aes(x = reorder(term, mean_est), y = mean_est)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_mean, ymax = upper_mean), width = 0) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Average Profile CIs Across 100 Subsamples",
    x = "Model Term",
    y = "Estimate"
  )

# Sign stability
sign_stabilityPC3 <- resultsPC3 %>%
  mutate(significant = ifelse(lower > 0 | upper < 0, 1, 0)) %>%
  group_by(term) %>%
  summarise(prop_significant = mean(significant))

ggplot(sign_stabilityPC3, aes(x = reorder(term, prop_significant), 
                              y = prop_significant)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Proportion of Subsamples Where Term is Significant",
    x = "Model Term",
    y = "Proportion Significant"
  )



# Final Sign Stability Plotting - PC1 & PC2  ---------------
resultsPC1 <- readRDS("clean_data/datasets/modelconsistency/resultsPC1.rds")
resultsPC2 <- readRDS("clean_data/datasets/modelconsistency/resultsPC2.rds")

resultsPC1 <- readRDS("clean_data/datasets/modelconsistency/model4mk2resultsPC1.rds")
resultsPC2 <- readRDS("clean_data/datasets/modelconsistency/model4mk2resultsPC2.rds")

head(resultsPC1)
head(resultsPC2)

# Add PC label
resultsPC1$PC <- "PC1"
resultsPC2$PC <- "PC2"

# Combine
combined_results <- bind_rows(resultsPC1, resultsPC2)

# Recalculate significance as numeric
combined_results <- combined_results %>%
  mutate(significant = ifelse(lower > 0 | upper < 0, 1, 0))

# Compute proportion significant per term per PC
stability_df <- combined_results %>%
  group_by(PC, term) %>%
  summarise(prop_significant = mean(significant),
            .groups = "drop")



# Create Term Groups
# 1. TimeRangeFactor only (main effects)
time_terms <- stability_df %>%
  filter(str_detect(term, "^TimeRangeFactor") & !str_detect(term, ":"))

# 2. Strahler main + interactions
strahler_terms <- stability_df %>%
  filter(term == "Strahler" |
           str_detect(term, "Strahler:TimeRangeFactor") |
           str_detect(term, "TimeRangeFactor.*:Strahler"))

# 3. QBR main + interactions
qbr_terms <- stability_df %>%
  filter(term == "QBR" |
           str_detect(term, "QBR:TimeRangeFactor") |
           str_detect(term, "TimeRangeFactor.*:QBR"))

# Plot Function
plot_stability <- function(df, title_text) {
  
  ggplot(df,
         aes(x = prop_significant,
             y = reorder(term, prop_significant),
             colour = PC)) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    scale_x_continuous(limits = c(0,1)) +
    theme_minimal(base_size = 14) +
    labs(
      title = title_text,
      x = "Proportion of Subsamples Significant",
      y = "Model Term",
      colour = "Component"
    )
}

p1 <- plot_stability(time_terms,
                     "Sign Stability: Time of Day Effects")

p2 <- plot_stability(strahler_terms,
                     "Sign Stability: Strahler Effects")

p3 <- plot_stability(qbr_terms,
                     "Sign Stability: QBR Effects")

p1
p2
p3





# Version 2 with dawn

# --- Prepare Dawn terms ---
add_dawn_terms <- function(stability_df) {
  
  # Extract intercept and main effects for numeric variables
  dawn_main <- stability_df %>%
    filter(term %in% c("(Intercept)", "Strahler", "QBR")) %>%
    mutate(term = case_when(
      term == "(Intercept)" ~ "TimeRangeFactorDawn",
      term == "Strahler"    ~ "Strahler:Dawn",
      term == "QBR"         ~ "QBR:Dawn",
      TRUE                  ~ term
    ))
  
  # Combine with original data
  bind_rows(stability_df, dawn_main)
}

combined_with_dawn <- add_dawn_terms(stability_df)

# --- Filter term groups for plotting ---

# --- Ensure proper ordering for times ---
time_order <- c("TimeRangeFactorDawn", "TimeRangeFactorMidday",
                "TimeRangeFactorDusk", "TimeRangeFactorMidnight")


# --- Define proper order for times ---
time_order_clean <- c("Dawn", "Midday", "Dusk", "Midnight")

# Time of day main effects
time_terms <- combined_with_dawn %>%
  filter(str_detect(term, "^TimeRangeFactor") & !str_detect(term, ":")) %>%
  mutate(
    term = str_remove(term, "^TimeRangeFactor"),
    # Reverse factor so Dawn is at top in horizontal bar plot
    term = factor(term, levels = rev(time_order_clean))
  )

# Strahler interactions
strahler_terms <- combined_with_dawn %>%
  filter(str_detect(term, "Strahler:Dawn|TimeRangeFactor.*:Strahler")) %>%
  mutate(term_label = case_when(
    term == "Strahler:Dawn" ~ "Dawn",
    str_detect(term, "Midday") ~ "Midday",
    str_detect(term, "Dusk") ~ "Dusk",
    str_detect(term, "Midnight") ~ "Midnight",
    TRUE ~ term
  ),
  term_label = factor(term_label, levels = rev(time_order_clean))  # reverse
  )

# QBR interactions
qbr_terms <- combined_with_dawn %>%
  filter(str_detect(term, "QBR:Dawn|QBR:TimeRangeFactor")) %>%
  mutate(term_label = case_when(
    term == "QBR:Dawn" ~ "Dawn",
    str_detect(term, "Midday") ~ "Midday",
    str_detect(term, "Dusk") ~ "Dusk",
    str_detect(term, "Midnight") ~ "Midnight",
    TRUE ~ term
  ),
  term_label = factor(term_label, levels = rev(time_order_clean))  # reverse
  )

# --- Plotting function for bars ---
plot_stability_bar <- function(df, x_col, y_col, title_text) {
  ggplot(df, aes_string(x = x_col, y = y_col, fill = "PC")) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
    theme_minimal(base_size = 14) +
    labs(
      title = title_text,
      x = "Proportion of Subsamples Significant",
      y = NULL,
      fill = "Principal Component"
    ) +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12))
}


# --- Create plots ---
p_time <- plot_stability_bar(time_terms, "prop_significant", "term",
                             "Sign Stability: Time of Day Effects")

p_strahler <- plot_stability_bar(strahler_terms, "prop_significant", "term_label",
                                 "Sign Stability: Strahler x Time of Day Interaction")
# Need to add raw Strahler In

p_qbr <- plot_stability_bar(qbr_terms, "prop_significant", "term_label",
                            "Sign Stability: QBR x Time of Day Interaction")

# --- Display plots vertically ---
library(gridExtra)
grid.arrange(p_time, p_strahler, p_qbr, ncol = 1)

# Trials
ggplot(sign_stability, aes(x = prop_significant, y = term, fill = sign_direction)) +
  geom_col() +
  facet_wrap(~PC) +
  coord_flip()



# Group Plot ----------------

stability_df <- stability_df %>%
  filter(term != "(Intercept)")

stability_df <- stability_df %>%
  mutate(term_clean = case_when(
    
    # Time main effects
    str_detect(term, "^TimeRangeFactorMidday$") ~ "Time:Midday",
    str_detect(term, "^TimeRangeFactorDusk$") ~ "Time:Dusk",
    str_detect(term, "^TimeRangeFactorMidnight$") ~ "Time:Midnight",
    
    # Dawn is reference — create it manually by using baselines
    term == "QBR" ~ "QBR:Dawn",
    term == "Strahler" ~ "Strahler:Dawn",
    
    # QBR interactions
    str_detect(term, "QBR:TimeRangeFactorMidday") ~ "QBR:Midday",
    str_detect(term, "QBR:TimeRangeFactorDusk") ~ "QBR:Dusk",
    str_detect(term, "QBR:TimeRangeFactorMidnight") ~ "QBR:Midnight",
    
    # Strahler interactions
    str_detect(term, "TimeRangeFactorMidday:Strahler") ~ "Strahler:Midday",
    str_detect(term, "TimeRangeFactorDusk:Strahler") ~ "Strahler:Dusk",
    str_detect(term, "TimeRangeFactorMidnight:Strahler") ~ "Strahler:Midnight",
    
    TRUE ~ NA_character_
  ))

# Add dawn for time
dawn_rows <- stability_df %>%
  distinct(PC) %>%
  mutate(
    term_clean = "Time:Dawn",
    prop_significant = 1
  )

stability_df <- bind_rows(stability_df, dawn_rows)

# Define Ordering
# Define Ordering with unique spacers
term_order <- c(
  # Time
  "Time:Dawn", "Time:Midday", "Time:Dusk", "Time:Midnight",
  
  "spacer1",   # spacer between Time and QBR
  
  # QBR
  "QBR:Dawn", "QBR:Midday", "QBR:Dusk", "QBR:Midnight",
  
  "spacer2",   # spacer between QBR and Strahler
  
  # Strahler
  "Strahler:Dawn", "Strahler:Midday", 
  "Strahler:Dusk", "Strahler:Midnight"
)

# Assign factor levels
stability_df$term_clean <- factor(
  stability_df$term_clean,
  levels = term_order
)

# Optional: replace actual data rows for spacers with NA to keep them empty
stability_df$prop_significant[stability_df$term_clean %in% c("spacer1","spacer2")] <- NA

# Plot
ggplot(stability_df,
       aes(x = term_clean,
           y = prop_significant,
           fill = PC)) +
  
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7) +
  
  scale_fill_manual(values = c("grey20", "grey70")) +
  
  # Keep empty spacer level
  scale_x_discrete(drop = FALSE) +
  #Remove spacer labels
  
  
  # Red 0.9 threshold
  geom_hline(yintercept = 0.9,
             colour = "red",
             linetype = "dotted",
             linewidth = 0.7) +
  
  # Separator between QBR and Strahler
  geom_vline(xintercept = 8.5,
             linetype = "dotted") +
  
  scale_y_continuous(limits = c(0,1)) +
  
  labs(x = NULL,
       y = "Proportion of iterations with 95% CI excluding zero",
       fill = "Component") +
  
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

# Plot v2
ggplot(stability_df,
      aes(x = term_clean, y = prop_significant, fill = PC)) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7, na.rm = TRUE) +   # ignore NA for spacers
  scale_fill_manual(values = c("grey20", "grey70")) +
  geom_hline(yintercept = 0.9, colour = "red", linetype = "dotted") +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_discrete(labels = function(x) x) +  # blanks stay as blank
  labs(x = NULL, y = "Proportion of Significant Iterations", fill = "Component") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank())


# Heatmap
# Remove spacer rows for heatmap (they break tiles)
heatmap_df <- stability_df %>%
  filter(!term_clean %in% c("spacer1", "spacer2"))

# Ensure ordering is identical to plot v2
heatmap_df$term_clean <- factor(
  heatmap_df$term_clean,
  levels = term_order[!term_order %in% c("spacer1", "spacer2")]
)

heatmap_wide <- heatmap_df %>%
  select(term_clean, PC, prop_significant) %>%
  pivot_wider(names_from = PC,
              values_from = prop_significant)

heatmap_long <- heatmap_wide %>%
  pivot_longer(cols = c(PC1, PC2),
               names_to = "PC",
               values_to = "prop_significant")

heatmap_long <- heatmap_long %>%
  mutate(
    strong = prop_significant >= 0.9,
    label = ifelse(strong,
                   paste0(sprintf("%.2f", prop_significant), "*"),
                   sprintf("%.2f", prop_significant))
  )

# Reverse Order
heatmap_long$term_clean <- factor(
  heatmap_long$term_clean,
  levels = rev(levels(heatmap_long$term_clean))
)

# rename terms + Times
heatmap_long <- heatmap_long %>%
  mutate(
    group = case_when(
      str_detect(term_clean, "^Time:") ~ "Time",
      str_detect(term_clean, "^QBR:") ~ "QBR",
      str_detect(term_clean, "^Strahler:") ~ "Strahler"
    ),
    
    time = str_remove(term_clean, "Time:|QBR:|Strahler:")
  )

heatmap_long$time <- dplyr::recode(
  heatmap_long$time,
  "Dawn" = "Morning",
  "Midday" = "Daytime",
  "Dusk" = "Evening",
  "Midnight" = "Night"
)

# Give Levels
heatmap_long$time <- factor(
  heatmap_long$time,
  levels = rev(c("Morning", "Daytime", "Evening", "Night"))
)

heatmap_long$group <- factor(heatmap_long$group,
                             levels = c("Time", "QBR", "Strahler"))

# Rename PCs
heatmap_long$PC <- dplyr::recode(heatmap_long$PC,
                          "PC1" = "Compound Index 1",
                          "PC2" = "Compound Index 2"
)


# Plot
ggplot(heatmap_long,
       aes(x = PC,
           y = time,
           fill = prop_significant)) +
  
  geom_tile(color = "white", linewidth = 0.5) +
  
  geom_text(aes(label = label,
                fontface = ifelse(strong, "bold", "plain")),
            colour = "black",
            size = 4) +
  
  scale_fill_gradientn(
    colours = c("#d73027", "#FF7134", "#fee08b",
                "#91cf60", "#1a9850"),
    limits = c(0.5, 1),
    oob = scales::squish,
    name = "Proportion\nSignificant"
  ) +
  
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  
  
  labs(x = "",
       y = "Model Terms") +
  
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = "bold"),
    axis.text.y = element_text(size = 12),
    legend.position = "right"
  )
