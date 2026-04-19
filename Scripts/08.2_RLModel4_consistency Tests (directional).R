library(lme4)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(stringr)
library(lmerTest) 

# Setup -----------------------------------------------------------------------

global_ds <- read.csv("clean_data/datasets/PCAs/rainless_global2325_pca.csv")

head(global_ds)

global_ds <- global_ds %>%
  mutate(
    Deployment = case_when(
      Date >= 20231116 & Date <= 20231203 ~ "Monsoon 2023",
      Date >= 20231230 & Date <= 20240208 ~ "Dry Transition 2024",
      Date >= 20240401 & Date <= 20240501 ~ "Dry 2024",
      Date >= 20240607 & Date <= 20240707 ~ "Monsoon Transition 2024",
      Date >= 20250605 & Date <= 20250716 ~ "Monsoon 2025",
      TRUE ~ NA_character_
    ),
    Season = case_when(
      Date >= 20231116 & Date <= 20231203 ~ "Monsoon",
      Date >= 20231230 & Date <= 20240208 ~ "Dry",
      Date >= 20240401 & Date <= 20240501 ~ "Dry",
      Date >= 20240607 & Date <= 20240707 ~ "Monsoon",
      Date >= 20250605 & Date <= 20250716 ~ "Monsoon",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Season)) %>%
  mutate(
    Time_num   = as.numeric(Time),
    TimeHHMMSS = stringr::str_pad(Time_num, width = 6, pad = "0"),
    TimeRangeFactor = case_when(
      TimeHHMMSS >= "040001" & TimeHHMMSS <= "100000" ~ "Morning",
      TimeHHMMSS >= "100001" & TimeHHMMSS <= "160000" ~ "Day",
      TimeHHMMSS >= "160001" & TimeHHMMSS <= "220000" ~ "Evening",
      TimeHHMMSS >= "220001" | TimeHHMMSS <= "040000" ~ "Night",
      TRUE ~ "Other"
    ),
    TimeRangeFactor = factor(TimeRangeFactor,
                             levels = c("Morning", "Day", "Evening", "Night", "Other")),
    Site   = factor(Site),
    Season = factor(Season),
    Device = factor(Device),
    QBR_bin = case_when(
      QBR >= 95 & QBR <= 100 ~ 1,
      QBR >= 75 & QBR <  95  ~ 2,
      QBR >= 55 & QBR <  75  ~ 3,
      QBR >= 30 & QBR <  55  ~ 4,
      QBR <  30               ~ 5,
      TRUE ~ NA_real_
    )
  )

# Functions -------------------------------------------------------------------

sample_one_device_per_site_deployment <- function(dat, n_days = 10) {
  dat <- dat %>%
    mutate(Date = as.Date(as.character(Date), format = "%Y%m%d"))
  
  chosen_devices <- dat %>%
    distinct(Site, Deployment, Device) %>%
    group_by(Site, Deployment) %>%
    slice_sample(n = 1) %>%
    ungroup()
  
  dat_subset <- dat %>%
    semi_join(chosen_devices, by = c("Site", "Deployment", "Device"))
  
  sampled <- dat_subset %>%
    group_by(Site, Deployment, Device) %>%
    group_modify(~ {
      available_dates <- sort(unique(.x$Date))
      if (length(available_dates) <= n_days) return(.x)
      valid_starts <- available_dates[available_dates <= max(available_dates) - (n_days - 1)]
      start_date   <- sample(valid_starts, 1)
      end_date     <- start_date + (n_days - 1)
      .x %>% filter(Date >= start_date & Date <= end_date)
    }) %>%
    ungroup()
  
  return(sampled)
}

# Model 4: Season as random intercept (not fixed factor)
fit_model4_and_extract_ci <- function(dat, response_var) {
  formula_obj <- as.formula(
    paste0(response_var,
           " ~ QBR_bin * TimeRangeFactor + ",
           "Strahler * TimeRangeFactor + ",
           "Season + (1 | Site)")
  )
  model <- lmer(formula_obj, data = dat, REML = TRUE)
  ci    <- confint(model, method = "Wald")
  ci    <- ci[rownames(ci) %in% names(fixef(model)), ]
  
  tibble(
    term     = rownames(ci),
    estimate = fixef(model)[rownames(ci)],
    lower    = ci[, 1],
    upper    = ci[, 2]
  )
}

# Extract Anova f and P values
fit_model4_and_extract_fp <- function(dat, response_var) {
  formula_obj <- as.formula(
    paste0(response_var,
           " ~ QBR_bin * TimeRangeFactor + ",
           "Strahler * TimeRangeFactor + ",
           "Season + (1 | Site)")
  )
  # lmerTest::lmer adds Satterthwaite F and p-values to the ANOVA table
  model <- lmerTest::lmer(formula_obj, data = dat, REML = TRUE)
  
  aov_table <- anova(model, type = 3)  # Type III to handle interaction terms properly
  
  tibble(
    term    = rownames(aov_table),
    F_value = aov_table[["F value"]],
    p_value = aov_table[["Pr(>F)"]],
    df_num  = aov_table[["NumDF"]],
    df_den  = aov_table[["DenDF"]]
  )
}

# Run Iterations (CIs)--------------------------------------------------------------
set.seed(123)
n_runs <- 200
pcs    <- c("PC1", "PC2")

all_model4_results <- map_dfr(pcs, function(pc) {
  message("Running iterations for ", pc)
  map_dfr(1:n_runs, function(i) {
    message("  Iteration ", i)
    sampled_data <- sample_one_device_per_site_deployment(global_ds)
    fit_model4_and_extract_ci(sampled_data, pc) %>%
      mutate(iteration = i, PC = pc)
  })
})

saveRDS(all_model4_results,
        "clean_data/datasets/modelconsistency/RLnew2model4_results_allPCs.rds")



# CIs Evaluation ------------------------------------------------------------------

resultsall <- readRDS("clean_data/datasets/modelconsistency/RLnew2model4_results_allPCs.rds")

resultsall <- resultsall %>%
  mutate(
    significant = ifelse(lower > 0 | upper < 0, 1, 0),
    sign        = sign(estimate)
  )

message("Overall proportion significant: ", round(mean(resultsall$significant), 3))
message("Sign distribution:")
print(table(resultsall$sign))

# CI Stability: significance + direction -----------------------------------------

stability_df <- resultsall %>%
  group_by(term, PC) %>%
  summarise(
    # Significance stability
    prop_significant      = mean(significant),
    
    # Direction metrics across ALL iterations
    prop_positive         = mean(sign ==  1),
    prop_negative         = mean(sign == -1),
    dominant_direction    = ifelse(mean(estimate) >= 0, "positive", "negative"),
    direction_consistency = max(mean(sign == 1), mean(sign == -1)),
    
    # Direction among only significant iterations
    prop_sig_positive = ifelse(
      sum(significant) > 0,
      sum(significant == 1 & sign ==  1) / sum(significant),
      NA_real_
    ),
    prop_sig_negative = ifelse(
      sum(significant) > 0,
      sum(significant == 1 & sign == -1) / sum(significant),
      NA_real_
    ),
    
    # Mean estimate and spread
    mean_estimate = mean(estimate),
    sd_estimate   = sd(estimate),
    mean_lower    = mean(lower),
    mean_upper    = mean(upper),
    
    .groups = "drop"
  ) %>%
  mutate(
    directionally_stable = direction_consistency >= 0.90,
    fully_stable         = prop_significant >= 0.90 & directionally_stable,
    stability_class = case_when(
      fully_stable                          ~ "Stable (sig. + direction)",
      prop_significant >= 0.90              ~ "Significant but direction variable",
      directionally_stable                  ~ "Consistent direction but low significance",
      TRUE                                  ~ "Unstable"
    )
  )

message("\nStability classification summary:")
print(table(stability_df$stability_class, stability_df$PC))

# CI Clean term labels -----------------------------------------------------------
# Model 4 has no Season fixed effects — simpler label set than Model 5

stability_df <- stability_df %>%
  mutate(term_clean = case_when(
    term == "(Intercept)"                        ~ "Time:Morning",
    term == "TimeRangeFactorDay"                 ~ "Time:Day",
    term == "TimeRangeFactorEvening"             ~ "Time:Evening",
    term == "TimeRangeFactorNight"               ~ "Time:Night",
    term == "QBR_bin"                            ~ "QBR:Morning",
    term == "Strahler"                           ~ "Strahler:Morning",
    term == "QBR_bin:TimeRangeFactorDay"         ~ "QBR:Day",
    term == "QBR_bin:TimeRangeFactorEvening"     ~ "QBR:Evening",
    term == "QBR_bin:TimeRangeFactorNight"       ~ "QBR:Night",
    term == "TimeRangeFactorDay:Strahler"        ~ "Strahler:Day",
    term == "TimeRangeFactorEvening:Strahler"    ~ "Strahler:Evening",
    term == "TimeRangeFactorNight:Strahler"      ~ "Strahler:Night",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(term_clean))

# CI Plot prep -------------------------------------------------------------------

y_order <- c(
  "Time:Morning", "Time:Day",      "Time:Evening", "Time:Night",
  "QBR:Morning",  "QBR:Day",       "QBR:Evening",  "QBR:Night",
  "Strahler:Morning", "Strahler:Day", "Strahler:Evening", "Strahler:Night"
)

plot_df <- stability_df %>%
  mutate(
    term_clean = factor(term_clean, levels = rev(y_order)),
    
    # Extract group BEFORE stripping prefix
    group = case_when(
      str_detect(term_clean, "^Time:")     ~ "Time",
      str_detect(term_clean, "^QBR:")      ~ "QBR",
      str_detect(term_clean, "^Strahler:") ~ "Strahler"
    ),
    group = factor(group, levels = c("Time", "QBR", "Strahler")),
    
    # Strip prefix from y-axis label AFTER group is assigned
    term_clean = str_remove(term_clean, "^[^:]+:"),
    term_clean = factor(term_clean, levels = rev(c("Morning", "Day", "Evening", "Night"))),
    
    sig_label = case_when(
      prop_significant >= 0.90 ~ paste0(sprintf("%.2f", prop_significant), "*"),
      TRUE                     ~ sprintf("%.2f", prop_significant)
    ),
    dir_label = case_when(
      dominant_direction == "positive" & direction_consistency >= 0.90 ~ "▲",
      dominant_direction == "negative" & direction_consistency >= 0.90 ~ "▼",
      TRUE ~ "~"
    ),
    combined_label = paste0(sig_label, " ", dir_label)
  )


# CI Plot Combined ------------------------------------------------------------


p_model4_combined <- ggplot(plot_df,
                     aes(x = PC, y = term_clean, fill = fully_stable)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = combined_label,
                fontface = ifelse(fully_stable, "bold", "plain")),
            colour = "black", size = 3.8) +
  scale_fill_manual(
    values = c("TRUE" = "#C0DD97", "FALSE" = "#F1EFE8"),
    labels = c("TRUE" = "Stable (sig. + direction)", "FALSE" = "Unstable"),
    name   = NULL
  ) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  labs(x = "", y = "Model Term") +
  theme_bw(base_size = 14) +
  theme(
    panel.grid        = element_blank(),
    strip.placement   = "outside",
    strip.background  = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = "bold"),
    axis.text.y       = element_text(size = 12),
    axis.text.x       = element_text(angle = 0),
    legend.position   = "none",
    plot.subtitle     = element_text(size = 9)
  )

p_model4_combined


ggsave("model4consistency.png", p_model4_combined,
       width = 4, height = 6, units = "in", dpi = 300)

library(patchwork) # For putting plots together

model_consistencies <- ( p_model4_combined | p_model5_combined) +            # Row 2
  plot_layout(widths = c(1, 2))# Row 2
model_consistencies




ggsave("modelsconsistency.png", model_consistencies,
       width = 10, height = 6, units = "in", dpi = 300)
# CI Export stability table ------------------------------------------------------

stability_export <- stability_df %>%
  select(PC, term_clean, prop_significant, dominant_direction,
         direction_consistency, prop_sig_positive, prop_sig_negative,
         fully_stable, stability_class, mean_estimate, sd_estimate,
         mean_lower, mean_upper) %>%
  arrange(PC, term_clean)

write.csv(stability_export,
          "clean_data/datasets/modelconsistency/RLmodel4_stability_summary.csv",
          row.names = FALSE)

message("\nDone. Outputs saved:")
message("  model4_significance_stability.png")
message("  model4_direction_stability.png")
message("  model4_combined_stability.png")
message("  model4_stability_summary.csv")

# CI Run Iterations (P and F vals) --------------------------

set.seed(123)
n_runs <- 200
pcs    <- c("PC1", "PC2")

all_model4_fp_results <- map_dfr(pcs, function(pc) {
  message("Running iterations for ", pc)
  map_dfr(1:n_runs, function(i) {
    message("  Iteration ", i)
    sampled_data <- sample_one_device_per_site_deployment(global_ds)
    fit_model4_and_extract_fp(sampled_data, pc) %>%
      mutate(iteration = i, PC = pc)
  })
})

all_model4_fp_results 


write.csv(all_model4_fp_results,
          "clean_data/datasets/modelconsistency/RLmodel4fp_results_allPCs.csv")

# Plotting ----------------------
all_model4_fp_results <- read.csv("clean_data/datasets/modelconsistency/RLmodel4fp_results_allPCs.csv")



model4_fp <- all_model4_fp_results %>%
  group_by(PC, term) %>%
  summarise(
    median_F   = median(F_value, na.rm = TRUE),
    prop_p05   = mean(p_value < 0.05, na.rm = TRUE),
    prop_p10   = mean(p_value < 0.10, na.rm = TRUE),
    median_p   = median(p_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(PC, term)




# Summarise your fp results first
model4_summary <- all_model4_fp_results %>%
  group_by(PC, term) %>%
  summarise(
    median_F  = median(F_value, na.rm = TRUE),
    prop_p05  = mean(p_value < 0.05, na.rm = TRUE),
    median_p  = median(p_value, na.rm = TRUE),
    .groups   = "drop"
  )

# Clean term labels
term_labels <- c(
  "QBR_bin"                          = "QBR",
  "Strahler"                         = "Strahler",
  "TimeRangeFactor"                  = "Time of Day",
  "Season"                           = "Season",
  "QBR_bin:TimeRangeFactor"          = "QBR × Time",
  "QBR_bin:Season"                   = "QBR × Season",
  "TimeRangeFactor:Strahler"         = "Strahler × Time",
  "Season:Strahler"                  = "Strahler × Season",
  "TimeRangeFactor:Season"           = "Time × Season",
  "QBR_bin:TimeRangeFactor:Season"   = "QBR × Time × Season",
  "TimeRangeFactor:Season:Strahler"  = "Strahler × Time × Season"
)

# Assign hierarchy groups for faceting
term_groups <- tibble(
  term = names(term_labels),
  term_clean = term_labels,
  group = c(
    "Main Effects", "Main Effects", "Main Effects", "Main Effects",
    "Two-Way", "Two-Way", "Two-Way", "Two-Way", "Two-Way",
    "Three-Way", "Three-Way"
  )
)

# Build plot_df
model4_summary

plot_df <- model4_summary %>%
  left_join(term_groups, by = "term") %>%
  filter(!is.na(term_clean)) %>%
  mutate(
    stable      = prop_p05 >= 0.70,
    F_label     = sprintf("F=%.1f", median_F),
    prop_label  = sprintf("%.2f", prop_p05),
    combined_label = paste0(F_label, "\n", prop_label),
    term_clean  = factor(term_clean, levels = rev(term_labels)),
    group       = factor(group, levels = c("Main Effects", "Two-Way", "Three-Way")),
    PC_label    = paste0(PC)
  )

plot_df


# Plot
p_model4_fp <- ggplot(plot_df,
                      aes(x = PC_label, y = term_clean, fill = stable)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(label    = combined_label,
        fontface = ifelse(stable, "bold", "plain")),
    colour = "black", size = 3.5
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#C0DD97", "FALSE" = "#F1EFE8"),
    labels = c("TRUE" = "Stable (prop ≥ 0.70)", "FALSE" = "Unstable"),
    name   = NULL
  ) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  labs(
    x = "", y = "Model Term",
    caption = "Cell values: median F-statistic (top), proportion of iterations p < 0.05 (bottom)\nGreen = stable effect (≥70% of iterations significant)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid         = element_blank(),
    strip.placement    = "outside",
    strip.background   = element_blank(),
    strip.text.y.left  = element_text(angle = 0, face = "bold"),
    axis.text.y        = element_text(size = 11),
    axis.text.x        = element_text(size = 12, face = "bold"),
    legend.position    = "none",
    plot.caption       = element_text(size = 8, colour = "grey40")
  )

p_model4_fp

