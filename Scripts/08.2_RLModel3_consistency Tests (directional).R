library(lme4)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(stringr)
library(lmerTest) 
library(MuMIn)

# Setup -----------------------------------------------------------------------

m3_ds <- read.csv("clean_data/datasets/PCAs/rainless_global2325_pca.csv")

m3_ds <- m3_ds %>%
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


# Fit Model 3 (CIs)
fit_RLmodel3_and_extract_ci <- function(dat, response_var) {
  formula_obj <- as.formula(
    paste0(response_var,
           " ~ QBR_bin + Strahler + ",
           "Season + TimeRangeFactor + (1 | Site)")
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

# Fit model 3 (F and P vals)
fit_RLmodel3_and_extract_fp <- function(dat, response_var) {
  formula_obj <- as.formula(
    paste0(response_var,
           " ~ QBR_bin + Strahler + ",
           "Season + TimeRangeFactor + (1 | Site)")
  )
  model     <- lmerTest::lmer(formula_obj, data = dat, REML = TRUE)
  aov_table <- anova(model, type = 3)
  
  tibble(
    term    = rownames(aov_table),
    F_value = aov_table[["F value"]],
    p_value = aov_table[["Pr(>F)"]],
    df_num  = aov_table[["NumDF"]],
    df_den  = aov_table[["DenDF"]]
  )
}


# Extract R2 Values
fit_RLmodel3_r2 <- function(dat, response_var) {
  formula_obj <- as.formula(
    paste0(response_var,
           " ~ QBR_bin + Strahler + ",
           "Season + TimeRangeFactor +  (1 | Site)")
  )
  
  model <- lmer(formula_obj, data = dat, REML = TRUE)
  r2_vals <- MuMIn::r.squaredGLMM(model)
  
  tibble(
    R2_marginal    = r2_vals[1],
    R2_conditional = r2_vals[2]
  )
}


# Run Iterations CIs --------------------------------------------------------------

set.seed(123)
n_runs <- 200
pcs    <- c("PC1", "PC2")

all_model3_CI_results <- map_dfr(pcs, function(pc) {
  message("Running iterations for ", pc)
  map_dfr(1:n_runs, function(i) {
    message("  Iteration ", i)
    sampled_data <- sample_one_device_per_site_deployment(m3_ds)
    
    fit_RLmodel3_and_extract_ci(sampled_data, pc) %>%
      mutate(iteration = i, PC = pc)
  })
})

saveRDS(all_model3_CI_results,
        "clean_data/datasets/modelconsistency/RLmodel3_CI_results_allPCs.rds")

write.csv(all_model3_CI_results,
          "clean_data/datasets/modelconsistency/RLmodel3_CI_results_allPCs.csv")

# Run R2
all_model3_r2_results <- map_dfr(pcs, function(pc) {
  message("Running iterations for ", pc)
  map_dfr(1:n_runs, function(i) {
    message("  Iteration ", i)
    sampled_data <- sample_one_device_per_site_deployment(m3_ds)
    
    fit_RLmodel3_r2(sampled_data, pc) %>%
      mutate(iteration = i, PC = pc)
  })
})
# Look at R2
model3_r2_summary <- all_model3_r2_results %>%
  group_by(PC) %>%
  summarise(
    mean_R2_marginal = mean(R2_marginal),
    sd_R2_marginal   = sd(R2_marginal),
    mean_R2_cond     = mean(R2_conditional),
    sd_R2_cond       = sd(R2_conditional)
  )

model3_r2_summary

# CI Evaluation ------------------------------------------------------------------

model3_resultsall <- readRDS("clean_data/datasets/modelconsistency/RLmodel3_CI_results_allPCs.rds")

model3_resultsall <- model3_resultsall %>%
  mutate(
    significant = ifelse(lower > 0 | upper < 0, 1, 0),
    sign        = sign(estimate)
  )

message("Overall proportion significant: ", round(mean(model3_resultsall$significant), 3))
message("Sign distribution:")
print(table(model3_resultsall$sign))

# CI Stability: significance + direction -----------------------------------------

model3_stability_df <- model3_resultsall %>%
  group_by(term, PC) %>%
  summarise(
    prop_significant      = mean(significant),
    prop_positive         = mean(sign ==  1),
    prop_negative         = mean(sign == -1),
    dominant_direction    = ifelse(mean(estimate) >= 0, "positive", "negative"),
    direction_consistency = max(mean(sign == 1), mean(sign == -1)),
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
print(table(model3_stability_df$stability_class, model3_stability_df$PC))

# CI Clean term labels -----------------------------------------------------------

model3_stability_df <- model3_stability_df %>%
  mutate(term_clean = case_when(
    term == "(Intercept)" ~ "Intercept",
    term == "QBR_bin"     ~ "QBR Class",
    term == "Strahler"    ~ "Stream Order",
    term == "SeasonMonsoon" ~ "Monsoon Shift",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(term_clean))

# CI Plot prep -------------------------------------------------------------------

y_order <- c("Intercept", "Monsoon Shift", "QBR Class", "Stream Order")

model3_plot_df <- model3_stability_df %>%
  mutate(
    term_clean = factor(term_clean, levels = rev(y_order)),
    group = case_when(
      term_clean == "Intercept" ~ "Intercept",
      term_clean == "QBR Class"       ~ "QBR Class",
      term_clean == "Stream Order"  ~ "Stream Order",
      term_clean == "Season"    ~ "Monsoon Shift"
    ),
    group = factor(group, levels = c("Intercept", "Season", "QBR", "Strahler")),
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

# CI Plot ------------------

p_combined <- ggplot(model3_plot_df,
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
  labs(title    = "Model A",
       x = "", y = "Model Term") +
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

p_combined

# Export CI stability table ------------------------------------------------------

stability_export <- stability_df %>%
  select(PC, term_clean, prop_significant, dominant_direction,
         direction_consistency, prop_sig_positive, prop_sig_negative,
         fully_stable, stability_class, mean_estimate, sd_estimate,
         mean_lower, mean_upper) %>%
  arrange(PC, term_clean)

write.csv(stability_export,
          "clean_data/datasets/modelconsistency/RLmodel3_stability_summary.csv",
          row.names = FALSE)

message("\nDone. Outputs saved:")
message("  RLmodel3_significance_stability.png")
message("  RLmodel3_direction_stability.png")
message("  RLmodel3_combined_stability.png")
message("  RLmodel3_stability_summary.csv")

# P and F Values ---------------------
set.seed(123)
n_runs <- 200
pcs    <- c("PC1", "PC2")

all_model3_fp_results <- map_dfr(pcs, function(pc) {
  message("Running iterations for ", pc)
  map_dfr(1:n_runs, function(i) {
    message("  Iteration ", i)
    sampled_data <- sample_one_device_per_site_deployment(m3_ds)
    fit_RLmodel3_and_extract_fp(sampled_data, pc) %>%
      mutate(iteration = i, PC = pc)
  })
})

write.csv(all_model3_fp_results,
        "clean_data/datasets/modelconsistency/RLmodel3_fp_results_allPCs.csv")

# Plotting ----------------------
all_model3_fp_results <- read.csv("clean_data/datasets/modelconsistency/RLmodel3_fp_results_allPCs.csv")

# Summarise your fp results first
model3_fp <- all_model3_fp_results %>%
  group_by(PC, term) %>%
  summarise(
    median_F   = median(F_value, na.rm = TRUE),
    prop_p05   = mean(p_value < 0.05, na.rm = TRUE),
    prop_p10   = mean(p_value < 0.10, na.rm = TRUE),
    median_p   = median(p_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(PC, term)

# Clean term labels
term_labels <- c(
  "Intercept"                       = "Intercept",
  "QBR_bin"                          = "QBR",
  "Strahler"                         = "Strahler",
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
model3_summary

model3_fp_plot_df <- model3_fp %>%
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

model3_fp_plot_df


# Plot
p_model3_fp <- ggplot(plot_df,
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

p_model3_fp

