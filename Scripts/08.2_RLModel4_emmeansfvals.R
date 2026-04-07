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
fit_model4_extract_effects <- function(dat, response_var) {
  
  formula_obj <- as.formula(
    paste0(response_var,
           " ~ QBR_bin * TimeRangeFactor + ",
           "Strahler * TimeRangeFactor + ",
           "(1 | Site) + (1 | Season)")
  )
  
  model <- lmer(formula_obj, data = dat, REML = TRUE)
  
  # -----------------------------
  # 1. ANOVA (Type III F-values)
  # -----------------------------
  anova_res <- anova(model, type = 3) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    dplyr::select(term, `F value`) %>%
    rename(F_value = `F value`)
  
  # -----------------------------
  # 2. QBR EFFECT PER TIME
  # -----------------------------
  qbr_emm <- emmeans(model, ~ QBR_bin | TimeRangeFactor)
  
  qbr_trend <- contrast(qbr_emm, "poly") %>%
    as.data.frame() %>%
    dplyr::filter(contrast == "linear") %>%
    dplyr::mutate(term = "QBR_trend")
  
  # -----------------------------
  # 3. STRAHLER EFFECT PER TIME
  # -----------------------------
  strahler_emm <- emmeans(model, ~ Strahler | TimeRangeFactor)
  
  strahler_trend <- contrast(strahler_emm, "poly") %>%
    as.data.frame() %>%
    dplyr::filter(contrast == "linear") %>%
    dplyr::mutate(term = "Strahler_trend")
  
  # -----------------------------
  # Combine
  # -----------------------------
  effects_res <- bind_rows(qbr_trend, strahler_trend)
  
  return(list(
    anova   = anova_res,
    effects = effects_res
  ))
}


# Run Iterations --------------------------------------------------------------

set.seed(123)
n_runs <- 100
pcs    <- c("PC1", "PC2")

all_anova <- list()
all_emm   <- list()

for (pc in pcs) {
  message("Running iterations for ", pc)
  
  for (i in 1:n_runs) {
    message("  Iteration ", i)
    
    sampled_data <- sample_one_device_per_site_deployment(global_ds)
    
    res <- fit_model4_extract_effects(sampled_data, pc)
    
    # Store ANOVA
    all_anova[[length(all_anova) + 1]] <-
      res$anova %>%
      mutate(iteration = i, PC = pc)
    
    # Store EMMEANS
    all_emm[[length(all_emm) + 1]] <-
      res$emmeans %>%
      mutate(iteration = i, PC = pc)
  }
}

all_anova_df <- bind_rows(all_anova)
all_emm_df   <- bind_rows(all_emm)

saveRDS(all_anova_df,
        "clean_data/datasets/modelconsistency/model4_anova_bootstrap.rds")

saveRDS(all_emm_df,
        "clean_data/datasets/modelconsistency/model4_emmeans_bootstrap.rds")

# Evaluation ------------------------------------------------------------------

resultsall <- readRDS("clean_data/datasets/modelconsistency/RLnew2model4_results_allPCs.rds")

resultsall <- resultsall %>%
  mutate(
    significant = ifelse(lower > 0 | upper < 0, 1, 0),
    sign        = sign(estimate)
  )

message("Overall proportion significant: ", round(mean(resultsall$significant), 3))
message("Sign distribution:")
print(table(resultsall$sign))

# Stability: significance + direction -----------------------------------------

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

# Clean term labels -----------------------------------------------------------
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

# Plot prep -------------------------------------------------------------------

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


# Plot 1: Significance stability ----------------------------------------------

p_significance <- ggplot(plot_df,
                         aes(x = PC, y = term_clean, fill = prop_significant)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sig_label,
                fontface = ifelse(prop_significant >= 0.90, "bold", "plain")),
            colour = "black", size = 4) +
  scale_fill_gradientn(
    colours = c("#d73027", "#FF7134", "#fee08b", "#91cf60", "#1a9850"),
    limits  = c(0.5, 1),
    oob     = scales::squish,
    name    = "Proportion\nSignificant"
  ) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  labs(title = "Model 4 — Significance Stability (200 subsamples)",
       x = "", y = "Model Term") +
  theme_bw(base_size = 14) +
  theme(
    panel.grid        = element_blank(),
    strip.placement   = "outside",
    strip.background  = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = "bold"),
    axis.text.y       = element_text(size = 12),
    axis.text.x       = element_text(angle = 0),
    legend.position   = "right"
  )

ggsave(p_significance,
       file = "RLmodel4_significance_stability.png",
       width = 4, height = 4)

# Plot 2: Directional consistency ---------------------------------------------

p_direction <- ggplot(plot_df,
                      aes(x = PC, y = term_clean, fill = direction_consistency)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(sprintf("%.2f", direction_consistency), "\n", dir_label),
                fontface = ifelse(direction_consistency >= 0.90, "bold", "plain")),
            colour = "black", size = 3.5, lineheight = 0.9) +
  scale_fill_gradientn(
    colours = c("#d73027", "#FF7134", "#fee08b", "#91cf60", "#1a9850"),
    limits  = c(0.5, 1),
    oob     = scales::squish,
    name    = "Direction\nConsistency"
  ) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  labs(title = "Model 4 — Directional Consistency (200 subsamples)",
       subtitle = "▲ positive  ▼ negative  ~ variable",
       x = "", y = "Model Term") +
  theme_bw(base_size = 14) +
  theme(
    panel.grid        = element_blank(),
    strip.placement   = "outside",
    strip.background  = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = "bold"),
    axis.text.y       = element_text(size = 12),
    axis.text.x       = element_text(angle = 0),
    legend.position   = "right"
  )

ggsave(p_direction,
       file = "RLmodel4_direction_stability.png",
       width = 4, height = 4)

# Plot 3: Combined ------------------------------------------------------------


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
# Export stability table ------------------------------------------------------

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
