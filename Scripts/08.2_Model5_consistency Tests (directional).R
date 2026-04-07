library(lme4)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(stringr)

# Setup -----------------------------------------------------------------------

global_ds <- read.csv("clean_data/datasets/PCAs/global2325_pca.csv")

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
    Time_num = as.numeric(Time),
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

fit_model_and_extract_ci <- function(dat, response_var) {
  formula_obj <- as.formula(
    paste0(response_var,
           " ~ QBR_bin * TimeRangeFactor * Season + ",
           "Strahler * TimeRangeFactor * Season + ",
           "(1 | Site)")
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

# Run Iterations --------------------------------------------------------------

set.seed(123)
n_runs <- 200
pcs    <- c("PC1", "PC2")

all_results <- map_dfr(pcs, function(pc) {
  message("Running iterations for ", pc)
  map_dfr(1:n_runs, function(i) {
    message("  Iteration ", i)
    sampled_data <- sample_one_device_per_site_deployment(global_ds)
    fit_model_and_extract_ci(sampled_data, pc) %>%
      mutate(iteration = i, PC = pc)
  })
})

saveRDS(all_results,
        "clean_data/datasets/modelconsistency/newmodel5_results_allPCs.rds")

# Evaluation ------------------------------------------------------------------

resultsall <- readRDS("clean_data/datasets/modelconsistency/newmodel5_results_allPCs.rds")

resultsall <- resultsall %>%
  mutate(
    significant = ifelse(lower > 0 | upper < 0, 1, 0),
    sign        = sign(estimate)
  )

# Overall summary
message("Overall proportion significant: ", round(mean(resultsall$significant), 3))
message("Sign distribution:")
print(table(resultsall$sign))

# Stability: significance + direction -----------------------------------------

stability_df <- resultsall %>%
  group_by(term, PC) %>%
  summarise(
    # Significance stability: proportion of iterations where CI excludes zero
    prop_significant    = mean(significant),
    
    # Direction metrics
    prop_positive       = mean(sign ==  1),   # proportion of iters with positive estimate
    prop_negative       = mean(sign == -1),   # proportion of iters with negative estimate
    
    # Dominant direction and how consistent it is
    dominant_direction  = ifelse(mean(estimate) >= 0, "positive", "negative"),
    direction_consistency = max(mean(sign == 1), mean(sign == -1)),
    
    # Among only the significant iterations, is direction consistent?
    prop_sig_positive   = ifelse(
      sum(significant) > 0,
      sum(significant == 1 & sign ==  1) / sum(significant),
      NA_real_
    ),
    prop_sig_negative   = ifelse(
      sum(significant) > 0,
      sum(significant == 1 & sign == -1) / sum(significant),
      NA_real_
    ),
    
    # Mean estimate and spread
    mean_estimate       = mean(estimate),
    sd_estimate         = sd(estimate),
    mean_lower          = mean(lower),
    mean_upper          = mean(upper),
    
    .groups = "drop"
  ) %>%
  # A term is "directionally stable" if >90% of ALL iterations share one sign
  mutate(
    directionally_stable = direction_consistency >= 0.90,
    
    # "Fully stable" = significant AND directionally consistent
    fully_stable = prop_significant >= 0.90 & directionally_stable,
    
    # Stability classification for reporting
    stability_class = case_when(
      fully_stable                          ~ "Stable (sig. + direction)",
      prop_significant >= 0.90              ~ "Significant but direction variable",
      directionally_stable                  ~ "Consistent direction but low significance",
      TRUE                                  ~ "Unstable"
    )
  )

# Print summary of stability classes
message("\nStability classification summary:")
print(table(stability_df$stability_class, stability_df$PC))

# Clean term labels -----------------------------------------------------------

stability_df <- stability_df %>%
  mutate(term_clean = case_when(
    term == "(Intercept)"                                        ~ "Time:Morning",
    term == "TimeRangeFactorDay"                                 ~ "Time:Day",
    term == "TimeRangeFactorEvening"                             ~ "Time:Evening",
    term == "TimeRangeFactorNight"                               ~ "Time:Night",
    term == "SeasonMonsoon"                                      ~ "Time:Morning (Monsoon shift)",
    term == "QBR_bin"                                            ~ "QBR:Morning",
    term == "Strahler"                                           ~ "Strahler:Morning",
    str_detect(term, "^QBR_bin:TimeRangeFactorDay$")             ~ "QBR:Day",
    str_detect(term, "^QBR_bin:TimeRangeFactorEvening$")         ~ "QBR:Evening",
    str_detect(term, "^QBR_bin:TimeRangeFactorNight$")           ~ "QBR:Night",
    str_detect(term, "^TimeRangeFactorDay:Strahler$")            ~ "Strahler:Day",
    str_detect(term, "^TimeRangeFactorEvening:Strahler$")        ~ "Strahler:Evening",
    str_detect(term, "^TimeRangeFactorNight:Strahler$")          ~ "Strahler:Night",
    str_detect(term, "^TimeRangeFactorDay:SeasonMonsoon$")       ~ "Time:Day (Monsoon shift)",
    str_detect(term, "^TimeRangeFactorEvening:SeasonMonsoon$")   ~ "Time:Evening (Monsoon shift)",
    str_detect(term, "^TimeRangeFactorNight:SeasonMonsoon$")     ~ "Time:Night (Monsoon shift)",
    term == "QBR_bin:SeasonMonsoon"                              ~ "QBR:Morning (Monsoon shift)",
    term %in% c("Strahler:SeasonMonsoon", "SeasonMonsoon:Strahler") ~ "Strahler:Morning (Monsoon shift)",
    str_detect(term, "^QBR_bin:TimeRangeFactor.*:SeasonMonsoon$") ~
      str_replace(term, "QBR_bin:TimeRangeFactor(.*):SeasonMonsoon", "QBR:\\1 (Monsoon shift)"),
    str_detect(term, "^TimeRangeFactor.*:SeasonMonsoon:Strahler$") ~
      str_replace(term, "TimeRangeFactor(.*):SeasonMonsoon:Strahler", "Strahler:\\1 (Monsoon shift)"),
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(term_clean))

# Plot prep -------------------------------------------------------------------

plot_df <- stability_df %>%
  mutate(
    Season = if_else(str_detect(term_clean, "Monsoon shift"), "Monsoon", "Dry"),
    term_clean = str_remove(term_clean, " \\(Monsoon shift\\)")
  )

y_order <- c(
  "Time:Morning", "Time:Day",      "Time:Evening", "Time:Night",
  "QBR:Morning",  "QBR:Day",       "QBR:Evening",  "QBR:Night",
  "Strahler:Morning", "Strahler:Day", "Strahler:Evening", "Strahler:Night"
)

plot_df <- plot_df %>%
  mutate(
    PC_Season = interaction(PC, Season, sep = " "),
    PC_Season = factor(PC_Season,
                       levels = c("PC1 Dry", "PC1 Monsoon", "PC2 Dry", "PC2 Monsoon")),
    term_clean = factor(term_clean, levels = rev(y_order)),
    group = case_when(
      str_detect(term_clean, "^Time:")     ~ "Time",
      str_detect(term_clean, "^QBR:")      ~ "QBR",
      str_detect(term_clean, "^Strahler:") ~ "Strahler"
    ),
    group = factor(group, levels = c("Time", "QBR", "Strahler")),
    
    # Labels: significance stability + direction indicator
    sig_label = case_when(
      prop_significant >= 0.90 ~ paste0(sprintf("%.2f", prop_significant), "*"),
      TRUE                     ~ sprintf("%.2f", prop_significant)
    ),
    dir_label = case_when(
      dominant_direction == "positive" & direction_consistency >= 0.90 ~ "▲",
      dominant_direction == "negative" & direction_consistency >= 0.90 ~ "▼",
      TRUE ~ "~"   # direction not consistent
    ),
    combined_label = paste0(sig_label, " ", dir_label)
  )

# Plot 1: Significance stability heatmap (original) ---------------------------

p_significance <- ggplot(plot_df,
                         aes(x = PC_Season, y = term_clean, fill = prop_significant)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sig_label,
                fontface = ifelse(prop_significant >= 0.90, "bold", "plain")),
            colour = "black", size = 3.8) +
  scale_fill_gradientn(
    colours = c("#d73027", "#FF7134", "#fee08b", "#91cf60", "#1a9850"),
    limits  = c(0.5, 1),
    oob     = scales::squish,
    name    = "Proportion\nSignificant"
  ) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  labs(title = "Significance Stability Across 200 Subsamples",
       x = "", y = "Model Term") +
  theme_bw(base_size = 14) +
  theme(
    panel.grid      = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

ggsave(p_significance,
       file  = "model5_significance_stability.png",
       width = 6, height = 5)

# Plot 2: Directional consistency heatmap -------------------------------------

p_direction <- ggplot(plot_df,
                      aes(x = PC_Season, y = term_clean, fill = direction_consistency)) +
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
  labs(title = "Directional Consistency Across 200 Subsamples",
       subtitle = "▲ = consistently positive  ▼ = consistently negative  ~ = variable",
       x = "", y = "Model Term") +
  theme_bw(base_size = 14) +
  theme(
    panel.grid       = element_blank(),
    strip.placement  = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = "bold"),
    axis.text.y  = element_text(size = 12),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

ggsave(p_direction,
       file  = "model5_direction_stability.png",
       width = 6, height = 5)

# Plot 3: Combined heatmap — significance filled, direction as text -----------

p_combined <- ggplot(plot_df,
                     aes(x = PC_Season, y = term_clean, fill = prop_significant)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = combined_label,
                fontface = ifelse(fully_stable, "bold", "plain")),
            colour = "black", size = 3.5) +
  scale_fill_gradientn(
    colours = c("#d73027", "#FF7134", "#fee08b", "#91cf60", "#1a9850"),
    limits  = c(0.5, 1),
    oob     = scales::squish,
    name    = "Proportion\nSignificant"
  ) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  labs(title = "Significance & Directional Stability Across 200 Subsamples",
       subtitle = "Values show prop. significant | ▲ positive  ▼ negative  ~ variable direction\n* = significant AND directionally stable (both ≥ 0.90)",
       x = "", y = "Model Term") +
  theme_bw(base_size = 14) +
  theme(
    panel.grid       = element_blank(),
    strip.placement  = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = "bold"),
    axis.text.y  = element_text(size = 11),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    plot.subtitle   = element_text(size = 9)
  )
p_combined
ggsave(p_combined,
       file  = "model5_combined_stability.png",
       width = 7, height = 5)

# Export stability table ------------------------------------------------------

stability_export <- stability_df %>%
  select(PC, term_clean, prop_significant, dominant_direction,
         direction_consistency, prop_sig_positive, prop_sig_negative,
         fully_stable, stability_class, mean_estimate, sd_estimate,
         mean_lower, mean_upper) %>%
  arrange(PC, term_clean)

write.csv(stability_export,
          "clean_data/datasets/modelconsistency/model5_stability_summary.csv",
          row.names = FALSE)

message("\nDone. Outputs saved:")
message("  model5_significance_stability.png")
message("  model5_direction_stability.png")
message("  model5_combined_stability.png")
message("  model5_stability_summary.csv")