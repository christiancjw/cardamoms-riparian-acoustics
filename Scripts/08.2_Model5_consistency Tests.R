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
    
    TimeRangeFactor = factor(
      TimeRangeFactor,
      levels = c("Morning","Day","Evening","Night","Other")
    ),
    
    Site = factor(Site),
    Season = factor(Season),
    Device = factor(Device)
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
fit_model_and_extract_ci <- function(dat, response_var) {
  
  formula_obj <- as.formula(
    paste0(response_var,
           " ~ QBR * TimeRangeFactor * Season + ",
           "Strahler * TimeRangeFactor * Season + ",
           "(1 | Site)")
  )
  
  model <- lmer(formula_obj, data = dat, REML = TRUE)
  
  ci <- confint(model, method = "profile", parm = "beta_")
  estimates <- fixef(model)
  
  tibble(
    term = names(estimates),
    estimate = as.numeric(estimates),
    lower = ci[,1],
    upper = ci[,2]
  )
}

# Run Iterations ---------------------------------
set.seed(123)
n_runs <- 50
pcs <- c("PC1","PC2")

all_results <- map_dfr(pcs, function(pc){
  
  message("Running iterations for ", pc)
  
  map_dfr(1:n_runs, function(i){
    
    sampled_data <- sample_one_device_per_site_season(global_ds)
    
    fit_model_and_extract_ci(sampled_data, pc) %>%
      mutate(iteration = i,
             PC = pc)
  })
})

# Save
saveRDS(all_results,
        "clean_data/datasets/modelconsistency/results_allPCs.rds")



# Evaluation of model  --------------------------------------------
resultsall <- readRDS("clean_data/datasets/modelconsistency/results_allPCs.rds")

# Basic Processing
resultsall <- resultsall %>%
  mutate(
    significant = ifelse(lower > 0 | upper < 0, 1, 0),
    sign = sign(estimate)
  )

mean(resultsall$significant)
table(resultsall$sign)

# Quick plot
summary_resultsall <- resultsall %>%
  group_by(term) %>%
  summarise(
    mean_est = mean(estimate),
    lower_mean = mean(lower),
    upper_mean = mean(upper),
    sd_est = sd(estimate)
  )

ggplot(summary_resultsall, aes(x = reorder(term, mean_est), y = mean_est)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_mean, ymax = upper_mean), width = 0) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Average Profile CIs Across 100 Subsamples",
    x = "Model Term",
    y = "Estimate"
  )



# Plot Sign stability
ggplot(sign_stabilityall, aes(x = reorder(term, prop_significant), 
                              y = prop_significant)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Proportion of Subsamples Where Term is Significant (PC1)",
    x = "Model Term",
    y = "Proportion Significant"
  )

# Sign stability per PC / Term
stability_df <- resultsall %>%
  group_by(term, PC) %>%
  summarise(
    prop_significant = mean(significant),
    .groups = "drop"
  )

head(resultsall)

# Rebuild Model Terms
stability_df <- stability_df %>%
  
  mutate(term_clean = case_when(
    # Main Time Effects (Dry ref)
    term == "(Intercept)" ~ "Time:Dawn",
    term == "TimeRangeFactorDay" ~ "Time:Day",
    term == "TimeRangeFactorEvening" ~ "Time:Evening",
    term == "TimeRangeFactorNight" ~ "Time:Night",
    # Main Season Effect
    term == "SeasonMonsoon" ~ "Time:Dawn (Monsoon shift)",
    # Main slopes - since dawn and dry season are references
    term == "QBR" ~ "QBR:Dawn",
    term == "Strahler" ~ "Strahler:Dawn",
    # QBR × TIME (Dry season)
    str_detect(term, "^QBR:TimeRangeFactorDay$") ~ "QBR:Day",
    str_detect(term, "^QBR:TimeRangeFactorEvening$") ~ "QBR:Evening",
    str_detect(term, "^QBR:TimeRangeFactorNight$") ~ "QBR:Night",
    # STRAHLER × TIME (Dry season)
    str_detect(term, "^TimeRangeFactorDay:Strahler$") ~ "Strahler:Day",
    str_detect(term, "^TimeRangeFactorEvening:Strahler$") ~ "Strahler:Evening",
    str_detect(term, "^TimeRangeFactorNight:Strahler$") ~ "Strahler:Night",
    # TIME × SEASON SHIFTS
    str_detect(term, "^TimeRangeFactorDay:SeasonMonsoon$") ~
      "Time:Day (Monsoon shift)",
    str_detect(term, "^TimeRangeFactorEvening:SeasonMonsoon$") ~
      "Time:Evening (Monsoon shift)",
    str_detect(term, "^TimeRangeFactorNight:SeasonMonsoon$") ~
      "Time:Night (Monsoon shift)",
    # QBR × SEASON (Dawn shift)
    term == "QBR:SeasonMonsoon" ~
      "QBR:Dawn (Monsoon shift)",
    # STRAHLER × SEASON (Dawn shift)
    # handles both orders
    term %in% c("Strahler:SeasonMonsoon",
                "SeasonMonsoon:Strahler") ~
      "Strahler:Dawn (Monsoon shift)",
    # 3-WAY: QBR × TIME × SEASON MAn idk
    str_detect(term, "^QBR:TimeRangeFactor.*:SeasonMonsoon$") ~
      str_replace(term,
                  "QBR:TimeRangeFactor(.*):SeasonMonsoon",
                  "QBR:\\1 (Monsoon shift)"),
    # 3-WAY: STRAHLER × TIME × SEASON
    str_detect(term, "^TimeRangeFactor.*:SeasonMonsoon:Strahler$") ~
      str_replace(term,
                  "TimeRangeFactor(.*):SeasonMonsoon:Strahler",
                  "Strahler:\\1 (Monsoon shift)"),
    TRUE ~ NA_character_
  )) %>%
  
  filter(!is.na(term_clean))

# Remake Heatmap
# 1. Extract Season directly from term_clean

plot_df <- stability_df %>%
  mutate(
    Season = if_else(
      str_detect(term_clean, "Monsoon shift"),
      "Monsoon",
      "Dry"              # reference level
    ),
    
    # Remove the shift label from display
    term_clean = str_remove(term_clean, " \\(Monsoon shift\\)")
  )

# 2. Build 4-column PC × Season structure
plot_df <- plot_df %>%
  mutate(
    PC_Season = interaction(PC, Season, sep = " "),
    PC_Season = factor(
      PC_Season,
      levels = c(
        "PC1 Dry",
        "PC1 Monsoon",
        "PC2 Dry",
        "PC2 Monsoon"
      )
    )
  )

# 3. Enforce Correct Term Ordering
y_order <- c(
  # Time
  "Time:Dawn", "Time:Day", "Time:Evening", "Time:Night",
  
  # QBR
  "QBR:Dawn", "QBR:Day", "QBR:Evening", "QBR:Night",
  
  # Strahler
  "Strahler:Dawn", "Strahler:Day",
  "Strahler:Evening", "Strahler:Night"
)

plot_df$term_clean <- factor(
  plot_df$term_clean,
  levels = rev(y_order)
)

# 4. Add significance labels
plot_df <- plot_df %>%
  mutate(
    strong = prop_significant >= 0.9,
    label = ifelse(
      strong,
      paste0(sprintf("%.2f", prop_significant), "*"),
      sprintf("%.2f", prop_significant)
    )
  )

# 5. Create grouping for facet rows

plot_df <- plot_df %>%
  mutate(
    group = case_when(
      str_detect(term_clean, "^Time:") ~ "Time",
      str_detect(term_clean, "^QBR:") ~ "QBR",
      str_detect(term_clean, "^Strahler:") ~ "Strahler"
    )
  )

plot_df$group <- factor(
  plot_df$group,
  levels = c("Time", "QBR", "Strahler")
)

# Plot the bitch
ggplot(plot_df,
       aes(x = PC_Season,
           y = term_clean,
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
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

