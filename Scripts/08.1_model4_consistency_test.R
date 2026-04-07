library(lme4)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

# Setup -----------------------------------------------------------------------

# Read data
global_ds <- read.csv("clean_data/datasets/PCAs/global2325_pca.csv")

# Clean Data Tables - adding values to use for filtering 

# Read data
global_ds <- read.csv("clean_data/datasets/PCAs/global2325_pca.csv")

# Clean Data Tables - adding values to use for filtering 
global_ds <- global_ds %>%
  mutate(
    Deployment = case_when(
      Date >= 20231116 & Date <= 20231203 ~ "Monsoon 2023",
      Date >= 20231230 & Date <= 20240208 ~ "Dry Transition 2024",
      Date >= 20240401 & Date <= 20240501 ~ "Dry 2024",
      Date >= 20240607 & Date <= 20240707 ~ "Monsoon Transition 2024",
      Date >= 20250605 & Date <= 20250716 ~ "Monsoon 2025",
      TRUE ~ NA_character_
    )) %>%
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
  ) %>%  # Create QBR_Bin
  mutate(QBR_bin = case_when(
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
# Function to Subsample Data Randomly
sample_one_device_per_site_deployment <- function(dat, n_days = 10) {
  
  dat <- dat %>%
    mutate(Date = as.Date(as.character(Date), format = "%Y%m%d"))
  
  # 1. Random device per Site × Deployment
  chosen_devices <- dat %>%
    distinct(Site, Deployment, Device) %>%
    group_by(Site, Deployment) %>%
    slice_sample(n = 1) %>%
    ungroup()
  
  dat_subset <- dat %>%
    semi_join(chosen_devices, by = c("Site", "Deployment", "Device"))
  
  # 2. Sample 10-day window
  sampled <- dat_subset %>%
    group_by(Site, Deployment, Device) %>%
    group_modify(~{
      
      available_dates <- sort(unique(.x$Date))
      
      # if fewer than n_days exist, just keep all
      if(length(available_dates) <= n_days){
        return(.x)
      }
      
      valid_starts <- available_dates[
        available_dates <= max(available_dates) - (n_days - 1)
      ]
      
      start_date <- sample(valid_starts, 1)
      end_date <- start_date + (n_days - 1)
      
      .x %>% filter(Date >= start_date & Date <= end_date)
      
    }) %>%
    ungroup()
  
  return(sampled)
}


# Function to fit model & extract CIs (PCx)
fit_model_and_extract_ci <- function(dat, response_var) {
  
  formula_obj <- as.formula(
    paste0(response_var,
           " ~ QBR_bin * TimeRangeFactor + ",
           "Strahler * TimeRangeFactor + ",
           "(1 | Site) + (1 | Season)")
  )
  
  model <- lmer(formula_obj, data = dat, REML = TRUE)
  
  ci <- confint(model, method = "Wald")
  ci <- ci[rownames(ci) %in% names(fixef(model)), ]
  
  tibble(
    term = rownames(ci),
    estimate = fixef(model)[rownames(ci)],
    lower = ci[,1],
    upper = ci[,2]
  )
}

# Run extractions
set.seed(123)
n_runs <- 200
pcs <- c("PC1","PC2")

all_results <- map_dfr(pcs, function(pc){
  
  message("Running iterations for ", pc)
  
  map_dfr(1:n_runs, function(i){
    
    message("Iteration ", i)
    
    sampled_data <- sample_one_device_per_site_deployment(global_ds)
    
    fit_model_and_extract_ci(sampled_data, pc) %>%
      mutate(iteration = i,
             PC = pc)
  })
})


# Save
saveRDS(all_results,
        "clean_data/datasets/modelconsistency/new2model4_results_allPCs.rds")


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


# Evaluation of model  --------------------------------------------
combined_results <- readRDS("clean_data/datasets/modelconsistency/new2model4_results_allPCs.rds")

head(combined_results)

resultsall <- combined_results %>%
  mutate(
    significant = ifelse(lower > 0 | upper < 0, 1, 0),
    sign = sign(estimate)
  )

mean(resultsall$significant)
table(resultsall$sign)

# Sign Stability
stability_df <- resultsall %>%
  group_by(term, PC) %>%
  summarise(
    prop_significant = mean(significant),
    .groups = "drop"
  )

# Clean Terms
stability_df <- stability_df %>%
  
  mutate(term_clean = case_when(
    
    # Time effects
    term == "(Intercept)" ~ "Time:Morning",
    term == "TimeRangeFactorDay" ~ "Time:Day",
    term == "TimeRangeFactorEvening" ~ "Time:Evening",
    term == "TimeRangeFactorNight" ~ "Time:Night",
    
    # Main slopes
    term == "QBR_bin" ~ "QBR:Morning",
    term == "Strahler" ~ "Strahler:Morning",
    
    # QBR × TIME
    term == "QBR_bin:TimeRangeFactorDay" ~ "QBR:Day",
    term == "QBR_bin:TimeRangeFactorEvening" ~ "QBR:Evening",
    term == "QBR_bin:TimeRangeFactorNight" ~ "QBR:Night",
    
    # STRAHLER × TIME
    term == "TimeRangeFactorDay:Strahler" ~ "Strahler:Day",
    term == "TimeRangeFactorEvening:Strahler" ~ "Strahler:Evening",
    term == "TimeRangeFactorNight:Strahler" ~ "Strahler:Night",
    
    TRUE ~ NA_character_
  )) %>%
  
  filter(!is.na(term_clean)) 

# Clean data
# Extract group and clean term labels for plotting
plot_df <- stability_df %>%
  mutate(
    # Extract group for faceting
    group = str_extract(term_clean, "^[^:]+"),
    
    # Remove prefix for plotting
    term_clean = str_remove(term_clean, "^[^:]+:")
  )

# Ensure group is a factor for facet order
plot_df$group <- factor(
  plot_df$group,
  levels = c("Time", "QBR", "Strahler")
)

# Re-order y-axis by time labels (Morning / Day / Evening / Night)
y_order <- c("Morning", "Day", "Evening", "Night")  # includes Dawn for reference
plot_df$term_clean <- factor(
  plot_df$term_clean,
  levels = rev(y_order)
)


# Significance Labels
plot_df <- plot_df %>%
  mutate(
    strong = prop_significant >= 0.9,
    label = ifelse(
      strong,
      paste0(sprintf("%.2f", prop_significant), "*"),
      sprintf("%.2f", prop_significant)
    )
  )

# Row grouping
plot_df$group <- factor(
  plot_df$group,
  levels = c("Time", "QBR", "Strahler")
)

# Plot it
model4consistency <- ggplot(plot_df,
       aes(x = PC,
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
    axis.text.x = element_text(angle = 0),
    legend.position = "right"
  )

ggsave(model4consistency, file="model4consistency.png", width=4, height=4)
