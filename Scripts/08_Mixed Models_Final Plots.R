# Setup ------------------

# Load libraries
library(tidyverse) # for data manipulation and plotting
library(lme4) # for mixed models
library(broom.mixed) # for looking at mixed model outputs
library(car) # for VIF (higher than 5 or 10 is bad)
library(lubridate) # for date/time manipulations
library(ggfortify) # for model diagnostic plots
library(MuMIn) # for r2 values GLMM
library(purrr) # For diel analysis
library(patchwork) # For putting plots together
library(emmeans) # For plotting true effect sizes

# Read in Data
global_ds <- read.csv("clean_data/datasets/PCAs/rainless_single_pca.csv")

# Check Data
head(global_ds)
str(global_ds)

# Set Colour Ordering

# QBR colours
qbr_colors <- c(
  "Natural (95–100)" = "#006BA6",
  "Good (75–90)"     = "#22A122",
  "Fair (55–70)"     = "#DBCB43",
  "Poor (30–50)"     = "#FF7134",
  "Bad (<25)"        = "#AF3245"
)

qbr_colors <- c(
  "Natural" = "#006BA6",
  "Good"     = "#22A122",
  "Fair"     = "#DBCB43",
  "Poor"     = "#FF7134",
  "Bad"        = "#AF3245"
)

# Strahler colours
strahler_colors <- c(
  "1" = "#006BA6",
  "2"   = "#22A122",
  "3"  = "#DBCB43",
  "4" = "#FF7134",
  "5" = "#AF3245"
)



strahler_class_colors <- c(
  "1st Order" = "#006BA6",
  "2nd Order" = "#22A122",
  "3rd Order" = "#DBCB43",
  "4th Order" = "#FF7134",
  "5th Order" = "#AF3245"
)

strahler_class_colors <- c(
  "1st Order" = "#266489",
  "2nd Order" = "#68B9C0",
  "3rd Order" = "#90D585",
  "4th Order" = "#F3C151",
  "5th Order" = "#F37F64"
)

strahler_colors <- c(
  "1" = "#266489",
  "2" = "#68B9C0",
  "3" = "#90D585",
  "4" = "#F3C151",
  "5" = "#F37F64"
)

qbr_order <- c("Natural (95–100)", "Good (75–90)", "Fair (55–70)", "Poor (30–50)", "Bad (<25)")

qbr_order <- c("Natural", "Good", "Fair", "Poor", "Bad")

strahler_order <- c("1st Order", "2nd Order", "3rd Order", "4th Order", "5th Order")



# Data Setup  ----------------------------------

# Add Seasonal and Time Range variables to the dataset

global_ds <- global_ds %>%
  mutate(
    Deployment = case_when(
      Date >= 20231116 & Date <= 20231203 ~ "Monsoon 2023",
      Date >= 20231230 & Date <= 20240208 ~ "Dry Transition 2024",
      Date >= 20240401 & Date <= 20240501 ~ "Dry 2024",
      Date >= 20240607 & Date <= 20240707 ~ "Monsoon Transition 2024",
      Date >= 20250605 & Date <= 20250716 ~ "Monsoon 2025",
      TRUE ~ NA_character_
    ))

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
  filter(!is.na(Season))

# Create a TimeRange factor
global_ds <- global_ds %>%
  mutate(Time_num = as.numeric(Time)) %>% 
  # Convert numeric time into HHMMSS string with 6 digits
  mutate(TimeHHMMSS = str_pad(Time_num, width = 6, side = "left", pad = "0")) %>%
  mutate(TimeRangeFactor = case_when(
    TimeHHMMSS >= "040001" & TimeHHMMSS <= "100000" ~ "Morning",
    TimeHHMMSS >= "100001" & TimeHHMMSS <= "160000" ~ "Day",
    TimeHHMMSS >= "160001" & TimeHHMMSS <= "220000" ~ "Evening",
    TimeHHMMSS >= "220001" | TimeHHMMSS <= "040000" ~ "Night",
    TRUE ~ "Other"
  )) %>% 
  mutate(TimeRangeFactor = factor(TimeRangeFactor, levels = c("Morning","Day","Evening","Night"))) %>%  # Create QBR_Bin
  mutate(QBR_bin = case_when(
    QBR >= 95 & QBR <= 100 ~ 1,            # Natural
    QBR >= 75 & QBR < 95 ~ 2,              # Good
    QBR >= 55 & QBR < 75 ~ 3,              # Fair
    QBR >= 30 & QBR < 55 ~ 4,              # Poor
    QBR < 30 ~ 5,                           # Bad
    TRUE ~ NA_real_
  )
  )

head(global_ds)

# For plotting - rename strahler to strahler order
global_ds <- global_ds %>%
  rename(Strahler_Order = Strahler) %>%   # rename column
  mutate(
    QBR_Class = case_when(
      QBR_Class == "Natural (95–100)" ~ "Natural",
      QBR_Class == "Good (75–90)" ~ "Good",
      QBR_Class == "Fair (55–70)" ~ "Fair",
      QBR_Class == "Poor (30–50)" ~ "Poor",
      QBR_Class == "Bad (<25)" ~ "Bad",
      TRUE ~ QBR_Class
    )
  )

# Fiddle with dates and times
global_ds <-
  global_ds %>%
  # convert Date to a date
  mutate(date = ymd(Date)) %>%
  # extract month
  mutate(month = month(date)) %>%
  # extract year
  mutate(year = year(date)) %>%
  # Convert time to HHMMSS
  mutate(Time = str_pad(Time, width = 6, side = "left", pad = "0"))


# Show data structure 
ggplot(global_ds,
       aes(x = QBR, y = Strahler, colour = Site)) +
  geom_point() +
  theme_minimal()




# Full Scatter Plot  ------

# Ensure Strahler is a factor for grouping
global_ds <- global_ds %>%
  mutate(Strahler_f = factor(Strahler))

#
ggplot(global_ds,
       aes(x = PC1, y = PC2, color = Strahler_Class)) +
  # Raw points
  geom_point(alpha = 0.3, size = 0.05) +
  
  # Ellipses (95% confidence)
  stat_ellipse(level = 0.95, linewidth = 0.8) +
  
  ylim(-10, 6) +
  xlim(-8, 4) +
  
  theme_minimal() +
  labs(
    x = "Compound Index 1 (37.9% Variance)",
    y = "Compound Index 2 (14.8% Variance)",
    color = "Strahler Order",
    title = "Acoustic Structure Across Stream Order"
  )


# Time based scatters --------

# Bin
global_ds <- global_ds %>%
  mutate(
    TimeHHMMSS = sprintf("%06s", TimeHHMMSS),  # ensure 6 digits
    TimeBin = case_when(
      TimeHHMMSS >= "040001" & TimeHHMMSS <= "100000" ~ "Morning 04:00 - 10:00",
      TimeHHMMSS >= "100001" & TimeHHMMSS <= "160000" ~ "Day 10:00 - 16:00",
      TimeHHMMSS >= "160001" & TimeHHMMSS <= "220000" ~ "Evening 16:00 - 22:00",
      TimeHHMMSS >= "220001" | TimeHHMMSS <= "040000" ~ "Night 22:00 - 04:00"
    ),
    TimeBin = factor(TimeBin,
                     levels = c("Morning 04:00 - 10:00", 
                                "Day 10:00 - 16:00",
                                "Evening 16:00 - 22:00",
                                "Night 22:00 - 04:00"))
  )


# Plot

# Strahler Colours
ggplot(global_ds,
       aes(x = PC1, y = PC2, color = Strahler_f)) +
  geom_point(alpha = 0.3, size = 0.5) +
  stat_ellipse(level = 0.95, linewidth = 0.8) +
  facet_wrap(~ TimeBin, ncol = 2) +
  coord_cartesian(xlim = c(-8, 4),
                  ylim = c(-10, 6)) +
  scale_color_manual(values = strahler_colors) +
  theme_minimal() +
  labs(
    x = "Compound Index 1 (37.9% Variance)",
    y = "Compound Index 2 (14.8% Variance)",
    color = "Strahler Order"
  )


#QBR Colour 
ggplot(global_ds,
       aes(x = PC1, y = PC2, color = QBR_Class)) +
  geom_point(alpha = 0.3, size = 0.05) +
  stat_ellipse(level = 0.95, linewidth = 0.8) +
  facet_wrap(~ TimeBin, ncol = 2) +
  coord_cartesian(xlim = c(-8, 4),
                  ylim = c(-10, 6)) +
  scale_color_manual(values = qbr_colors) +
  theme_minimal() +
  labs(
    x = "Compound Index 1 (37.9% Variance)",
    y = "Compound Index 2 (14.8% Variance)",
    color = "Strahler Order",
    title = "Acoustic Structure Across Stream Order by Time of Day"
  )

# Seasonal Scatter ------
global_ds <- global_ds %>%
  mutate(
    Season = factor(Season, levels = c("Dry", "Monsoon")),
    Strahler_f = factor(Strahler)
  )

# Function to plot
plot_seasonal_scatter <- function(df, colour_var, season_filter) {
  
  df_season <- df %>% filter(Season == season_filter)
  
  ggplot(df_season,
         aes(x = PC1, y = PC2, color = .data[[colour_var]])) +
    
    geom_point(alpha = 0.3, size = 0.4) +
    
    stat_ellipse(level = 0.95, linewidth = 0.8) +
    
    coord_cartesian(
      xlim = c(-8, 4),
      ylim = c(-10, 6)
    ) +
    
    scale_color_manual(values = palettes[[colour_var]]) +
    
    theme_classic() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank()
    ) +
    
    labs(
      x = "Compound Index 1 (37.9% Variance)",
      y = "Compound Index 2 (14.8% Variance)",
      title = paste(season_filter, "Season")
    )
}


# Ensure Check Palettes
palettes <- list(
  Strahler_Class = strahler_class_colors,
  QBR_Class = qbr_colors
)

p_Strahler_Dry <- plot_seasonal_scatter(global_ds, "Strahler_Class", "Dry")
p_Strahler_Monsoon <- plot_seasonal_scatter(global_ds, "Strahler_Class", "Monsoon")

p_QBR_Dry <- plot_seasonal_scatter(global_ds, "QBR_Class", "Dry")
p_QBR_Monsoon <- plot_seasonal_scatter(global_ds, "QBR_Class", "Monsoon")

Strahler_seasonal <- p_Strahler_Dry + p_Strahler_Monsoon +
  plot_layout(ncol = 2) +
  plot_annotation(title = "Acoustic Structure Across Stream Order – Seasonal Variation")

Strahler_seasonal

QBR_seasonal <- p_QBR_Dry + p_QBR_Monsoon +
  plot_layout(ncol = 2) +
  plot_annotation(title = "Acoustic Structure Across Riparian Condition – Seasonal Variation")

QBR_seasonal


# Seasonal Scatter (Split by Times) ----------------
global_ds <- global_ds %>%
  mutate(
    Season = factor(Season, levels = c("Dry", "Monsoon")),
    TimeRangeFactor = factor(TimeRangeFactor,
                             levels = c("Morning", "Day", "Evening", "Night")),
    Strahler_f = factor(Strahler)
  )

# Function
plot_season_time_scatter <- function(df, colour_var, season_filter) {
  
  df_season <- df %>% 
    filter(Season == season_filter)
  
  ggplot(df_season,
         aes(x = PC1, y = PC2, color = .data[[colour_var]])) +
    
    geom_point(alpha = 0.3, size = 0.4) +
    
    stat_ellipse(level = 0.95, linewidth = 0.8) +
    
    facet_wrap(~ TimeRangeFactor, ncol = 2) +
    
    coord_cartesian(
      xlim = c(-8, 4),
      ylim = c(-10, 6)
    ) +
    
    scale_color_manual(values = palettes[[colour_var]]) +
    
    theme_classic() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      strip.text = element_text(face = "bold")
    ) +
    
    labs(
      x = "Compound Index 1 (37.9% Variance)",
      y = "Compound Index 2 (14.8% Variance)",
      title = paste(season_filter, "Season")
    )
}

Strahler_Dry_Time <- plot_season_time_scatter(global_ds,
                                              "Strahler_Class",
                                              "Dry")

Strahler_Monsoon_Time <- plot_season_time_scatter(global_ds,
                                                  "Strahler_Class",
                                                  "Monsoon")

QBR_Dry_Time <- plot_season_time_scatter(global_ds,
                                         "QBR_Class",
                                         "Dry")

QBR_Monsoon_Time <- plot_season_time_scatter(global_ds,
                                             "QBR_Class",
                                             "Monsoon")

Strahler_Dry_Time
Strahler_Monsoon_Time

QBR_Dry_Time
QBR_Monsoon_Time

# Model 4 (Season as Random) ---------------------------

# PC1 + Explorations
model4_PC1 <- lmer(PC1 ~ QBR_bin * TimeRangeFactor +
                     Strahler * TimeRangeFactor +
                     (1 | Site)
                   + (1 | Season), 
                   data = global_ds)

# Check variance 
r.squaredGLMM(model4_PC1)

# Check VIF
vif(model4_PC1) # higher than 5 or 10 is bad

# Show Fixed EFfect Values for Model 
broom.mixed::tidy(model4_PC1, effects = "fixed")
# Show Random Effect Values for model
broom.mixed::tidy(model4_PC1, effects = "ran_pars")

# Confidence Int ervals PC1
confint(model4_PC1, method="Wald")
profconfintPC1 <- confint(model4_PC1, method="profile")
bootconfintPC1 <- confint(model4_PC1, method="boot")

profconfintPC1
# R2 Values
model4_r2_vals_PC1 <- r.squaredGLMM(model4_PC1)
model4_r2_vals_PC1

coef(model4_PC1)  # gives raw fixed effects

# PC2 + Explorations
model4_PC2 <- lmer(PC2 ~ QBR_bin * TimeRangeFactor +
                     Strahler * TimeRangeFactor +
                     (1 | Site)
                   + (1 | Season), 
                   data = global_ds)

# Check variance 
r.squaredGLMM(model4_PC2)

# Check VIF
vif(model4_PC2) # higher than 5 or 10 is bad

# Show Fixed EFfect Values for Model 
broom.mixed::tidy(model4_PC2, effects = "fixed")
# Show Random Effect Values for model
broom.mixed::tidy(model4_PC2, effects = "ran_pars")

# Confidence Int ervals PC2
confint(model4_PC2, method="Wald")
profconfintPC2 <- confint(model4_PC2, method="profile")
bootconfintPC2 <- confint(model4_PC2, method="boot")

profconfintPC2
# R2 Values
model4_r2_vals_PC2 <- r.squaredGLMM(model4_PC2)
model4_r2_vals_PC2

coef(model4_PC2)  # gives raw fixed effects

# Model 5 ----------

# PC1 + Explorations
model5_PC1 <- lmer(PC1 ~ QBR * TimeRangeFactor * Season +
                     Strahler * TimeRangeFactor * Season +
                     (1 | Site), 
                   data = global_ds)

# Check variance 
r.squaredGLMM(model5_PC1)

# Check VIF
vif(model5_PC1) # higher than 5 or 10 is bad

# Show Fixed EFfect Values for Model 
broom.mixed::tidy(model5_PC1, effects = "fixed")
# Show Random Effect Values for model
broom.mixed::tidy(model5_PC1, effects = "ran_pars")

# Confidence Int ervals PC1
confint(model5_PC1, method="Wald")
profconfintPC1 <- confint(model5_PC1, method="profile")
bootconfintPC1 <- confint(model5_PC1, method="boot")

profconfintPC1
# R2 Values
model5_r2_vals_PC1 <- r.squaredGLMM(model5_PC1)
model5_r2_vals_PC1

coef(model5_PC1)  # gives raw fixed effects





# Fit PC2 model (same structure
model5_PC2 <- lmer(PC2 ~ QBR * TimeRangeFactor * Season +
                     Strahler * TimeRangeFactor * Season +
                     (1 | Site), 
                   data = global_ds)

# Check Variance 
r.squaredGLMM(model5_PC2)

# Check VIF
vif(model5_PC2) # higher than 5 or 10 is bad

# Show Fixed EFfect Values for Model 
broom.mixed::tidy(model5_PC2, effects = "fixed")
# Show Random Effect Values for model
broom.mixed::tidy(model5_PC2, effects = "ran_pars")

# Confidence Int ervals PC2
confint(model5_PC2, method="Wald")
profconfintPC2 <- confint(model5_PC2, method="profile")
bootconfintPC2 <- confint(model5_PC2, method="boot")

# R2 Values
model5_r2_vals_PC2 <- r.squaredGLMM(model5_PC2)
model5_r2_vals_PC2

coef(model5_PC2)  # gives raw fixed effects

# Model 4 fixed effects with CI (wald) -------------
extract_coefs <- function(model, pc_label){
  
  broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      PC = pc_label,
      term_clean = str_replace_all(term, "TimeRangeFactor", ""),
      term_clean = str_replace_all(term_clean, ":", " × ")
    )
}

coef_PC1 <- extract_coefs(model4_PC1, "PC1")
coef_PC2 <- extract_coefs(model4_PC2, "PC2")

coef_df <- bind_rows(coef_PC1, coef_PC2)

# Forest plot (wald)
ggplot(coef_df,
       aes(x = estimate,
           y = reorder(term_clean, estimate),
           color = PC)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2) +
  facet_wrap(~PC, scales = "free_x") +
  theme_minimal() +
  labs(x = "Effect size (β)",
       y = "",
       title = "Fixed Effects on Acoustic Structure (PC1 & PC2)")


# Model 4 fixed effects with CI (boot) ---------------------------
bootconfintPC1 <- confint(model4_PC1, method="boot")
bootconfintPC2 <- confint(model4_PC2, method="boot")


# Convert Boot to Tidy Data
boot_PC1 <- bootconfintPC1 %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  rename(
    conf.low  = `2.5 %`,
    conf.high = `97.5 %`
  ) %>%
  filter(!str_detect(term, "^\\."))

boot_PC2 <- bootconfintPC2 %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  rename(
    conf.low  = `2.5 %`,
    conf.high = `97.5 %`
  ) %>%
  filter(!str_detect(term, "^\\."))


# Extract model coefficients
coef_PC1 <- broom.mixed::tidy(model4_PC1, effects = "fixed") %>%
  select(term, estimate) %>%
  left_join(boot_PC1, by = "term") %>%
  mutate(PC = "PC1")

coef_PC2 <- broom.mixed::tidy(model4_PC2, effects = "fixed") %>%
  select(term, estimate) %>%
  left_join(boot_PC2, by = "term") %>%
  mutate(PC = "PC2")

# Combine PCs 
coef_df <- bind_rows(coef_PC1, coef_PC2)

# Clean Terms
coef_df <- coef_df %>%
  mutate(
    term_clean = case_when(
      
      # QBR slopes
      term == "QBR" ~ "Dawn × QBR",
      term == "QBR:TimeRangeFactorDay" ~ "Day × QBR",
      term == "QBR:TimeRangeFactorEvening" ~ "Evening × QBR",
      term == "QBR:TimeRangeFactorNight" ~ "Night × QBR",
      
      # Strahler slopes
      term == "Strahler" ~ "Dawn × Strahler",
      term == "TimeRangeFactorDay:Strahler" ~ "Day × Strahler",
      term == "TimeRangeFactorEvening:Strahler" ~ "Evening × Strahler",
      term == "TimeRangeFactorNight:Strahler" ~ "Night × Strahler",
      
      # Time contrasts
      term == "(Intercept)" ~ "Dawn",
      term == "TimeRangeFactorDay" ~ "Day",
      term == "TimeRangeFactorEvening" ~ "Evening",
      term == "TimeRangeFactorNight" ~ "Night",
      
      TRUE ~ term
    )
  )

# Separate the QBR and Strahler Terms
coef_df <- coef_df %>%
  mutate(
    effect_group = case_when(
      str_detect(term, "^QBR") ~ "QBR",
      str_detect(term, "Strahler") ~ "Strahler",
      TRUE ~ "Time"
    )
  )

# Filter Datasets
coef_qbr <- coef_df %>%
  filter(effect_group == "QBR") %>%
  mutate(
    term_clean = if_else(
      effect_group == "QBR",
      str_remove(term_clean, " × QBR"),
      term_clean
    )
  )

coef_stra <- coef_df %>%
  filter(effect_group == "Strahler") %>%
  mutate(
    term_clean = if_else(
      effect_group == "Strahler",
      str_remove(term_clean, " × Strahler"),
      term_clean
    )
  )

# Model 4 True Slopes (emmeans) --------------------------------

# Strahler slopes by time
stra_slopes_PC1 <- emtrends(model4_PC1,
                            ~ TimeRangeFactor,
                            var = "Strahler")

stra_slopes_PC2 <- emtrends(model4_PC2,
                            ~ TimeRangeFactor,
                            var = "Strahler")

# QBR slopes by time
qbr_slopes_PC1 <- emtrends(model4_PC1,
                           ~ TimeRangeFactor,
                           var = "QBR_bin")

qbr_slopes_PC2 <- emtrends(model4_PC2,
                           ~ TimeRangeFactor,
                           var = "QBR_bin")

# Convert to dataframes
stra_PC1_df <- as.data.frame(stra_slopes_PC1) %>%
  mutate(PC = "PC1", Predictor = "Strahler")

stra_PC2_df <- as.data.frame(stra_slopes_PC2) %>%
  mutate(PC = "PC2", Predictor = "Strahler")

qbr_PC1_df <- as.data.frame(qbr_slopes_PC1) %>%
  mutate(PC = "PC1", Predictor = "QBR_bin")

qbr_PC2_df <- as.data.frame(qbr_slopes_PC2) %>%
  mutate(PC = "PC2", Predictor = "QBR_bin")

# Combine Slopes
slopes_df <- bind_rows(
  stra_PC1_df,
  stra_PC2_df,
  qbr_PC1_df,
  qbr_PC2_df
)

# Ploting helper: set the timings

# Convert TimeRangeFactor to POSIXct for plotting
slopes_df <- slopes_df %>%
  mutate(
    # Assign times for alignment with diel plots
    Time_num = case_when(
      TimeRangeFactor == "Night"   ~ hms("04:00:00"),
      TimeRangeFactor == "Morning" ~ hms("10:00:00"),
      TimeRangeFactor == "Day"     ~ hms("16:00:00"),
      TimeRangeFactor == "Evening" ~ hms("22:00:00")
    ),
    # Convert to POSIXct for ggplot
    Time_POSIX = as.POSIXct(Time_num, origin = "1970-01-01", tz = "UTC"),
    # Keep a separate column for x-axis labels
    Time_label = TimeRangeFactor
  )

# Plot PC1 QBR Slope
pc1_qbr <- slopes_df %>%
  filter(PC == "PC1", Predictor == "QBR_bin") %>%
  mutate(TimeRangeFactor = factor(TimeRangeFactor, levels = c("Night", "Morning","Day","Evening")))

effectplot_QBR_PC1 <- ggplot(pc1_qbr, aes(x = TimeRangeFactor, y = QBR_bin.trend)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  theme_classic()  +
  labs(title = "QBR", x = element_blank(), y = "Model 4 Slope") +
  ylim(-0.75, 0.55) +
  theme(
    axis.title.x = element_blank(),   # remove x-axis title
    axis.text.x = element_blank(),    # remove x-axis labels
    axis.ticks.x = element_blank(),    # remove x-axis ticks
    axis.line.x = element_blank()     # remove x-axis line
  )

# Plot PC1 Strahler Slope
pc1_stra <- slopes_df %>%
  filter(PC == "PC1", Predictor == "Strahler") %>%
  mutate(TimeRangeFactor = factor(TimeRangeFactor, levels = c("Night", "Morning","Day","Evening")))

effectplot_strahler_PC1 <- ggplot(pc1_stra, aes(x = TimeRangeFactor, y = Strahler.trend)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  theme_classic() +
  labs(title = "Strahler Order", x = element_blank(), y = NULL) +
  ylim(-0.75, 0.55) +
  theme(
    axis.title.x = element_blank(),   # remove x-axis title
    axis.text.x = element_blank(),    # remove x-axis labels
    axis.ticks.x = element_blank(),    # remove x-axis ticks
    axis.line.x = element_blank()     # remove x-axis line
  )

# Plot PC2 QBR Slope
pc1_qbr <- slopes_df %>%
  filter(PC == "PC2", Predictor == "QBR_bin") %>%
  mutate(TimeRangeFactor = factor(TimeRangeFactor, levels = c("Night", "Morning","Day","Evening")))

effectplot_QBR_PC2 <- ggplot(pc1_qbr, aes(x = TimeRangeFactor, y = QBR_bin.trend)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  theme_classic() +
  labs(title = "QBR", x = element_blank(), y = "Model 4 Slope") +
  theme(
    axis.title.x = element_blank(),   # remove x-axis title
    axis.text.x = element_blank(),    # remove x-axis labels
    axis.ticks.x = element_blank(),    # remove x-axis ticks
    axis.line.x = element_blank()     # remove x-axis line
  ) + ylim (-0.6, 0.6)


# Plot PC2 Strahler Slope
pc1_stra <- slopes_df %>%
  filter(PC == "PC2", Predictor == "Strahler") %>%
  mutate(TimeRangeFactor = factor(TimeRangeFactor, levels = c("Night", "Morning","Day","Evening")))

effectplot_strahler_PC2 <- ggplot(pc1_stra, aes(x = TimeRangeFactor, y = Strahler.trend)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  theme_classic() +
  labs(title = "Strahler Order", x = element_blank(), y = NULL) +
  theme(
    axis.title.x = element_blank(),   # remove x-axis title
    axis.text.x = element_blank(),    # remove x-axis labels
    axis.ticks.x = element_blank(),    # remove x-axis ticks
    axis.line.x = element_blank()     # remove x-axis line
  ) + ylim (-0.6, 0.6)








# Model 4 Diel Plot  -------------------------------
# Load data
plotting_ds <- read.csv("clean_data/datasets/PCAs/rainless_single_pca.csv")

plotting_ds <- plotting_ds %>%
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
  filter(!is.na(Season))



# For plotting - rename strahler to strahler order
plotting_ds <- plotting_ds %>%
  rename(Strahler_Order = Strahler) %>%   # rename column
  mutate(
    QBR_Class = case_when(
      QBR_Class == "Natural (95–100)" ~ "Natural",
      QBR_Class == "Good (75–90)" ~ "Good",
      QBR_Class == "Fair (55–70)" ~ "Fair",
      QBR_Class == "Poor (30–50)" ~ "Poor",
      QBR_Class == "Bad (<25)" ~ "Bad",
      TRUE ~ QBR_Class
    )
  )

# --- Reference date and Time Bins ---
ref_date <- make_date(2000,1,1)
time_bins <- tibble::tibble(
  label = c("Morning", "Day", "Evening", "Night"),
  start = c("04:00:01", "10:00:01", "16:00:01", "22:00:01"),
  end   = c("10:00:00", "16:00:00", "22:00:00", "04:00:00")
) %>%
  mutate(
    start_POSIX = make_datetime(year = year(ref_date), month = month(ref_date), day = day(ref_date),
                                hour = as.numeric(substr(start,1,2)),
                                min  = as.numeric(substr(start,4,5)),
                                sec  = as.numeric(substr(start,7,8))),
    end_POSIX = make_datetime(year = year(ref_date), month = month(ref_date), day = day(ref_date),
                              hour = as.numeric(substr(end,1,2)),
                              min  = as.numeric(substr(end,4,5)),
                              sec  = as.numeric(substr(end,7,8)))
  )

# --- Preprocess data ---
plotting_ds <- plotting_ds %>%
  mutate(
    Time_str = sprintf("%06d", Time),
    Hour     = as.numeric(substr(Time_str,1,2)),
    Minute   = as.numeric(substr(Time_str,3,4)),
    Time_POSIX = make_datetime(year = 2000, month = 1, day = 1, hour = Hour, min = Minute),
    Time_bin = floor_date(Time_POSIX, "30 minutes")
  )

# --- Summarise: mean + SE + 95% CI for each PC and grouping ---
summarise_diel <- function(df, pc, group_var){
  if(group_var == "QBR_Class"){
    df[[group_var]] <- factor(df[[group_var]],
                              levels = c("Natural", 
                                         "Good", 
                                         "Fair", 
                                         "Poor", 
                                         "Bad"))
  }
  
  df %>%
    group_by(Time_bin, .data[[group_var]]) %>%
    summarise(
      mean_val = mean(.data[[pc]], na.rm = TRUE),
      n = sum(!is.na(.data[[pc]])),
      se_val = sd(.data[[pc]], na.rm = TRUE)/sqrt(n),
      ci_val = 1.96*se_val,
      .groups = "drop"
    )
}

# Summarise each dataset
diel_QBR_PC1_df <- summarise_diel(plotting_ds, "PC1", "QBR_Class")
diel_QBR_PC2_df <- summarise_diel(plotting_ds, "PC2", "QBR_Class")
diel_Strahler_PC1_df <- summarise_diel(plotting_ds, "PC1", "Strahler_Order")
diel_Strahler_PC2_df <- summarise_diel(plotting_ds, "PC2", "Strahler_Order")

# Make plotting break times
# Combine all Time_bin columns to get the global min/max
all_times <- c(
  diel_QBR_PC1_df$Time_bin,
  diel_QBR_PC2_df$Time_bin,
  diel_Strahler_PC1_df$Time_bin,
  diel_Strahler_PC2_df$Time_bin
)

# Make consistent break times at 04:00, 08:00, 12:00, etc.
break_times <- seq(
  from = floor_date(min(all_times), unit = "day") + hours(4),  # start at 04:00
  to   = floor_date(max(all_times), unit = "day") + hours(22), # end at 22:00
  by   = "6 hours"                                              # 6-hour intervals
)

# Define palettes
palettes <- list(
  QBR_Class = qbr_colors,
  Strahler_Order = strahler_colors
)


# Plotting:

# --- Create four independent ggplots ---

# PC1 QBR 
p_QBR_PC1 <- ggplot(diel_QBR_PC1_df, aes(x = Time_bin, y = mean_val, color = QBR_Class)) +
  geom_ribbon(aes(ymin = mean_val - ci_val, ymax = mean_val + ci_val, fill = QBR_Class),
              alpha = 0.2, colour = NA) + 
  geom_smooth(aes(group = QBR_Class), method = "loess", span = 0.2, se = FALSE, linewidth = 1) +
  scale_color_manual(values = palettes$QBR_Class) +
  scale_fill_manual(values = palettes$QBR_Class, guide = "none") +
  geom_vline(xintercept = as.numeric(time_bins$start_POSIX), linetype = "dotted") +
  geom_vline(xintercept = as.numeric(time_bins$end_POSIX), linetype = "dotted") +
  labs(x = "Time", y = "Compound Index 1", color = NULL, caption = "QBR Class") + 
  ylim(-4, 1.7) +
  scale_x_datetime(date_labels = "%H:%M", breaks = break_times) +
  theme_classic() +
  theme(legend.position = "bottom", legend.direction = "horizontal", plot.caption = element_text(hjust = 0.5))

p_QBR_PC1


# PC1 Strahller 
diel_Strahler_PC1_df <- diel_Strahler_PC1_df %>%
  mutate(Strahler_Order = factor(Strahler_Order))


p_Strahler_PC1 <- ggplot(diel_Strahler_PC1_df, aes(x = Time_bin, y = mean_val, color = Strahler_Order)) +
  geom_ribbon(aes(ymin = mean_val - ci_val, ymax = mean_val + ci_val, fill = Strahler_Order),
              alpha = 0.2, colour = NA) +
  geom_smooth(aes(group = Strahler_Order), method = "loess", span = 0.2, se = FALSE, linewidth = 1) +
  scale_color_manual(values = palettes$Strahler_Order) +
  scale_fill_manual(values = palettes$Strahler_Order, guide = "none") +
  geom_vline(xintercept = as.numeric(time_bins$start_POSIX), linetype = "dotted") +
  geom_vline(xintercept = as.numeric(time_bins$end_POSIX), linetype = "dotted") +
  labs(x = "Time", y = NULL, color = NULL, caption = "Strahler Order") + 
  ylim(-4, 1.7) +
  scale_x_datetime(date_labels = "%H:%M", breaks = break_times) +
  theme_classic() +
  theme(legend.position = "bottom", legend.direction = "horizontal", plot.caption = element_text(hjust = 0.5))

p_Strahler_PC1

# PC2 QBR 
p_QBR_PC2 <- ggplot(diel_QBR_PC2_df, aes(x = Time_bin, y = mean_val, color = QBR_Class)) +
  geom_ribbon(aes(ymin = mean_val - ci_val, ymax = mean_val + ci_val, fill = QBR_Class),
              alpha = 0.2, colour = NA) +
  geom_smooth(aes(group = QBR_Class), method = "loess", span = 0.2, se = FALSE, linewidth = 1) +
  scale_color_manual(values = palettes$QBR_Class) +
  scale_fill_manual(values = palettes$QBR_Class, guide = "none") +
  geom_vline(xintercept = as.numeric(time_bins$start_POSIX), linetype = "dotted") +
  geom_vline(xintercept = as.numeric(time_bins$end_POSIX), linetype = "dotted") +
  ylim(-1, 2.3) +
  labs(x = "Time" , y = "Compound Index 2", color = NULL, caption = "QBR Class") + 
  scale_x_datetime(date_labels = "%H:%M", breaks = break_times) +
  theme_classic() +
  theme(legend.position = "bottom", legend.direction = "horizontal", plot.caption = element_text(hjust = 0.5))


# PC2 Strahler
diel_Strahler_PC2_df <- diel_Strahler_PC2_df %>%
  mutate(Strahler_Order = factor(Strahler_Order))

p_Strahler_PC2 <- ggplot(diel_Strahler_PC2_df, aes(x = Time_bin, y = mean_val, color = Strahler_Order)) +
  geom_ribbon(aes(ymin = mean_val - ci_val, ymax = mean_val + ci_val, fill = Strahler_Order),
              alpha = 0.2, colour = NA) +
  geom_smooth(aes(group = Strahler_Order), method = "loess", span = 0.2, se = FALSE, linewidth = 1) +
  scale_color_manual(values = palettes$Strahler_Order) +
  scale_fill_manual(values = palettes$Strahler_Order, guide = "none") +
  geom_vline(xintercept = as.numeric(time_bins$start_POSIX), linetype = "dotted") +
  geom_vline(xintercept = as.numeric(time_bins$end_POSIX), linetype = "dotted") +
  ylim(-1, 2.3) +
  labs(x = "Time" , y = NULL , color = NULL, caption = "Strahler Order") + 
  scale_x_datetime(date_labels = "%H:%M", breaks = break_times) +
  theme_classic() +
  theme(legend.position = "bottom", legend.direction = "horizontal", plot.caption = element_text(hjust = 0.5))

p_Strahler_PC2


# --- Patchwork combinations ---
PC1_combined <- p_QBR_PC1 + p_Strahler_PC1 + plot_layout(ncol = 2) + 
  plot_annotation(title = "Compound Index 1 Diel Patterns")

PC2_combined <- p_QBR_PC2 + p_Strahler_PC2 + plot_layout(ncol = 2) + 
  plot_annotation(title = "Compound Index 2 Diel Patterns")

# --- Display ---
PC1_combined

PC2_combined






# Model 4 Joint Plot: -------------------------------
p_QBR_PC1
p_Strahler_PC1
p_QBR_PC2
p_Strahler_PC2

effectplot_QBR_PC1
effectplot_strahler_PC1
effectplot_QBR_PC2
effectplot_strahler_PC2

# Grid PC1 with effect plots half the height of diel plots
pc1_grid <- (effectplot_QBR_PC1 | effectplot_strahler_PC1) /  # Row 1
  (p_QBR_PC1        | p_Strahler_PC1) +            # Row 2
  plot_layout(heights = c(1, 3))  # relative row heights: top row 1, bottom row 2 twice as tall

pc1_grid

# Grid PC2 with effect plots half the height of diel plots
pc2_grid <- (effectplot_QBR_PC2 | effectplot_strahler_PC2) /  # Row 1
  (p_QBR_PC2        | p_Strahler_PC2) +            # Row 2
  plot_layout(heights = c(1, 2))  # relative row heights: top row 1, bottom row 2 twice as tall

pc2_grid


# Other format
pc1_QBR_grid <- (effectplot_QBR_PC1) /  # Row 1
  (p_QBR_PC1) +            # Row 2
  plot_layout(heights = c(1, 3))  # relative row heights: top row 1, bottom row 2 twice as tall

pc1_QBR_grid

pc1_strah_grid <- (effectplot_strahler_PC1) /  # Row 1
  (p_Strahler_PC1) +            # Row 2
  plot_layout(heights = c(1, 3))  # relative row heights: top row 1, bottom row 2 twice as tall

pc1_strah_grid








# Model 5 Diel Plot  -----------------------------------

# --- Reference date and Time Bins ---
ref_date <- make_date(2000,1,1)
time_bins <- tibble::tibble(
  label = c("Morning", "Day", "Evening", "Night"),
  start = c("04:00:01", "10:00:01", "16:00:01", "22:00:01"),
  end   = c("10:00:00", "16:00:00", "22:00:00", "04:00:00")
) %>%
  mutate(
    start_POSIX = make_datetime(year = year(ref_date), month = month(ref_date), day = day(ref_date),
                                hour = as.numeric(substr(start,1,2)),
                                min  = as.numeric(substr(start,4,5)),
                                sec  = as.numeric(substr(start,7,8))),
    end_POSIX = make_datetime(year = year(ref_date), month = month(ref_date), day = day(ref_date),
                              hour = as.numeric(substr(end,1,2)),
                              min  = as.numeric(substr(end,4,5)),
                              sec  = as.numeric(substr(end,7,8)))
  )

# --- Preprocess data ---
plotting_ds <- plotting_ds %>%
  mutate(
    Time_str = sprintf("%06d", Time),
    Hour     = as.numeric(substr(Time_str,1,2)),
    Minute   = as.numeric(substr(Time_str,3,4)),
    Time_POSIX = make_datetime(year = 2000, month = 1, day = 1, hour = Hour, min = Minute),
    Time_bin = floor_date(Time_POSIX, "30 minutes")
  )


# Function to subset data



summarise_diel <- function(df, pc, group_var, season_filter = NULL){
  
  # Optional seasonal filter
  if(!is.null(season_filter)){
    df <- df %>% filter(Season == season_filter)
  }
  
  if(group_var == "QBR_Class"){
    df[[group_var]] <- factor(df[[group_var]],
                              levels = c("Natural", 
                                         "Good", 
                                         "Fair", 
                                         "Poor", 
                                         "Bad"))
  }
  
  df %>%
    group_by(Time_bin, .data[[group_var]]) %>%
    summarise(
      mean_val = mean(.data[[pc]], na.rm = TRUE),
      n = sum(!is.na(.data[[pc]])),
      se_val = sd(.data[[pc]], na.rm = TRUE)/sqrt(n),
      ci_val = 1.96*se_val,
      .groups = "drop"
    )
}

# Create Plotting Datasets
diel_QBR_PC1_dry       <- summarise_diel(plotting_ds, "PC1", "QBR_Class", "Dry")
diel_Strahler_PC1_dry  <- summarise_diel(plotting_ds, "PC1", "Strahler_Order", "Dry") %>%
  mutate(Strahler_Order = factor(Strahler_Order))

diel_QBR_PC2_dry       <- summarise_diel(plotting_ds, "PC2", "QBR_Class", "Dry")
diel_Strahler_PC2_dry  <- summarise_diel(plotting_ds, "PC2", "Strahler_Order", "Dry") %>%
  mutate(Strahler_Order = factor(Strahler_Order))

diel_QBR_PC1_monsoon       <- summarise_diel(plotting_ds, "PC1", "QBR_Class", "Monsoon")
diel_Strahler_PC1_monsoon  <- summarise_diel(plotting_ds, "PC1", "Strahler_Order", "Monsoon") %>%
  mutate(Strahler_Order = factor(Strahler_Order))

diel_QBR_PC2_monsoon       <- summarise_diel(plotting_ds, "PC2", "QBR_Class", "Monsoon")
diel_Strahler_PC2_monsoon  <- summarise_diel(plotting_ds, "PC2", "Strahler_Order", "Monsoon") %>%
  mutate(Strahler_Order = factor(Strahler_Order))

# Plot QBR PC1
p_QBR_PC1_dry <- ggplot(diel_QBR_PC1_dry, aes(x = Time_bin, y = mean_val, color = QBR_Class)) +
  geom_ribbon(aes(ymin = mean_val - ci_val, ymax = mean_val + ci_val, fill = QBR_Class),
              alpha = 0.1, colour = NA) + 
  geom_smooth(aes(group = QBR_Class), method = "loess", span = 0.2, se = FALSE, size = 1) +
  scale_color_manual(values = palettes$QBR_Class) +
  scale_fill_manual(values = palettes$QBR_Class, guide = "none") +
  geom_vline(xintercept = as.numeric(time_bins$start_POSIX), linetype = "dotted") +
  geom_vline(xintercept = as.numeric(time_bins$end_POSIX), linetype = "dotted") +
  labs(x = NULL, y = "Compound Index 1") + 
  ylim(-5, 2) +
  scale_x_datetime(date_labels = "%H:%M", breaks = break_times) +
  theme_classic() +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank()) +
  annotate("text",
           x = min(diel_QBR_PC1_dry$Time_bin),
           y = max(diel_QBR_PC1_dry$mean_val + diel_QBR_PC1_dry$ci_val),
           label = "QBR (Dry)",
           hjust = 0.1, vjust = 1,
           fontface = "bold",
           size = 4)

p_QBR_PC1_dry

p_QBR_PC1_monsoon <- ggplot(diel_QBR_PC1_monsoon, aes(x = Time_bin, y = mean_val, color = QBR_Class)) +
  geom_ribbon(aes(ymin = mean_val - ci_val, ymax = mean_val + ci_val, fill = QBR_Class), 
              alpha = 0.1, colour = NA) + 
  geom_smooth(aes(group = QBR_Class), method = "loess", span = 0.2, se = FALSE, size = 1) +
  scale_color_manual(values = palettes$QBR_Class) +
  scale_fill_manual(values = palettes$QBR_Class, guide = "none") +
  geom_vline(xintercept = as.numeric(time_bins$start_POSIX), linetype = "dotted") +
  geom_vline(xintercept = as.numeric(time_bins$end_POSIX), linetype = "dotted") +
  labs(x = NULL, y = "Compound Index 1") + 
  ylim(-5, 2) +
  scale_x_datetime(date_labels = "%H:%M", breaks = break_times) +
  theme_classic() +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank()) +
  annotate("text",
           x = min(diel_QBR_PC1_monsoon$Time_bin),
           y = max(diel_QBR_PC1_monsoon$mean_val + diel_QBR_PC1_monsoon$ci_val),
           label = "QBR (Monsoon)",
           hjust = 0.1, vjust = 1,
           fontface = "bold",
           size = 4)

p_QBR_PC1_monsoon

# Plot Strahler PC1
p_Strahler_PC1_dry <- ggplot(diel_Strahler_PC1_dry, aes(x = Time_bin, y = mean_val, color = Strahler_Order)) +
  geom_ribbon(aes(ymin = mean_val - ci_val, ymax = mean_val + ci_val, fill = Strahler_Order),
              alpha = 0.1, colour = NA) + 
  geom_smooth(aes(group = Strahler_Order), method = "loess", span = 0.2, se = FALSE, size = 1) +
  scale_color_manual(values = palettes$Strahler_Order) +
  scale_fill_manual(values = palettes$Strahler_Order, guide = "none") +
  geom_vline(xintercept = as.numeric(time_bins$start_POSIX), linetype = "dotted") +
  geom_vline(xintercept = as.numeric(time_bins$end_POSIX), linetype = "dotted") +
  labs(x = NULL, y = "Compound Index 1") + 
  ylim(-5, 2) +
  scale_x_datetime(date_labels = "%H:%M", breaks = break_times) +
  theme_classic() +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank()) +
  annotate("text",
           x = min(diel_Strahler_PC1_dry$Time_bin),
           y = max(diel_Strahler_PC1_dry$mean_val + diel_Strahler_PC1_dry$ci_val),
           label = "Strahler (Dry)",
           hjust = 0.1, vjust = 1,
           fontface = "bold",
           size = 4)

p_Strahler_PC1_dry

p_Strahler_PC1_monsoon <- ggplot(diel_Strahler_PC1_monsoon, aes(x = Time_bin, y = mean_val, color = Strahler_Order)) +
  geom_ribbon(aes(ymin = mean_val - ci_val, ymax = mean_val + ci_val, fill = Strahler_Order), 
              alpha = 0.1, colour = NA) + 
  geom_smooth(aes(group = Strahler_Order), method = "loess", span = 0.2, se = FALSE, size = 1) +
  scale_color_manual(values = palettes$Strahler_Order) +
  scale_fill_manual(values = palettes$Strahler_Order, guide = "none") +
  geom_vline(xintercept = as.numeric(time_bins$start_POSIX), linetype = "dotted") +
  geom_vline(xintercept = as.numeric(time_bins$end_POSIX), linetype = "dotted") +
  labs(x = NULL, y = "Compound Index 1") + 
  ylim(-5, 2) +
  scale_x_datetime(date_labels = "%H:%M", breaks = break_times) +
  theme_classic() +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank()) +
  annotate("text",
           x = min(diel_Strahler_PC1_monsoon$Time_bin),
           y = max(diel_Strahler_PC1_monsoon$mean_val + diel_Strahler_PC1_monsoon$ci_val),
           label = "Strahler (Monsoon)",
           hjust = 0.1, vjust = 1,
           fontface = "bold",
           size = 4)

p_Strahler_PC1_monsoon


# Patchwork PC1
p_QBR_PC1_dry
p_QBR_PC1_monsoon

# Grid PC1 with effect plots half the height of diel plots
pc1_seasonal_grid <- (p_QBR_PC1_dry | p_Strahler_PC1_dry) /  # Row 1
  (p_QBR_PC1_monsoon        | p_Strahler_PC1_monsoon) +            # Row 2
  plot_layout(heights = c(1, 1))  # relative row heights: top row 1, bottom row 2 twice as tall

pc1_seasonal_grid
