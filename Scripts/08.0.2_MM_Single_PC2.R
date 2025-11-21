
# Load libraries
library(tidyverse) # for data manipulation and plotting
library(lme4) # for mixed models
library(broom.mixed) # for looking at mixed model outputs
library(car) # for VIF (higher than 5 or 10 is bad)
library(lubridate) # for date/time manipulations
library(ggfortify) # for model diagnostic plots
library(MuMIn) # for r2 values GLMM
library(purrr) # For diel analysis

#--------------------------------------------
# Global Rainless Single Data ---------------
#--------------------------------------------
# Read in and check data
setwd("/Users/christianching/Documents/Projects/cardamoms-riparian-acoustics")
global_ds <- read.csv("clean_data/datasets/PCAs/global_single_pca.csv")

head(global_ds)

str(global_ds)

# Add Seasonal and Time Range variables to the dataset ----------------------------------

global_ds <- global_ds %>%
  mutate(
    Season = case_when(
      Date >= 20231116 & Date <= 20231203 ~ "Nov 2023",
      Date >= 20231230 & Date <= 20240208 ~ "Jan 2024",
      Date >= 20240401 & Date <= 20240501 ~ "Apr 2024",
      Date >= 20240607 & Date <= 20240707 ~ "Jun 2024",
      Date >= 20250605 & Date <= 20250716 ~ "Jun 2025",
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
    TimeHHMMSS >= "050000" & TimeHHMMSS <= "090000" ~ "Dawn",
    TimeHHMMSS >= "103000" & TimeHHMMSS <= "143000" ~ "Midday",
    TimeHHMMSS >= "153000" & TimeHHMMSS <= "193000" ~ "Dusk",
    TimeHHMMSS >= "220000" | TimeHHMMSS <= "020000" ~ "Midnight",
    TRUE ~ "Other"
  )) %>% 
  mutate(TimeRangeFactor = factor(TimeRangeFactor, levels = c("Dawn","Midday","Dusk","Midnight","Other")))


# Quick plot to identify the replication issue
ggplot(global_ds, aes(x = QBR, y = Strahler, colour = as.factor(Branch))) +
  geom_point() +
  facet_wrap(~Site)

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


# Model 1: Initial Linear Model without random effects ------------------------------------------------
model1 <- lm(PC2 ~ QBR + Strahler, data = global_ds)
# look at diagnostics
autoplot(model1)
# look at significance
anova(model1)
# look at effect sizes
summary(model1)

# A big issue in these models is likely to be multicollinearity
# check this with VIF...
vif(model1) # higher than 5 or 10 is bad


# Model 2: Mixed Effects Model incorporating random effects -------------------------

# fitting mixed-effects model
# fixed effect is QBR & Strahler, modelling PC2 as a function of QBR and Strahler
# random effects (intercepts) are month and site (need to add time of day here) 
# To see if model assumptions hold: 
# Residuals should look randomly scattered (no trend).
# Variance should be even across predictions (homoscedasticity).
# Outliers show up as extreme residuals.
model2 <- lmer(PC2 ~ QBR + Strahler + (1|Site) + (1|Season), data = global_ds)

# Plot model diagnostics
# 1. scaled residuals vs fitted
plot(model2, resid(., scaled=TRUE) ~ fitted(.), abline = 0)

# 2. box plots by groups
plot(model2, factor(Site) ~ resid(., scaled=TRUE))
plot(model2, factor(Season) ~ resid(., scaled=TRUE))
plot(model2, factor(QBR) ~ resid(., scaled=TRUE))

# Outputs: fixed and random effects. broom.mixed= turns messy model output into neat tables.
# 1. fixed effects
broom.mixed::tidy(model2, effects = "fixed")
# 2. random parameters ([co]variance/correlations) -  random effect variances (standard deviations of intercepts).
#     Tells us how much Site, and Month contribute to variability in PC2.
broom.mixed::tidy(model2, effects = "ran_pars")
# 3. random values (== Random EFfects above) - actual random-effect estimates for each level.
#     Useful for understanding which sites deviate most strongly.
broom.mixed::tidy(model2, effects = "ran_vals")
# 4. random coefficients - random intercepts added to the fixed intercept.
#     If fixed intercept = 1.77, and Site A random effect = +0.995, then Site A’s baseline = 1.77 + 0.995 ≈ 2.77.
broom.mixed::tidy(model2, effects = "ran_coefs")

# Model 2: Inferences (p-values, CIs) -----------------------------------------

# Likelihood Ratio (LR) Tests

# Is the fixed effect of QBR significant?
# Note this should use ML not REML
# anova(model2, model2B) = compares log-likelihooglobal_ds. 
# If the p-value is small (<0.05), the dropped variable (QBR) significantly improves model fit.
model2B <- lmer(PC2 ~ Strahler + (1|Site) 
                + (1|Season), data = global_ds)
anova(model2, model2B) # yes

# Is the fixed effect of Strahler significant?
# Note this should use ML not REML
model2C <- lmer(PC2 ~ QBR + (1|Site)
                + (1|Season), data = global_ds)
anova(model2, model2C) # NO

# Is there an effect of Site?
# Note this should use REML not ML so use refit = FALSE WHY
# This is mostly just to check that all the random effects
# are really needed. If they are biologically important
# leave them in even if not significant
model2D <- lmer(PC2 ~ QBR + Strahler 
                + (1|Season), data = global_ds)
anova(model2, model2D, refit = FALSE) # Yes

# Is there an effect of Season?
# Note this should use REML not ML so use refit = FALSE
model2F <- lmer(PC2 ~ QBR + Strahler + (1|Site), 
                 data = global_ds)
anova(model2, model2F, refit = FALSE) # Yes

# Is there an effect of Year?
# Note this should use REML not ML so use refit = FALSE
model2F <- lmer(PC2 ~ QBR + Strahler + (1|Site) 
                + (1|Season), data = global_ds)
anova(model2, model2F, refit = FALSE) # Yes



# Confidence intervals 
# more appropriate than p values in mixed models
# "profile"
confint(model2, method="Wald")
confint(model2, method="profile") # this takes a while to run - more accurate, uses likelihood profiling.
# "bootstrap"
confint(model2, method="boot") # this takes a while to run -  most robust, resamples data to generate intervals.

# R² values
model2_r2_vals <- r.squaredGLMM(model2)
model2_r2_vals


# Model 3: Testing Diel Cycles --------------------------------------------------------------------------
model3 <- lmer(PC2 ~ QBR + Strahler + TimeRangeFactor +  
                 (1|Site) + (1|Season),
               data = global_ds)

# Likelihood Ratio Tests - does this model integrating time improve model fit?
anova(model3, model2, refit = FALSE) # Yes

# Show Fixed EFfect Values for Model
broom.mixed::tidy(model3, effects = "fixed")
# Show Random Effect Values for model
broom.mixed::tidy(model3, effects = "ran_pars")

# Confidence Intervals
confint(model3, method="Wald")

confint(model3, method="profile")
confint(model3, method="boot") 

# R² values
model3_r2_vals <- r.squaredGLMM(model3)
model3_r2_vals

acf(resid(model3))


# Model 4: QBR Time Interaction -------------------------------------------------------
# QBR as a interaction - QBR * Timerange - slope of QBR varying by time of day
# Testing if QBR behaves differently at different times
model4 <- lmer(PC2 ~ QBR * TimeRangeFactor + Strahler +
                 (1|Site) + (1|Season),
               data = global_ds)

# Likelihood Ratio Tests - does this model integrating time improve model fit?
anova(model3, model4)

# Show Fixed EFfect Values for Model
broom.mixed::tidy(model4, effects = "fixed")
# Show Random Effect Values for model
broom.mixed::tidy(model4, effects = "ran_pars")

# Confidence Intervals
confint(model4, method="Wald")
confint(model4, method="profile")
confint(model4, method="boot")

# R2 Values
model4_r2_vals <- r.squaredGLMM(model4)
model4_r2_vals

coef(model4)  # gives raw fixed effects



### Some random plotting stuff --------------
## Plotting - PC2 Over 24h cycle
# Convert Time to decimal hours
global_ds <- global_ds %>%
  mutate(Time = str_pad(Time, width = 6, side = "left", pad = "0"),
         Hour = as.numeric(substr(Time, 1, 2)) + 
           as.numeric(substr(Time, 3, 4))/60 + 
           as.numeric(substr(Time, 5, 6))/3600)

# Create QBR bins for smoother plotting (optional)
global_ds <- global_ds %>%
  mutate(QBR_group = cut(QBR, breaks = c(0, 50, 75, 100), labels = c("Low", "Medium", "High")))

# Summarize: mean PC2 per Hour per QBR_group
PC2_summary <- global_ds %>%
  group_by(Hour, QBR_group) %>%
  summarise(mean_PC2 = mean(PC2, na.rm = TRUE), .groups = "drop")

# Plot
ggplot(PC2_summary, aes(x = Hour, y = mean_PC2, color = QBR_group)) +
  geom_line(size = 1.2) +
  geom_smooth(aes(group = QBR_group), method = "loess", se = TRUE, linetype = "dashed") +
  scale_color_manual(values = c("Low" = "#ffffb2", "Medium" = "#fd8d3c", "High" = "#b10026")) +
  scale_x_continuous(breaks = seq(0, 24, by = 2)) +
  labs(
    x = "Time of Day (Hours)",
    y = "Mean PC2",
    color = "QBR Group",
    title = "Diel Variation of PC2 by QBR Group",
    subtitle = "Smoothed daily trends using mean PC2 per time"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )
