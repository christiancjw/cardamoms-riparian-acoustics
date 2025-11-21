
# Load libraries
library(tidyverse) # for data manipulation and plotting
library(lme4) # for mixed models
library(broom.mixed) # for looking at mixed model outputs
library(car) # for VIF (higher than 5 or 10 is bad)
library(lubridate) # for date/time manipulations
library(ggfortify) # for model diagnostic plots
library(pbkrtest)
library(Matrix)
library(MuMIn) # for r2 values GLMM


setwd("/Users/christianching/Documents/Projects/cardamoms-riparian-acoustics")
glo_rl_ds <- read_csv("clean_data/datasets/PCAs/global2325_single_rainless_pca.csv")

# Other Data Read-in
glo_ds <- read_csv("clean_data/datasets/PCAs/global2325_single_pca.csv")
jan24_ds <- read_csv("clean_data/datasets/PCAs/jan24_single_pca.csv")
apr24_ds <- read_csv("clean_data/datasets/PCAs/apr24_single_pca.csv")
jun24_ds <-read_csv("clean_data/datasets/PCAs/jun24_single_pca.csv")

#--------------------------------------------
# Global Rainless Single Data ---------------
#--------------------------------------------
# Read in and check data
global_ds <- read.csv("clean_data/datasets/PCAs/global2325_single_rainless_pca.csv")
global_ds <-global_ds %>% 
  rename(QBR = QBR_Score)

head(global_ds)

str(global_ds)

# --------------------------------------------------------
# Add Season variable to the dataset
# --------------------------------------------------------
global_ds <- global_ds %>%
  mutate(
    Season = case_when(
      Date >= 20231116 & Date <= 20231203 ~ "Nov 2023",
      Date >= 20231130 & Date <= 20240208 ~ "Jan 2024",
      Date >= 20240401 & Date <= 20240501 ~ "Apr 2024",
      Date >= 20240607 & Date <= 20240707 ~ "Jun 2024",
      Date >= 20250605 & Date <= 20250716 ~ "Jun 2025",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Season))

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
  # rescale Time to be between 0 and 1440 (i.e. minutes in a day).
  mutate(minutes = round(((Time-0)*(1440-0)/(max(Time)-min(Time)) + 0), digits = 0))

#-----------------
# Initial model without random effects ------------------------------------------------
model1 <- lm(PC1 ~ QBR + Strahler, data = global_ds)
# look at diagnostics
autoplot(model1)
# look at significance
anova(model1)
# look at effect sizes
summary(model1)


# A big issue in these models is likely to be multicollinearity
# check this with VIF...
vif(model1) # higher than 5 or 10 is bad




#----------------- 
# Mixed models 
ggplot(global_ds, aes(x = QBR, y = PC1, colour = Site)) +
  geom_point()+
  geom_line()

# fitting mixed-effects model
# fixed effect is QBR & Strahler, modelling PC1 as a function of QBR and Strahler
# random effects (intercepts) are month and site (need to add time of day here) 
# To see if model assumptions hold: 
    # Residuals should look randomly scattered (no trend).
    # Variance should be even across predictions (homoscedasticity).
    # Outliers show up as extreme residuals.
model2 <- lmer(PC1 ~ QBR + Strahler + (1|Site) + (1|month), data = global_ds)


# Plot model diagnostics
# 1. scaled residuals vs fitted
plot(model2, resid(., scaled=TRUE) ~ fitted(.), abline = 0)


# 2. box plots by groups
plot(model2, factor(Site) ~ resid(., scaled=TRUE))
plot(model2, factor(month) ~ resid(., scaled=TRUE))
plot(model2, factor(QBR) ~ resid(., scaled=TRUE))


# Outputs: fixed and random effects. broom.mixed= turns messy model output into neat tables.
# 1. fixed effects
broom.mixed::tidy(model2, effects = "fixed")
# 2. random parameters ([co]variance/correlations) -  random effect variances (standard deviations of intercepts).
#     Tells us how much Site, and Month contribute to variability in PC1.
broom.mixed::tidy(model2, effects = "ran_pars")
# 3. random values (== Random EFfects above) - actual random-effect estimates for each level.
#     Useful for understanding which sites deviate most strongly.
broom.mixed::tidy(model2, effects = "ran_vals")
# 4. random coefficients - random intercepts added to the fixed intercept.
#     If fixed intercept = 1.77, and Site A random effect = +0.995, then Site A’s baseline = 1.77 + 0.995 ≈ 2.77.
broom.mixed::tidy(model2, effects = "ran_coefs")

# Inference (p-values, CIs) 


#-----------------
# Likelihood Ratio (LR) Tests
#----------------- 

# Is the fixed effect of QBR significant?
# Note this should use ML not REML
# anova(model2, model2B) = compares log-likelihooglobal_ds. 
# If the p-value is small (<0.05), the dropped variable (QBR) significantly improves model fit.
model2 <- lmer(PC1 ~ QBR + Strahler + (1|Site) + (1|year) 
               + (1|month), data = global_ds)
model2B <- lmer(PC1 ~ Strahler + (1|Site) + (1|year) 
                + (1|month), data = global_ds)
anova(model2, model2B) # yes

# Is the fixed effect of Strahler significant?
# Note this should use ML not REML
model2 <- lmer(PC1 ~ QBR + Strahler + (1|Site) + (1|year) 
               + (1|month), data = global_ds)
model2C <- lmer(PC1 ~ QBR + (1|Site) + (1|year) 
                + (1|month), data = global_ds)
anova(model2, model2C) # NO

# Is there an effect of Site?
# Note this should use REML not ML so use refit = FALSE WHY
# This is mostly just to check that all the random effects
# are really needed. If they are biologically important
# leave them in even if not significant
model2 <- lmer(PC1 ~ QBR + Strahler + (1|Site) + (1|year) 
               + (1|month), data = global_ds)
model2D <- lmer(PC1 ~ QBR + Strahler + (1|year) 
                + (1|month), data = global_ds)
anova(model2, model2D, refit = FALSE) # Yes

# Is there an effect of Year?
# Note this should use REML not ML so use refit = FALSE
model2 <- lmer(PC1 ~ QBR + Strahler + (1|Site) + (1|year) 
               + (1|month), data = global_ds)
model2E <- lmer(PC1 ~ QBR + Strahler + (1|Site) 
                + (1|month), data = global_ds)
anova(model2, model2E, refit = FALSE) # NO - so could leave off the year

# Is there an effect of Month?
# Note this should use REML not ML so use refit = FALSE
model2 <- lmer(PC1 ~ QBR + Strahler + (1|Site) + (1|year) 
               + (1|month), data = global_ds)
model2F <- lmer(PC1 ~ QBR + Strahler + (1|Site) 
                + (1|year), data = global_ds)
anova(model2, model2F, refit = FALSE) # Yes

# Confidence intervals are more appropriate than p values 
# in mixed models

# "Wald" - fast but approximate / only fixed effects
confint(model2, method="Wald") # these are the "worst" - fast, approximate, relies on standard errors (can be misleading).
# "profile" 
confint(model2, method="profile") # this takes a while to run - more accurate, uses likelihood profiling.
# "bootstrap"
confint(model2, method="boot") # this takes a while to run -  most robust, resamples data to generate intervals.



## Report in the paper

#parameter name | slope | lower-95 | upper-95 | random effect (SD)
# broom.mixed::tidy(model2, effects = "fixed")
# gives the slope for QBR and Strahler

# confint(model2, method="profile")
# gives CI for fixed effect slopes and sd for random effects

# Main combined model
model_season <- lmer(
  PC1 ~ QBR * Season + Strahler + (1|Site) + (1|month),
  data = global_ds
)

# Summary
summary(model_season)

# Bootstrap confidence intervals
confint(model_season, method = "boot")

# Neat table of fixed effects
broom.mixed::tidy(model_season, effects = "fixed", conf.int = TRUE)

# R² values
r2_vals <- r.squaredGLMM(model_season)
r2_vals



## Seasonality ------------
library(MuMIn)  # for R² of mixed models

# --------------------------------------------------------
# 1. Add Season variable to the dataset
# --------------------------------------------------------
global_ds <- global_ds %>%
  mutate(
    Season = case_when(
      Date >= 20231130 & Date <= 20240208 ~ "Jan 2024",
      Date >= 20240401 & Date <= 20240501 ~ "Apr 2024",
      Date >= 20240607 & Date <= 20240707 ~ "Jun 2024",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Season))

# --------------------------------------------------------
# 2. Fit seasonal models separately
# --------------------------------------------------------
# Jan
model_jan <- lmer(PC1 ~ QBR + Strahler + (1|Site) + (1|month),
                  data = filter(global_ds, Season == "Jan 2024"))
confint(model_jan, method = "boot")  # bootstrap CI
r.squaredGLMM(model_jan)

# Apr
model_apr <- lmer(PC1 ~ QBR + Strahler + (1|Site) + (1|month),
                  data = filter(global_ds, Season == "Apr 2024"))
confint(model_apr, method = "boot")
r.squaredGLMM(model_apr)

# Jun
model_jun <- lmer(PC1 ~ QBR + Strahler + (1|Site) + (1|month),
                  data = filter(global_ds, Season == "Jun 2024"))
confint(model_jun, method = "boot")
r.squaredGLMM(model_jun)

# --------------------------------------------------------
# 3. Combined model with Season factor + interactions
# --------------------------------------------------------
model_season <- lmer(
  PC1 ~ QBR * Season + Strahler + (1|Site) + (1|month),
  data = global_ds
)

summary(model_season)                   # fixed effects + interactions
broom.mixed::tidy(model_season)         # neat table of results
confint(model_season, method = "boot")  # bootstrap CI
r.squaredGLMM(model_season)             # marginal + conditional R²

# --------------------------------------------------------
# 4. Quick visualisation
# --------------------------------------------------------
ggplot(global_ds, aes(x = QBR, y = PC1, color = Season)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    title = "Effect of QBR on PC1 across seasons",
    x = "QBR Score",
    y = "PC1"
  )
