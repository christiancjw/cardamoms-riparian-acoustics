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
library(lmerTest)

# Read in Data
final_ds <- read.csv("clean_data/datasets/PCAs/rainless_single_pca.csv")


# Check Data
head(final_ds)
str(final_ds)

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

final_ds <- final_ds %>%
  mutate(
    Deployment = case_when(
      Date >= 20231116 & Date <= 20231203 ~ "Monsoon 2023",
      Date >= 20231230 & Date <= 20240208 ~ "Dry Transition 2024",
      Date >= 20240401 & Date <= 20240501 ~ "Dry 2024",
      Date >= 20240607 & Date <= 20240707 ~ "Monsoon Transition 2024",
      Date >= 20250605 & Date <= 20250716 ~ "Monsoon 2025",
      TRUE ~ NA_character_
    ))

final_ds <- final_ds %>%
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
final_ds <- final_ds %>%
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

head(final_ds)

# For plotting - rename strahler to strahler order
final_ds <- final_ds %>%
  rename(Strahler_Order = Strahler) %>%   # rename column
  mutate(
    QBR_Class = case_when( # Renaming QBR terms too
      QBR_Class == "Natural (95–100)" ~ "Natural",
      QBR_Class == "Good (75–90)" ~ "Good",
      QBR_Class == "Fair (55–70)" ~ "Fair",
      QBR_Class == "Poor (30–50)" ~ "Poor",
      QBR_Class == "Bad (<25)" ~ "Bad",
      TRUE ~ QBR_Class
    )
  )

# Fiddle with dates and times
final_ds <-
  final_ds %>%
  # convert Date to a date
  mutate(date = ymd(Date)) %>%
  # extract month
  mutate(month = month(date)) %>%
  # extract year
  mutate(year = year(date)) %>%
  # Convert time to HHMMSS
  mutate(Time = str_pad(Time, width = 6, side = "left", pad = "0"))




# Model 3 -----------

model3_pc1 <- lmer(PC1 ~ QBR_bin  +
                     Strahler_Order  +
                     Season + 
                     TimeRangeFactor + 
                     (1 | Site) , 
                   data = final_ds)

# Check significance
anova(model3_pc1)

# Check variance 
r.squaredGLMM(model3_pc1)

# Check VIF
vif(model3_pc1) # higher than 5 or 10 is bad

# Show Fixed EFfect Values for Model 
broom.mixed::tidy(model3_pc1, effects = "fixed")
# Show Random Effect Values for model
broom.mixed::tidy(model3_pc1, effects = "ran_pars")

# Confidence Intervals PC1
confint(model3_pc1, method="Wald")
profconfintPC1 <- confint(model4_PC1, method="profile")
model3_bootconfintPC1 <- confint(model3_PC1, method="boot")


# PC2
model3_pc2 <- lmer(PC2 ~ QBR_bin  +
                     Strahler_Order  +
                     Season + TimeRangeFactor +
                     (1 | Site) , 
                   data = final_ds)

# Check significance
anova(model3_pc2)

# Check variance 
r.squaredGLMM(model3_pc2)

# Check VIF
vif(model3_pc2) # higher than 5 or 10 is bad

# Show Fixed EFfect Values for Model 
broom.mixed::tidy(model3_pc2, effects = "fixed")
# Show Random Effect Values for model
broom.mixed::tidy(model3_pc2, effects = "ran_pars")

# Confidence Intervals pc2
confint(model3_pc2, method="Wald")
profconfintpc2 <- confint(model4_pc2, method="profile")
model3_bootconfintpc2 <- confint(model3_pc2, method="boot")


# Model 4 (Season as Random) ---------------------------
# PC1 + Explorations

# PC1 + Explorations
model4_PC1 <- lmer(PC1 ~ QBR_bin * TimeRangeFactor +
                     Strahler_Order * TimeRangeFactor +
                     Season +
                     (1 | Site), 
                   data = final_ds)

# Check significance
anova(model4_PC1)

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


coef(model4_PC1)  # gives raw fixed effects


# PC2 + Explorations
model4_PC2 <- lmer(PC2 ~ QBR_bin * TimeRangeFactor +
                     Strahler_Order * TimeRangeFactor +
                     Season +
                     (1 | Site), 
                   data = final_ds)

# Check Significance
anova(model4_PC2)

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

coef(model4_PC2)  # gives raw fixed effects

# Model 5 ----------

# PC1 + Explorations
model5_PC1 <- lmer(PC1 ~ QBR_bin * TimeRangeFactor * Season +
                     Strahler_Order * TimeRangeFactor * Season +
                     (1 | Site), 
                   data = final_ds)

# Significance
anova(model4_PC1, model5_PC1)

isSingular(model5_PC1)

anova(model5_PC1)

# Check variance 
r.squaredGLMM(model5_PC1)

# Checking Random
# Check residual normality
qqnorm(resid(model5_PC1))
qqline(resid(model5_PC1))

# Check homoscedasticity
plot(fitted(model5_PC1), resid(model5_PC1))
abline(h = 0, lty = 2)

# Check for influential sites
library(influence.ME)
infl <- influence(model5_PC1, group = "Site")
plot(infl, which = "cook")

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
model5_PC2 <- lmer(PC2 ~ QBR_bin * TimeRangeFactor * Season +
                     Strahler_Order * TimeRangeFactor * Season +
                     (1 | Site), 
                   data = final_ds)

# Check AIC
AICc(model3_pc2, model4_PC2, model5_PC2)

# Check Fit
anova(model4_PC2, model5_PC2)

# Check Variance 
r.squaredGLMM(model5_PC2)

model5_PC2 <- lmer(PC2 ~ QBR_bin * TimeRangeFactor +
                      Strahler_Order * TimeRangeFactor +
                      Season * TimeRangeFactor +
                      (1 | Site), 
                    data = final_ds)

r.squaredGLMM(model5_PC2)

anova(model5_PC2a)

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

# Model Fitting
vif(model3_pc1)
vif(model4_PC1) 
vif(model5_PC1) 

vif(model3_pc2)
vif(model4_PC2) 
vif(model5_PC2) 


r.squaredGLMM(model3_pc2)
r.squaredGLMM(model4_PC2)
r.squaredGLMM(model5_PC1)


AICc(model3_pc1, model4_PC1, model5_PC1)

AICc(model3_pc2, model4_PC2, model5_PC2)


# Mixed Models Stastistics for Results V2 ----------------------

# Function to extract fixed + random effects with bootstrap CIs
extract_model_terms_boot_all <- function(model, model_name, nboot = 1000){
  
  # --- Fixed effects ---
  tidy_fixed <- broom.mixed::tidy(model, effects = "fixed") %>%
    select(term, estimate, std.error, statistic)
  
  # --- Random effects ---
  tidy_random <- broom.mixed::tidy(model, effects = "ran_pars") %>%
    select(term, estimate) %>%
    mutate(std.error = NA, statistic = NA)  # no SE/t-stat for ran_pars
  
  # Combine fixed + random for boot CI matching
  all_terms <- bind_rows(
    tidy_fixed %>% mutate(effect_type = "fixed"),
    tidy_random %>% mutate(effect_type = "random")
  )
  
  # --- Bootstrap confidence intervals ---
  boot_ci <- confint(model, method = "boot", nsim = nboot)
  boot_df <- as.data.frame(boot_ci)
  boot_df$term <- rownames(boot_df)
  rownames(boot_df) <- NULL
  names(boot_df)[1:2] <- c("conf.low", "conf.high")
  
  # Merge boot CIs
  all_terms <- all_terms %>%
    left_join(boot_df, by = "term") %>%
    mutate(model = model_name)
  
  return(all_terms)
}

# Run all models ---
model_terms_boot_all <- bind_rows(
  extract_model_terms_boot_all(model3_pc1, "Model3_PC1"),
  extract_model_terms_boot_all(model3_pc2, "Model3_PC2"),
  extract_model_terms_boot_all(model4_PC1, "Model4_PC1"),
  extract_model_terms_boot_all(model4_PC2, "Model4_PC2"),
  extract_model_terms_boot_all(model5_PC1, "Model5_PC1"),
  extract_model_terms_boot_all(model5_PC2, "Model5_PC2")
)

# --- Export ---
write.csv(model_terms_boot_all, "mixed_model_coefficients_boot_all.csv")


# Model 3:
model3_terms_boot_all <- bind_rows(
  extract_model_terms_boot_all(model3_pc1, "Model3_PC1"),
  extract_model_terms_boot_all(model3_pc2, "Model3_PC2")
)

write.csv(model3_terms_boot_all, "mixed_model3_coefficients_boot_all.csv")


# Mixed Models Stats (CI + ANOVAs)------------
extract_model_terms_boot_all <- function(model, model_name, nboot = 1000){
  
  # --- Fixed effects ---
  tidy_fixed <- broom.mixed::tidy(model, effects = "fixed") %>%
    dplyr::select(term, estimate, std.error, statistic)
  
  # --- Random effects ---
  tidy_random <- broom.mixed::tidy(model, effects = "ran_pars") %>%
    dplyr::select(term, estimate) %>%
    dplyr::mutate(std.error = NA, statistic = NA)
  
  # Combine fixed + random
  all_terms <- dplyr::bind_rows(
    tidy_fixed %>% dplyr::mutate(effect_type = "fixed"),
    tidy_random %>% dplyr::mutate(effect_type = "random")
  )
  
  # --- Bootstrap CIs ---
  boot_ci <- confint(model, method = "boot", nsim = nboot)
  
  boot_df <- as.data.frame(boot_ci)
  boot_df$term <- rownames(boot_df)
  rownames(boot_df) <- NULL
  names(boot_df)[1:2] <- c("conf.low", "conf.high")
  
  all_terms <- all_terms %>%
    dplyr::left_join(boot_df, by = "term") %>%
    dplyr::mutate(model = model_name)
  
  # --- ANOVA table ---
  anova_df <- as.data.frame(anova(model)) %>%
    tibble::rownames_to_column("term") %>%
    dplyr::mutate(model = model_name)
  
  # --- return BOTH ---
  list(
    coefficients = all_terms,
    anova = anova_df
  )
}

model_terms_boot_all <- bind_rows(
  extract_model_terms_boot_all(model3_pc1, "Model3_PC1")$coefficients,
  extract_model_terms_boot_all(model3_pc2, "Model3_PC2")$coefficients,
  extract_model_terms_boot_all(model4_PC1, "Model4_PC1")$coefficients,
  extract_model_terms_boot_all(model4_PC2, "Model4_PC2")$coefficients,
  extract_model_terms_boot_all(model5_PC1, "Model5_PC1")$coefficients,
  extract_model_terms_boot_all(model5_PC2, "Model5_PC2")$coefficients
)


anova_all <- bind_rows(
  extract_model_terms_boot_all(model3_pc1, "Model3_PC1")$anova,
  extract_model_terms_boot_all(model3_pc2, "Model3_PC2")$anova,
  extract_model_terms_boot_all(model4_PC1, "Model4_PC1")$anova,
  extract_model_terms_boot_all(model4_PC2, "Model4_PC2")$anova,
  extract_model_terms_boot_all(model5_PC1, "Model5_PC1")$anova,
  extract_model_terms_boot_all(model5_PC2, "Model5_PC2")$anova
)

write.csv(model_terms_boot_all, "mixed_model_coefficients_boot_all.csv", row.names = FALSE)
write.csv(anova_all, "mixed_model_anova_all.csv", row.names = FALSE)