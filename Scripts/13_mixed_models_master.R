# Load libraries
library(tidyverse) # for data manipulation and plotting
library(lme4) # for mixed models
library(broom.mixed) # for looking at mixed model outputs
library(car) # for VIF (higher than 5 or 10 is bad)
library(lubridate) # for date/time manipulations
library(ggfortify) # for model diagnostic plots
library(MuMIn) # for r2 values GLMM
library(purrr) # For diel analysis
library(hms)
library(mgcv) # for GAMM models

# Data Read-In ----------------------------------------
datasets <- list(
  structured_ds    = read.csv("clean_data/datasets/PCAs/single_pca.csv"),
  global_ds        = read.csv("clean_data/datasets/PCAs/global2325_pca.csv"),
  rl_structured_ds = read.csv("clean_data/datasets/PCAs/rainless_single_pca.csv"),
  rl_global_ds     = read.csv("clean_data/datasets/PCAs/rainless_global2325_pca.csv")
)

#=======================================================#
# Helper: Bootstrap CI for fixed effects of lmer models #
#=======================================================#
boot_ci_lmer <- function(model, nsim = 2000){
  boot_obj <- bootMer(model, 
                      FUN = function(fit) fixef(fit),
                      nsim = nsim,
                      seed = 123,
                      use.u = FALSE,
                      type = "parametric")
  
  cis <- t(apply(boot_obj$t, 2, quantile, probs = c(0.025, 0.975)))
  out <- data.frame(
    term = names(fixef(model)),
    conf.low = cis[,1],
    conf.high = cis[,2],
    row.names = NULL
  )
  return(out)
}

# Function to run mixed models ----------------------------------------
run_pca_lmm <- function(df, dataset_name){
  
  message("\n===============================")
  message("▶ Running models for dataset: ", dataset_name)
  message("===============================\n")
  
  #=======================#
  #   CLEAN / MUTATE
  #=======================#
  message("Cleaning and preparing data...")
  df <- df %>%
    mutate(
      Season = case_when(
        Date >= 20231116 & Date <= 20231203 ~ "Nov 2023",
        Date >= 20231230 & Date <= 20240208 ~ "Jan 2024",
        Date >= 20240401 & Date <= 20240501 ~ "Apr 2024",
        Date >= 20240607 & Date <= 20240707 ~ "Jun 2024",
        Date >= 20250605 & Date <= 20250716 ~ "Jun 2025",
        TRUE ~ NA_character_
      ),
      Time = str_pad(Time, width = 6, side = "left", pad = "0"),
      Hour = as.numeric(substr(Time, 1, 2)) +
        as.numeric(substr(Time, 3, 4)) / 60 +
        as.numeric(substr(Time, 5, 6)) / 3600
    ) %>%
    filter(!is.na(Season))
  
  #=======================#
  #     FIT MODELS
  #=======================#
  message("Fitting linear model (LM) m1...")
  m1 <- lm(PC1 ~ QBR + Strahler, data = df)
  
  message("Fitting mixed model (LMM) m2 + reduced tests...")
  m2  <- lmer(PC1 ~ QBR_Class + Strahler + (1|Site) + (1|Season), data = df)
  m2B <- lmer(PC1 ~ Strahler + (1|Site) + (1|Season), data = df)
  m2C <- lmer(PC1 ~ QBR_Class + (1|Site) + (1|Season), data = df)
  
  message("Fitting GAMM m3 (smooth time) ...")
  m3 <- gamm(
    PC1 ~ QBR + Strahler + s(Hour, bs="cc"),
    random = list(Site = ~1, Season = ~1),
    data = df
  )
  
  message("Fitting GAMM m4 (interaction time*QBR) ...")
  m4 <- gamm(
    PC1 ~ Strahler + s(Hour, bs="cc") + ti(Hour, QBR, bs=c("cc","tp")),
    random = list(Site = ~1, Season = ~1),
    data = df
  )
  
  #=======================#
  #  BOOTSTRAP + CI EXTRACTION
  #=======================#
  message("Extracting confidence intervals...")
  
  message("LM CI...")
  m1_ci <- confint(m1) %>% 
    as.data.frame() %>%
    rownames_to_column("term") %>%
    rename(conf.low = `2.5 %`, conf.high = `97.5 %`)
  
  message("Bootstrapping LMM CI (may take a moment)...")
  m2_ci <- boot_ci_lmer(m2)
  
  message("GAMM CI...")
  m3_ci <- confint(m3$gam) %>% as.data.frame() %>% rownames_to_column("term")
  m4_ci <- confint(m4$gam) %>% as.data.frame() %>% rownames_to_column("term")
  names(m3_ci)[2:3] <- c("conf.low","conf.high")
  names(m4_ci)[2:3] <- c("conf.low","conf.high")
  
  message("✔ Completed all models for: ", dataset_name)
  
  return(list(
    dataset = dataset_name,
    
    model1_fixed = broom.mixed::tidy(m1),
    model1_ci = m1_ci,
    
    model2_fixed = broom.mixed::tidy(m2),
    model2_random = broom.mixed::tidy(m2, effects = "ran_pars"),
    model2_ci = m2_ci,
    model2_r2 = as.data.frame(r.squaredGLMM(m2)),
    model2_LRT_QBR = broom.mixed::tidy(anova(m2, m2B)),
    model2_LRT_Strahler = broom.mixed::tidy(anova(m2, m2C)),
    
    model3_fixed = broom.mixed::tidy(m3$gam),
    model3_random = broom.mixed::tidy(m3$lme, effects="ran_pars"),
    model3_ci = m3_ci,
    model3_r2 = data.frame(R2 = summary(m3$gam)$r.sq),
    
    # FIXED HERE
    model3_LRT_time = anova(m3$gam)$anova_table %>%
      as.data.frame() %>%
      rownames_to_column("term"),
    
    model4_fixed = broom.mixed::tidy(m4$gam),
    model4_random = broom.mixed::tidy(m4$lme, effects="ran_pars"),
    model4_ci = m4_ci,
    model4_r2 = data.frame(R2 = summary(m4$gam)$r.sq),
    
    # FIXED HERE
    model4_LRT_interaction = anova(m4$gam)$anova_table %>%
      as.data.frame() %>%
      rownames_to_column("term")
  ))
}

# Run Model -----------------------
model_outputs <- imap(datasets, run_pca_lmm)  # purrr::imap provides names!

# Save Model Outputs
save_model_outputs <- function(out){
  folder <- paste0("clean_data/datasets/model_outputs/", out$dataset)
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)
  
  message("\n Saving outputs for dataset: ", out$dataset)
  
  walk(names(out)[-1], ~ {
    write.csv(out[[.x]], file = paste0(folder, "/", .x, ".csv"), row.names = FALSE)
    message("  ✔ Saved ", .x)
  })
}
save_model_outputs

walk(model_outputs, save_model_outputs)
