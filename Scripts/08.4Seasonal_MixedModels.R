wet_ds <- final_ds %>%
  filter(Season %in% c("Monsoon"))

wet_ds_2324 <- wet_ds  %>%
  filter(year %in% c("2023" ,"2024"))

dry_ds <- final_ds %>%
  filter(Season %in% c("Dry"))

dry_ds_2324 <- dry_ds  %>%
  filter(year %in% c("2023" ,"2024"))



model4_dry_PC1 <- lmer(PC1 ~ QBR_bin * TimeRangeFactor +
                     Strahler_Order * TimeRangeFactor +
                     (1 | Site), 
                   data = dry_ds)

model4_wet_PC1 <- lmer(PC1 ~ QBR_bin * TimeRangeFactor +
                         Strahler_Order * TimeRangeFactor +
                         (1 | Site), 
                       data = wet_ds)
anova(model4_dry_PC1)
anova(model4_wet_PC1)

