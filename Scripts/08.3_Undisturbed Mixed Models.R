
library(dplyr)


stream_ds <- final_ds %>%
  mutate(StreamGroup = ifelse(Strahler_Order <= 3, "Low (1–3)", "High (4–5)"))

model_PC2_stream <- lmer(
  PC2 ~ StreamGroup + QBR_bin + TimeRangeFactor + Season + (1 | Site),
  data = stream_ds
)

anova(model_PC2_stream)
summary(model_PC2_stream)


model_PC2_interaction <- lmer(
  PC2 ~ Strahler_Order * QBR_bin + TimeRangeFactor + Season + (1 | Site),
  data = final_ds
)

anova(model_PC2_interaction)



ggplot(stream_ds, aes(x = StreamGroup, y = PC2, fill = StreamGroup)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Stream Order Group", y = "PC2 (Acoustic Composition)")



model_threshold <- lmer(
  PC2 ~ StreamGroup + TimeRangeFactor + Season + (1 | Site),
  data = stream_ds
)

anova(model_threshold)
