library(dplyr)
library(fixest)

# Step 1: crea la dummy "high_nl" per ogni anno
final_regression_data <- final_regression_data %>%
  group_by(year) %>%
  mutate(med_nl = median(nightlight_norm, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(high_nl = ifelse(nightlight_norm > med_nl, 1, 0))

# Step 2: crea l'interazione tra indice2 e la dummy
final_regression_data <- final_regression_data %>%
  mutate(indice2_highNL = indice2 * high_nl)

# Step 3: stima il modello con l'interazione
model_interaction <- feols(n_conflicts ~ indice2 * high_nl + nightlight_norm | ADM2_NAME + year, 
                           data = final_regression_data)

summary(model_interaction)

# Step 1: crea la dummy "high_nl" usando il terzo quartile per ogni anno
final_regression_data <- final_regression_data %>%
  group_by(year) %>%
  mutate(q3_nl = quantile(nightlight_norm, probs = 0.75, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(high_nl = ifelse(nightlight_norm > q3_nl, 1, 0))

# Step 2: interazioni esplicite con indice2
final_regression_data <- final_regression_data %>%
  mutate(
    indice2_highNL = indice2 * high_nl,
    indice2_lowNL  = indice2 * (1 - high_nl)
  )

# Step 3: regressioni
model_interaction1 <- feols(
  n_conflicts ~ indice2 * high_nl + nightlight_norm | ADM2_NAME + year,
  data = final_regression_data
)
summary(model_interaction1)

model_interaction2 <- feols(
  n_conflicts ~ indice2_highNL + indice2_lowNL | ADM2_NAME + year,
  data = final_regression_data
)
summary(model_interaction2)

