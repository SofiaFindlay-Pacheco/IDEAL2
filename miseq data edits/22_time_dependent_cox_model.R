# Load required libraries
library(survival)
library(survminer)
library(dplyr)
library(tidyr)

# Create a long-format dataset with time-dependent covariates
time_varying_data <- final_miseq_data_clean %>%
  pivot_longer(cols = c(anaplasma_present, babesia_present, theileria_present, ehrlichia_present),
               names_to = "bacteria",
               values_to = "presence") %>%
  arrange(calf_id, sample_week)  # Ensure data is ordered by time

# Convert presence variable to numeric (1 = Present, 0 = Absent)
time_varying_data$presence <- as.numeric(time_varying_data$presence)

# Create bacteria presence variables (1 = Present, 0 = Absent)
final_miseq_data_clean$anaplasma_present <- ifelse(rowSums(select(final_miseq_data_clean, starts_with("anaplasma")), na.rm = TRUE) > 0, 1, 0)
final_miseq_data_clean$babesia_present <- ifelse(rowSums(select(final_miseq_data_clean, starts_with("babesia")), na.rm = TRUE) > 0, 1, 0)
final_miseq_data_clean$theileria_present <- ifelse(rowSums(select(final_miseq_data_clean, starts_with("theileria")), na.rm = TRUE) > 0, 1, 0)
final_miseq_data_clean$ehrlichia_present <- ifelse(rowSums(select(final_miseq_data_clean, starts_with("ehrlichia")), na.rm = TRUE) > 0, 1, 0)

# Ensure sample_week is numeric
time_varying_data <- time_varying_data %>%
  mutate(sample_week = as.numeric(sample_week)) %>%  # Convert sample_week to numeric
  group_by(calf_id, bacteria) %>%
  mutate(start_time = lag(sample_week, default = 0),  # Lag requires numeric values
         stop_time = sample_week) %>%
  ungroup()

# Create start and stop time for each infection period
time_varying_data <- time_varying_data %>%
  group_by(calf_id, bacteria) %>%
  mutate(start_time = lag(sample_week, default = 0),
         stop_time = sample_week) %>%
  ungroup()

# Fit Cox model with time-dependent covariates
cox_time_dependent_no_cluster <- coxph(Surv(start_time, stop_time, event) ~ presence, 
                                       data = time_varying_data)

# Plot the forest plot
ggforest(cox_time_dependent_no_cluster, data = time_varying_data)


# Extract summary of the model with clustering
cox_summary <- summary(cox_time_dependent)

# Create a dataframe for ggplot
forest_data <- data.frame(
  Variable = rownames(cox_summary$coefficients),
  HR = cox_summary$coefficients[, 2],  # Hazard Ratio
  Lower_CI = cox_summary$conf.int[, 3],  # Lower 95% CI
  Upper_CI = cox_summary$conf.int[, 4]   # Upper 95% CI
)

# Load ggplot2 for manual forest plot
library(ggplot2)

ggplot(forest_data, aes(x = HR, y = Variable)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +  # Reference line
  xlab("Hazard Ratio") +
  ylab("Variable") +
  theme_minimal()
