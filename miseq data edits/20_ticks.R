
# Load libraries
library(survival)
library(survminer)
library(dplyr)



# List of tick species columns
tick_columns <- c("rhipicephalus_appendiculatus_infestation")

# Define time window for early infection
early_window <- 51  # Change to 12 if needed

# Summarize tick load, merging "Severe" with "Present"
windowed_data <- final_miseq_data_clean %>%
  filter(sample_week <= early_window) %>%
  group_by(calf_id) %>%
  summarize(
    tick_load = case_when(
      any(across(all_of(tick_columns), ~ .x %in% c("Present", "Severe"))) ~ "Present",
      TRUE ~ "Absent"
    ),
    time_to_event = max(time_to_event, na.rm = TRUE),
    event = max(event, na.rm = TRUE)
  ) %>%
  ungroup()

# Convert tick_load to factor with only two levels
windowed_data$tick_load <- factor(windowed_data$tick_load, levels = c("Absent", "Present"))

# Convert time_to_event to numeric
windowed_data$time_to_event <- as.numeric(windowed_data$time_to_event)

# Check High vs. Low distribution
table(windowed_data$tick_load)


################################## KM plot #############################################

# Kaplan-Meier survival fit
km_fit <- survfit(Surv(time_to_event, event) ~ tick_load, data = windowed_data)

# Plot with censoring
ggsurvplot(km_fit, data = windowed_data,
           conf.int = TRUE, 
           pval = TRUE, 
           risk.table = TRUE, 
           censor = TRUE,  # Show censored data
           ggtheme = theme_minimal(),
           title = "Kaplan-Meier Survival Based on tick presence",
           palette = c("red", "blue"))

windowed_data$tick_load <- as.factor(windowed_data$tick_load)

# cox model
cox_model_tick <- coxph(Surv(time_to_event, event) ~ tick_load, data = windowed_data)

# Display summary of the Cox model
summary(cox_model_tick)

# Generate forest plot
ggforest(cox_model_tick, data = windowed_data)

################# Early vs late Infestation #####################################

# Define early infection cutoff
early_cutoff <- 26

# Identify earliest infestation before 26 weeks per calf
early_infestation_data <- final_miseq_data_clean %>%
  filter(sample_week <= early_cutoff & rhipicephalus_appendiculatus_infestation == "Present") %>%
  group_by(calf_id) %>%
  summarize(earliest_infestation_week = min(sample_week, na.rm = TRUE)) %>%
  ungroup()

# Merge back with main dataset
windowed_data <- final_miseq_data_clean %>%
  left_join(early_infestation_data, by = "calf_id") %>%
  mutate(
    early_infestation = ifelse(!is.na(earliest_infestation_week), "Yes", "No")  # If there's an early infestation, mark as Yes
  ) %>%
  group_by(calf_id) %>%
  summarize(
    early_infestation = first(early_infestation),
    time_to_event = max(time_to_event, na.rm = TRUE),
    event = max(event, na.rm = TRUE)
  ) %>%
  ungroup()

# Convert early_infestation to factor
windowed_data$early_infestation <- factor(windowed_data$early_infestation, levels = c("No", "Yes"))

# Kaplan-Meier survival fit for early infestation
km_fit_early <- survfit(Surv(time_to_event, event) ~ early_infestation, data = windowed_data)

# Plot Kaplan-Meier curve
ggsurvplot(km_fit_early, data = windowed_data,
           conf.int = TRUE, 
           pval = TRUE, 
           risk.table = TRUE, 
           censor = TRUE,  
           ggtheme = theme_minimal(),
           title = "Kaplan-Meier Survival Based on Early rhipicephalus_appendiculatus Infestation",
           palette = c("red", "blue"))

# Cox Proportional Hazards Model for early infestation
cox_model_early <- coxph(Surv(time_to_event, event) ~ early_infestation, data = windowed_data)

# Display summary of Cox model
summary(cox_model_early)

# Generate forest plot
ggforest(cox_model_early, data = windowed_data)

