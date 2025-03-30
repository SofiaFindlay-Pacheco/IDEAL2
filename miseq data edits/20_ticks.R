
# Load libraries
library(survival)
library(survminer)
library(dplyr)

##################################### Set up data #################################

# Clean up survival status column
final_miseq_data_clean$dead_or_alive_at_end_of_study <- as.factor(final_miseq_data_clean$dead_or_alive_at_end_of_study)

# Group all "Dead" statuses together
final_miseq_data_clean$dead_or_alive_at_end_of_study <- recode(final_miseq_data_clean$dead_or_alive_at_end_of_study,
                                                               "Dead: Infectious death" = "Dead",
                                                               "Dead: Death by trauma" = "Dead",
                                                               "Alive" = "Alive",
                                                               "Censored" = "Censored")

# Assign numeric event status (1 = Dead, 0 = Censored/Alive)
final_miseq_data_clean$event <- ifelse(final_miseq_data_clean$dead_or_alive_at_end_of_study == "Dead", 1, 0)

# Manually set one error
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(date_last_visit_with_data = case_when(
    calf_id == "CA020610160" ~ as.Date("2008-07-17"),  # Set specific date for this calf
    TRUE ~ date_last_visit_with_data  # Keep existing values for others
  ))

# Calculate survival time
# Ensure survival time is numeric
final_miseq_data_clean$date_last_visit_with_data <- as.Date(final_miseq_data_clean$date_last_visit_with_data)
final_miseq_data_clean$date_of_birth <- as.Date(final_miseq_data_clean$date_of_birth)

# Now subtract date of death - date of birth
final_miseq_data_clean$time_to_event <- as.numeric(final_miseq_data_clean$date_of_death - final_miseq_data_clean$date_of_birth)

# Handle the data without date of death, do it as date of last data - date of birth
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(time_to_event = ifelse(
    is.na(date_of_death) & event == 1, 
    as.numeric(date_last_visit_with_data - date_of_birth), 
    as.numeric(date_of_death - date_of_birth)
  ))
final_miseq_data_clean$time_to_event <- final_miseq_data_clean$time_to_event / 7

# Update the 'time_to_event' for alive calves (event == 0) to the max_week
max_week <- 51 
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(
    time_to_event = ifelse(event == 0, max_week, time_to_event)  # Set to max_week for alive calves
  )

# Adjust time_to_event only for censored cases
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(
    time_to_event = ifelse(
      dead_or_alive_at_end_of_study == "Censored",  # Only modify censored calves
      as.numeric(date_last_visit_with_data - date_of_birth) / 7,  # Time until last visit
      time_to_event  # Keep existing values for Alive & Dead calves
    )
  )

# List of tick species columns
tick_columns <- c("boophilus_spp_infestation")
                #  rhipicephalus_appendiculatus_infestation")

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
  filter(sample_week <= early_cutoff & boophilus_spp_infestation == "Present") %>%
  group_by(calf_id) %>%
  summarize(earliest_i0nfestation_week = min(sample_week, na.rm = TRUE)) %>%
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
           title = "Kaplan-Meier Survival Based on Early boophilus_spp Infestation",
           palette = c("red", "blue"))

# Cox Proportional Hazards Model for early infestation
cox_model_early <- coxph(Surv(time_to_event, event) ~ early_infestation, data = windowed_data)

# Display summary of Cox model
summary(cox_model_early)

# Generate forest plot
ggforest(cox_model_early, data = windowed_data)

