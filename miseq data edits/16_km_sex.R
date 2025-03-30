### Download necessary libraries ############
library(survival)
library(survminer)
library(dplyr)

############################### Data prep ##########################################``
# Clean up survival status column
final_miseq_data_clean$dead_or_alive_at_end_of_study <- as.factor(final_miseq_data_clean$dead_or_alive_at_end_of_study)

# Group all "Dead" statuses together
final_miseq_data_clean$dead_or_alive_at_end_of_study <- recode(final_miseq_data_clean$dead_or_alive_at_end_of_study,
                                                               "Dead: Infectious death" = "Dead",
                                                               "Dead: Death by trauma" = "Censored",
                                                               "Alive" = "Alive",
                                                               "Censored" = "Censored")

# Assign numeric event status (1 = Dead, 0 = Censored/Alive)
final_miseq_data_clean$event <- ifelse(final_miseq_data_clean$dead_or_alive_at_end_of_study == "Dead", 1, 0)

# Calculate survival time
# Ensure survival time is numeric
final_miseq_data_clean$date_last_visit_with_data <- as.Date(final_miseq_data_clean$date_last_visit_with_data)
final_miseq_data_clean$date_of_birth <- as.Date(final_miseq_data_clean$date_of_birth)

# Now subtract date of death - date of birth
final_miseq_data_clean$time_to_event <- as.numeric(final_miseq_data_clean$date_of_death - final_miseq_data_clean$date_of_birth)

# Handle the data without date of death, do it as date of last data - dat of birth
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(time_to_event = ifelse(
    is.na(date_of_death) & event == 1, 
    as.numeric(date_last_visit_with_data - date_of_birth), 
    as.numeric(date_of_death - date_of_birth)
  ))
final_miseq_data_clean$time_to_event <- final_miseq_data_clean$time_to_event / 7

# Manually set one error
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(date_last_visit_with_data = case_when(
    calf_id == "CA020610160" ~ as.Date("2008-07-17"),  # Set specific date for this calf
    TRUE ~ date_last_visit_with_data  # Keep existing values for others
  ))

# Update the 'time_to_event' for alive calves (event == 0) to the max_week
max_week <- 51
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(
    time_to_event = ifelse(event == 0, max_week, time_to_event)  # Set to max_week for alive calves
  )

# Convert 'calf_Sex' to a factor
final_miseq_data_clean$calf_sex <- as.factor(final_miseq_data_clean$calf_sex)

# Adjust time_to_event only for censored cases
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(
    time_to_event = ifelse(
      dead_or_alive_at_end_of_study == "Censored",  # Only modify censored calves
      as.numeric(date_last_visit_with_data - date_of_birth) / 7,  # Time until last visit
      time_to_event  # Keep existing values for Alive & Dead calves
    )
  )

############################ KM plot ######################

# Summarize survival data grouped by calf_id
windowed_data_sex <- final_miseq_data_clean %>%
  group_by(calf_id) %>%
  summarize(
    calf_sex = first(calf_sex),  # Get sex for each calf
    time_to_event = max(time_to_event, na.rm = TRUE),
    event = max(event, na.rm = TRUE)  # Retains event=0 for alive & censored
  ) %>%
  ungroup()

# Convert 'time_to_event' to numeric
windowed_data_sex$time_to_event <- as.numeric(windowed_data_sex$time_to_event)

# Kaplan-Meier survival fit based on sex
km_fit_sex <- survfit(Surv(time_to_event, event) ~ calf_sex, data = windowed_data_sex)

# Plot Kaplan-Meier curve
ggsurvplot(km_fit_sex, data = windowed_data_sex,
           conf.int = TRUE, 
           pval = TRUE, 
           risk.table = TRUE, 
           censor = TRUE,  # Show censored data with vertical dashes
           ggtheme = theme_minimal(),
           title = "Kaplan-Meier Survival Curve: Impact of Calf Sex on Mortality",
           palette = c("red", "blue"))

# cox model
cox_model_sex <- coxph(Surv(time_to_event, event) ~ calf_sex, data = windowed_data_sex)

# Display summary of the Cox model
summary(cox_model_sex)

# Generate forest plot
ggforest(cox_model_sex, data = windowed_data_sex)

