### Download necessary libraries ############
library(survival)
library(survminer)
library(dplyr)

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

