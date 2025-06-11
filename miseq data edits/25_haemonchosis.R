library(dplyr)

# Filter only deaths from ECF
surv_data <- final_miseq_data_clean %>%
  filter(definitive_aetiological_cause == "Dead") %>%  # adjust if label is different
  group_by(calf_id) %>%
  summarise(
    time = max(sample_week, na.rm = TRUE),       # or use days from enrollment
    event = 1,                                    # all are events (died of ECF)
    haemonchosis = first(haemonchosis_coinfection)  # assumes same within calf
  )

library(survival)
library(survminer)

# Fit survival object
surv_obj <- Surv(time = surv_data$time, event = surv_data$event)

# Fit survival curves by haemonchosis presence
fit <- survfit(surv_obj ~ haemonchosis, data = surv_data)

# Plot survival curves
ggsurvplot(fit, data = surv_data,
           pval = TRUE,
           risk.table = TRUE,
           conf.int = TRUE,
           xlab = "Time (days)",
           title = "Survival Analysis: ECF Deaths by Haemonchosis Co-Infection",
           legend.title = "Haemonchosis Co-infection")









### Load libraries ###
library(survival)
library(survminer)
library(dplyr)

############################ KM plot: ECF Deaths & Haemonchosis ######################

# Step 1: Filter to only calves that died of ECF
ecf_only <- final_miseq_data_clean %>%
  filter(definitive_aetiological_cause == "Dead")  # adjust if needed

# Step 2: Summarize one row per calf
ecf_surv_data <- ecf_only %>%
  group_by(calf_id) %>%
  summarize(
    time_to_event = max(time_to_event, na.rm = TRUE),  # use appropriate time var
    event = max(event, na.rm = TRUE),                  # should be 1 for all rows here
    haemonchosis_coinfection = first(haemonchosis_coinfection)
  ) %>%
  ungroup()

# Step 3: Convert time to numeric if needed
ecf_surv_data$time_to_event <- as.numeric(ecf_surv_data$time_to_event)

# Step 4: Fit Kaplan-Meier model by haemonchosis co-infection status
km_fit_haem <- survfit(Surv(time_to_event, event) ~ haemonchosis_coinfection, data = ecf_surv_data)

# Step 5: Plot survival curve
ggsurvplot(km_fit_haem, data = ecf_surv_data,
           conf.int = TRUE, 
           pval = TRUE, 
           risk.table = TRUE, 
           censor = TRUE,
           ggtheme = theme_minimal(),
           title = "Survival Analysis: ECF Deaths by Haemonchosis Co-Infection",
           legend.title = "Haemonchosis Co-Infection",
           palette = c("orange", "darkgreen"))

