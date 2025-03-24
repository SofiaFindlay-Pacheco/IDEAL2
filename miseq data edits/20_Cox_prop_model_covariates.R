# Load libraries
library(survival)
library(survminer)
library(dplyr)

# Clean up survival status column
final_miseq_data_clean$dead_or_alive_at_end_of_study <- as.factor(final_miseq_data_clean$dead_or_alive_at_end_of_study)

# Group "Dead" statuses together
final_miseq_data_clean$dead_or_alive_at_end_of_study <- recode(final_miseq_data_clean$dead_or_alive_at_end_of_study,
                                                               "Dead: Infectious death" = "Dead",
                                                               "Dead: Death by trauma" = "Dead",
                                                               "Alive" = "Alive")

# Remove censored data
final_miseq_data_clean <- final_miseq_data_clean %>% filter(dead_or_alive_at_end_of_study != "Censored")

# Convert survival time and event indicator
final_miseq_data_clean$time_to_event <- as.numeric(final_miseq_data_clean$sample_week)
final_miseq_data_clean$event <- ifelse(final_miseq_data_clean$dead_or_alive_at_end_of_study == "Dead", 1, 0)

# Convert Sex (M = 0, F = 1)
final_miseq_data_clean$calf_sex <- ifelse(final_miseq_data_clean$calf_sex == "M", 0, 1)

# Create Binary Variables for Bacteria Groups
final_miseq_data_clean$anaplasma_present <- ifelse(
  rowSums(select(final_miseq_data_clean, starts_with("anaplasma")), na.rm = TRUE) > 0, 1, 0)

final_miseq_data_clean$babesia_present <- ifelse(
  rowSums(select(final_miseq_data_clean, starts_with("babesia")), na.rm = TRUE) > 0, 1, 0)

final_miseq_data_clean$theileria_present <- ifelse(
  rowSums(select(final_miseq_data_clean, starts_with("theileria")), na.rm = TRUE) > 0, 1, 0)

final_miseq_data_clean$ehrlichia_present <- ifelse(
  rowSums(select(final_miseq_data_clean, starts_with("ehrlichia")), na.rm = TRUE) > 0, 1, 0)

# ====== 🏥 Run Cox Proportional Hazards Model for Bacteria & Sex ======

cox_interaction <- coxph(Surv(time_to_event, event) ~ 
                           anaplasma_present * calf_sex + 
                           babesia_present * calf_sex + 
                           theileria_present * calf_sex + 
                           ehrlichia_present * calf_sex, 
                         data = final_miseq_data_clean)

summary(cox_interaction)
ggforest(cox_interaction, data = final_miseq_data_clean)
