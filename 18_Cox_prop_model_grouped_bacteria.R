
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

# ====== 🦠 Create Grouped Bacterial Presence Variables (Binary: 1 = Present, 0 = Absent) ======

# Anaplasma Group
final_miseq_data_clean$anaplasma_present <- ifelse(
  rowSums(select(final_miseq_data_clean, contains("anaplasma")), na.rm = TRUE) > 0, 1, 0)

# Babesia Group
final_miseq_data_clean$babesia_present <- ifelse(
  rowSums(select(final_miseq_data_clean, starts_with("babesia")), na.rm = TRUE) > 0, 1, 0)

# Theileria Group
final_miseq_data_clean$theileria_present <- ifelse(
  rowSums(select(final_miseq_data_clean, starts_with("theileria")), na.rm = TRUE) > 0, 1, 0)

# Ehrlichia Group
final_miseq_data_clean$ehrlichia_present <- ifelse(
  rowSums(select(final_miseq_data_clean, starts_with("ehrlichia")), na.rm = TRUE) > 0, 1, 0)

# ====== 🏥 Run Cox Proportional Hazards Models for Each Bacteria Group ======

# Cox Model: Anaplasma
cox_anaplasma <- coxph(Surv(time_to_event, event) ~ anaplasma_present, data = final_miseq_data_clean)
print(summary(cox_anaplasma))
ggforest(cox_anaplasma, data = final_miseq_data_clean)

# Cox Model: Babesia
cox_babesia <- coxph(Surv(time_to_event, event) ~ babesia_present, data = final_miseq_data_clean)
print(summary(cox_babesia))
ggforest(cox_babesia, data = final_miseq_data_clean)

# Cox Model: Theileria
cox_theileria <- coxph(Surv(time_to_event, event) ~ theileria_present, data = final_miseq_data_clean)
print(summary(cox_theileria))
ggforest(cox_theileria, data = final_miseq_data_clean)

# Cox Model: Ehrlichia
cox_ehrlichia <- coxph(Surv(time_to_event, event) ~ ehrlichia_present, data = final_miseq_data_clean)
print(summary(cox_ehrlichia))
ggforest(cox_ehrlichia, data = final_miseq_data_clean)

