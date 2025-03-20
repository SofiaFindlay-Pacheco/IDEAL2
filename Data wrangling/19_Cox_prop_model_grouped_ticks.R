
###############
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

# ====== 🎯 Convert Tick Infestation Columns to Numeric ======
tick_columns <- c("rhipicephalus_appendiculatus_infestation", "amblyomma_spp_infestation")

# Define levels and convert
final_miseq_data_clean[tick_columns] <- lapply(final_miseq_data_clean[tick_columns], function(x) {
  factor(x, levels = c("Absent", "Present", "Severe"), labels = c(0, 1, 2)) %>% as.numeric()
})

# ====== 🏥 Run Cox Proportional Hazards Models for Each Tick Group ======

# Cox Model: Rhipicephalus Appendiculatus Infestation
cox_rhipicephalus <- coxph(Surv(time_to_event, event) ~ rhipicephalus_appendiculatus_infestation, 
                           data = final_miseq_data_clean)
print(summary(cox_rhipicephalus))
ggforest(cox_rhipicephalus, data = final_miseq_data_clean)

# Cox Model: Amblyomma spp. Infestation
cox_amblyomma <- coxph(Surv(time_to_event, event) ~ amblyomma_spp_infestation, 
                       data = final_miseq_data_clean)
print(summary(cox_amblyomma))
ggforest(cox_amblyomma, data = final_miseq_data_clean)

