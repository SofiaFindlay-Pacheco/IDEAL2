# Load necessary libraries
library(survival)
library(survminer)
library(dplyr)

# upload file path of sublocation data through farm data from IDEAL
sublocation_data <- "C:/Users/sofia/OneDrive - University of Edinburgh/master/R studio/IDEAL statistics/Edited original data/ideal_farm.xlsx"
sublocation_data <- read_excel(sublocation_data)
sublocation_data_clean <- sublocation_data %>% clean_names()


# Merge Sublocation data
final_miseq_data_clean <- final_miseq_data_clean %>%
  left_join(sublocation_data_clean %>% select(calf_id, sublocation), by = "calf_id")

# Clean up survival status column
final_miseq_data_clean$dead_or_alive_at_end_of_study <- as.factor(final_miseq_data_clean$dead_or_alive_at_end_of_study)

# Group "Dead" statuses together
final_miseq_data_clean$dead_or_alive_at_end_of_study <- recode(final_miseq_data_clean$dead_or_alive_at_end_of_study,
                                                               "Dead: Infectious death" = "Dead",
                                                               "Dead: Death by trauma" = "Dead",
                                                               "Alive" = "Alive")

# Remove censored data
final_miseq_data_clean <- final_miseq_data_clean %>% filter(dead_or_alive_at_end_of_study != "Censored")

# Ensure variables are in the correct format
final_miseq_data_clean$time_to_event <- as.numeric(final_miseq_data_clean$sample_week)
final_miseq_data_clean$event <- ifelse(final_miseq_data_clean$dead_or_alive_at_end_of_study == "Dead", 1, 0)

# Convert categorical variables
final_miseq_data_clean$sublocation <- as.factor(final_miseq_data_clean$sublocation)  # Convert sublocation to a factor

# Create bacteria presence variables (1 = Present, 0 = Absent)
final_miseq_data_clean$anaplasma_present <- ifelse(rowSums(select(final_miseq_data_clean, starts_with("anaplasma")), na.rm = TRUE) > 0, 1, 0)
final_miseq_data_clean$babesia_present <- ifelse(rowSums(select(final_miseq_data_clean, starts_with("babesia")), na.rm = TRUE) > 0, 1, 0)
final_miseq_data_clean$theileria_present <- ifelse(rowSums(select(final_miseq_data_clean, starts_with("theileria")), na.rm = TRUE) > 0, 1, 0)
final_miseq_data_clean$ehrlichia_present <- ifelse(rowSums(select(final_miseq_data_clean, starts_with("ehrlichia")), na.rm = TRUE) > 0, 1, 0)

# Fit Cox Proportional Hazards Model with Frailty (Random Effect for Sublocation)
cox_frailty <- coxph(Surv(time_to_event, event) ~ anaplasma_present + babesia_present + 
                       theileria_present + ehrlichia_present + frailty(sublocation), 
                     data = final_miseq_data_clean)

cox_interaction_sub <- coxph(Surv(time_to_event, event) ~ 
                           anaplasma_present * sublocation + 
                           babesia_present * sublocation + 
                           theileria_present * sublocation + 
                           ehrlichia_present * sublocation, 
                         data = final_miseq_data_clean)
# Display model summary
summary(cox_interaction_sub)

# Visualize Hazard Ratios using ggforest
ggforest(cox_interaction_sub, data = final_miseq_data_clean)
############
# Load required libraries
library(survival)
library(survminer)
library(dplyr)

# Ensure the dataset has the required columns
# final_miseq_data_clean should contain 'time_to_event', 'death', and 'sublocation'
# 'time_to_event' is the time (e.g., weeks) until death or censoring
# 'death' is the survival outcome (1 = dead, 0 = alive)
# 'sublocation' is the categorical variable for sublocation

# Fit a Cox proportional hazards model with a frailty term for sublocation
cox_model <- coxph(Surv(time_to_event, death) ~ anaplasma_present + 
                     babesia_present + theileria_present + ehrlichia_present + 
                     frailty(sublocation), 
                   data = final_miseq_data_clean)

# Print summary of the model
summary(cox_model)

# Extract the variance of the frailty term
frailty_variance <- cox_model$frailvar
print(paste("Variance of the sublocation effect:", round(frailty_variance, 3)))

# Plot hazard ratios
ggforest(cox_model, data = final_miseq_data_clean)

# Calculate the hazard ratio range for sublocation
sublocation_HR_range <- exp(c(-1.96 * sqrt(frailty_variance), 1.96 * sqrt(frailty_variance)))
print(paste("Hazard Ratio range due to sublocation effect:", round(sublocation_HR_range[1], 2), "to", round(sublocation_HR_range[2], 2)))

fit_km <- survfit(Surv(time_to_event, event) ~ sublocation, data = final_miseq_data_clean)
ggsurvplot(fit_km, data = final_miseq_data_clean, pval = TRUE, conf.int = TRUE)
