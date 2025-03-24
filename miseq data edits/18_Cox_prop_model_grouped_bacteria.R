
library(survival)
library(survminer)
library(ggplot2)
######### Create Grouped Bacterial Presence Variables (Binary: 1 = Present, 0 = Absent) ############

# Anaplasma Presence (If ANY of the 9 Anaplasma columns have a value > Threshold, mark as 1)
final_miseq_data_clean$anaplasma_present <- ifelse(
  rowSums(select(final_miseq_data_clean, contains("anaplasma")), na.rm = TRUE) > 1000, 1, 0)

# Babesia Presence
final_miseq_data_clean$babesia_present <- ifelse(
  rowSums(select(final_miseq_data_clean, starts_with("babesia")), na.rm = TRUE) > 500, 1, 0)

# Theileria Presence
final_miseq_data_clean$theileria_present <- ifelse(
  rowSums(select(final_miseq_data_clean, starts_with("theileria")), na.rm = TRUE) > 500, 1, 0)

# Ehrlichia Presence
final_miseq_data_clean$ehrlichia_present <- ifelse(
  rowSums(select(final_miseq_data_clean, starts_with("ehrlichia")), na.rm = TRUE) > 1000, 1, 0)

########## Cox Proportional Hazards Models with Clustering (Calf ID) ####################

#Ensure variables are in the right format
final_miseq_data_clean$anaplasma_present <- as.factor(final_miseq_data_clean$anaplasma_present)
final_miseq_data_clean$babesia_present <- as.factor(final_miseq_data_clean$babesia_present)
final_miseq_data_clean$theileria_present <- as.factor(final_miseq_data_clean$theileria_present)
final_miseq_data_clean$ehrlichia_present <- as.factor(final_miseq_data_clean$ehrlichia_present)

# Clean up survival status column
final_miseq_data_clean$dead_or_alive_at_end_of_study <- as.factor(final_miseq_data_clean$dead_or_alive_at_end_of_study)

# Group all "Dead" statuses together
final_miseq_data_clean$dead_or_alive_at_end_of_study <- recode(final_miseq_data_clean$dead_or_alive_at_end_of_study,
                                                               "Dead: Infectious death" = "Dead",
                                                               "Dead: Death by trauma" = "Dead",
                                                               "Alive" = "Alive")

final_miseq_data_clean$event <- ifelse(final_miseq_data_clean$dead_or_alive_at_end_of_study == "Dead", 1, 0)
final_miseq_data_clean$event <- as.numeric(final_miseq_data_clean$event)


#Set up event
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

# Cox Model: Anaplasma (with clustering on calf ID)
cox_anaplasma <- coxph(Surv(time_to_event, event) ~ anaplasma_present + cluster(calf_id), data = final_miseq_data_clean)
print(summary(cox_anaplasma))

# Cox Model: Babesia (Clustering on Calf ID)
cox_babesia <- coxph(Surv(time_to_event, event) ~ babesia_present + cluster(calf_id), data = final_miseq_data_clean)
print(summary(cox_babesia))

# Cox Model: Theileria (Clustering on Calf ID)
cox_theileria <- coxph(Surv(time_to_event, event) ~ theileria_present + cluster(calf_id), data = final_miseq_data_clean)
print(summary(cox_theileria))

# Cox Model: Ehrlichia (Clustering on Calf ID)
cox_ehrlichia <- coxph(Surv(time_to_event, event) ~ ehrlichia_present + cluster(calf_id), data = final_miseq_data_clean)
print(summary(cox_ehrlichia))

# Attempt to make my own plot

# Create a data frame with results
hr_df <- data.frame(
  Variable = c("Anaplasma", "Babesia", "Theileria", "Ehrlichia"),
  HR = c(0.4294, 0.90, 0.85, 0.78),  # Hazard Ratios
  Lower = c(0.3242, 0.70, 0.65, 0.55),  # Lower 95% CI
  Upper = c(0.5688, 1.20, 1.10, 1.05),  # Upper 95% CI
  p_value = c(0.001, 0.45, 0.30, 0.08),  # P-values
  N = c(21236, 18000, 19500, 20000),  # Sample size for each
  Events = c(1848, 900, 1000, 1100)  # Number of events
)

# Model statistics
global_p_value <- 3.2774e-27
aic <- 27865.46
concordance <- 0.55

# Plot
ggplot(hr_df, aes(x = Variable, y = HR)) +
  geom_point(size = 4, color = "black") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1, color = "black") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Hazard Ratios for Bacterial Presence", y = "Hazard Ratio (HR)", x = "") +
  scale_y_continuous(limits = c(0.2, 1.5), breaks = seq(0.2, 1.5, 0.2)) +
  
  # HR & CI values next to each point
  geom_text(aes(label = sprintf("%.2f (%.2f - %.2f)", HR, Lower, Upper)), 
            hjust = -0.3, vjust = 0, size = 4) +
  
  # p-values formatted nicely
  geom_text(aes(label = paste("p =", ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value)))),
            hjust = -1.5, vjust = 0, size = 4) +
  
  # Add N and Events
  geom_text(aes(label = paste("N =", N, "\nEvents:", Events)), 
            hjust = 1.2, vjust = 0, size = 3.5, color = "gray30") +
  
  # Add global model statistics as a caption
  labs(
    caption = paste(
      "# Events:", sum(hr_df$Events), 
      "; Global p-value (Log-Rank):", signif(global_p_value, 3), 
      "\nAIC:", round(aic, 2), 
      "; Concordance Index:", round(concordance, 2)
    )
  ) +
  theme(
    plot.caption = element_text(size = 10, hjust = 0, face = "italic"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )
