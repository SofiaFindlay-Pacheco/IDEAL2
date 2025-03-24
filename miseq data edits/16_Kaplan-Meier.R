
##############################
# Load libraries
library(survival)
library(survminer)
library(dplyr)

# Clean up survival status column
final_miseq_data_clean$dead_or_alive_at_end_of_study <- as.factor(final_miseq_data_clean$dead_or_alive_at_end_of_study)

# Group all "Dead" statuses together
final_miseq_data_clean$dead_or_alive_at_end_of_study <- recode(final_miseq_data_clean$dead_or_alive_at_end_of_study,
                                                               "Dead: Infectious death" = "Dead",
                                                               "Dead: Death by trauma" = "Dead",
                                                               "Alive" = "Alive")

# Assign numeric event status (1 = Dead, 0 = Censored/Alive)
final_miseq_data_clean$event <- ifelse(final_miseq_data_clean$dead_or_alive_at_end_of_study == "Dead", 1, 0)

# Ensure survival time is numeric
final_miseq_data_clean$date_last_visit_with_data <- as.Date(final_miseq_data_clean$date_last_visit_with_data)
final_miseq_data_clean$date_of_birth <- as.Date(final_miseq_data_clean$date_of_birth)

# Now subtract safely
final_miseq_data_clean$time_to_event <- as.numeric(final_miseq_data_clean$date_of_death - final_miseq_data_clean$date_of_birth)
# Handle the deaths without date of death
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(time_to_event = ifelse(
    is.na(date_of_death) & event == 1, 
    as.numeric(date_last_visit_with_data - date_of_birth), 
    as.numeric(date_of_death - date_of_birth)
  ))
final_miseq_data_clean$time_to_event <- final_miseq_data_clean$time_to_event / 7

# List of bacterial species columns
bacteria_columns <- c(
  "anaplasma_bovis_u03775_ae", "anaplasma_bovis_ab983439_ae", "anaplasma_marginale_cp000030_ae",
  "anaplasma_platys_like_ku585990_ae", "anaplasma_phagocytophilum_u02521_ae", 
  "candidatus_anaplasma_boleense_ku586025_ae", "uncultured_anaplasma_sp_clone_saso_ky924885_ae",
  "uncultured_anaplasma_sp_jn862825_ae"
)

# Define time window for early infection
early_window <- 51  # Change to 12 if needed

# Summarize bacterial load for the first 15 weeks per calf (Without Inflating Load)
windowed_data <- final_miseq_data_clean %>%
  filter(sample_week <= early_window) %>%
  group_by(calf_id) %>%
  summarize(
    bacterial_load = ifelse(any(across(all_of(bacteria_columns), ~ sum(.x > 1000, na.rm = TRUE) > 0)), "High", "Low"),
    time_to_event = max(sample_week, na.rm = TRUE),
    event = max(event, na.rm = TRUE)
  ) %>%
  ungroup()

# Convert time_to_event to numeric
windowed_data$time_to_event <- as.numeric(windowed_data$time_to_event)

# Check High vs. Low distribution
table(windowed_data$bacterial_load)

# Kaplan-Meier survival fit
km_fit <- survfit(Surv(time_to_event, event) ~ bacterial_load, data = windowed_data)

# Plot with censoring
ggsurvplot(km_fit, data = windowed_data,
           conf.int = TRUE, 
           pval = TRUE, 
           risk.table = TRUE, 
           censor = TRUE,  # Show censored data
           ggtheme = theme_minimal(),
           title = "Kaplan-Meier Survival Based on Early Infection (First 10 Weeks) to Anaplasma species",
           palette = c("red", "blue"))

##########################
###############################
#####################
# Load libraries
library(survival)
library(survminer)
library(dplyr)

# Clean up survival status column
final_miseq_data_clean$dead_or_alive_at_end_of_study <- as.factor(final_miseq_data_clean$dead_or_alive_at_end_of_study)

# Group all "Dead" statuses together
final_miseq_data_clean$dead_or_alive_at_end_of_study <- recode(final_miseq_data_clean$dead_or_alive_at_end_of_study,
                                                               "Dead: Infectious death" = "Dead",
                                                               "Dead: Death by trauma" = "Dead",
                                                               "Alive" = "Alive")

# Assign numeric event status (1 = Dead, 0 = Censored/Alive)
final_miseq_data_clean$event <- ifelse(final_miseq_data_clean$dead_or_alive_at_end_of_study == "Dead", 1, 0)

# Ensure survival time is numeric
final_miseq_data_clean$time_to_event <- as.numeric(final_miseq_data_clean$sample_week)

# List of bacterial species columns
bacteria_columns <- c(
  "anaplasma_bovis_u03775_ae", "anaplasma_bovis_ab983439_ae", "anaplasma_marginale_cp000030_ae",
  "anaplasma_platys_like_ku585990_ae", "anaplasma_phagocytophilum_u02521_ae", 
  "candidatus_anaplasma_boleense_ku586025_ae", "uncultured_anaplasma_sp_clone_saso_ky924885_ae",
  "uncultured_anaplasma_sp_jn862825_ae", "anaplasma_platys_ef139459_ae"
)

# Define time window for early infection
early_window <- 51  # Change to 12 if needed
threshold <- 1000   # Bacterial load threshold

# Loop through each Anaplasma species
for (bacteria in bacteria_columns) {
  cat("\nRunning Kaplan-Meier for:", bacteria, "\n")
  
  # Classify bacterial load per calf for the current species
  species_data <- final_miseq_data_clean %>%
    filter(sample_week <= early_window) %>%
    group_by(calf_id) %>%
    summarize(
      bacterial_load = ifelse(any(get(bacteria) > threshold, na.rm = TRUE), "High", "Low"),
      time_to_event = max(sample_week, na.rm = TRUE),
      event = max(event, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Convert time_to_event to numeric
  species_data$time_to_event <- as.numeric(species_data$time_to_event)
  
  # Kaplan-Meier survival analysis
  km_fit <- survfit(Surv(time_to_event, event) ~ bacterial_load, data = species_data)
  
  # Plot results
  print(ggsurvplot(km_fit, data = species_data,
                   conf.int = TRUE, pval = TRUE, risk.table = TRUE,
                   censor = TRUE,  # Show censored data
                   ggtheme = theme_minimal(),
                   title = paste("Kaplan-Meier Survival for", bacteria),
                   palette = c("red", "blue")))
}
