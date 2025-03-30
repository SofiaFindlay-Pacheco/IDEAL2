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

# List of bacterial species columns
#bacteria_columns <- c( "theileria_mutans_af078815_tb"   ,                                                      
 #                      "theileria_sp_strain_msd_af078816_tb"       ,                                           
  #                     "theileria_parva_l02366_tb"                 ,                                           
   #                    "theileria_taurotragi_l19082_tb"            ,                                           
    #                   "theileria_velifera_af097993_tb" )


#bacteria_columns <- c( "anaplasma_bovis_u03775_ae"    ,                                                        
 #          "anaplasma_bovis_ab983439_ae",                                                       
  #        "anaplasma_marginale_cp000030_ae",                                                      
   #      "anaplasma_platys_like_ku585990_ae",                                                    
    #  "anaplasma_phagocytophilum_u02521_ae",                                                  
#       "candidatus_anaplasma_boleense_ku586025_ae"        ,                                    
 #     "uncultured_anaplasma_sp_clone_saso_ky924885_ae"     ,                                  
  #   "uncultured_anaplasma_sp_jn862825_ae",
#    "anaplasma_platys_ef139459_ae")

bacteria_columns <- c( "ehrlichia_sp_tibet_ehrlichia_canis_ehrlichia_minasensis_af414399_ay394465_mt163430_ae"  ,
      "ehrlichia_ruminantium_x61659_ae")

#bacteria_columns <- c( "babesia_bigemina_ay603402_tb"  ,                                                       
 #           "babesia_bigemina_lk391709_tb"  ,                                                       
  #         "babesia_bigemina_ku206291_tb"  ,
   #        "babesia_bovis_kf928959_tb"     ,                                                      
    #      "babesia_bovis_aaxt01000002_tb" ,                                                   
     #    "babesia_bovis_ay603398_tb"     ,                                                  
      #  "babesia_bovis_jq437260_tb" )

# Define time window for early infection
early_window <- 51  # Change to 12 if needed

# Summarize bacterial load for the first 15 weeks per calf (Without Inflating Load), swap this for the hashtahgged lines for thresholds above 0
windowed_data <- final_miseq_data_clean %>%
  filter(sample_week <= early_window) %>%
  group_by(calf_id) %>%
  summarize(
    bacterial_load = ifelse(any(across(all_of(bacteria_columns), ~ sum(.x > 0, na.rm = TRUE) > 0)), "High", "Low"),
    time_to_event = max(time_to_event, na.rm = TRUE),
    event = max(event, na.rm = TRUE)
  )%>%
  ungroup()

# Set a threshold for high bacterial load
#threshold <- 10  # Adjust this value based on your data distribution

# Identify earliest seroconversion week where bacterial load exceeds threshold
#seroconversion_data <- final_miseq_data_clean %>%
#  group_by(calf_id) %>%
#  summarize(
#    earliest_mutans = suppressWarnings(min(sample_week[theileria_mutans_af078815_tb > threshold], na.rm = TRUE)),
#    earliest_velifera = suppressWarnings(min(sample_week[theileria_velifera_af097993_tb > threshold], na.rm = TRUE)),
#    earliest_parva = suppressWarnings(min(sample_week[theileria_parva_l02366_tb > threshold], na.rm = TRUE)),
#    .groups = "drop"
#  ) %>%
#  mutate(across(starts_with("earliest_"), ~ ifelse(is.infinite(.), NA_real_, .)))  # Convert Inf to NA

# Convert time_to_event to numeric
windowed_data$time_to_event <- as.numeric(windowed_data$time_to_event)

# Check High vs. Low distribution
table(windowed_data$bacterial_load)

#################################### Kaplan-Meier survival fit #################################
km_fit <- survfit(Surv(time_to_event, event) ~ bacterial_load, data = windowed_data)

# Plot with censoring
ggsurvplot(km_fit, data = windowed_data,
           conf.int = TRUE, 
           pval = TRUE, 
           risk.table = TRUE, 
           censor = TRUE,  # Show censored data
           ggtheme = theme_minimal(),
           title = "Kaplan-Meier Survival Based on Early Infection (First 10 Weeks) to Ehlrichia species",
           palette = c("red", "blue"))

windowed_data$bacterial_load <- as.factor(windowed_data$bacterial_load)
table <- tbl_cross(data = windowed_data, row = bacterial_load, col = event, percent = "row")
table

# cox model
cox_model_bacteria <- coxph(Surv(time_to_event, event) ~ bacterial_load, data = windowed_data)

# Display summary of the Cox model
summary(cox_model_bacteria)

# Generate forest plot
ggforest(cox_model_bacteria, data = windowed_data)

########################## Add this on for individual Km plots per species #####################

# Define time window for early infection
early_window <- 51  # Change to 12 if needed
threshold <- 0   # Bacterial load threshold

# Loop through each Anaplasma species
for (bacteria in bacteria_columns) {
  cat("\nRunning Kaplan-Meier for:", bacteria, "\n")
  
  # Classify bacterial load per calf for the current species
  species_data <- final_miseq_data_clean %>%
    filter(sample_week <= early_window) %>%
    group_by(calf_id) %>%
    summarize(
      bacterial_load = ifelse(any(get(bacteria) > threshold, na.rm = TRUE), "High", "Low"),
      time_to_event = max(time_to_event, na.rm = TRUE),
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
table <- tbl_cross(data = windowed_data, row = bacterial_load, col = event, percent = "row")
table
