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
                                                               "Alive" = "Alive",
                                                               "Censored" = "Censored")

# Assign numeric event status (1 = Dead, 0 = Censored/Alive)
final_miseq_data_clean$event <- ifelse(final_miseq_data_clean$dead_or_alive_at_end_of_study == "Dead", 1, 0)

# Ensure date columns are in Date format
final_miseq_data_clean$date_last_visit_with_data <- as.Date(final_miseq_data_clean$date_last_visit_with_data)
final_miseq_data_clean$date_of_birth <- as.Date(final_miseq_data_clean$date_of_birth)

# Calculate survival time
final_miseq_data_clean$time_to_event <- as.numeric(final_miseq_data_clean$date_of_death - final_miseq_data_clean$date_of_birth)

# Handle cases without a date of death
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(time_to_event = ifelse(
    is.na(date_of_death) & event == 1, 
    as.numeric(date_last_visit_with_data - date_of_birth), 
    as.numeric(date_of_death - date_of_birth)
  ))

final_miseq_data_clean$time_to_event <- final_miseq_data_clean$time_to_event / 7  # Convert to weeks

# Manually set one error
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(date_last_visit_with_data = case_when(
    calf_id == "CA020610160" ~ as.Date("2008-07-17"),  # Set specific date for this calf
    TRUE ~ date_last_visit_with_data  # Keep existing values for others
  ))

# Set max time for alive cases
max_week <- 51 
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(
    time_to_event = ifelse(event == 0, max_week, time_to_event)
  )

# Adjust time_to_event for censored cases
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(
    time_to_event = ifelse(
      dead_or_alive_at_end_of_study == "Censored",
      as.numeric(date_last_visit_with_data - date_of_birth) / 7,
      time_to_event
    )
  )

# 🔹 Manually enter the pathogens column you want to analyze
pathogens <- "anaplasma_platys_ef139459_ae"  # Change this to any column of interest

#"theileria_mutans_af078815_tb"   ,                                                      
#                      "theileria_sp_strain_msd_af078816_tb"       ,                                           
#                     "theileria_parva_l02366_tb"                 ,                                           
#                    "theileria_taurotragi_l19082_tb"            ,                                           
#                   "theileria_velifera_af097993_tb" )


#pathogens_columns <- c( "anaplasma_bovis_u03775_ae"    ,                                                        
#          "anaplasma_bovis_ab983439_ae",                                                       
#        "anaplasma_marginale_cp000030_ae",                                                      
#      "anaplasma_platys_like_ku585990_ae",                                                    
#  "anaplasma_phagocytophilum_u02521_ae",                                                  
#       "candidatus_anaplasma_boleense_ku586025_ae"        ,                                    
#     "uncultured_anaplasma_sp_clone_saso_ky924885_ae"     ,                                  
#   "uncultured_anaplasma_sp_jn862825_ae",
#    "anaplasma_platys_ef139459_ae")

#pathogens_columns <- c( "ehrlichia_sp_tibet_ehrlichia_canis_ehrlichia_minasensis_af414399_ay394465_mt163430_ae"  ,
 #                      "ehrlichia_ruminantium_x61659_ae")

#pathogens_columns <- c( "babesia_bigemina_ay603402_tb"  ,                                                       
#           "babesia_bigemina_lk391709_tb"  ,                                                       
#         "babesia_bigemina_ku206291_tb"  ,
#        "babesia_bovis_kf928959_tb"     ,                                                      
#      "babesia_bovis_aaxt01000002_tb" ,                                                   
#    "babesia_bovis_ay603398_tb"     ,                                                  
#  "babesia_bovis_jq437260_tb" )

# Define early seroconversion cutoff
cutoff <- 51

# Identify earliest seroconversion (first detection > 0) for each calf
seroconversion_data <- final_miseq_data_clean %>%
  filter(sample_week <= cutoff & .data[[pathogens]] > 0) %>%
  group_by(calf_id) %>%
  summarize(earliest_seroconversion_week = min(sample_week, na.rm = TRUE), .groups = "drop")

# Merge with main dataset
windowed_data <- final_miseq_data_clean %>%
  left_join(seroconversion_data, by = "calf_id") %>%
  mutate(
    early_seroconversion = ifelse(!is.na(earliest_seroconversion_week), "Early", "Late/No Seroconversion")
  ) %>%
  group_by(calf_id) %>%
  summarize(
    early_seroconversion = first(early_seroconversion),
    time_to_event = max(time_to_event, na.rm = TRUE),
    event = max(event, na.rm = TRUE),
    .groups = "drop"
  )

# Convert to factor
windowed_data$early_seroconversion <- factor(windowed_data$early_seroconversion, levels = c("Late/No Seroconversion", "Early"))

# Kaplan-Meier survival fit
km_fit_sero <- survfit(Surv(time_to_event, event) ~ early_seroconversion, data = windowed_data)


# Plot Kaplan-Meier curve
ggsurvplot(km_fit_sero, data = windowed_data,
           conf.int = TRUE, 
           pval = TRUE, 
           risk.table = TRUE, 
           censor = TRUE,  
           ggtheme = theme_minimal(),
           title = paste("Kaplan-Meier Survival Based on Early Seroconversion to", pathogens),
           palette = c("red", "blue"))

  
  # Cox Proportional Hazards Model for early seroconversion
  cox_model_sero <- coxph(Surv(time_to_event, event) ~ early_seroconversion, data = windowed_data)
  
  # Display summary of Cox model
  print(summary(cox_model_sero))
  
  # Generate forest plot
  ggforest(cox_model_sero, data = windowed_data)
  
  
##############################################################################
  ##############################
  # Load libraries
  library(survival)
  library(survminer)
  library(dplyr)
  
  # Ensure survival status is categorized properly
  final_miseq_data_clean$dead_or_alive_at_end_of_study <- as.factor(final_miseq_data_clean$dead_or_alive_at_end_of_study)
  final_miseq_data_clean$dead_or_alive_at_end_of_study <- recode(final_miseq_data_clean$dead_or_alive_at_end_of_study,
                                                                 "Dead: Infectious death" = "Dead",
                                                                 "Dead: Death by trauma" = "Censored",
                                                                 "Alive" = "Alive",
                                                                 "Censored" = "Censored")
  
  # Assign numeric event status (1 = Dead, 0 = Censored/Alive)
  final_miseq_data_clean$event <- ifelse(final_miseq_data_clean$dead_or_alive_at_end_of_study == "Dead", 1, 0)
  
  # Ensure date columns are in Date format
  final_miseq_data_clean$date_last_visit_with_data <- as.Date(final_miseq_data_clean$date_last_visit_with_data)
  final_miseq_data_clean$date_of_birth <- as.Date(final_miseq_data_clean$date_of_birth)
  
  # Calculate survival time
  final_miseq_data_clean$time_to_event <- as.numeric(final_miseq_data_clean$date_of_death - final_miseq_data_clean$date_of_birth)
  
  # Handle cases without a date of death
  final_miseq_data_clean <- final_miseq_data_clean %>%
    mutate(time_to_event = ifelse(
      is.na(date_of_death) & event == 1, 
      as.numeric(date_last_visit_with_data - date_of_birth), 
      as.numeric(date_of_death - date_of_birth)
    ))
  
  final_miseq_data_clean$time_to_event <- final_miseq_data_clean$time_to_event / 7  # Convert to weeks
  
  # Manually set one error
  final_miseq_data_clean <- final_miseq_data_clean %>%
    mutate(date_last_visit_with_data = case_when(
      calf_id == "CA020610160" ~ as.Date("2008-07-17"),  # Set specific date for this calf
      TRUE ~ date_last_visit_with_data  # Keep existing values for others
    ))
  
  # Set max time for alive cases
  max_week <- 51 
  final_miseq_data_clean <- final_miseq_data_clean %>%
    mutate(
      time_to_event = ifelse(event == 0, max_week, time_to_event)
    )
  
  # Adjust time_to_event for censored cases
  final_miseq_data_clean <- final_miseq_data_clean %>%
    mutate(
      time_to_event = ifelse(
        dead_or_alive_at_end_of_study == "Censored",
        as.numeric(date_last_visit_with_data - date_of_birth) / 7,
        time_to_event
      )
    )
  
  # Set early seroconversion cutoff
  cutoff <- 26
  
  # Identify earliest seroconversion for each pathogens, swap this for the hashtahgged lines for thresholds above 0
  seroconversion_data <- final_miseq_data_clean %>%
    group_by(calf_id) %>%
    summarize(
      earliest_mutans = suppressWarnings(min(sample_week[theileria_mutans_af078815_tb > 0], na.rm = TRUE)),
      earliest_velifera = suppressWarnings(min(sample_week[theileria_velifera_af097993_tb > 0], na.rm = TRUE)),
      earliest_parva = suppressWarnings(min(sample_week[theileria_parva_l02366_tb > 0], na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(across(starts_with("earliest_"), ~ ifelse(is.infinite(.), NA_real_, .)))  # Convert Inf to NA
  
  # Set a threshold for high pathogens load
  #threshold <- 10  # Adjust this value based on your data distribution
  
  # Identify earliest seroconversion week where pathogens load exceeds threshold
  #seroconversion_data <- final_miseq_data_clean %>%
  #  group_by(calf_id) %>%
  #  summarize(
  #    earliest_mutans = suppressWarnings(min(sample_week[theileria_mutans_af078815_tb > threshold], na.rm = TRUE)),
  #    earliest_velifera = suppressWarnings(min(sample_week[theileria_velifera_af097993_tb > threshold], na.rm = TRUE)),
  #    earliest_parva = suppressWarnings(min(sample_week[theileria_parva_l02366_tb > threshold], na.rm = TRUE)),
  #    .groups = "drop"
  #  ) %>%
  #  mutate(across(starts_with("earliest_"), ~ ifelse(is.infinite(.), NA_real_, .)))  # Convert Inf to NA
  
  # Merge with main dataset
  windowed_data <- final_miseq_data_clean %>%
    left_join(seroconversion_data, by = "calf_id") %>%
    mutate(
      # Replace NA with Inf for comparison purposes
      earliest_mutans = ifelse(is.na(earliest_mutans), Inf, earliest_mutans),
      earliest_velifera = ifelse(is.na(earliest_velifera), Inf, earliest_velifera),
      earliest_parva = ifelse(is.na(earliest_parva), Inf, earliest_parva),
      
      # Define seroconversion order groups
      seroconversion_order = case_when(
        earliest_mutans < earliest_parva & earliest_mutans < earliest_velifera ~ "Mutans First", 
        earliest_velifera < earliest_parva & earliest_velifera < earliest_mutans ~ "Velifera First",
        (earliest_mutans == earliest_velifera) & (earliest_mutans < earliest_parva) ~ "Mutans/Velifera First",
        earliest_parva < earliest_mutans & earliest_parva < earliest_velifera ~ "Parva First",
        TRUE ~ "No Seroconversion"
      )
    ) %>%
    # Convert Inf back to NA for clarity in the final output
    mutate(across(starts_with("earliest_"), ~ ifelse(. == Inf, NA_real_, .))) %>%
    group_by(calf_id) %>%
    summarize(
      seroconversion_order = first(seroconversion_order),
      time_to_event = max(time_to_event, na.rm = TRUE),
      event = max(event, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Group Mutans First, Velifera First, and Mutans/Velifera First into one category
  windowed_data <- windowed_data %>%
    mutate(seroconversion_order = case_when(
      seroconversion_order %in% c("Mutans First", "Velifera First", "Mutans/Velifera First") ~ "Mutans/Velifera First",
      TRUE ~ seroconversion_order  # Keep other values unchanged
    ))
  
  # Convert to factor for plotting
windowed_data$seroconversion_order <- factor(windowed_data$seroconversion_order, 
                                               levels = c("No Seroconversion", "Mutans/Velifera First", "Parva First"))
  
  # Kaplan-Meier survival fit
km_fit <- survfit(Surv(time_to_event, event) ~ seroconversion_order, data = windowed_data)
  
# Plot Kaplan-Meier curve
ggsurvplot(km_fit, data = windowed_data,
             conf.int = TRUE, 
             pval = TRUE, 
             risk.table = TRUE, 
             censor = TRUE,  
             ggtheme = theme_minimal(),
             title = "Kaplan-Meier Survival by Seroconversion Order",
             palette = c("black", "blue", "orange"))
  
  cox_model <- coxph(Surv(time_to_event, event) ~ seroconversion_order, data = windowed_data)
  
  # View model summary
  summary(cox_model)
  # Test proportional hazards assumption
  cox.zph(cox_model)
  ggforest(cox_model, data = windowed_data)
  
# Summary table
table <- tbl_cross(data = windowed_data, row = seroconversion_order, col = event, percent = "row")
table

swrite.csv(windowed_data, "windowed.csv", row.names = FALSE)
