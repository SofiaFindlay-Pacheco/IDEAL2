# Load libraries
library(nnet)
library(janitor)
library(summarytools)
library(gtsummary)
library(dplyr)
library(tidyr)
library(readxl)
library(tidyverse)
library(survival)
library(survminer)

# List of pathogens species columns
#pathogens_columns <- c( "theileria_mutans_af078815_tb"   ,                                                      
 #                      "theileria_sp_strain_msd_af078816_tb"       ,                                           
  #                     "theileria_parva_l02366_tb"                 ,                                           
   #                    "theileria_taurotragi_l19082_tb"            ,                                           
    #                   "theileria_velifera_af097993_tb" )


#pathogens_columns <- c( "anaplasma_bovis_u03775_ae"    ,                                                        
 #          "anaplasma_bovis_ab983439_ae",                                                       
  #      "anaplasma_marginale_cp000030_ae",                                                      
   #     "anaplasma_platys_like_ku585990_ae",                                                    
    #  "anaplasma_phagocytophilum_u02521_ae",                                                  
     #  "candidatus_anaplasma_boleense_ku586025_ae"        ,                                    
#      "uncultured_anaplasma_sp_clone_saso_ky924885_ae"     ,                                  
 #    "uncultured_anaplasma_sp_jn862825_ae",
  #  "anaplasma_platys_ef139459_ae")

#pathogens_columns <- c( "ehrlichia_sp_tibet_ehrlichia_canis_ehrlichia_minasensis_af414399_ay394465_mt163430_ae"  ,
 #     "ehrlichia_ruminantium_x61659_ae")

pathogens_columns <- c( "babesia_bigemina_ay603402_tb"  ,                                                       
            "babesia_bigemina_lk391709_tb"  ,                                                       
         "babesia_bigemina_ku206291_tb"  ,
        "babesia_bovis_kf928959_tb"     ,                                                      
        "babesia_bovis_aaxt01000002_tb" ,                                                   
         "babesia_bovis_ay603398_tb"     ,                                                  
        "babesia_bovis_jq437260_tb" )

# Define time window for early infection
early_window <- 51  # Change to 12 if needed

# Summarize pathogens load for the first 15 weeks per calf (Without Inflating Load), swap this for the hashtahgged lines for thresholds above 0
windowed_data <- final_miseq_data_clean %>%
  filter(sample_week <= early_window) %>%
  group_by(calf_id) %>%
  summarize(
    pathogens_load = ifelse(any(across(all_of(pathogens_columns), ~ sum(.x > 0, na.rm = TRUE) > 0)), "High", "Low"),
    time_to_event = max(time_to_event, na.rm = TRUE),
    event = max(event, na.rm = TRUE)
  )%>%
  ungroup()

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

# Convert time_to_event to numeric
windowed_data$time_to_event <- as.numeric(windowed_data$time_to_event)

# Check High vs. Low distribution
table(windowed_data$pathogens_load)

#################################### Kaplan-Meier survival fit #################################
km_fit <- survfit(Surv(time_to_event, event) ~ pathogens_load, data = windowed_data)

# Plot with censoring
ggsurvplot(km_fit, data = windowed_data,
           conf.int = TRUE, 
           pval = TRUE, 
           risk.table = TRUE, 
           censor = TRUE,  # Show censored data
           ggtheme = theme_minimal(),
           title = "Kaplan-Meier Survival Based on Infection to Babesia species",
           palette = c("red", "blue"))

windowed_data$pathogens_load <- as.factor(windowed_data$pathogens_load)
table <- tbl_cross(data = windowed_data, row = pathogens_load, col = event, percent = "row")
table

# cox model
cox_model_pathogens <- coxph(Surv(time_to_event, event) ~ pathogens_load, data = windowed_data)

# Display summary of the Cox model
summary(cox_model_pathogens)

# Generate forest plot
ggforest(cox_model_pathogens, data = windowed_data)

########################## Add this on for individual Km plots per species #####################

# Define time window for early infection
early_window <- 51  # Change to 12 if needed
threshold <- 0   # pathogens load threshold

# Loop through each Anaplasma species
for (pathogens in pathogens_columns) {
  cat("\nRunning Kaplan-Meier for:", pathogens, "\n")
  
  # Classify pathogens load per calf for the current species
  species_data <- final_miseq_data_clean %>%
    filter(sample_week <= early_window) %>%
    group_by(calf_id) %>%
    summarize(
      pathogens_load = ifelse(any(get(pathogens) > threshold, na.rm = TRUE), "High", "Low"),
      time_to_event = max(time_to_event, na.rm = TRUE),
      event = max(event, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Convert time_to_event to numeric
  species_data$time_to_event <- as.numeric(species_data$time_to_event)
  
  # Kaplan-Meier survival analysis
  km_fit <- survfit(Surv(time_to_event, event) ~ pathogens_load, data = species_data)
  
  # Plot results
  print(ggsurvplot(km_fit, data = species_data,
                   conf.int = TRUE, pval = TRUE, risk.table = TRUE,
                   censor = TRUE,  # Show censored data
                   ggtheme = theme_minimal(),
                   title = paste("Kaplan-Meier Survival for", pathogens),
                   palette = c("red", "blue")))
}
table <- tbl_cross(data = windowed_data, row = pathogens_load, col = event, percent = "row")
table
