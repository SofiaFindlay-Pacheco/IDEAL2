############## Construct data ###########################
#install.packages("epitools")
library(epitools)  # Load package for OR & RR
library(ggplot2)

#############  Calculate time to event and event#############
# Clean up survival status column
final_miseq_data_clean$dead_or_alive_at_end_of_study <- as.factor(final_miseq_data_clean$dead_or_alive_at_end_of_study)

# Group all "Dead" statuses together
final_miseq_data_clean$dead_or_alive_at_end_of_study <- recode(final_miseq_data_clean$dead_or_alive_at_end_of_study,
                                                               "Dead: Infectious death" = "Dead",
                                                               "Dead: Death by trauma" = "Dead",
                                                               "Alive" = "Alive")

# Assign numeric event status (1 = Dead, 0 = Censored/Alive)
final_miseq_data_clean$event <- ifelse(final_miseq_data_clean$dead_or_alive_at_end_of_study == "Dead", 1, 0)

# Calculate survival time
# Ensure survival time is numeric
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

# Select Columns
miseq_seroconversion_clean <- select(final_miseq_data_clean, calf_id, event, time_to_event)
# Only distinct CalfID, only on of each calfID
miseq_seroconversion_clean <-distinct(miseq_seroconversion_clean,calf_id,event, time_to_event)

# Make seroconversion columns

# Convert dataset to long format
final_miseq_reduced <- select(final_miseq_data_clean, calf_id, sample_week, anaplasma_bovis_u03775_ae, anaplasma_marginale_cp000030_ae, anaplasma_platys_like_ku585990_ae, anaplasma_phagocytophilum_u02521_ae,                                                   
                              candidatus_anaplasma_boleense_ku586025_ae, uncultured_anaplasma_sp_clone_saso_ky924885_ae, uncultured_anaplasma_sp_jn862825_ae, ehrlichia_sp_tibet_ehrlichia_canis_ehrlichia_minasensis_af414399_ay394465_mt163430_ae,
                              ehrlichia_ruminantium_x61659_ae, anaplasma_bovis_ab983439_ae, anaplasma_platys_ef139459_ae,                                                          
                              babesia_bigemina_ay603402_tb, babesia_bigemina_lk391709_tb, babesia_bigemina_ku206291_tb,                                                         
                              theileria_mutans_af078815_tb, theileria_sp_strain_msd_af078816_tb, theileria_parva_l02366_tb,                                                            
                              theileria_taurotragi_l19082_tb, theileria_velifera_af097993_tb, babesia_bovis_kf928959_tb, babesia_bovis_aaxt01000002_tb,                                                         
                              babesia_bovis_ay603398_tb, babesia_bovis_jq437260_tb, babesia_bovis_mh569533_tb)

#pathogens_cols <- c( "theileria_mutans_af078815_tb"   ,                                                      
 #                        "theileria_sp_strain_msd_af078816_tb"       ,                                           
  #                       "theileria_parva_l02366_tb"                 ,                                           
   #                      "theileria_taurotragi_l19082_tb"            ,                                           
    #                     "theileria_velifera_af097993_tb" )
  
  
  #pathogens_cols <- c( "anaplasma_bovis_u03775_ae"    ,                                                        
   #          "anaplasma_bovis_ab983439_ae",                                                       
    #        "anaplasma_marginale_cp000030_ae",                                                      
     #      "anaplasma_platys_like_ku585990_ae",                                                    
      #    "anaplasma_phagocytophilum_u02521_ae",                                                  
  #     "candidatus_anaplasma_boleense_ku586025_ae"        ,                                    
   #     "uncultured_anaplasma_sp_clone_saso_ky924885_ae"     ,                                  
    #   "uncultured_anaplasma_sp_jn862825_ae",
#      "anaplasma_platys_ef139459_ae")
  
  pathogens_cols <- c( "ehrlichia_sp_tibet_ehrlichia_canis_ehrlichia_minasensis_af414399_ay394465_mt163430_ae"  ,
        "ehrlichia_ruminantium_x61659_ae")
  
  #pathogens_cols <- c( "babesia_bigemina_ay603402_tb"  ,                                                       
  #            "babesia_bigemina_lk391709_tb"  ,                                                       
  #           "babesia_bigemina_ku206291_tb"  ,
  #           "babesia_bovis_kf928959_tb"     ,                                                      
  #          "babesia_bovis_aaxt01000002_tb" ,                                                   
  #         "babesia_bovis_ay603398_tb"     ,                                                  
  #        "babesia_bovis_jq437260_tb" )

# Pivot dataset to long format (keeping all calves for each pathogens)
long_data <- final_miseq_reduced %>%
  select(calf_id, sample_week, all_of(pathogens_cols)) %>%  # Select only necessary columns
  pivot_longer(
    cols = all_of(pathogens_cols),  # Pivot only pathogens columns
    names_to = "pathogens",
    values_to = "load"
  )

# Identify first seroconversion week (but keeping ALL calves)
seroconversion_data <- long_data %>%
  group_by(calf_id, pathogens) %>%
  summarise(seroconversion_week = ifelse(any(load > 0), min(sample_week[load > 0]), NA), .groups = "drop")  

# Merge with survival data, ensuring all calves are included
final_miseq_seroconversion_clean_long <- expand.grid(
  calf_id = unique(miseq_seroconversion_clean$calf_id),
  pathogens = pathogens_cols
) %>%
  left_join(miseq_seroconversion_clean, by = "calf_id") %>%  # Merge survival data
  left_join(seroconversion_data, by = c("calf_id", "pathogens"))  # Merge seroconversion data

# Assign non-seroconverters a high seroconversion week (e.g.,52)
max_week <- 51
final_miseq_seroconversion_clean_long$seroconversion_week <- as.numeric(final_miseq_seroconversion_clean_long$seroconversion_week)

final_miseq_seroconversion_clean_long <- final_miseq_seroconversion_clean_long %>%
  group_by(calf_id) %>%
  summarise(
    seroconversion_week = mean(
      ifelse(is.na(seroconversion_week), max_week, seroconversion_week), 
      na.rm = TRUE
    ),
    time_to_event = first(time_to_event),  # Keeps the first non-NA value
    event = first(event)  # Keeps the first occurrence of event (0 or 1)
  ) %>%
  ungroup()  # Always good practice after `group_by()`

# Update the 'time_to_event' for alive calves (event == 0) to the max_week
final_miseq_seroconversion_clean_long <- final_miseq_seroconversion_clean_long %>%
  mutate(
    time_to_event = ifelse(event == 0, max_week, time_to_event)  # Set to max_week for alive calves
  )

# Ensure correct data types
#final_miseq_seroconversion_clean_long$pathogens <- as.factor(final_miseq_seroconversion_clean_long$pathogens)
final_miseq_seroconversion_clean_long$event <- as.factor(final_miseq_seroconversion_clean_long$event)

# Check structure
str(final_miseq_seroconversion_clean_long)


# Filter dataset for one specific bacterium
#specific_pathogens <- "theileria_parva_l02366_tb"
#final_miseq_seroconversion_clean_long <- final_miseq_seroconversion_clean_long %>%
#  filter(pathogens == specific_pathogens)

# Ensure factor and numeric conversions

# 1. Summary Statistics
summary(final_miseq_seroconversion_clean_long)

# 2. Visualize Seroconversion Time Distribution
ggplot(final_miseq_seroconversion_clean_long, aes(x = seroconversion_week, fill = "Ehlrichia")) +
  geom_histogram(binwidth = 5, alpha = 0.7, position = "dodge") +
  labs(title = "Distribution of Seroconversion Time by pathogens Type", 
       x = "Weeks to Seroconversion", 
       y = "Count") +
  theme_minimal()

# 3. Chi-Squared Test (Association between Seroconversion Time and Mortality)
final_miseq_seroconversion_clean_long$early_seroconversion <- ifelse(final_miseq_seroconversion_clean_long$seroconversion_week <= median(final_miseq_seroconversion_clean_long$seroconversion_week, na.rm = TRUE), "Early", "Late")
chisq_test <- chisq.test(table(final_miseq_seroconversion_clean_long$early_seroconversion, final_miseq_seroconversion_clean_long$event))
print(chisq_test)

# Create contingency table
seroconv_table <- table(final_miseq_seroconversion_clean_long$early_seroconversion, final_miseq_seroconversion_clean_long$event)
print(seroconv_table)

# Compute Odds Ratio and Relative Risk
or_rr_results <- epitab(seroconv_table, method = "oddsratio")
print(or_rr_results)

# Logistic Regression: Mortality (event) ~ Early vs. Late Seroconversion
logit_model <- glm(event ~ early_seroconversion, data = final_miseq_seroconversion_clean_long, family = binomial)
summary(logit_model)

# 4. Logistic Regression (Seroconversion Timing and Mortality)
logistic_model <- glm(event ~ seroconversion_week, 
                      data = final_miseq_seroconversion_clean_long, 
                      family = binomial)
# Summary of the logistic regression model
summary(logistic_model)

# 5. Kaplan-Meier Curve (Seroconversion Probability over Time)
# Kaplan-Meier curve for survival probability over time, based on event and seroconversion_week
km_fit <- survfit(Surv(seroconversion_week, event == 1) ~ 1, 
                  data = final_miseq_seroconversion_clean_long)
# Plot the Kaplan-Meier curve
ggsurvplot(km_fit, data = final_miseq_seroconversion_clean_long, 
           pval = TRUE, conf.int = TRUE, 
           risk.table = TRUE, 
           ggtheme = theme_minimal())


# 6. Cox Proportional-Hazards Model (Time to Death)
# Run Cox Proportional-Hazards Model without the pathogens variable
cox_model <- coxph(Surv(time_to_event, event == 1) ~ seroconversion_week, data = final_miseq_seroconversion_clean_long)
# View the summary of the model
summary(cox_model)
# Plot the forest plot of the model
ggforest(cox_model, data = final_miseq_seroconversion_clean_long)

