###################### Individual bacteria load graphs over time #######################
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Define the bacteria columns
bacteria_columns <- c( "theileria_mutans_af078815_tb"   ,                                                      
                       "theileria_sp_strain_msd_af078816_tb"       ,                                           
                       "theileria_parva_l02366_tb"                 ,                                           
                       "theileria_taurotragi_l19082_tb"            ,                                           
                       "theileria_velifera_af097993_tb" )


#bacteria_columns <- c( "anaplasma_bovis_u03775_ae"    ,                                                        
#           "anaplasma_bovis_ab983439_ae",                                                       
#          "anaplasma_marginale_cp000030_ae",                                                      
#         "anaplasma_platys_like_ku585990_ae",                                                    
#        "anaplasma_phagocytophilum_u02521_ae",                                                  
#       "candidatus_anaplasma_boleense_ku586025_ae"        ,                                    
#      "uncultured_anaplasma_sp_clone_saso_ky924885_ae"     ,                                  
#     "uncultured_anaplasma_sp_jn862825_ae",
#    "anaplasma_platys_ef139459_ae")

#bacteria_columns <- c( "ehrlichia_sp_tibet_ehrlichia_canis_ehrlichia_minasensis_af414399_ay394465_mt163430_ae"  ,
#      "ehrlichia_ruminantium_x61659_ae")

#bacteria_columns <- c( "babesia_bigemina_ay603402_tb"  ,                                                       
#            "babesia_bigemina_lk391709_tb"  ,                                                       
#           "babesia_bigemina_ku206291_tb"  ,
#           "babesia_bovis_kf928959_tb"     ,                                                      
#          "babesia_bovis_aaxt01000002_tb" ,                                                   
#         "babesia_bovis_ay603398_tb"     ,                                                  
#        "babesia_bovis_jq437260_tb" )

# Clean up the "Dead or Alive at End of Study" column
final_miseq_data_clean$dead_or_alive_at_end_of_study <- as.factor(final_miseq_data_clean$dead_or_alive_at_end_of_study)

# Group all "Dead" statuses together (including different causes of death)
final_miseq_data_clean$dead_or_alive_at_end_of_study <- recode(final_miseq_data_clean$dead_or_alive_at_end_of_study,
                                                               "Dead: Infectious death" = "Dead",
                                                               "Dead: Death by trauma" = "Dead",
                                                               "Alive" = "Alive")


# Transform data: Gather bacteria into a long format for faceting
long_miseq_data <- final_miseq_data_clean %>%
  select(sample_week, dead_or_alive_at_end_of_study, all_of(bacteria_columns)) %>%
  pivot_longer(cols = all_of(bacteria_columns), names_to = "Bacteria", values_to = "Value") %>%
  group_by(sample_week, dead_or_alive_at_end_of_study, Bacteria) %>%
  summarize(Average_Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  arrange(sample_week)

# Plot the average values for each bacteria type in a faceted layout
ggplot(long_miseq_data, aes(x = sample_week, y = Average_Value, 
                            color = dead_or_alive_at_end_of_study, 
                            group = dead_or_alive_at_end_of_study)) +
  geom_line(size = 1.2) +  # Line plot without rolling average
  scale_color_manual(values = c("Alive" = "green", "Dead" = "red")) + 
  facet_wrap(~Bacteria, scales = "free_y") +  # Separate plots per bacteria
  labs(
    title = "Trends in Theileria Bacteria Over Time by Survival Outcome",
    x = "Sample Week",
    y = "Average Bacteria Value",
    color = "Survival Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############################ Bacteria load over time in lumped species #############################
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Define the bacteria columns
bacteria_columns <- c( "theileria_mutans_af078815_tb"   ,                                                      
                       "theileria_sp_strain_msd_af078816_tb"       ,                                           
                       "theileria_parva_l02366_tb"                 ,                                           
                       "theileria_taurotragi_l19082_tb"            ,                                           
                       "theileria_velifera_af097993_tb" )


#bacteria_columns <- c( "anaplasma_bovis_u03775_ae"    ,                                                        
#           "anaplasma_bovis_ab983439_ae",                                                       
#          "anaplasma_marginale_cp000030_ae",                                                      
#         "anaplasma_platys_like_ku585990_ae",                                                    
#        "anaplasma_phagocytophilum_u02521_ae",                                                  
#       "candidatus_anaplasma_boleense_ku586025_ae"        ,                                    
#      "uncultured_anaplasma_sp_clone_saso_ky924885_ae"     ,                                  
#     "uncultured_anaplasma_sp_jn862825_ae",
#    "anaplasma_platys_ef139459_ae")

#bacteria_columns <- c( "ehrlichia_sp_tibet_ehrlichia_canis_ehrlichia_minasensis_af414399_ay394465_mt163430_ae"  ,
#      "ehrlichia_ruminantium_x61659_ae")

#bacteria_columns <- c( "babesia_bigemina_ay603402_tb"  ,                                                       
#            "babesia_bigemina_lk391709_tb"  ,                                                       
#           "babesia_bigemina_ku206291_tb"  ,
#           "babesia_bovis_kf928959_tb"     ,                                                      
#          "babesia_bovis_aaxt01000002_tb" ,                                                   
#         "babesia_bovis_ay603398_tb"     ,                                                  
#        "babesia_bovis_jq437260_tb" )

# Clean up the "Dead or Alive at End of Study" column
final_miseq_data_clean$dead_or_alive_at_end_of_study <- as.factor(final_miseq_data_clean$dead_or_alive_at_end_of_study)

# Group all "Dead" statuses together (including different causes of death)
final_miseq_data_clean$dead_or_alive_at_end_of_study <- recode(final_miseq_data_clean$dead_or_alive_at_end_of_study,
                                                               "Dead: Infectious death" = "Dead",
                                                               "Dead: Death by trauma" = "Dead",
                                                               "Alive" = "Alive")

# Compute the **total bacterial load per sample week** for each survival status
total_miseq_data <- final_miseq_data_clean %>%
  select(sample_week, dead_or_alive_at_end_of_study, all_of(bacteria_columns)) %>%
  group_by(sample_week, dead_or_alive_at_end_of_study) %>%
  summarize(Total_Bacteria_Load = sum(across(all_of(bacteria_columns)), na.rm = TRUE), .groups = "drop") %>%
  arrange(sample_week)

# Plot the total bacterial load over time
ggplot(total_miseq_data, aes(x = sample_week, y = Total_Bacteria_Load, 
                             color = dead_or_alive_at_end_of_study, 
                             group = dead_or_alive_at_end_of_study)) +
  geom_line(size = 1.5) +  # Line plot
  scale_y_log10(labels = scales::comma) +  # Apply log scale to Y-axis
  scale_color_manual(values = c("Alive" = "green", "Dead" = "red")) + 
  labs(
    title = "Total Theileria Load Over Time by Survival Status",
    x = "Sample Week",
    y = "Log-Scaled Total Bacteria Load",
    color = "Survival Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
