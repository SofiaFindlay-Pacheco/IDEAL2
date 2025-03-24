

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Define the bacteria columns
bacteria_columns <- c("babesia_bigemina_ay603402_tb"  ,                                                       
                      "babesia_bigemina_lk391709_tb"  ,                                                       
                      "babesia_bigemina_ku206291_tb"  ,
                      "babesia_bovis_kf928959_tb"     ,                                                      
                      "babesia_bovis_aaxt01000002_tb" ,                                                   
                      "babesia_bovis_ay603398_tb"     ,                                                  
                      "babesia_bovis_jq437260_tb") 

# Clean up the "Dead or Alive at End of Study" column
final_miseq_data_clean$dead_or_alive_at_end_of_study <- as.factor(final_miseq_data_clean$dead_or_alive_at_end_of_study)

# Group all "Dead" statuses together (including different causes of death)
final_miseq_data_clean$dead_or_alive_at_end_of_study <- recode(final_miseq_data_clean$dead_or_alive_at_end_of_study,
                                                               "Dead: Infectious death" = "Dead",
                                                               "Dead: Death by trauma" = "Dead",
                                                               "Alive" = "Alive")

# Remove censored data
final_miseq_data_clean <- final_miseq_data_clean %>%
  filter(dead_or_alive_at_end_of_study != "Censored")

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
    title = "Total Babesia Load Over Time by Survival Status",
    x = "Sample Week",
    y = "Log-Scaled Total Bacteria Load",
    color = "Survival Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
