#Individual bacteria load graphs over time
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(zoo)
library(tidyr)

# Define the bacteria columns
bacteria_columns <- c( "theileria_mutans_af078815_tb"   ,                                                      
                                "theileria_sp_strain_msd_af078816_tb"       ,                                           
                                   "theileria_parva_l02366_tb"                 ,                                           
                                     "theileria_taurotragi_l19082_tb"            ,                                           
                                        "theileria_velifera_af097993_tb" )



# Clean up the "Dead or alive at end of study" column
final_miseq_data_clean$dead_or_alive_at_end_of_study <- as.factor(final_miseq_data_clean$dead_or_alive_at_end_of_study)

# Group all "Dead" statuses together (including different death causes)
final_miseq_data_clean$dead_or_alive_at_end_of_study <- recode(final_miseq_data_clean$dead_or_alive_at_end_of_study,
                                                               "Dead: Infectious death" = "Dead",
                                                               "Dead: Death by trauma" = "Dead",
                                                               "Alive" = "Alive")
#Remove censored data
final_miseq_data_clean <- final_miseq_data_clean %>%   #Remove censored data
  filter(dead_or_alive_at_end_of_study != "Censored")
table(final_miseq_data_clean$dead_or_alive_at_end_of_study) # Check survival status counts

# Transform data: Gather bacteria into a long format for faceting
long_miseq_data <- final_miseq_data_clean %>%
  select(sample_week, dead_or_alive_at_end_of_study, all_of(bacteria_columns)) %>%
  pivot_longer(cols = all_of(bacteria_columns), names_to = "Bacteria", values_to = "Value") %>%
  group_by(sample_week, dead_or_alive_at_end_of_study, Bacteria) %>%
  summarize(Average_Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  arrange(sample_week) %>%
  group_by(dead_or_alive_at_end_of_study, Bacteria) %>%
  mutate(Rolling_Avg = zoo::rollmean(Average_Value, k = 3, fill = NA, align = "center"))

# Plot the rolling averages for each bacteria type in a faceted layout
ggplot(long_miseq_data, aes(x = sample_week, y = Rolling_Avg, 
                            color = dead_or_alive_at_end_of_study, 
                            group = dead_or_alive_at_end_of_study)) +
  #ylim(0,3) +
  geom_line(size = 1.2) +  # Smooth rolling average line
  scale_color_manual(values = c("Alive" = "green", "Dead" = "red")) + 
  facet_wrap(~Bacteria, scales = "free_y") +  # Separate plots per bacteria
  labs(
    title = "Trends in Theileria Bacteria Over Time by Survival Outcome",
    x = "Sample Week",
    y = "Rolling Average Value",
    color = "Survival Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#No rolling average
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
                      "babesia_bovis_jq437260_tb" )


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
    title = "Trends in Babesia Bacteria Over Time by Survival Outcome",
    x = "Sample Week",
    y = "Average Bacteria Value",
    color = "Survival Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##########################################################################################################
#Bacteria load over time in lumped species
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
