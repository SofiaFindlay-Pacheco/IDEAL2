# Load necessary libraries
library(dplyr)
library(ggplot2)
library(zoo)
library(tidyr)

# Define the bacteria columns
bacteria_columns <- c( "theileria_mutans_af078815_tb",                                                         
                       "theileria_sp_strain_msd_af078816_tb",                                                  
                       "theileria_parva_l02366_tb",                                                            
                       "theileria_taurotragi_l19082_tb",                                                      
                       "theileria_velifera_af097993_tb"   )

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
  geom_line(size = 1.2) +  # Smooth rolling average line
  scale_color_manual(values = c("Alive" = "green", "Dead" = "red")) + 
  facet_wrap(~Bacteria, scales = "free_y") +  # Separate plots per bacteria
  labs(
    title = "Sliding Window Analysis of Theileria & Related Bacteria by Survival Status",
    x = "Sample Week",
    y = "Rolling Average Value",
    color = "Survival Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
