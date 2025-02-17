# install.packages("zoo") 

# Library
library(dplyr)
library(ggplot2)
library(zoo)  

# Clean up the "Dead or alive at end of study" column
vertical_Brief_Serology$dead_or_alive_at_end_of_study <- as.factor(vertical_Brief_Serology$dead_or_alive_at_end_of_study)

# Group all "Dead" statuses together (including different death causes)
vertical_Brief_Serology$dead_or_alive_at_end_of_study <- recode(vertical_Brief_Serology$dead_or_alive_at_end_of_study,
                                                     "Dead: Infectious death" = "Dead",
                                                     "Dead: Death by trauma" = "Dead",
                                                     "Alive" = "Alive")
vertical_Brief_Serology <- vertical_Brief_Serology %>%   #Remove censored data
  filter(dead_or_alive_at_end_of_study != "Censored")
table(vertical_Brief_Serology$dead_or_alive_at_end_of_study) # Check survival status counts

# Plot the bacteria data with survival status as a factor (color)
vertical_Brief_Serology%>%                      # Filter and prepare for plotting
  filter(Bacteria == "serology_t_parva") %>%
  #filter(Bacteria=="serology_a_marginale")%>%
  #filter(Bacteria == "serology_b_bigemina")%>%
  #filter(Bacteria == "serology_t_mutans")%>%
  group_by(calf_id) %>% 
  arrange(sample_week)%>%
  ungroup() %>%
  ggplot(aes(x = sample_week, y = Average_Value, color = dead_or_alive_at_end_of_study, group = calf_id)) +
  geom_line() +  
  geom_point(size = 2) +  # Optional: add points for more clarity
  scale_color_manual(values = c("Alive" = "green", "Dead" = "red")) +  # Custom colors for survival status
  labs(
    title = "Bacteria Levels and Survival Status Over Time",
    x = "SampleWeek",
    y = "Serology Value",
    color = "Survival Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###

# Average bacteria levels depending on whether the calf is alive or dead

avg_data <- vertical_Brief_Serology %>%
  filter(Bacteria == "serology_t_parva") %>%
  #filter(Bacteria == "serology_a_marginale") %>%
  #filter(Bacteria == "serology_b_bigemina")%>%
  #filter(Bacteria == "serology_t_mutans")%>%
  group_by(sample_week, dead_or_alive_at_end_of_study) %>%
  summarize(Average_Value = mean(Average_Value, na.rm = TRUE), .groups = "drop")

ggplot(avg_data, aes(x = sample_week, y = Average_Value, color = dead_or_alive_at_end_of_study, group = dead_or_alive_at_end_of_study)) +
  geom_line(size = 1.5) +  # Line for averages
  scale_color_manual(values = c("Alive" = "green", "Dead" = "red")) +  # Custom colors for survival status
  labs(
    title = "Average Bacteria Levels by Survival Status Over Time",
    x = "SampleWeek",
    y = "Average Serology Value",
    color = "Survival Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Sliding window
# Calculate moving averages using a rolling window of size 3
avg_data <- vertical_Brief_Serology %>%
  filter(Bacteria == "serology_t_parva") %>%
  group_by(sample_week, dead_or_alive_at_end_of_study) %>%
  summarize(Average_Value = mean(Average_Value, na.rm = TRUE), .groups = "drop") %>%
  arrange(sample_week) %>%  # Ensure data is sorted by SampleWeek
  group_by(dead_or_alive_at_end_of_study) %>%
  mutate(Rolling_Avg = zoo::rollmean(Average_Value, k = 3, fill = NA, align = "center"))

# Visualize the data with sliding window averages
ggplot(avg_data, aes(x = sample_week, y = Rolling_Avg, color = dead_or_alive_at_end_of_study, group = dead_or_alive_at_end_of_study)) +
  geom_line(size = 1.5) +  # Line for rolling averages
  geom_point(aes(y = Average_Value), alpha = 0.5) +  # Points for raw averages
  scale_color_manual(values = c("Alive" = "green", "Dead" = "red")) +  # Custom colors
  labs(
    title = "Sliding Window Analysis of Bacteria Levels by Survival Status",
    x = "SampleWeek",
    y = "Rolling Average Serology Value",
    color = "Survival Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

