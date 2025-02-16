# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(janitor)


# Convert dates to Date format
#Brief_serology$`SampleWeek` <- as.Date(Brief_serology$`SampleWeek`)  # Converts to Date type

# Handle missing values (if necessary)
Brief_serology_clean <- Brief_serology_clean %>% filter(!is.na(sample_week))

Brief_serology_clean$serology_t_parva <- as.numeric(Brief_serology_clean$serology_t_parva)
Brief_serology_clean$serology_t_mutans <- as.numeric(Brief_serology_clean$serology_t_mutans)
Brief_serology_clean$serology_b_bigemina <- as.numeric(Brief_serology_clean$serology_b_bigemina)
Brief_serology_clean$serology_a_marginale <- as.numeric(Brief_serology_clean$serology_a_marginale)

# Aggregate data by  visit date
aggregated_data <- Brief_serology_clean %>%
  group_by(sample_week) %>%
  summarize(
    Avg_T_parva = mean(serology_t_parva, na.rm = TRUE),
    Avg_T_mutans = mean(serology_t_mutans, na.rm = TRUE),
    Avg_B_bigemina = mean(serology_b_bigemina, na.rm = TRUE),
    Avg_A_marginale = mean(serology_a_marginale, na.rm = TRUE)
  )

library(tidyr)

# Reshape data to long format
long_data <- aggregated_data %>%
  pivot_longer(
    cols = starts_with("Avg_"),
    names_to = "Bacteria",
    values_to = "Average_Value"
  )

long_data <- na.omit(long_data)

# Create the timeline plot
ggplot(long_data, aes(x = sample_week, y = Average_Value, color = Bacteria, group = Bacteria)) +
  geom_line(linewidth = 1) +                      
  geom_point(size = 2) +                     
  scale_color_manual(values = c("red", "blue", "green", "purple")) +  
  labs(
    title = "Group-Level Timeline of Bacteria",
    x = "Serology Visit Date",
    y = "Average Serology Value",
    color = "Bacteria"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

