# Libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(janitor)
library(tidyr)

# Calculating the Average serology values per week
# Handle missing values
Brief_serology_clean <- Brief_serology_clean %>% filter(!is.na(sample_week))

# Convert columns to numbric format
Brief_serology_clean$serology_t_parva <- as.numeric(Brief_serology_clean$serology_t_parva)
Brief_serology_clean$serology_t_mutans <- as.numeric(Brief_serology_clean$serology_t_mutans)
Brief_serology_clean$serology_b_bigemina <- as.numeric(Brief_serology_clean$serology_b_bigemina)
Brief_serology_clean$serology_a_marginale <- as.numeric(Brief_serology_clean$serology_a_marginale)

# Aggregate data by sample week to get overall bacterial levels each sample week overall for all calfs
serology_by_Sample_week <- Brief_serology_clean %>%
  group_by(sample_week) %>%
  summarize(
    Avg_T_parva = mean(serology_t_parva, na.rm = TRUE),
    Avg_T_mutans = mean(serology_t_mutans, na.rm = TRUE),
    Avg_B_bigemina = mean(serology_b_bigemina, na.rm = TRUE),
    Avg_A_marginale = mean(serology_a_marginale, na.rm = TRUE)
  )

# Reshape data to long format
vertical_serology_by_Sample_week <- serology_by_Sample_week %>%
  pivot_longer(
    cols = starts_with("Avg_"),
    names_to = "Bacteria",
    values_to = "Average_Value"
  )

# Remove NAs
vertical_serology_by_Sample_week <- na.omit(vertical_serology_by_Sample_week)

# Create the timeline plot
ggplot(vertical_serology_by_Sample_week, aes(x = sample_week, y = Average_Value, color = Bacteria, group = Bacteria)) +
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

