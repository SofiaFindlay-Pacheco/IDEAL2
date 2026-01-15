# Library
library(tidyr)
library(dplyr)
library(janitor)

# Find first detections

# Reshape the data to vertical format, including CalfID
vertical_Brief_Serology <- Brief_serology_clean %>%
  pivot_longer(
    cols = starts_with("Serology"),  # Columns containing the bacteria data
    names_to = "Bacteria",  # New column for bacteria names
    values_to = "Average_Value"  # Values will go into this column
  )
#Remove NAs
vertical_Brief_Serology <- vertical_Brief_Serology %>% filter(!is.na(sample_week))

# make the serology data long format
final_miseq_data_clean <- final_miseq_data_clean %>%
  pivot_longer(cols = 41:44, names_to = "pathogen", values_to = "serology_value")

# Group by CalfID and Bacteria, and filter for first detection of each bacterium
first_detection <- final_miseq_data_clean %>%
  group_by(calf_id, pathogen) %>%
  filter(case_when(
    pathogen == "serology_t_parva" ~ serology_value >= 20,
    pathogen == "serology_t_mutans" ~ serology_value >= 20,
    pathogen == "serology_b_bigemina" ~ serology_value >= 15,
    pathogen == "serology_a_marginale" ~ serology_value >= 15,
  )) %>% # Assuming values > 0 indicate detection
  slice_min(order_by = sample_week, n = 1)  # Keep the first detection

# Create scatter plot for first detection of bacteria
ggplot(first_detection, aes(x = sample_week, y = calf_id, color = pathogen)) +
  geom_point(size = 4) +                      # Scatter points for first detection
  scale_color_manual(values = c("red", "blue", "green", "yellow")) +  # Custom colors for each bacterium
  labs(
    title = "First Detection of Bacteria by Visit Date",
    x = "Serology Visit Date",
    y = "Sample (Calf)",
    color = "Bacteria"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

## Now first detection for smaller amount of calfIDs

#Select which CaldIDs i want to include
Reduced_vertical_Brief_Serology <- vertical_Brief_Serology[1:120, ]

#Set threshold from which to base the seroconversion
Reduced_first_detection <- Reduced_vertical_Brief_Serology %>%
  group_by(calf_id, Bacteria) %>%
  filter(case_when(
    Bacteria == "serology_t_parva" ~ Average_Value >= 20,
    Bacteria == "serology_t_mutans" ~ Average_Value >= 20,
    Bacteria == "serology_b_bigemina" ~ Average_Value >= 15,
    Bacteria == "serology_a_marginale" ~ Average_Value >= 15,
  )) %>% # Assuming values > 0 indicate detection
  slice_min(order_by = sample_week, n = 1)  # Keep the first detection

# Create scatter plot 
ggplot(Reduced_first_detection, aes(x = sample_week, y = calf_id, color = Bacteria, group = Bacteria)) +
  geom_point(size = 4) +                      # Scatter points for first detection
  scale_color_manual(values = c("red", "blue", "green", "yellow")) +  # Custom colors for each bacterium
  labs(
    title = "First Detection of Bacteria by Visit Date",
    x = "Serology Visit Date",
    y = "Sample (Calf)",
    color = "Bacteria"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


# Calculate the proportion of calves that seoconverted for each bacterium each week

# Summarize the data to get the number of calves that first seroconverted each week for each bacterium
seroconversion_summary <- first_detection %>%
  group_by(sample_week, Bacteria) %>%
  summarise(
    calves_seroconverted = n_distinct(calf_id),  # Number of unique calves that seroconverted
    .groups = 'drop'
  )

# Summarize total calves available for each bacterium
total_calves <- first_detection %>%
  group_by(Bacteria) %>%
  summarise(total_calves = n_distinct(calf_id), .groups = 'drop')

# Merge the total calves data with the seroconversion summary to calculate proportion
seroconversion_proportion <- seroconversion_summary %>%
  left_join(total_calves, by = "Bacteria") %>%
  mutate(proportion = calves_seroconverted / total_calves)  # Proportion of calves that seroconverted

# Filter out samples before week 3
seroconversion_proportion <- seroconversion_proportion %>%
  filter(sample_week >= 01)

# Create a plot showing the proportion of calves seroconverting each week
ggplot(seroconversion_proportion, aes(x = sample_week, y = proportion, color = Bacteria, group = Bacteria)) +
  geom_line(size = 1.2) +                      # Line plot for proportion of seroconversion
  geom_point(size = 3) +                       # Points for seroconversion data
  scale_color_manual(values = c("red", "blue", "green", "yellow")) +  # Custom colors for each bacterium
  labs(
    title = "Proportion of Calves Seroconverting by Week and Bacteria",
    x = "Serology Visit Date (Week)",
    y = "Proportion of Calves Seroconverting",
    color = "Bacteria"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels



