library(tidyr)
library(dplyr)
library(janitor)


# Reshape the data to long format, including CalfID
long_data2 <- Brief_serology_clean %>%
  pivot_longer(
    cols = starts_with("Serology"),  # Columns containing the bacteria data
    names_to = "Bacteria",  # New column for bacteria names
    values_to = "Average_Value"  # Values will go into this column
  )
long_data2_clean <- long_data2 %>% filter(!is.na(sample_week))

# Check the structure of the long data to ensure CalfID is included
head(long_data2_clean)

#clean_data <- long_data2 %>%
long_data2_clean <- long_data2_clean %>% filter(!is.na(Average_Value))

# Group by CalfID and Bacteria, and filter for first detection of each bacterium
first_detection <- long_data2_clean %>%
  group_by(calf_id, Bacteria) %>%
  filter(case_when(
    Bacteria == "serology_t_parva" ~ Average_Value >= 20,
    Bacteria == "serology_t_mutans" ~ Average_Value >= 20,
    Bacteria == "serology_b_bigemina" ~ Average_Value >= 15,
    Bacteria == "serology_a_marginale" ~ Average_Value >= 15,
  )) %>% # Assuming values > 0 indicate detection
  slice_min(order_by = sample_week, n = 1)  # Keep the first detection

# Create scatter plot for first detection of bacteria
ggplot(first_detection, aes(x = sample_week, y = calf_id, color = Bacteria)) +
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

# Calculate proportion
# Count total animals and positive animals at each visit for each bacterium
visit_summary <- long_data2_clean %>%
  filter(!is.na(Average_Value)) %>%  # Ensure no missing values
  group_by(sample_week, Bacteria) %>%
  summarize(
    Total_Animals = n_distinct(calf_id),  # Total animals sampled
    Positive_Animals = sum(case_when(
      Bacteria == "serology_t_parva" ~ Average_Value >= 20,
      Bacteria == "serology_t_mutans" ~ Average_Value >= 20,
      Bacteria == "serology_b_bigemina" ~ Average_Value >= 15,
      Bacteria == "serology_a_marginale" ~ Average_Value >= 15,
      TRUE ~ 0
    ), na.rm = TRUE)  # Count positives based on thresholds
  ) %>%
  mutate(Proportion_Positive = Positive_Animals / Total_Animals)

# Inspect the summary data
head(visit_summary)

# Create a line plot showing proportion of positives over time
ggplot(visit_summary, aes(x = sample_week, y = Proportion_Positive, color = Bacteria, group = Bacteria)) +
  geom_line(size = 1.2) +                   # Line plot for proportions
  geom_point(size = 3) +                    # Points at each SampleWeek
  scale_color_manual(values = c("red", "blue", "green", "yellow")) +  # Custom colors for each bacterium
  labs(
    title = "Proportion of Animals Positive Over Time",
    x = "Sample Week",
    y = "Proportion Positive",
    color = "Bacteria"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

##Brief

Brief_long_data2 <- long_data2[70:140, ]

Brief_first_detection <- Brief_long_data2 %>%
  group_by(calf_id, Bacteria) %>%
  filter(case_when(
    Bacteria == "serology_t_parva" ~ Average_Value >= 20,
    Bacteria == "serology_t_mutans" ~ Average_Value >= 20,
    Bacteria == "serology_b_bigemina" ~ Average_Value >= 15,
    Bacteria == "serology_a_marginale" ~ Average_Value >= 15,
  )) %>% # Assuming values > 0 indicate detection
  slice_min(order_by = sample_week, n = 1)  # Keep the first detection

# Create scatter plot for first detection of bacteria
ggplot(Brief_first_detection, aes(x = sample_week, y = calf_id, color = Bacteria, group = Bacteria)) +
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

# Introduce sliding window

# Install and load the required package
#install.packages("zoo")  # For rolling functions
library(zoo)

# Calculate the proportion of positive animals for each bacterium at each visit
visit_summary <- long_data2 %>%
  filter(!is.na(Average_Value)) %>%  # Ensure no missing values
  group_by(sample_week, Bacteria) %>%
  summarize(
    Total_Animals = n_distinct(calf_id),  # Total animals sampled
    Positive_Animals = sum(case_when(
      Bacteria == "serology_t_parva" ~ Average_Value >= 20,
      Bacteria == "serology_t_mutans" ~ Average_Value >= 20,
      Bacteria == "serology_b_bigemina" ~ Average_Value >= 15,
      Bacteria == "serology_a_marginale" ~ Average_Value >= 15, 
      TRUE ~ 0
    ), na.rm = TRUE),  # Count positives based on thresholds
    .groups = "drop"
  ) %>%
  mutate(Proportion_Positive = Positive_Animals / Total_Animals)

# Inspect the summary data
head(visit_summary)

# Create a line plot showing proportion of positives over time
ggplot(visit_summary, aes(x = sample_week, y = Proportion_Positive, color = Bacteria, group =  Bacteria)) +
  geom_line(size = 1.2) +                   # Line plot for proportions
  geom_point(size = 3) +                    # Points at each SampleWeek
  scale_color_manual(values = c("red", "blue", "green", "yellow")) +  # Custom colors for each bacterium
  labs(
    title = "Proportion of Animals Positive Over Time",
    x = "Sample Week",
    y = "Proportion Positive",
    color = "Bacteria"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Calculate rolling averages for sliding window
visit_summary <- visit_summary %>%
  group_by(Bacteria) %>%
  arrange(sample_week) %>%
  mutate(Sliding_Avg = zoo::rollmean(Proportion_Positive, k = 3, fill = NA, align = "center"))

# Inspect the summary data
head(visit_summary)

# Calculate rolling averages for sliding window
visit_summary <- visit_summary %>%
  group_by(Bacteria) %>%
  arrange(sample_week) %>%
  mutate(Sliding_Avg = zoo::rollmean(Proportion_Positive, k = 3, fill = NA, align = "center"))

# Create a line plot showing proportion of positives over time with sliding window overlay
ggplot(visit_summary, aes(x = sample_week, y = Proportion_Positive, color = Bacteria, group = Bacteria )) +
  geom_line(size = 1.2) +                   # Line plot for proportions (raw data)
  geom_line(aes(y = Sliding_Avg), size = 1.2, linetype = "dashed") +  # Add rolling average line
  labs(
    title = "Proportion of Animals Positive Over Time",
    x = "Sample Week",
    y = "Proportion Positive",
    color = "Bacteria"
  ) +
  theme_minimal()


# Inspect the summary data
head(visit_summary)

# Create a line plot showing proportion of positives over time with sliding window overlay
ggplot(visit_summary, aes(x = sample_week, y = Proportion_Positive, color = Bacteria, group = Bacteria)) +
  geom_line(size = 1.2) +                   # Line plot for proportions (raw data)          #remove this for only sliding window
  geom_point(size = 3) +                    # Points at each SampleWeek                     #remove this for only sliding window
  geom_line(aes(y = Sliding_Avg), size = 1.2, linetype = "dashed") +  # Sliding window line
  scale_color_manual(values = c("red", "blue", "green", "yellow")) +  # Custom colors for each bacterium
  labs(
    title = "Proportion of Animals Positive Over Time with Sliding Window",
    x = "Sample Week",
    y = "Proportion Positive",
    color = "Bacteria"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
 
