# Library
library(zoo)

# Calculating the Proportion of animals testing positive - Count total animals and positive animals at each visit for each bacterium
visit_summary <- vertical_Brief_Serology %>%
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

# Introduce sliding window
# Calculate rolling averages for sliding window
visit_summary <- visit_summary %>%
  group_by(Bacteria) %>%
  arrange(sample_week) %>%
  mutate(Sliding_Avg = zoo::rollmean(Proportion_Positive, k = 3, fill = NA, align = "center"))

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

