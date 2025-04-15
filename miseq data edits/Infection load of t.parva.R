# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Define just the Theileria parva column
pathogen_column <- "theileria_parva_l02366_tb"

# Make sure your calf ID column exists (adjust if your column name is different, e.g., calf_id)
# Compute the pathogen load per calf per week
parva_miseq_data <- final_miseq_data_clean %>%
  select(sample_week, calf_id, definitive_aetiological_cause, all_of(pathogen_column)) %>%
  group_by(sample_week, calf_id, definitive_aetiological_cause) %>%
  summarize(Parva_Load = sum(.data[[pathogen_column]], na.rm = TRUE), .groups = "drop") %>%
  arrange(sample_week, calf_id)


# Plot the Theileria parva load over time for each calf
ggplot(parva_miseq_data, aes(x = sample_week, y = Parva_Load, 
                             color = definitive_aetiological_cause, 
                             group = calf_id)) +
  geom_line(size = 1) +  # Thin lines because many calves
  scale_y_log10(labels = scales::comma) +  # Log scale Y-axis
  scale_color_manual(values = c("Alive" = "green", "Dead" = "red")) + 
  labs(
    title = "Theileria parva Load Over Time for Each Calf",
    x = "Sample Week",
    y = "Log-Scaled T. parva Load",
    color = "Survival Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Define just the Theileria parva column
pathogen_column <- "theileria_parva_l02366_tb"

# Compute cumulative pathogen load per calf over time
parva_miseq_data <- final_miseq_data_clean %>%
  select(sample_week, calf_id, definitive_aetiological_cause, all_of(pathogen_column)) %>%
  group_by(sample_week, calf_id, definitive_aetiological_cause) %>%
  summarize(Parva_Load = sum(.data[[pathogen_column]], na.rm = TRUE), .groups = "drop") %>%
  arrange(calf_id, sample_week) %>%
  group_by(calf_id) %>%
  mutate(Cumulative_Parva_Load = cumsum(Parva_Load)) %>%
  ungroup()

# Plot cumulative load over time for each calf
ggplot(parva_miseq_data, aes(x = sample_week, y = Cumulative_Parva_Load, 
                             color = definitive_aetiological_cause, 
                             group = calf_id)) +
  geom_line(size = 1) +
  scale_y_log10(labels = scales::comma) +  # Log scale Y-axis
  scale_color_manual(values = c("Alive" = "green", "Dead" = "red")) + 
  labs(
    title = "Cumulative Theileria parva Load Over Time for Each Calf",
    x = "Sample Week",
    y = "Log-Scaled Cumulative T. parva Load",
    color = "Survival Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

