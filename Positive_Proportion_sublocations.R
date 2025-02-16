
# Load required libraries
library(dplyr)
library(ggplot2)
library(zoo)
library(RColorBrewer)
library(janitor)

# Read data
sublocation_data <- read.csv("C:/Users/sofia/OneDrive - University of Edinburgh/master/original data/ideal_farm.csv")
sublocation_data_clean <- sublocation_data %>% clean_names()


# Merge Sublocation data
long_data2_clean <- long_data2_clean %>%
  left_join(sublocation_data_clean %>% select(calf_id, sublocation), by = "calf_id")

# Deduplicate long_data2
long_data2_dedup <- long_data2_clean %>%
  distinct(calf_id, sample_week, Bacteria, .keep_all = TRUE)

# Recalculate visit summary
visit_summary <- long_data2_dedup %>%
  filter(!is.na(Average_Value)) %>%
  group_by(sample_week, Bacteria, sublocation) %>%
  summarize(
    Total_Animals = n_distinct(calf_id),
    Positive_Animals = sum(case_when(
      Bacteria == "serology_t_parva" ~ Average_Value >= 20,
      Bacteria == "serology_t_mutans" ~ Average_Value >= 20,
      Bacteria == "serology_b_bigemina" ~ Average_Value >= 15,
      Bacteria == "serology_a_marginale" ~ Average_Value >= 15,
      TRUE ~ 0
    ), na.rm = TRUE)
  ) %>%
  filter(Total_Animals > 0) %>%
  mutate(Proportion_Positive = Positive_Animals / Total_Animals)

# Sliding window average
visit_summary <- visit_summary %>%
  group_by(Bacteria, sublocation) %>%
  arrange(sample_week) %>%
  mutate(Sliding_Avg = zoo::rollmean(Proportion_Positive, k = 3, fill = NA, align = "center"))

visit_summary_filtered <- visit_summary %>%
  filter(!is.na(Sliding_Avg))  # Keep only rows where Sliding_Avg is not NA

# Create line plot with faceting
ggplot(visit_summary_filtered, aes(x = sample_week, y = Sliding_Avg, color = sublocation, group = sublocation)) +
  geom_line(size = 1.1) +                              # Add rolling average lines
  geom_point(size = 2, alpha = 0.7) +                  # Add points for emphasis
  scale_color_manual(values = rep(brewer.pal(9, "Set1"), length.out = n_distinct(visit_summary$sublocation))) +
  labs(
    title = "Proportion of Animals Positive by Sublocation",
    x = "Sample Week",
    y = "Proportion Positive (3-Week Avg)",
    color = "Sublocation"
  ) +
  coord_cartesian(ylim = c(0, 1)) +                    # Limit y-axis to 0-1
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray90"),
    legend.position = "right"
  ) +
  facet_wrap(~ Bacteria, ncol = 2, scales = "free_y")  # Facet by Bacteria, 2 columns

