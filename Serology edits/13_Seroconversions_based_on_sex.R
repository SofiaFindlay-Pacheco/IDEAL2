# Library
library(dplyr)
library(ggplot2)
library(janitor)

# Filter and summarize seroconversion data
seroconversion_data <- vertical_Brief_Serology %>%
  filter(
    (Bacteria == "serology_t_parva" & Average_Value >= 20) |
      (Bacteria == "serology_t_mutans" & Average_Value >= 20) |
      (Bacteria == "serology_b_bigemina" & Average_Value >= 15) |
      (Bacteria == "serology_a_marginale" & Average_Value >= 15)
  ) %>%
  group_by(visit_id, Bacteria, calf_sex) %>%
  summarize(seroconversion_week = min(sample_week, na.rm = TRUE), .groups = "drop") %>%
  mutate(Seroconversion_Week = as.numeric(seroconversion_week))  # Convert 'seroconversion_week' to numeric

# Plot with debugging info and position_jitter() to Spread Out Points
ggplot(seroconversion_data %>% filter(!is.na(calf_sex)),  
       aes(x = Seroconversion_Week, y = Bacteria, color = calf_sex)) +
  geom_point(size = 4, alpha = 0.7, position = position_jitter(width = 0.2, height = 0.2)) + 
  scale_color_manual(values = c("M" = "blue", "F" = "pink")) +  
  labs(
    title = "Seroconversion Timing by Sex",
    x = "Week of Seroconversion",
    y = "Bacteria",
    color = "Calf Sex"
  ) +
  theme_minimal()




#####

  # Filter and summarize seroconversion data for ONE bacteria
  seroconversion_data <- vertical_Brief_Serology %>%
    filter(
    #  Bacteria == "serology_t_parva" & Average_Value >= 20
      # Bacteria == "serology_t_mutans" & Average_Value >= 20
       Bacteria == "serology_b_bigemina" & Average_Value >= 15
      # Bacteria == "serology_a_marginale" & Average_Value >= 15
    ) %>%
    group_by(visit_id, Bacteria, calf_sex, calf_id) %>%
    summarize(seroconversion_week = min(sample_week, na.rm = TRUE), .groups = "drop")%>%
    mutate(Seroconversion_Week = as.numeric(seroconversion_week))  # Convert 'seroconversion_week' to numeric
  
  # Plot with debugging info
  ggplot(data = seroconversion_data %>% filter(!is.na(calf_sex)),  # Remove NAs
         aes(x = Seroconversion_Week, y = calf_id, color = calf_sex, group = calf_sex)) +
    geom_point(size = 4, alpha = 0.7) +
    scale_color_manual(values = c("M" = "blue", "F" = "pink")) +  # Remove gray (NA)
    labs(
      title = "Seroconversion Timing by Sex",
      x = "Week of Seroconversion",
      y = "calf id",
      color = "Calf Sex"
    ) +
    theme_minimal()

  
  
  
  #####
  
# Some ratios still based on ONE bacteria
# Count M and F per seroconversion week + Compute M/F Ratio
seroconversion_counts <- seroconversion_data %>%
  group_by(seroconversion_week, calf_sex) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = calf_sex, values_from = Count, values_fill = list(Count = 0)) %>%
  mutate(Ratio_Male_Female = ifelse(F == 0, NA, M / F))  # Avoid division by zero

# Plot: Seroconversion timing for each calf
ggplot(seroconversion_counts, aes(x = seroconversion_week)) +
  geom_line(aes(y = M, color = "Male"), size = 1, group = "M") +  # Plot for males
  geom_line(aes(y = F, color = "Female"), size = 1, group = "F") +  # Plot for females
  labs(
    title = "Seroconversion Count by Sex",
    x = "Week of Seroconversion",
    y = "Count",
    color = "Calf Sex"
  ) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "pink")) +
  theme_minimal()

# Convert seroconversion_week to numeric
seroconversion_counts$seroconversion_week <- as.numeric(seroconversion_counts$seroconversion_week)

# Plot: Male-to-Female Seroconversion Ratio Over Time
ggplot(seroconversion_counts, aes(x = seroconversion_week, y = Ratio_Male_Female)) +
  geom_line() + 
  geom_point(size = 3) +
  labs(
    title = "Male-to-Female Seroconversion Ratio Over Time",
    x = "Week of Seroconversion",
    y = "M/F Ratio"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),  # Adjust x-axis text size
    axis.text.y = element_text(size = 10)   # Adjust y-axis text size
  ) +
  scale_x_continuous(breaks = seq(min(seroconversion_counts$seroconversion_week), 
                                  max(seroconversion_counts$seroconversion_week), 
                                  by = 1))  # Customizing x-axis ticks


