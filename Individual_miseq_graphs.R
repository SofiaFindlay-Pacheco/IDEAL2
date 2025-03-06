# Library
library(dplyr)
library(ggplot2)
library(zoo)

# Clean up the "Dead or alive at end of study" column
final_miseq_data_clean$dead_or_alive_at_end_of_study <- as.factor(final_miseq_data_clean$dead_or_alive_at_end_of_study)

# Group all "Dead" statuses together (including different death causes)
final_miseq_data_clean$dead_or_alive_at_end_of_study <- recode(final_miseq_data_clean$dead_or_alive_at_end_of_study,
                                                                "Dead: Infectious death" = "Dead",
                                                                "Dead: Death by trauma" = "Dead",
                                                                "Alive" = "Alive")
#Remove censored data
final_miseq_data_clean <- final_miseq_data_clean %>%   #Remove censored data
  filter(dead_or_alive_at_end_of_study != "Censored")
table(final_miseq_data_clean$dead_or_alive_at_end_of_study) # Check survival status counts

# Graph with sliding window for each bacteria, Anaplasma

# Calculate moving averages using a rolling window of size 3
avg_miseq_data <- final_miseq_data_clean %>%
  select(sample_week, dead_or_alive_at_end_of_study, anaplasma_platys_ef139459_ae) %>%  # Keep only relevant columns
  group_by(sample_week, dead_or_alive_at_end_of_study) %>%
  summarize(Average_Value = mean(anaplasma_platys_ef139459_ae, na.rm = TRUE), .groups = "drop") %>%
  arrange(sample_week) %>%  # Ensure data is sorted by SampleWeek
  group_by(dead_or_alive_at_end_of_study) %>%
  mutate(Rolling_Avg = zoo::rollmean(Average_Value, k = 3, fill = NA, align = "center"))

# Visualize the data with sliding window averages
ggplot(avg_miseq_data, aes(x = sample_week, y = Rolling_Avg, color = dead_or_alive_at_end_of_study, group = dead_or_alive_at_end_of_study)) +
  geom_line(size = 1.5) +  # Line for rolling averages
  geom_point(aes(y = Average_Value), alpha = 0.5) +  # Points for raw averages
  scale_color_manual(values = c("Alive" = "green", "Dead" = "red")) +  # Custom colors
  labs(
    title = "Sliding Window Analysis of Anaplasma bovis by Survival Status",
    x = "SampleWeek",
    y = "Rolling Average Value",
    color = "Survival Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# T-test
t.test(anaplasma_platys_ef139459_ae ~ dead_or_alive_at_end_of_study, data = final_miseq_data_clean)

# logistic regression
model <- glm(dead_or_alive_at_end_of_study ~ anaplasma_platys_ef139459_ae, 
             data = final_miseq_data_clean, family = binomial)
summary(model)


##################

# Select relevant columns (replace `col2`, `col3`, ..., `col7` with actual column names)
selected_columns <- c("anaplasma_platys_ef139459_ae", "anaplasma_bovis_u03775_ae", "anaplasma_bovis_ab983439_ae", "anaplasma_marginale_cp000030_ae", "anaplasma_platys_like_ku585990_ae", "anaplasma_phagocytophilum_u02521_ae", "candidatus_anaplasma_boleense_ku586025_ae", "uncultured_anaplasma_sp_clone_saso_ky924885_ae", "uncultured_anaplasma_sp_jn862825_ae" )

# Calculate moving averages using a rolling window of size 3
avg_miseq_data <- final_miseq_data_clean %>%
  select(sample_week, dead_or_alive_at_end_of_study, all_of(selected_columns)) %>%  
  group_by(sample_week, dead_or_alive_at_end_of_study) %>%
  summarize(across(all_of(selected_columns), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  arrange(sample_week) %>%
  group_by(dead_or_alive_at_end_of_study) %>%
  mutate(across(all_of(selected_columns), ~ zoo::rollmean(.x, k = 3, fill = NA, align = "center"), .names = "Rolling_Avg_{.col}"))

# Check summary
summary(avg_miseq_data)

# Example Visualization for Anaplasma
ggplot(avg_miseq_data, aes(x = sample_week, y = Rolling_Avg_anaplasma_platys_ef139459_ae, 
                           color = dead_or_alive_at_end_of_study, group = dead_or_alive_at_end_of_study)) +
  geom_line(size = 1.5) +  
  geom_point(aes(y = selected_columns), alpha = 0.5) +  
  scale_color_manual(values = c("Alive" = "green", "Dead" = "red")) +  
  labs(
    title = "Sliding Window Analysis of Anaplasma bovis & Additional Bacteria by Survival Status",
    x = "SampleWeek",
    y = "Rolling Average Value",
    color = "Survival Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
