# Library
library(dplyr)
library(ggplot2)
library(zoo)
# install.packages("brglm")


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

# Graph with sliding window for each bacteria, ehrlichia

# Calculate moving averages using a rolling window of size 3
avg_miseq_data <- final_miseq_data_clean %>%
  select(sample_week, dead_or_alive_at_end_of_study, ehrlichia_ruminantium_x61659_ae) %>%  # Keep only relevant columns
  group_by(sample_week, dead_or_alive_at_end_of_study) %>%
  summarize(Average_Value = mean(ehrlichia_ruminantium_x61659_ae, na.rm = TRUE), .groups = "drop") %>%
  arrange(sample_week) %>%  # Ensure data is sorted by SampleWeek
  group_by(dead_or_alive_at_end_of_study) %>%
  mutate(Rolling_Avg = zoo::rollmean(Average_Value, k = 3, fill = NA, align = "center"))

# Visualize the data with sliding window averages
ggplot(avg_miseq_data, aes(x = sample_week, y = Rolling_Avg, color = dead_or_alive_at_end_of_study, group = dead_or_alive_at_end_of_study)) +
  geom_line(size = 1.5) +  # Line for rolling averages
  geom_point(aes(y = Average_Value), alpha = 0.5) +  # Points for raw averages
  scale_color_manual(values = c("Alive" = "green", "Dead" = "red")) +  # Custom colors
  labs(
    title = "Sliding Window Analysis of ehrlichia by Survival Status",
    x = "SampleWeek",
    y = "Rolling Average Value",
    color = "Survival Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# T-test
t.test(ehrlichia_ruminantium_x61659_ae ~ dead_or_alive_at_end_of_study, data = final_miseq_data_clean)

# logistic regression
model <- glm(dead_or_alive_at_end_of_study ~ ehrlichia_ruminantium_x61659_ae, 
             data = final_miseq_data_clean, family = binomial)
#summary(model)


##################
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(zoo)

# Define the bacteria columns
bacteria_columns <- c("ehrlichia_sp_tibet_ehrlichia_canis_ehrlichia_minasensis_af414399_ay394465_mt163430_ae", 
                      "ehrlichia_ruminantium_x61659_ae")

# Compute the sum of bacteria values for each sample week and survival status
avg_miseq_data <- final_miseq_data_clean %>%
  select(sample_week, dead_or_alive_at_end_of_study, all_of(bacteria_columns)) %>%
  group_by(sample_week, dead_or_alive_at_end_of_study) %>%
  summarize(Average_Value = sum(across(all_of(bacteria_columns)), na.rm = TRUE), .groups = "drop") %>%
  arrange(sample_week) %>%
  group_by(dead_or_alive_at_end_of_study) %>%
  mutate(Rolling_Avg = zoo::rollmean(Average_Value, k = 3, fill = NA, align = "center"))

# Plot the rolling average trend line only
ggplot(avg_miseq_data, aes(x = sample_week, y = Rolling_Avg, 
                           color = dead_or_alive_at_end_of_study, 
                           group = dead_or_alive_at_end_of_study)) +
  geom_line(size = 1.5) +  # Smooth rolling average line
  scale_color_manual(values = c("Alive" = "green", "Dead" = "red")) + 
  scale_y_continuous(labels = scales::comma) +  # Format Y-axis with commas +
  labs(
    title = "Rolling Average of Anaplasma Load by Survival Status",
    x = "Sample Week",
    y = "Rolling Average Bacteria Load",
    color = "Survival Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# T-test
t.test(bacteria_columns ~ dead_or_alive_at_end_of_study, data = final_miseq_data_clean)

# logistic regression
model <- glm(dead_or_alive_at_end_of_study ~ bacteria_columns, 
             data = final_miseq_data_clean, family = binomial)
# summary(model)

####################################
# Remove rows with missing values in relevant columns
final_miseq_data_clean_complete <- final_miseq_data_clean %>%
  drop_na(bacteria_columns, sum_bacterial_load, dead_or_alive_at_end_of_study)

# Fit the models again using the cleaned dataset
model_1 <- glm(dead_or_alive_at_end_of_study ~ bacteria_columns, 
               data = final_miseq_data_clean_complete, 
               family = binomial)

model_2 <- glm(dead_or_alive_at_end_of_study ~ sum_bacterial_load, 
               data = final_miseq_data_clean_complete, 
               family = binomial)

model_3 <- glm(dead_or_alive_at_end_of_study ~ anaplasma_platys_ef139459_ae + sum_bacterial_load, 
               data = final_miseq_data_clean_complete, 
               family = binomial)

# Compare AIC values again
AIC(model_1, model_2, model_3)
