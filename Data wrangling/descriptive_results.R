# run upset graphs first
############################ Descriptive #############################
# Prop infected with all strains of one pathogen, e.g theileria: 
# Choose the pathogen columns
pathogen_cols <- c( "theileria_mutans_af078815_tb"   ,                                                      
                    "theileria_sp_strain_msd_af078816_tb"       ,                                           
                    "theileria_parva_l02366_tb"             ,                                           
                    "theileria_velifera_af097993_tb" ,
                    "anaplasma_bovis_ab983439_ae",
                    "anaplasma_phagocytophilum_u02521_ae",
"anaplasma_platys_like_ku585990_ae",
"uncultured_anaplasma_sp_clone_saso_ky924885_ae")

# Reshape and summarise
prop_infected_long <- UpSet_graph_data %>%
  group_by(sample_week, calf_id) %>%
  summarise(across(all_of(pathogen_cols), ~ as.integer(any(. > 0))), .groups = "drop") %>%
  pivot_longer(cols = all_of(pathogen_cols), names_to = "pathogen", values_to = "infected") %>%
  group_by(sample_week, pathogen) %>%
  summarise(n_infected = sum(infected), n_calves = n_distinct(calf_id), prop = n_infected / n_calves, .groups = "drop")

# Calculate whether each calf was infected with ANY Theileria at each week
prop_theileria <- UpSet_graph_data %>%
  group_by(sample_week, calf_id) %>%
  summarise(
    pathogen_cols = as.integer(any(across(all_of(pathogen_cols), ~ . > 0))),
    .groups = "drop"
  ) %>%
  group_by(sample_week) %>%
  summarise(
    n_infected = sum(pathogen_cols),
    n_calves = n_distinct(calf_id),
    prop = n_infected / n_calves,
    .groups = "drop"
  )

####
#write.csv(prop_theileria, "prop_infected_by_week_Theileria.csv", row.names = FALSE)

############################################################################### 
library(ggplot2)
library(scales)

# Plot the proportion of calves infected with any Theileria strain
ggplot(prop_theileria, aes(x = sample_week, y = prop)) +
  geom_line(aes(group = 1), color = "black", size = 1, alpha = 0.3) +  # group = 1 is the key fix
  geom_point(color = "black", size = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Proportion of Calves Infected with Any Theileria Strain by Week",
    x = "Week",
    y = "Proportion Infected (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

###########################################################################################################

library(dplyr)
# Sample week clean up
weekly_summary <- final_miseq_data_clean %>%
  mutate(sample_week = floor(sample_week)) %>%
  group_by(sample_week) %>%
  summarise(
    n_calves = n_distinct(calf_id),
    n_deaths = sum(event == 1, na.rm = TRUE),
    n_alive_or_censored = sum(event == 0, na.rm = TRUE)
  )

#write.csv(weekly_summary, "weekly_summary.csv", row.names = FALSE)

###########################################################################################################

# Reshape and summarise
prop_infected_long <- UpSet_graph_data %>%
  group_by(sample_week, calf_id) %>%
  summarise(across(all_of(pathogen_cols), ~ as.integer(any(. > 0))), .groups = "drop") %>%
  pivot_longer(cols = all_of(pathogen_cols), names_to = "pathogen", values_to = "infected") %>%
  group_by(sample_week, pathogen) %>%
  summarise(n_infected = sum(infected), n_calves = n_distinct(calf_id), prop = n_infected / n_calves, .groups = "drop")

library(ggplot2)

ggplot(prop_infected_long, aes(x = sample_week, y = prop, color = pathogen, group = pathogen)) +
  geom_line(size = 1, alpha = 0.3) +  # faint lines
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Proportion of Calves Infected per Sample Week",
    x = "Sample Week",
    y = "Proportion Infected"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.title = element_blank())

############################################################################################ Heatmap version
ggplot(prop_infected_long, aes(x = sample_week, y = pathogen, fill = prop)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "magma", name = "prop") +
  theme_minimal() +
  labs(
    title = "Heatmap of Infection Proportion by Pathogen and Week",
    x = "Sample Week",
    y = "Pathogen"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#############




library(ggplot2)

# Create new variable to group time_to_event
survival_data <- survival_data %>%
  mutate(
    event_group = ifelse(time_to_event == 51, "Week 51 (Alive/Censored)", "Before Week 51")
  )

# Plot
ggplot(survival_data, aes(x = time_to_event, fill = as.factor(event))) +
  geom_histogram(binwidth = 2, color = "black") +
  facet_wrap(~event_group, scales = "free_y") +
  scale_fill_manual(values = c("0" = "#4575b4", "1" = "#d73027"),
                    labels = c("0" = "Alive/Censored", "1" = "Died"),
                    name = "Outcome") +
  labs(
    title = "Calf Time to Event Distribution (by Event Status)",
    x = "Weeks of Age",
    y = "Number of Calves"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

############

# Load libraries
library(dplyr)
library(ggplot2)
library(survival)

# Basic survival data
survival_data <- final_miseq_data_clean %>%
  distinct(calf_id, .keep_all = TRUE) %>%
  select(calf_id, event, time_to_event, definitive_aetiological_cause)

# Ensure time_to_event is numeric
survival_data$time_to_event <- as.numeric(survival_data$time_to_event)

# Add outcome label
survival_data <- survival_data %>%
  mutate(
    outcome_label = case_when(
      definitive_aetiological_cause == "Dead" ~ "Died",
      definitive_aetiological_cause == "Censored" ~ "Censored",
      definitive_aetiological_cause == "Alive" ~ "Alive"
    ),
    event_group = ifelse(time_to_event == 51, "Week 51", "Before Week 51")
  )

# Count totals
summary_stats <- survival_data %>%
  summarise(
    total_calves = n(),
    total_deaths = sum(outcome_label == "Died"),
    total_alive = sum(outcome_label == "Alive"),
    total_censored = sum(outcome_label == "Censored"),
    median_time_to_event = median(time_to_event),
    mean_time_to_event = mean(time_to_event),
    sd_time_to_event = sd(time_to_event)
  )

weekly_summary_stats <- survival_data %>%
  group_by(time_to_event) %>%
  summarise(
    total_calves = n(),
    total_deaths = sum(outcome_label == "Died"),
    total_alive = sum(outcome_label == "Alive"),
    total_censored = sum(outcome_label == "Censored"),
    .groups = "drop"
  )

# Plot
ggplot(survival_data, aes(x = time_to_event, fill = outcome_label)) +
  geom_histogram(binwidth = 2, color = "black") +
  facet_wrap(~event_group, scales = "free_y") +
  scale_fill_manual(
    values = c("Died" = "#d73027", "Censored" = "#4575b4", "Alive" = "#1a9850"),
    name = "Outcome"
  ) +
  labs(
    title = "Calf Time to Event Distribution by Outcome",
    x = "Weeks of Age",
    y = "Number of Calves"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

##############

library(dplyr)
library(ggplot2)

# Filter for only dead calves and select cause of death
death_causes <- final_miseq_data_clean %>%
  distinct(calf_id, .keep_all = TRUE) %>%
  filter(event == 1) %>%
  select(calf_id, definitive_aetiological_cause)

# Summarise cause of death frequencies
cause_counts <- death_causes %>%
  group_by(definitive_aetiological_cause) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# Plot
ggplot(cause_counts, aes(x = reorder(definitive_aetiological_cause, n), y = n)) +
  geom_col(fill = "#d73027", color = "black") +
  coord_flip() +
  labs(
    title = "Cause of Death in Calves",
    x = "Definitive Aetiological Cause",
    y = "Number of Deaths"
  ) +
  theme_minimal(base_size = 14)

library(dplyr)
library(ggplot2)

# Filter only dead calves and keep necessary columns
death_week_data <- final_miseq_data_clean %>%
  distinct(calf_id, .keep_all = TRUE) %>%
  filter(event == 1) %>%
  select(calf_id, time_to_event, definitive_aetiological_cause) %>%
  mutate(week = floor(as.numeric(time_to_event)))  # Round down to whole weeks

# Count deaths by week and cause
death_by_week <- death_week_data %>%
  group_by(week, definitive_aetiological_cause) %>%
  summarise(n = n(), .groups = "drop")

# Plot
library(viridis)

ggplot(death_by_week, aes(x = week, y = n, fill = definitive_aetiological_cause)) +
  geom_col(color = "black") +
  scale_x_continuous(breaks = seq(0, 51, by = 2)) +
  scale_fill_viridis_d(option = "turbo", name = "Cause of Death") +
  labs(
    title = "Deaths by Week and Cause in Calves",
    x = "Week of Age",
    y = "Number of Deaths"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")



















# Reshape and summarise
prop_alive_long <- UpSet_graph_data %>%
  group_by(sample_week, calf_id) %>%
  summarise(across(all_of(pathogen_cols), ~ as.integer(any(. > 0))), .groups = "drop") %>%
  pivot_longer(cols = all_of(pathogen_cols), names_to = "pathogen", values_to = "infected") %>%
  group_by(sample_week, pathogen) %>%
  summarise(n_infected = sum(infected), n_calves = n_distinct(calf_id), prop = n_infected / n_calves, .groups = "drop")

# Calculate whether each calf was infected with ANY Theileria at each week
prop_theileria <- UpSet_graph_data %>%
  group_by(sample_week, calf_id) %>%
  summarise(
    pathogen_cols = as.integer(any(across(all_of(pathogen_cols), ~ . > 0))),
    .groups = "drop"
  ) %>%
  group_by(sample_week) %>%
  summarise(
    n_infected = sum(pathogen_cols),
    n_calves = n_distinct(calf_id),
    prop = n_infected / n_calves,
    .groups = "drop"
  )




##################################################################### Deaths over time
library(dplyr)
library(ggplot2)

# Summarize cumulative deaths over time
death_curve <- final_miseq_data_clean %>%
  filter(event == 1) %>%  # Only include death events
  group_by(time_to_event) %>%
  summarise(deaths = n()) %>%
  arrange(time_to_event) %>%
  mutate(cumulative_deaths = cumsum(deaths))

# Plot
ggplot(death_curve, aes(x = time_to_event, y = cumulative_deaths)) +
  geom_step(color = "red", size = 1.2) +
  labs(title = "Cumulative Number of Calves Dying Over Time",
       x = "Time to Event (e.g., weeks)",
       y = "Cumulative Deaths") +
  theme_minimal()



library(dplyr)
library(ggplot2)

# Summarize deaths over time (not cumulative)
death_curve <- final_miseq_data_clean %>%
  filter(event == 1) %>%  # Only include death events
  group_by(time_to_event) %>%
  summarise(deaths = n()) %>%
  arrange(time_to_event)

# Plot number of deaths at each time point
ggplot(death_curve, aes(x = time_to_event, y = deaths)) +
  geom_col(fill = "red") +  # bar plot to show deaths per time point
  labs(title = "Number of Calves Dying Over Time",
       x = "Time to Event (e.g., weeks)",
       y = "Number of Deaths") +
  theme_minimal()


library(dplyr)
library(ggplot2)

# Count deaths per time point, grouped by calf_id
death_counts <- final_miseq_data_clean %>%
  filter(event == 1) %>%
  distinct(calf_id, .keep_all = TRUE) %>%
  count(time_to_event, name = "deaths")

# Plot
ggplot(death_counts, aes(x = time_to_event, y = deaths)) +
  geom_col(fill = "red") +
  labs(title = "Number of Calves Dying at Each Time Point",
       x = "Time to Event (e.g., weeks)",
       y = "Number of Deaths") +
  theme_minimal()

