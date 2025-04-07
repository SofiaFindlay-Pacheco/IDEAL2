
# Load necessary library
#install.packages("summarytools")
#install.packages("gtsummary")

library(nnet)
library(janitor)
library(summarytools)
library(gtsummary)
library(dplyr)
library(tidyr)
library(readxl)
library(tidyverse)
library(survival)
library(survminer)


Genomics <- here("Edited original data", "Genomics.xlsx")
Genomics <- read_excel(Genomics)
Genomics_clean <- Genomics %>% clean_names()

########### Arrange data #####################################

# Clean up survival status column
final_miseq_data_clean$dead_or_alive_at_end_of_study <- as.factor(final_miseq_data_clean$dead_or_alive_at_end_of_study)

# Group all "Dead" statuses together
final_miseq_data_clean$dead_or_alive_at_end_of_study <- recode(final_miseq_data_clean$dead_or_alive_at_end_of_study,
                                                               "Dead: Infectious death" = "Dead",
                                                               "Dead: Death by trauma" = "Alive",
                                                               "Alive" = "Alive",
                                                               "Censored" = "Censored")


# Assign numeric event status (1 = Dead, 0 = Censored/Alive)
final_miseq_data_clean$event <- ifelse(final_miseq_data_clean$dead_or_alive_at_end_of_study == "Dead", 1, 0)

# Calculate survival time
# Ensure survival time is numeric
final_miseq_data_clean$date_last_visit_with_data <- as.Date(final_miseq_data_clean$date_last_visit_with_data)
final_miseq_data_clean$date_of_birth <- as.Date(final_miseq_data_clean$date_of_birth)

# Now subtract date of death - date of birth
final_miseq_data_clean$time_to_event <- as.numeric(final_miseq_data_clean$date_of_death - final_miseq_data_clean$date_of_birth)

# Handle the data without date of death, do it as date of last data - dat of birth
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(time_to_event = ifelse(
    is.na(date_of_death) & event == 1, 
    as.numeric(date_last_visit_with_data - date_of_birth), 
    as.numeric(date_of_death - date_of_birth)
  ))
final_miseq_data_clean$time_to_event <- final_miseq_data_clean$time_to_event / 7

# Manually set one error
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(date_last_visit_with_data = case_when(
    calf_id == "CA020610160" ~ as.Date("2008-07-17"),  # Set specific date for this calf
    TRUE ~ date_last_visit_with_data  # Keep existing values for others
  ))

# Update the 'time_to_event' for alive calves (event == 0) to the max_week
max_week <- 51
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(
    time_to_event = ifelse(event == 0, max_week, time_to_event)  # Set to max_week for alive calves
  )
# Adjust time_to_event only for censored cases
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(
    time_to_event = ifelse(
      dead_or_alive_at_end_of_study == "Censored",  # Only modify censored calves
      as.numeric(date_last_visit_with_data - date_of_birth) / 7,  # Time until last visit
      time_to_event  # Keep existing values for Alive & Dead calves
    )
  )

############ Merge the genomics and calf ID dataframe ########################

# Merge two data frames by 'calf_id', keeping all columns from both
merged_genomics <- full_join(final_miseq_data_clean, Genomics_clean, by = "calf_id")
merged_genomics <- merged_genomics %>%
  group_by(calf_id) %>%
  slice(1) %>%  # Select the first row of each group
  ungroup()
merged_genomics <- select(merged_genomics, calf_id, event, time_to_event, genotype, died, dead_or_alive_at_end_of_study)

merged_genomics <- merged_genomics %>%
  mutate(
    dead_or_alive_at_end_of_study = as.character(dead_or_alive_at_end_of_study),  # Convert factor to character
    dead_or_alive_at_end_of_study = ifelse(died == "No", "Alive", dead_or_alive_at_end_of_study),
    event = ifelse(died == "No", 0, event),
    time_to_event = ifelse(is.na(time_to_event) & died == "No", 51, time_to_event)  # Update only if NA
  ) %>%
  mutate(dead_or_alive_at_end_of_study = as.factor(dead_or_alive_at_end_of_study))  # Convert back to factor if needed

merged_genomics <- merged_genomics %>%
  filter(!calf_id %in% c("CA020610172", "CA051910553"))

################################ km plots ############################################
# Convert 'genotype' to a factor
merged_genomics$genotype <- as.factor(merged_genomics$genotype)

# Summarize survival data grouped by calf_id
merged_genomics <- merged_genomics %>%
  group_by(calf_id) %>%
  summarize(
    genotype = first(genotype),  # Get sex for each calf
    time_to_event = max(time_to_event, na.rm = TRUE),
    event = max(event, na.rm = TRUE)  # Retains event=0 for alive & censored
  ) %>%
  ungroup()

# Fit Kaplan-Meier model by genotype
km_fit_genomics <- survfit(Surv(time_to_event, event) ~ genotype, data = merged_genomics)

# Plot Kaplan-Meier curve
ggsurvplot(
  km_fit_genomics, 
  data = merged_genomics,
  conf.int = TRUE, 
  pval = TRUE, 
  risk.table = TRUE, 
  censor = TRUE,  
  censor.shape = "|",  # Show censored calves as vertical ticks
  censor.size = 3,  
  ggtheme = theme_minimal(),
  title = "Kaplan-Meier Survival Curve: Impact of Genotype on mortality",
  palette = c("red", "blue", "green")  # Different colors for 3 genotypes
)

# cox model
cox_model_genomics <- coxph(Surv(time_to_event, event) ~ genotype, data = merged_genomics)

# Display summary of the Cox model
summary(cox_model_genomics)

# Generate forest plot
ggforest(cox_model_genomics, data = merged_genomics)

################## Logistic regression #################

# Step 1: Create a contingency table for genotype and survival status (died)
contingency_table <- table(Genomics_clean$genotype, Genomics_clean$died)

table <- tbl_cross(data = Genomics_clean, row = genotype, col = died, percent = "row")
table

# Perform Fisher's Exact Test
fisher_test <- fisher.test(contingency_table)

# Print the results
print(fisher_test)

mosaicplot(table(Genomics_clean$genotype, Genomics_clean$died), main = "Effect of Genotype on Survival status")
