# Library
library(readxl)
library(janitor)
library(survival)
library(dplyr)


# Load the data for postmortem from IDEAL
file_path_postmortem <- "C:/Users/sofia/OneDrive - University of Edinburgh/master/R studio/IDEAL statistics/Edited original data/ideal_postmortem.xlsx"
postmortem <- read_excel(file_path_postmortem, sheet = "ideal_postmortem")
postmortem_clean <- postmortem %>% clean_names()

# Ensure necessary dates are in numeric then correct date format
postmortem_clean$date_of_death <- as.numeric(postmortem_clean$date_of_death)
postmortem_clean$date_of_death <- as.Date(postmortem_clean$date_of_death, origin = "1899-12-30")

# Merge serology with postmortem data
postmortem_clean <- postmortem_clean %>%
  select(calf_id, date_of_death)
wide_postmortem <- full_join(vertical_Brief_Serology, postmortem_clean, by = "calf_id")
# Select only desired columns from join
wide_postmortem <- wide_postmortem %>%
  select(calf_id, date_of_birth, date_of_death, calf_sex, Bacteria) #%>%
# Keep only one of repeated rows #################
#distinct(calf_id, .keep_all = TRUE)

# Replace missing event dates with study end date
#study_end_date <- as.Date("2008-12-31")
#wide_postmortem <- wide_postmortem %>%
  #mutate(date_of_death = ifelse(is.na(date_of_death), study_end_date, date_of_death))

# Calculate time to death/censoring in weeks
wide_postmortem <- wide_postmortem %>%
  mutate(Time_to_Death = as.numeric(difftime(date_of_death, date_of_birth, units = "days")) / 7)

# Event = 1 represents a death event, and Event = 0 represents a censored observation.
wide_postmortem <- wide_postmortem %>%
  mutate(Event = if_else(date_of_death < study_end_date, 1, 0))  # 1 = Death, 0 = Censored

#predict the hazard rate (the likelihood of death) based on the sex of the calf and the presence of different bacteria.
cox_model <- coxph(Surv(Time_to_Death, Event) ~ calf_sex + Bacteria, data = wide_postmortem)
summary(cox_model)

