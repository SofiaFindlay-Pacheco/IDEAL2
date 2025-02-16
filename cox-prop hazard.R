library(readxl)
library(janitor)

# Load the data
file_path_postmortem <- "C:/Users/sofia/OneDrive - University of Edinburgh/master/original data/Organisation of plate data/ideal_postmortem.xlsx"
postmortem <- read_excel(file_path_postmortem, sheet = "ideal_postmortem")
postmortem_clean <- postmortem %>% clean_names()

postmortem_clean$date_of_death <- as.numeric(postmortem_clean$date_of_death)
postmortem_clean$date_of_death <- as.Date(postmortem_clean$date_of_death, origin = "1899-12-30")

postmortem_clean <- postmortem_clean %>%
  select(visit_id, date_of_death)
wide_postmortem <- full_join(long_data2_clean, postmortem_clean, by = "visit_id")

# merge final wide with postmortem
wide_postmortem <- wide_postmortem %>%
  mutate(
    date_of_birth = as.Date(date_of_birth, format = "%Y-%m-%d"),
    date_of_death = as.Date(date_of_death, format = "%Y-%m-%d")
  )

# Replace missing event dates with study end date
study_end_date <- as.Date("2008-12-31")
wide_postmortem <- wide_postmortem %>%
  mutate(date_of_death = ifelse(is.na(date_of_death), study_end_date, date_of_death))

# Calculate time to death or censoring in weeks
wide_postmortem <- wide_postmortem %>%
  mutate(Time_to_Death = as.numeric(difftime(date_of_death, date_of_birth, units = "days")) / 7)

wide_postmortem <- wide_postmortem %>%
  mutate(Event = if_else(date_of_death < study_end_date, 1, 0))  # 1 = Death, 0 = Censored

library(survival)

cox_model <- coxph(Surv(Time_to_Death, Event) ~ calf_sex + Bacteria, data = wide_postmortem)
summary(cox_model)

