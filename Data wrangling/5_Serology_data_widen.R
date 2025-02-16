install.packages("tidyverse")

library(tidyverse)
library(readxl)
library(janitor)


# Load the data
file_path_serology <- "C:/Users/sofia/OneDrive - University of Edinburgh/master/original data/Organisation of plate data/Serology_data.xlsx"
serology <- read_excel(file_path_serology, sheet = "serology_data")
serology_clean <- serology %>% clean_names()

filtered_serology <- serology_clean %>%
  filter(grepl("Serology", test))


filtered_serology2 <- filtered_serology %>%
  select(visit_id, visit_date, test, quantitative_result_number)

wide_filtered_serology2 <- filtered_serology2 %>%
  pivot_wider(
    names_from = test,  
    values_from = quantitative_result_number  
  )

#wide_filtered_serology2 <- wide_filtered_serology2 %>%
 # rename("serology Visit date" = visit_date)
wide_filtered_serology2_clean <- wide_filtered_serology2 %>% clean_names()


final_wide_serology2 <- final_data_clean %>%
  select(-visit_date) %>%  # Remove the duplicate column before joining
  full_join(wide_filtered_serology2_clean, by = "visit_id")

#changing all the dates to weeks of life
final_wide_serology2[["visit_date"]] <- as.Date(final_wide_serology2[["visit_date"]], format = "%Y-%m-%d")
final_wide_serology2[["date_of_birth"]] <- as.Date(final_wide_serology2[["date_of_birth"]], format = "%Y-%m-%d")

# Add a new column for SampleWeek while keeping all original data depending on dates
#final_wide_serology2$Sample_Week <- ifelse(
 # !is.na(final_wide_serology2["visit_date"]) & !is.na(final_wide_serology2["date_of_birth"]), # Check for non-NA values
  #floor(as.numeric(difftime(final_wide_serology2$"visit_date", final_wide_serology2$"date_of_birth", units = "days")) / 7),
  #NA )# Set SampleWeek to NA if either date is missing)

#Sample week depending on VisitID
final_wide_serology2$sample_week <- substr(final_wide_serology2$visit_id, 4, 5)

# Brief_serology <- final_wide_serology2 %>% select (1,2,38,39,43,51,52, 142:145,149 )
Brief_serology <- final_wide_serology2 %>% select (1,2,26:27,30,38:39,128:132,136 )


# Sort the entire dataset based on the numeric part of CalfID
Brief_serology <- Brief_serology[order(as.numeric(gsub("[^0-9]", "", Brief_serology$calf_id))), ]
Brief_serology_clean <- Brief_serology %>% clean_names()

