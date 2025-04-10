#install.packages("tidyverse")

# Library
library(tidyverse)
library(readxl)
library(janitor)
library(here)


# Upload file path for the combined miseq results page
file_path_serology <- here("Edited original data", "Serology_data.xlsx")
serology <- read_excel(file_path_serology)

# Clean up names
serology_clean <- serology %>% clean_names()

# Filter to rows where the contents of the "test" column begins with "Serology"
filtered_serology <- serology_clean %>%
  filter(grepl("Serology", test))

#Filter to main columns of interest
filtered_serology_main <- filtered_serology %>%
  select(visit_id, visit_date, test, quantitative_result_number)

#Flips data frame to be horizontal not vertical
wide_serology <- filtered_serology_main %>%
  pivot_wider(
    names_from = test,  
    values_from = quantitative_result_number  
  )

# Clean up names
wide_serology_clean <- wide_serology %>% clean_names()

# Fix problem of duplicate Visit date columns
Serology_miseq_merge <- final_miseq_data_clean %>%
  select(-visit_date) %>%  # Remove the duplicate column before joining
  #merge miseq data and serology data
  full_join(wide_serology_clean, by = "visit_id")


#Sample week - changing all the dates to weeks of life

# Ensure all dates in date format
Serology_miseq_merge[["visit_date"]] <- as.Date(Serology_miseq_merge[["visit_date"]], format = "%Y-%m-%d")
Serology_miseq_merge[["date_of_birth"]] <- as.Date(Serology_miseq_merge[["date_of_birth"]], format = "%Y-%m-%d")

# Sample week based on first two numbers of VisitID
Serology_miseq_merge$sample_week <- substr(Serology_miseq_merge$visit_id, 4, 5)

# Select key columns of merged dataset
Brief_serology  <- Serology_miseq_merge %>% select (1:31, 34:35, 39:40, 47:51, 129:135 )

