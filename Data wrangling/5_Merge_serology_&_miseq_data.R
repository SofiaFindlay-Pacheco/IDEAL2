#install.packages("tidyverse")

# Library
library(tidyverse)
library(readxl)
library(janitor)


# Upload the file path for the serology data
file_path_serology <- "C:/Users/sofia/OneDrive - University of Edinburgh/master/R studio/IDEAL statistics/Edited original data/Serology_data.xlsx"
serology <- read_excel(file_path_serology, sheet = "serology_data")
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
wide_serology_clean <- wide_filtered_serology2 %>% clean_names()

# Fix problem of duplicate Visit date columns
Serology_miseq_merge <- final_miseq_data_clean %>%
  select(-visit_date) %>%  # Remove the duplicate column before joining
  #merge miseq data and serology data
  full_join(wide_serology_clean, by = "visit_id")


#Sample week - changing all the dates to weeks of life

# Ensure all dates in date format
Serology_miseq_merge[["visit_date"]] <- as.Date(Serology_miseq_merge[["visit_date"]], format = "%Y-%m-%d")
Serology_miseq_merge[["date_of_birth"]] <- as.Date(Serology_miseq_merge[["date_of_birth"]], format = "%Y-%m-%d")

# If sample week were to be calculated using equation
#Serology_miseq_merge$Sample_Week <- ifelse(
 # !is.na(Serology_miseq_merge["visit_date"]) & !is.na(Serology_miseq_merge["date_of_birth"]), # Check for non-NA values
  #floor(as.numeric(difftime(Serology_miseq_merge$"visit_date", Serology_miseq_merge$"date_of_birth", units = "days")) / 7),
  #NA )# Set SampleWeek to NA if either date is missing)

# Sample week based on first two numbers of VisitID
Serology_miseq_merge$sample_week <- substr(Serology_miseq_merge$visit_id, 4, 5)



# Select key columns of merged dataset, for just serology
Brief_serology  <- Serology_miseq_merge %>% select (1,2,26:27,30,38:39,128:132,136 )

# Sort the entire dataset based on the numeric part of CalfID
Brief_serology  <- Brief_serology [order(as.numeric(gsub("[^0-9]", "", Brief_serology $calf_id))), ]

# Clean up names
Brief_serology_clean <- Brief_serology  %>% clean_names()


#summary table
summary_table <- Serology_miseq_merge %>%
  select(visit_id, sample_week, visit_date, date_of_birth) 
