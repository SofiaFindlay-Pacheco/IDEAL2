# Library 
library(readxl)
library(janitor)


# Upload the file path for the IDEAL calf data
file_path <- "C:/Users/sofia/OneDrive - University of Edinburgh/master/R studio/IDEAL statistics/Edited original data/ideal_calf.xlsx"
ideal_calf <- read_excel(file_path, sheet = "ideal_calf")

#Ensure dates are correct
# Ensure 'Visit date' is numeric
ideal_calf$`Visit date` <- as.numeric(ideal_calf$`Visit date`)
# Convert numeric Excel serial dates to Date format
ideal_calf$`Visit date` <- as.Date(ideal_calf$`Visit date`, origin = "1899-12-30")
# Ensure 'Date last visit with data' is numeric
ideal_calf$`Date last visit with data` <- as.numeric(ideal_calf$`Date last visit with data`)
# Convert numeric Excel serial dates to Date format
ideal_calf$`Date last visit with data` <- as.Date(ideal_calf$`Date last visit with data`, origin = "1899-12-30")

# Combine merged_data with ideal_calf based on 'calfID' (merged_data) and 'visitID' (ideal_calf)
final_miseq_data <- merge(merged_data, ideal_calf, by.x = "VisitID", by.y = "VisitID", all.x = TRUE, all.y = FALSE)
#Clean data
final_miseq_data_clean <- final_miseq_data %>% clean_names()
# Reduce to only VRCs
final_miseq_data_clean <- final_miseq_data_clean %>%
  filter(
    grepl("^VRC", visit_id)
  )

# Sample week based on first two numbers of VisitID
final_miseq_data_clean$sample_week <- substr(final_miseq_data_clean$visit_id, 4, 5)

# Remove rows where anaplasma_bovis_u03775_ae is NA
final_miseq_data_clean <- final_miseq_data_clean %>%
  filter(!is.na(anaplasma_bovis_u03775_ae))

# Check how many calfs we have in total
# > final_miseq_data_clean <- final_miseq_data_clean %>%
  #+     distinct(calf_id, .keep_all = TRUE)