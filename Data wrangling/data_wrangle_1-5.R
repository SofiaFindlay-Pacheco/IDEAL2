# Libraries
library(dplyr)
library(tidyr)
library(readxl)
library(janitor)
library(tidyverse)
library(dplyr)
#install.packages("here")
library(here)

################### Rearrange the miseq data ############################

# Upload file path for the combined miseq results page
file_path_combined <- here("Edited original data", "combined results page.xlsx")
sheet_names <- excel_sheets(file_path_combined)

# Read all miseq sheets into a vertical list
sheets_list <- lapply(sheet_names, function(sheet) {read_excel(file_path_combined, sheet = sheet)})

# Turn combined sheets into one data frame
combined_data <- bind_rows(sheets_list)

# Organize sampleID names into adequate form for analysis. By combining rows of same sampleID. 'AE' and 'TB' results from the same sample are now a single row for the whole sample.

# Filter for rows with `AE` or `TB`
data_ae <- combined_data %>% filter(grepl("AE", `Sample ID`))
data_tb <- combined_data %>% filter(grepl("TB", `Sample ID`))

# Select specific columns: column 1, and columns 6 to 23
data_ae <- data_ae %>% select("Sample ID", 1, 6, 8:23, 25:31, 33)  
data_tb <- data_tb %>% select("Sample ID", 1, 6, 8:23, 25:31, 33)  

# Remove the suffix "AE" or "TB" from the SampleID
data_ae <- data_ae %>% mutate(`Sample ID` = gsub("AE", "", `Sample ID`))
data_tb <- data_tb %>% mutate(`Sample ID` = gsub("TB", "", `Sample ID`))  

# Join the data frames by `SampleID` to combine `AE` and `TB` data into one row per sample
combined_data <- full_join(data_ae, data_tb, by = "Sample ID", suffix = c("_AE", "_TB")) 

# Further refine combined data to only pathogens of interest, and check for NAs
combined_data <- combined_data %>% select (1:10, 22:23, 36: 46, 50:51)
combined_data[is.na(combined_data)] <- 0

# Trim `Sample ID` to the first 9 characters
combined_data <- combined_data %>% mutate(`Sample ID` = substr(`Sample ID`, 1, 9))

########################### Combine Miseq data with Calf IDs ################################

# Upload file path for the combined miseq results page
file_path_sample <- here("Edited original data", "ideal_sample.xlsx")
data <- read_excel(file_path_sample)


filtered_data <- data %>%
  filter(
    grepl("^RED", `SampleID`),
    grepl("RED", `Type of sample stored`)
  )

# Create a new data frame with VisitID (calf ID) and the corresponding "IDEAL SAMPLES ID" (code)
calf_codes <- filtered_data %>%
  select(VisitID, codes = `SampleID`)

# View the result
print(calf_codes)

# Merge the calf codes dataframe to correspond with the miseq data frame based on Sample ID so that visit ID can be seen on miseq data
merged_data <- merge(combined_data, calf_codes, by.x = "Sample ID", by.y = "codes", all = TRUE)
merged_data <- merged_data[, c("VisitID", setdiff(names(merged_data), "VisitID"))] #rearranges so VisitID is first

############################### Add important data from ideal calf data to database #############################

# Upload the file path for the IDEAL calf data
file_path <- here("Edited original data", "ideal_calf.xlsx")
ideal_calf <- read_excel(file_path)

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


# Reduce to no VRDs.
final_miseq_data_clean <- final_miseq_data_clean %>%
  filter(grepl("^VRC|^VCC", visit_id))

# Sample week based on first two numbers of VisitID
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(
    sample_week = case_when(
      str_starts(visit_id, "VRC") ~ as.numeric(substr(visit_id, 4, 5)),
      str_starts(visit_id, "VCC") ~ as.numeric(difftime(visit_date, date_of_birth, units = "days")) / 7,
      TRUE ~ NA_real_  # in case it's neither VRC nor VCC
    )
  )

################################ Add in date of death with postportem data #########################

# Upload the file path for the IDEAL postmortem data
postmortem <- here("Edited original data", "ideal_postmortem.xlsx")
ideal_postmortem <- read_excel(postmortem)

ideal_postmortem_clean <- ideal_postmortem %>% clean_names()
ideal_postmortem_clean$date_of_death <- as.numeric(ideal_postmortem_clean$date_of_death)
ideal_postmortem_clean$date_of_death <- as.Date(ideal_postmortem_clean$date_of_death, origin = "1899-12-30")

final_miseq_data_clean <- final_miseq_data_clean %>%
  left_join(select(ideal_postmortem_clean, calf_id, date_of_death), by = "calf_id")

################################## Sample week - changing all the dates to weeks of life ###############

# Ensure all dates in date format
final_miseq_data_clean[["visit_date"]] <- as.Date(final_miseq_data_clean[["visit_date"]], format = "%Y-%m-%d")
final_miseq_data_clean[["date_of_birth"]] <- as.Date(final_miseq_data_clean[["date_of_birth"]], format = "%Y-%m-%d")

# Remove rows where anaplasma_bovis_u03775_ae is NA, as it means no miseq data
final_miseq_data_clean <- final_miseq_data_clean %>%
  filter(!is.na(anaplasma_bovis_u03775_ae))

num_distinct_calves <- final_miseq_data_clean %>%
  summarise(n_distinct_calf = n_distinct(calf_id)) %>%
  pull(n_distinct_calf)

print(num_distinct_calves)


