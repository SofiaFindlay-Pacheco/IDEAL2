# Libraries
library(dplyr)
library(tidyr)
library(readxl)

#Rearrange the miseq file into adequate form

# Upload file path for the combined miseq results page
file_path_combined <- "C:/Users/sofia/OneDrive - University of Edinburgh/master/R studio/IDEAL statistics/Edited original data/combined results page.xlsx"
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
data_ae <- data_ae %>% select("Sample ID", 1, 6:23, 25:31, 33)  
data_tb <- data_tb %>% select("Sample ID", 1, 6:23, 25:31, 33)  

# Remove the suffix "AE" or "TB" from the SampleID
data_ae <- data_ae %>% mutate(`Sample ID` = sub("AE$", "", `Sample ID`))  
data_tb <- data_tb %>% mutate(`Sample ID` = sub("TB$", "", `Sample ID`))  

# Join the data frames by `SampleID` to combine `AE` and `TB` data into one row per sample
combined_data <- full_join(data_ae, data_tb, by = "Sample ID", suffix = c("_AE", "_TB")) 

# Further refine combined data to only bacteria of interest, and check for NAs
combined_data <- combined_data %>% select (1:11, 24, 38: 48, 52)
combined_data[is.na(combined_data)] <- 0
# Libraries
library(readxl)
library(dplyr)

# Upload file path for the IDEAL sample database
file_path_sample <- ("C:/Users/sofia/OneDrive - University of Edinburgh/master/R studio/IDEAL statistics/Edited original data/ideal_sample.xlsx")
data <- read_excel(file_path_sample, sheet = "ideal_sample")  

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
#install.packages("tidyverse")

# Remove rows where anaplasma_bovis_u03775_ae is NA
final_miseq_data_clean <- final_miseq_data_clean %>%
  filter(!is.na(anaplasma_bovis_u03775_ae))

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
wide_serology_clean <- wide_serology %>% clean_names()

# Fix problem of duplicate Visit date columns
Serology_miseq_merge <- final_miseq_data_clean %>%
  select(-visit_date) %>%  # Remove the duplicate column before joining
  #merge miseq data and serology data
  full_join(wide_serology_clean, by = "visit_id")

postmortem <- "C:/Users/sofia/OneDrive - University of Edinburgh/master/R studio/IDEAL statistics/Edited original data/ideal_postmortem.xlsx"
ideal_postmortem <- read_excel(postmortem, sheet = "ideal_postmortem")
ideal_postmortem_clean <- ideal_postmortem %>% clean_names()
ideal_postmortem_clean$date_of_death <- as.numeric(ideal_postmortem_clean$date_of_death)
ideal_postmortem_clean$date_of_death <- as.Date(ideal_postmortem_clean$date_of_death, origin = "1899-12-30")

final_miseq_data_clean <- final_miseq_data_clean %>%
  left_join(select(ideal_postmortem_clean, calf_id, date_of_death), by = "calf_id")

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
