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
combined_data <- combined_data %>% clean_names()

# Organize sampleID names into adequate form for analysis. By combining rows of same sampleID. 'AE' and 'TB' results from the same sample are now a single row for the whole sample.

# Filter for rows with `AE` or `TB`
data_ae <- combined_data %>% filter(grepl("AE", sample_id))
data_tb <- combined_data %>% filter(grepl("TB", sample_id))

# Select specific columns: column 1, and columns 6 to 23
data_ae <- data_ae %>% select(sample_id, 1, 6, 8:23, 25:31, 33)  
data_tb <- data_tb %>% select(sample_id, 1, 6, 8:23, 25:31, 33)  

# Remove the suffix "AE" or "TB" from the SampleID
data_ae <- data_ae %>% mutate(sample_id = gsub("AE", "", sample_id))
data_tb <- data_tb %>% mutate(sample_id = gsub("TB", "", sample_id))  

# Join the data frames by `SampleID` to combine `AE` and `TB` data into one row per sample
combined_data <- full_join(data_ae, data_tb, by = "sample_id", suffix = c("_AE", "_TB")) 

# Further refine combined data to only pathogens of interest, and check for NAs
combined_data <- combined_data %>% select (1:10, 22:23, 36: 46, 50:51)
combined_data[is.na(combined_data)] <- 0

# Trim `Sample ID` to the first 9 characters
combined_data <- combined_data %>% mutate(sample_id = substr(sample_id, 1, 9))

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
merged_data <- merge(combined_data, calf_codes, by.x = "sample_id", by.y = "codes", all = TRUE)
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


########## Add in serology #########################################################
# Upload file path for the combined miseq results page
file_path_serology <- here("Edited original data", "Serology_data.xlsx")
serology <- read_excel(file_path_serology)
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


final_miseq_data_clean <- final_miseq_data_clean %>%
  left_join(
    wide_serology_clean %>%
      select(visit_id, serology_t_parva, serology_t_mutans, serology_b_bigemina, serology_a_marginale),  # select only specific columns you want to add
    by = "visit_id"
  )


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
  left_join(select(ideal_postmortem_clean, calf_id, date_of_death, euthanised, immediate_cause_pathology, definitive_aetiological_cause, definitive_cause_pathogen, contributing_pathology_1, contributing_cause_1), by = "calf_id")

################################## Sample week - changing all the dates to weeks of life ###############

# Ensure all dates in date format
final_miseq_data_clean[["visit_date"]] <- as.Date(final_miseq_data_clean[["visit_date"]], format = "%Y-%m-%d")
final_miseq_data_clean[["date_of_birth"]] <- as.Date(final_miseq_data_clean[["date_of_birth"]], format = "%Y-%m-%d")

# Remove rows where anaplasma_bovis_u03775_ae is NA, as it means no miseq data
final_miseq_data_clean <- final_miseq_data_clean %>%
  filter(!is.na(anaplasma_bovis_u03775_ae))

final_miseq_data_clean <- final_miseq_data_clean %>%
  select(1:28, 32, 35:36, 40:41, 48, 130:141)

# mutate data columns as they are "unknown"
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(across(c(visit_date, date_of_birth, date_last_visit_with_data, date_of_death), ~ as.Date(.x, format = "%Y-%m-%d")))

num_distinct_calves <- final_miseq_data_clean %>%
  summarise(n_distinct_calf = n_distinct(calf_id)) %>%
  pull(n_distinct_calf)
print(num_distinct_calves)

########################### ADD in Agro-ecological zones ################################################
# upload file path of sublocation data through farm data from IDEAL
sublocation <- here("Edited original data", "ideal_farm.xlsx")
sublocation_data <- read_excel(sublocation)

sublocation_data_clean <- sublocation_data %>% clean_names()


# Merge Sublocation data
final_miseq_data_clean <- final_miseq_data_clean %>%
  left_join(sublocation_data_clean %>% select(calf_id, sublocation), by = "calf_id")

final_miseq_data_clean <- final_miseq_data_clean %>% rename(agro_ecological_zones = sublocation)

####################################################################################################
# Clean up survival status column
final_miseq_data_clean$dead_or_alive_at_end_of_study <- as.factor(final_miseq_data_clean$dead_or_alive_at_end_of_study)
final_miseq_data_clean$definitive_aetiological_cause <- as.factor(final_miseq_data_clean$definitive_aetiological_cause)

# Group all "Dead" statuses together
final_miseq_data_clean$dead_or_alive_at_end_of_study <- recode(final_miseq_data_clean$dead_or_alive_at_end_of_study,
                                                               "Dead: Infectious death" = "Dead",
                                                               "Dead: Death by trauma" = "Dead",
                                                               "Alive" = "Alive",
                                                               "Censored" = "Censored")

final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(definitive_aetiological_cause = case_when(
    definitive_aetiological_cause == "East coast fever" ~ "Dead",
    is.na(definitive_aetiological_cause) ~ "Alive", 
    definitive_aetiological_cause %in% c(
      "Haemonchosis", "Unknown", "Foreign body", "Actiomyces pyogenes", "Trauma", 
      "Heartwater", "Trypanosomiasis", "Turning sickness", "Cassava", "Mis-mothering", 
      "Bacterial pneumonia", "Black quarter", "Viral pneumonia", "Rabies", 
      "Arcanobacterium", "Babesiosis", "Salmonellosis"
    ) ~ "Censored",
    TRUE ~ definitive_aetiological_cause  # Keep others as they are
  ))

# Assign numeric event status (1 = Dead, 0 = Censored/Alive)
final_miseq_data_clean$event <- ifelse(final_miseq_data_clean$definitive_aetiological_cause == "Dead", 1, 0)

# Manually set one error
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(date_last_visit_with_data = case_when(
    calf_id == "CA020610160" ~ as.Date("2008-07-17"),  # Set specific date for this calf
    TRUE ~ date_last_visit_with_data  # Keep existing values for others
  ))

# Calculate survival time
# Ensure survival time is numeric
final_miseq_data_clean$date_last_visit_with_data <- as.Date(final_miseq_data_clean$date_last_visit_with_data)
final_miseq_data_clean$date_of_birth <- as.Date(final_miseq_data_clean$date_of_birth)

# Now subtract date of death - date of birth
final_miseq_data_clean$date_of_death <- as.Date(final_miseq_data_clean$date_of_death)
final_miseq_data_clean$time_to_event <- as.numeric(final_miseq_data_clean$date_of_death - final_miseq_data_clean$date_of_birth)

# Safer version preserving Date type
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(
    date_of_death = coalesce(date_of_death, date_last_visit_with_data)
  )

# Handle the data without date of death, do it as date of last data - date of birth
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(time_to_event = ifelse(
    is.na(date_of_death) & dead_or_alive_at_end_of_study == "Dead", 
    as.numeric(date_last_visit_with_data - date_of_birth), 
    as.numeric(date_of_death - date_of_birth)
  ))
final_miseq_data_clean$time_to_event <- final_miseq_data_clean$time_to_event / 7

# Update the 'time_to_event' for alive calves (event == 0) to the max_week
max_week <- 51 
final_miseq_data_clean <- final_miseq_data_clean %>%
  mutate(
    time_to_event = ifelse(dead_or_alive_at_end_of_study == "Alive", max_week, time_to_event)  # Set to max_week for alive calves
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

library(dplyr)
library(purrr)

final_miseq_data_clean <- final_miseq_data_clean %>%
  group_by(sample_id) %>%
  summarise(across(everything(), ~ {
    non_missing_non_zero <- .x[!is.na(.x) & .x != 0]
    if (length(non_missing_non_zero) > 0) {
      non_missing_non_zero[1]
    } else {
      # fallback: use 0 if available, else NA
      fallback <- .x[!is.na(.x)]
      if (length(fallback) > 0) fallback[1] else NA
    }
  }), .groups = "drop")

#Cattle_data <- final_miseq_data_clean %>%
 # select(calf_id, calf_sex, weight, sample_week, agro_ecological_zones)
distinct_data <- final_miseq_data_clean %>%
  distinct(calf_id, .keep_all = TRUE)



