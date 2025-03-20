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

