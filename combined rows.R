
library(dplyr)
library(tidyr)
library(readxl)

file_path_combined <- "C:/Users/sofia/OneDrive - University of Edinburgh/master/original data/Organisation of plate data/combined results page.xlsx"
sheet_names <- excel_sheets(file_path_combined)

# Read all sheets into a list
sheets_list <- lapply(sheet_names, function(sheet) {read_excel(file_path_combined, sheet = sheet)})

# Combine the sheets into one data frame, 
combined_data <- bind_rows(sheets_list)


# Combining AE and TB
# Step 1: Filter for rows with `AE` or `TB`
data_ae <- combined_data %>% filter(grepl("AE", `Sample ID`))
data_tb <- combined_data %>% filter(grepl("TB", "Sample ID"))

# Step 2: Select specific columns: column 1, and columns 6 to 23
data_ae <- data_ae %>% select("Sample ID", 1, 6:23, 25:31, 33)  # Select column 1 and columns 6 to 23
data_tb <- data_tb %>% select("Sample ID", 1, 6:23, 25:31, 33)  # Select column 1 and columns 6 to 23

# Step 3: Remove the suffix "AE" or "TB" from the `Sample ID` to match them
data_ae <- data_ae %>% mutate(`Sample ID` = sub("AE$", "", `Sample ID`))  
data_tb <- data_tb %>% mutate(`Sample ID` = sub("TB$", "", `Sample ID`))  

# Step 4: Join the data frames by `Sample ID` to combine `AE` and `TB` data into one row per sample
# add suffixes to distinguish AE and TB columns in the final data frame
combined_data <- full_join(data_ae, data_tb, by = "Sample ID", suffix = c("_AE", "_TB")) 

# View the combined data
head(combined_data)

colnames(combined_data)

combined_data <- combined_data %>% select (1:11, 24, 38: 48, 52)


combined_data[is.na(combined_data)] <- 0
