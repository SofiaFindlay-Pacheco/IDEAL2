# Load the necessary library for reading Excel
library(readxl)
library(janitor)


# Read the Excel file
file_path <- "C:/Users/sofia/OneDrive - University of Edinburgh/master/original data/Organisation of plate data/ideal_calf.xlsx"
ideal_calf <- read_excel(file_path, sheet = "ideal_calf")
colnames(ideal_calf)
print(ideal_calf) 

# Ensure 'Visit date' is numeric if it's not already
ideal_calf$`Visit date` <- as.numeric(ideal_calf$`Visit date`)

# Convert numeric Excel serial dates to Date format
ideal_calf$`Visit date` <- as.Date(ideal_calf$`Visit date`, origin = "1899-12-30")

# Combine merged_data with ideal_calf based on 'calfID' (merged_data) and 'visitID' (ideal_calf)
final_data <- merge(merged_data, ideal_calf, by.x = "VisitID", by.y = "VisitID", all.x = TRUE, all.y = FALSE)
final_data_clean <- final_data %>% clean_names()


