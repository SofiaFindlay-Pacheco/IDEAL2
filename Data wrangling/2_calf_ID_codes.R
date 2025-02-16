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

