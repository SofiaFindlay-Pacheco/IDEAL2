merged_data <- merge(combined_data, calf_codes, by.x = "Sample ID", by.y = "codes", all = TRUE)
merged_data <- merged_data[, c("VisitID", setdiff(names(merged_data), "VisitID"))] #
