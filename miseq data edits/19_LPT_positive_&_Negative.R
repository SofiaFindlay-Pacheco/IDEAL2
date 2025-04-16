
  
  
##############################################################################
  ##############################
  # Load libraries
  library(survival)
  library(survminer)
  library(dplyr)
  
  # Set early infection cutoff
  cutoff <- 26
  
  # Identify earliest infection for each pathogens, swap this for the hashtahgged lines for thresholds above 0
  infection_data <- final_miseq_data_clean %>%
    group_by(calf_id) %>%
    summarize(
      earliest_mutans = suppressWarnings(min(sample_week[theileria_mutans_af078815_tb > 0], na.rm = TRUE)),
      earliest_velifera = suppressWarnings(min(sample_week[theileria_velifera_af097993_tb > 0], na.rm = TRUE)),
      earliest_parva = suppressWarnings(min(sample_week[theileria_parva_l02366_tb > 0], na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(across(starts_with("earliest_"), ~ ifelse(is.infinite(.), NA_real_, .)))  # Convert Inf to NA
  
  # Set a threshold for high pathogens load
  #threshold <- 10  # Adjust this value based on your data distribution
  
  # Identify earliest infection week where pathogens load exceeds threshold
  #infection_data <- final_miseq_data_clean %>%
  #  group_by(calf_id) %>%
  #  summarize(
  #    earliest_mutans = suppressWarnings(min(sample_week[theileria_mutans_af078815_tb > threshold], na.rm = TRUE)),
  #    earliest_velifera = suppressWarnings(min(sample_week[theileria_velifera_af097993_tb > threshold], na.rm = TRUE)),
  #    earliest_parva = suppressWarnings(min(sample_week[theileria_parva_l02366_tb > threshold], na.rm = TRUE)),
  #    .groups = "drop"
  #  ) %>%
  #  mutate(across(starts_with("earliest_"), ~ ifelse(is.infinite(.), NA_real_, .)))  # Convert Inf to NA
  
  # Merge with main dataset
  windowed_data <- final_miseq_data_clean %>%
    left_join(infection_data, by = "calf_id") %>%
    mutate(
      # Replace NA with Inf for comparison purposes
      earliest_mutans = ifelse(is.na(earliest_mutans), Inf, earliest_mutans),
      earliest_velifera = ifelse(is.na(earliest_velifera), Inf, earliest_velifera),
      earliest_parva = ifelse(is.na(earliest_parva), Inf, earliest_parva),
      
      # Define infection order groups
      infection_order = case_when(
        earliest_mutans < earliest_parva & earliest_mutans < earliest_velifera ~ "Mutans First", 
        earliest_velifera < earliest_parva & earliest_velifera < earliest_mutans ~ "Velifera First",
        (earliest_mutans == earliest_velifera) & (earliest_mutans < earliest_parva) ~ "Mutans/Velifera First",
        earliest_parva < earliest_mutans & earliest_parva < earliest_velifera ~ "Parva First",
        TRUE ~ "No infection"
      )
    ) %>%
    # Convert Inf back to NA for clarity in the final output
    mutate(across(starts_with("earliest_"), ~ ifelse(. == Inf, NA_real_, .))) %>%
    group_by(calf_id) %>%
    summarize(
      infection_order = first(infection_order),
      time_to_event = max(time_to_event, na.rm = TRUE),
      event = max(event, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Group Mutans First, Velifera First, and Mutans/Velifera First into one category
  windowed_data <- windowed_data %>%
    mutate(infection_order = case_when(
      infection_order %in% c("Mutans First", "Velifera First", "Mutans/Velifera First") ~ "Mutans/Velifera First",
      TRUE ~ infection_order  # Keep other values unchanged
    ))
  
  # Convert to factor for plotting
windowed_data$infection_order <- factor(windowed_data$infection_order, 
                                               levels = c("No infection", "Mutans/Velifera First", "Parva First"))
  
  # Kaplan-Meier survival fit
km_fit <- survfit(Surv(time_to_event, event) ~ infection_order, data = windowed_data)
  
# Plot Kaplan-Meier curve
ggsurvplot(km_fit, data = windowed_data,
             conf.int = TRUE, 
             pval = TRUE, 
             risk.table = TRUE, 
             censor = TRUE,  
             ggtheme = theme_minimal(),
             title = "Kaplan-Meier Survival by infection Order",
             palette = c("black", "blue", "orange"))
  
  cox_model <- coxph(Surv(time_to_event, event) ~ infection_order, data = windowed_data)
  
  # View model summary
  summary(cox_model)
  # Test proportional hazards assumption
  cox.zph(cox_model)
  ggforest(cox_model, data = windowed_data)
  
# Summary table
table <- tbl_cross(data = windowed_data, row = infection_order, col = event, percent = "row")
table

swrite.csv(windowed_data, "windowed.csv", row.names = FALSE)
