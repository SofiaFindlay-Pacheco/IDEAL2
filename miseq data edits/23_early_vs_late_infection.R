
  
  ########################################################
  ##############################
  # Load libraries
  library(survival)
  library(survminer)
  library(dplyr)
  
  # Select T. parva as the pathogen of interest
  pathogens <- "theileria_parva_l02366_tb"  # Change if using another strain
  
  # Define early infection cutoff as 12 weeks
  cutoff <- 6
  
  # Identify earliest infection (first detection > 0) before cutoff
  infection_data <- final_miseq_data_clean %>%
    filter(sample_week <= cutoff & .data[[pathogens]] > 0) %>%
    group_by(calf_id) %>%
    summarize(earliest_infection_week = min(sample_week, na.rm = TRUE), .groups = "drop")
  
  # Merge with main data and define early vs late/no infection
  windowed_data <- final_miseq_data_clean %>%
    left_join(infection_data, by = "calf_id") %>%
    mutate(
      early_infection = ifelse(!is.na(earliest_infection_week), "Early (<12 weeks)", "Late/No Infection")
    ) %>%
    group_by(calf_id) %>%
    summarize(
      early_infection = first(early_infection),
      time_to_event = max(time_to_event, na.rm = TRUE),
      event = max(event, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Convert to factor (for plotting and model)
  windowed_data$early_infection <- factor(
    windowed_data$early_infection,
    levels = c("Late/No Infection", "Early (<12 weeks)")
  )
  
  # Kaplan-Meier Survival Analysis
  km_fit_sero <- survfit(Surv(time_to_event, event) ~ early_infection, data = windowed_data)
  
  ggsurvplot(km_fit_sero, data = windowed_data,
             conf.int = TRUE,
             pval = TRUE,
             risk.table = TRUE,
             censor = TRUE,
             ggtheme = theme_minimal(),
             title = "Kaplan-Meier Survival Based on Early T. parva Infection (<12 weeks)",
             palette = c("red", "blue"))
  
  # Cox Proportional Hazards Model
  cox_model_sero <- coxph(Surv(time_to_event, event) ~ early_infection, data = windowed_data)
  
  # Summary of Cox model
  print(summary(cox_model_sero))
  
  # Forest plot
  ggforest(cox_model_sero, data = windowed_data)
  