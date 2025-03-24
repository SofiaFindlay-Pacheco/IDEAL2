# Load libraries
library(survival)
library(survminer)

# Clean up survival status column
final_miseq_data_clean$dead_or_alive_at_end_of_study <- as.factor(final_miseq_data_clean$dead_or_alive_at_end_of_study)

# Group all "Dead" statuses together
final_miseq_data_clean$dead_or_alive_at_end_of_study <- recode(final_miseq_data_clean$dead_or_alive_at_end_of_study,
                                                               "Dead: Infectious death" = "Dead",
                                                               "Dead: Death by trauma" = "Dead",
                                                               "Alive" = "Alive")

# Remove censored data
final_miseq_data_clean <- final_miseq_data_clean %>% filter(dead_or_alive_at_end_of_study != "Censored")

# Convert survival time and status to numeric format
final_miseq_data_clean$time_to_event <- as.numeric(final_miseq_data_clean$sample_week)
final_miseq_data_clean$event <- ifelse(final_miseq_data_clean$dead_or_alive_at_end_of_study == "Dead", 1, 0)

# Tick infestation columns (categorical: "Absent" / "Present" / "Severe")
tick_columns <- c("rhipicephalus_appendiculatus_infestation",
                  "amblyomma_spp_infestation",
                  "boophilus_spp_infestation",
                  "hyalomma_spp_infestation")

# Ensure tick columns have correct factor levels
final_miseq_data_clean[tick_columns] <- lapply(final_miseq_data_clean[tick_columns], function(x) factor(x, levels = c("Absent", "Present", "Severe")))

# Reset graphics if needed
dev.off()

# Generate Kaplan-Meier plots for each tick species
for (tick in tick_columns) {
  print(paste("Processing:", tick))  # Debugging print
  
  # Check if the tick variable has at least two categories
  if (length(unique(final_miseq_data_clean[[tick]])) < 2) {
    print(paste("Skipping", tick, "as it has only one category"))
    next  # Skip if only one level (e.g., all "Absent")
  }
  
  # Fit Kaplan-Meier model
  km_fit <- survfit(as.formula(paste("Surv(time_to_event, event) ~", tick)), data = final_miseq_data_clean)
  
  # Check if the model has data
  if (length(km_fit$strata) == 0) {
    print(paste("Skipping", tick, "as no strata exist in survival model"))
    next
  }
  
  # Plot and print Kaplan-Meier curve
  plot <- ggsurvplot(km_fit, data = final_miseq_data_clean,
                     conf.int = TRUE,
                     pval = TRUE,
                     risk.table = TRUE,
                     ggtheme = theme_minimal(),
                     title = paste("Kaplan-Meier Survival Curve for", gsub("_", " ", tick)),
                     palette = c("blue", "orange", "red"))  # Colors for Absent, Present, Severe
  
  print(plot)  # Ensure the plot is displayed
}

