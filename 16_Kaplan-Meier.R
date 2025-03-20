# Install packages if not already installed
# install.packages("survival")
# install.packages("survminer")



###################################Group bacteria Kaplan-Meier
# Load libraries
library(survival)
library(survminer)

# Clean up the "Dead or alive at end of study" column
final_miseq_data_clean$dead_or_alive_at_end_of_study <- as.factor(final_miseq_data_clean$dead_or_alive_at_end_of_study)

# Group all "Dead" statuses together (including different death causes)
final_miseq_data_clean$dead_or_alive_at_end_of_study <- recode(final_miseq_data_clean$dead_or_alive_at_end_of_study,
                                                               "Dead: Infectious death" = "Dead",
                                                               "Dead: Death by trauma" = "Dead",
                                                               "Alive" = "Alive")

#Remove censored data
final_miseq_data_clean <- final_miseq_data_clean %>%   #Remove censored data
  filter(dead_or_alive_at_end_of_study != "Censored")
table(final_miseq_data_clean$dead_or_alive_at_end_of_study) # Check survival status counts

# Define bacteria columns **BEFORE** using them in calculations
bacteria_columns <- c("babesia_bigemina_ay603402_tb",                                                         
                       "babesia_bigemina_lk391709_tb",                                                         
                       "babesia_bigemina_ku206291_tb" ,      
                      "babesia_bovis_kf928959_tb"      ,                                                      
                     "babesia_bovis_aaxt01000002_tb"   ,                                                     
                      "babesia_bovis_ay603398_tb"        ,                                                    
                     "babesia_bovis_jq437260_tb")

# Ensure bacteria columns exist in the dataframe
missing_cols <- setdiff(bacteria_columns, colnames(final_miseq_data_clean))
if (length(missing_cols) > 0) {
  stop(paste("Error: The following bacteria columns are missing:", paste(missing_cols, collapse = ", ")))
}

# Convert survival time and status to proper format
final_miseq_data_clean$time_to_event <- as.numeric(final_miseq_data_clean$sample_week)
final_miseq_data_clean$event <- ifelse(final_miseq_data_clean$dead_or_alive_at_end_of_study == "Dead", 1, 0)


# Create a new column: "bacterial_load" (High if any species > 1000)
final_miseq_data_clean$bacterial_load <- ifelse(
  rowSums(final_miseq_data_clean[bacteria_columns] > 500, na.rm = TRUE) > 0, "High", "Low"
)

# Categorize tick severity (Modify threshold as needed)
final_miseq_data_clean$tick_severity <- ifelse(
  final_miseq_data_clean$amblyomma_spp_infestation > 100, "High", "Low"
)

# Check distribution of High vs. Low
table(final_miseq_data_clean$bacterial_load)

# Kaplan-Meier survival curve for Babesia infection
km_fit <- survfit(Surv(time_to_event, event) ~ bacterial_load, data = final_miseq_data_clean)

# Plot Kaplan-Meier curve
ggsurvplot(km_fit, data = final_miseq_data_clean,
           conf.int = TRUE,  # Show confidence intervals
           pval = TRUE,      # Show p-value for significance
           risk.table = TRUE, # Show risk table below the plot
           ggtheme = theme_minimal(), 
           title = "Kaplan-Meier survival curve for Babesia infection", 
           palette = c("red", "blue")) # Colors for the groups



#############################################Individual Kaplein-Meier
# Load libraries
library(survival)
library(survminer)
library(dplyr)

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

# List of bacterial species columns
bacteria_columns <- c(
  "anaplasma_bovis_u03775_ae", "anaplasma_bovis_ab983439_ae", "anaplasma_marginale_cp000030_ae",
  "anaplasma_platys_like_ku585990_ae", "anaplasma_phagocytophilum_u02521_ae", 
  "candidatus_anaplasma_boleense_ku586025_ae", "uncultured_anaplasma_sp_clone_saso_ky924885_ae",
  "uncultured_anaplasma_sp_jn862825_ae", "ehrlichia_sp_tibet_ehrlichia_canis_ehrlichia_minasensis_af414399_ay394465_mt163430_ae",
  "ehrlichia_ruminantium_x61659_ae", "anaplasma_platys_ef139459_ae", "babesia_bigemina_ay603402_tb",
  "babesia_bigemina_lk391709_tb", "babesia_bigemina_ku206291_tb", "theileria_mutans_af078815_tb",
  "theileria_sp_strain_msd_af078816_tb", "theileria_parva_l02366_tb", "theileria_taurotragi_l19082_tb",
  "theileria_velifera_af097993_tb", "babesia_bovis_kf928959_tb", "babesia_bovis_aaxt01000002_tb",
  "babesia_bovis_ay603398_tb", "babesia_bovis_jq437260_tb"
)

# Reset graphics if needed
dev.off()

# Generate Kaplan-Meier plots for each bacteria species
for (bacteria in bacteria_columns) {
  print(paste("Processing:", bacteria))  # Debugging print
  
  # Convert bacterial presence to a binary factor (0 = Absent, 1 = Present)
  final_miseq_data_clean[[bacteria]] <- ifelse(final_miseq_data_clean[[bacteria]] > 0, "Present", "Absent")
  final_miseq_data_clean[[bacteria]] <- as.factor(final_miseq_data_clean[[bacteria]])
  
  # Check if the variable has at least two categories
  if (length(unique(final_miseq_data_clean[[bacteria]])) < 2) {
    print(paste("Skipping", bacteria, "as it has only one category"))
    next  # Skip if all values are either only "Present" or only "Absent"
  }
  
  # Fit Kaplan-Meier model
  km_fit <- survfit(as.formula(paste("Surv(time_to_event, event) ~", bacteria)), data = final_miseq_data_clean)
  
  # Check if the model has data
  if (length(km_fit$strata) == 0) {
    print(paste("Skipping", bacteria, "as no strata exist in survival model"))
    next
  }
  
  # Plot and print Kaplan-Meier curve
  plot <- ggsurvplot(km_fit, data = final_miseq_data_clean,
                     conf.int = TRUE,
                     pval = TRUE,
                     risk.table = TRUE,
                     ggtheme = theme_minimal(),
                     title = paste("Kaplan-Meier Survival Curve for", gsub("_", " ", bacteria)),
                     palette = c("blue", "red"))  # Colors for Absent & Present
  
  print(plot)  # Ensure the plot is displayed
}
