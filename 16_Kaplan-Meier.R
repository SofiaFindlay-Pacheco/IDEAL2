# Install packages if not already installed
#install.packages("survival")
#install.packages("survminer")

# Load libraries
library(survival)
library(survminer)

# Convert survival time and status to proper format
final_miseq_data_clean$time_to_event <- as.numeric(final_miseq_data_clean$sample_week)
final_miseq_data_clean$event <- ifelse(final_miseq_data_clean$dead_or_alive_at_end_of_study == "Dead", 1, 0)

# Create a new column: "high_bacterial_load"
final_miseq_data_clean$bacterial_load <- ifelse(rowSums(final_miseq_data_clean[bacteria_columns] > 1000, na.rm = TRUE) > 0, "High", "Low")

bacteria_columns <- c( "theileria_mutans_af078815_tb",                                                         
                       "theileria_sp_strain_msd_af078816_tb",                                                  
                       "theileria_parva_l02366_tb",                                                            
                       "theileria_taurotragi_l19082_tb",                                                      
                       "theileria_velifera_af097993_tb"   )

# Kaplan-Meier survival curve for ehrlichia_ruminantium_x61659_ae infection
km_fit <- survfit(Surv(time_to_event, event) ~ bacterial_load, data = final_miseq_data_clean)

# Plot Kaplan-Meier curve
ggsurvplot(km_fit, data = final_miseq_data_clean,
           conf.int = TRUE,  # Show confidence intervals
           pval = TRUE,      # Show p-value for significance
           risk.table = TRUE, # Show risk table below the plot
           ggtheme = theme_minimal(), 
           palette = c("red", "blue")) # Colors for the groups
