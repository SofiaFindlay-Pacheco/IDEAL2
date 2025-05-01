# Load libraries
library(survival)
library(survminer)
library(dplyr)

# Identify earliest infection for each relevant pathogen
infection_data <- final_miseq_data_clean %>%
  group_by(calf_id) %>%
  summarize(
    earliest_parva = suppressWarnings(min(sample_week[theileria_parva_l02366_tb > 0], na.rm = TRUE)),
    
    # Combine any Anaplasma infections 
#    earliest_anaplasma = suppressWarnings(min(
 #     sample_week[
  #        anaplasma_bovis_u03775_ae > 0 |                                                            
   #       anaplasma_bovis_ab983439_ae > 0 |                                                        
    #      anaplasma_marginale_cp000030_ae > 0 |                                                       
     #     anaplasma_platys_like_ku585990_ae > 0 |                                                     
      #    anaplasma_phagocytophilum_u02521_ae > 0 |                                                  
       #   candidatus_anaplasma_boleense_ku586025_ae > 0 |                                           
        #  uncultured_anaplasma_sp_clone_saso_ky924885_ae > 0 |                                   
         # uncultured_anaplasma_sp_jn862825_ae > 0 | 
  #        anaplasma_platys_ef139459_ae > 0 
   #   ], na.rm = TRUE)
#    ),
 #   .groups = "drop"
#  ) %>%
#  mutate(across(everything(), ~ ifelse(is.infinite(.), NA_real_, .)))  # Convert Inf to NA

# Combine any Anaplasma infections 
earliest_anaplasma = suppressWarnings(min(
  sample_week[
    anaplasma_platys_ef139459_ae > 0 
  ], na.rm = TRUE)
),
.groups = "drop"
) %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), NA_real_, .)))  # Convert Inf to NA

# Merge with main data
windowed_data <- final_miseq_data_clean %>%
  left_join(infection_data, by = "calf_id") %>%
  mutate(
    # Replace NA with Inf for comparison logic
    earliest_parva = ifelse(is.na(earliest_parva), Inf, earliest_parva),
    earliest_anaplasma = ifelse(is.na(earliest_anaplasma), Inf, earliest_anaplasma),
    
    # Define co-infection groupings
    co_infection_status = case_when(
      is.finite(earliest_parva) & is.finite(earliest_anaplasma) ~ "Parva + Anaplasma",
      is.finite(earliest_parva) & !is.finite(earliest_anaplasma) ~ "Parva only",
      TRUE ~ "No Parva infection"
    ),
    
    # Reset NA for display
    earliest_parva = ifelse(earliest_parva == Inf, NA_real_, earliest_parva),
    earliest_anaplasma = ifelse(earliest_anaplasma == Inf, NA_real_, earliest_anaplasma)
  ) %>%
  group_by(calf_id) %>%
  summarize(
    co_infection_status = first(co_infection_status),
    time_to_event = max(time_to_event, na.rm = TRUE),
    event = max(event, na.rm = TRUE),
    .groups = "drop"
  )

# Drop "No Parva infection"
windowed_data <- windowed_data %>%
   filter(co_infection_status != "No Parva infection")

# Make factor for plotting
windowed_data$co_infection_status <- factor(
  windowed_data$co_infection_status,
  levels = c("Parva only", "Parva + Anaplasma")
)

# Kaplan-Meier survival fit
km_fit <- survfit(Surv(time_to_event, event) ~ co_infection_status, data = windowed_data)

# Plot Kaplan-Meier curve
ggsurvplot(
  km_fit, data = windowed_data,
  conf.int = TRUE,
  pval = TRUE,
  risk.table = TRUE,
  censor = TRUE,
  ggtheme = theme_minimal(),
  title = "Survival by Co-Infection Status (T. parva ± Anaplasma)",
  palette = c( "orange", "purple")
)

# Cox model
cox_model <- coxph(Surv(time_to_event, event) ~ co_infection_status, data = windowed_data)
summary(cox_model)
cox.zph(cox_model)
ggforest(cox_model, data = windowed_data)

# Summary table
table <- tbl_cross(data = windowed_data, row = co_infection_status, col = event, percent = "row")
table

