# Load necessary libraries
library(lme4)
library(dplyr)

# Assuming your data is in a data.frame called df
# Variables:
# / death_event: 0/1 
# / month: factor (categorical time variable)
# / genotype: TT, CT, CC
# / parasite1, parasite2: 0/1 time-varying covariates
# / infection_order: integer/categorical (e.g. 0: no infection, 1: p1→p2, etc.)
# / subject_id: ID for each individual

df <- final_miseq_data_clean %>%
  select(event, time_to_event, sample_week, calf_id, definitive_aetiological_cause)

df <- df %>%
  mutate(
    sample_week = as.factor(sample_week),                      # make time categorical
    sample_week = relevel(sample_week, ref = "1")              # week 1 is the reference
  )

df <- df %>%
  group_by(calf_id) %>%
  mutate(
    sample_week_num = as.numeric(as.character(sample_week)),  # convert factor to numeric
    is_dead = definitive_aetiological_cause == "Dead",
    is_last_sample = sample_week_num == max(sample_week_num, na.rm = TRUE),
    event = if_else(is_dead & is_last_sample, 1, 0)
  ) %>%
  ungroup()

######################  Add in genomics  #####################################
Genomics <- here("Edited original data", "Genomics.xlsx")
Genomics <- read_excel(Genomics)
Genomics_clean <- Genomics %>% clean_names()

# Merge two data frames by 'calf_id', keeping all columns from both
df <- full_join(df, Genomics_clean, by = "calf_id")
####################### Clean up data #################################################################
#Manually set added in genomic calves
# List of calf IDs you want to force event = 0
exclude_calves <- c("CA010110001", "CA010110005", "CA010110013")  # replace with your list

# Set event = 0 for these calves
df <- df %>%
  mutate(event = if_else(calf_id %in% exclude_calves, 0, event))

# List of calf IDs you want to force event = 0
exclude_calves <- c(
  "CA051910553", "CA041510442", "CA041510444", "CA041610456", "CA041610458",
  "CA041610460", "CA041610462", "CA041610463", "CA041710491", "CA041710504",
  "CA041710505", "CA051810513", "CA051810520", "CA051810521", "CA051810527",
  "CA051910543", "CA051910546", "CA051910550", "CA051910563", "CA051910566",
  "CA052010579", "CA052010583", "CA052010585", "CA052010589", "CA052010597",
  "CA041510439", "CA041610461", "CA041710493", "CA041710502", "CA041710503",
  "CA051810516", "CA051810519", "CA041510438", "CA041510440", "CA041510443",
  "CA041510447", "CA041610455", "CA041610467", "CA041610469", "CA041610474",
  "CA041610475", "CA041710484", "CA041710486", "CA041710492", "CA041710494",
  "CA051810514", "CA051810518", "CA051810528", "CA051810532", "CA051810534",
  "CA051810524", "CA051810525", "CA051810538", "CA051910542", "CA051910545",
  "CA051910547", "CA051910549", "CA051910564", "CA052010582", "CA041510441",
  "CA041610453", "CA041710483", "CA041710485", "CA051810531", "CA051910544",
  "CA052010588", "CA052010595", "CA041610457", "CA041710481", "CA041710490",
  "CA041710496", "CA051810512", "CA051810522", "CA052010596", "CA051910567"
)

# Set event = 0 for these calves
df <- df %>%
  mutate(event = if_else(calf_id %in% exclude_calves, 0, event))

ECF_calf <- c("CA020610172")
df <-df %>%
  mutate(event = if_else(calf_id %in% ECF_calf, 1, event))

###################### Add in pathogen data ###################################
pathogen_data <- final_miseq_data_clean %>%
  select(1:28, 39, 43, 47)

# in columns 3-26 change all numbers above 0 to 1  
pathogen_data <- pathogen_data %>%
  mutate(across(3:26, ~ ifelse(. > 0, 1, 0)))

# Sample week based on first two numbers of VisitID
pathogen_data$SampleWeek <- substr(pathogen_data$visit_id, 4, 5)

df <- full_join(
  df,
  pathogen_data %>% select(calf_id, 3:26),
  by = "calf_id",
  relationship = "many-to-many"
)

#################################### Add in infection order data ###########################

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
#Assign numbers
windowed_data <- windowed_data %>%
  mutate(infection_order = recode(infection_order,
                                "No infection" = 0,
                                "Mutans/Velifera First" = 1,
                                "Parva First" = 2))

df <- full_join(
  df,
  windowed_data %>% select(calf_id, infection_order),
  by = "calf_id",
  relationship = "many-to-many"
)
#####################################################################

df <- df %>%
  select(event, sample_week, calf_id, genotype, theileria_mutans_af078815_tb, theileria_parva_l02366_tb, theileria_velifera_af097993_tb, anaplasma_marginale_cp000030_ae,infection_order)


#################################

df <- df %>%
  mutate(
    sample_week = as.factor(sample_week),
    genotype = relevel(factor(genotype), ref = "CC"),
    theileria_mutans_af078815_tb = as.integer(theileria_mutans_af078815_tb),
    theileria_parva_l02366_tb = as.integer(theileria_parva_l02366_tb),
    theileria_velifera_af097993_tb = as.integer(theileria_velifera_af097993_tb),
    anaplasma_marginale_cp000030_ae = as.integer(anaplasma_marginale_cp000030_ae),
    infection_order = as.factor(infection_order),
    calf_id = as.factor(calf_id)
  )

# Fit the discrete time hazard model using logistic regression
    hazard_model <- glmer(
      event ~ sample_week + genotype + theileria_mutans_af078815_tb + theileria_parva_l02366_tb + theileria_velifera_af097993_tb + anaplasma_marginale_cp000030_ae + infection_order + (1 | calf_id),
      data = df,
      family = binomial(link = "logit"),
      control = glmerControl(optimizer = "bobyqa")
    )
    
# Summary of the model
    summary(hazard_model)
    
    
    # Load required libraries
    library(ggeffects)   # for marginal effects
    library(ggplot2)
    
    # Get predicted probabilities by month and genotype
    pred <- ggpredict(hazard_model, terms = c("sample_week", "genotype"))
    
    # Plot hazard over time by genotype
    ggplot(pred, aes(x = as.numeric(x), y = predicted, color = group)) +
      geom_line(size = 1.2) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
      labs(
        title = "Predicted Discrete-Time Hazard by Genotype",
        x = "Month",
        y = "Estimated Hazard (Probability of Death)",
        color = "Genotype",
        fill = "Genotype"
      ) +
      theme_minimal()