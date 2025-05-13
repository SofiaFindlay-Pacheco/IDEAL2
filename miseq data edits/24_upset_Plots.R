# Specific library
#install.packages("viridis")
#install.packages("ComplexUpset")
library(ComplexUpset)
library(dplyr)
library(ggplot2)
library(viridis)  


UpSet_graph_data <- final_miseq_data_clean %>%
  select(1:28, 39, 43, 47)

# in columns 3-26 change all numbers above 0 to 1  
UpSet_graph_data <- UpSet_graph_data %>%
  mutate(across(3:26, ~ ifelse(. > 0, 1, 0)))

# Sample week based on first two numbers of VisitID
UpSet_graph_data$SampleWeek <- substr(UpSet_graph_data$visit_id, 4, 5)

# Ensure sample_week is a factor (important for grouping colors)
UpSet_graph_data$sample_week <- as.factor(UpSet_graph_data$SampleWeek)

pathogen_cols <- c(
 "theileria_mutans_af078815_tb"   ,                                                      
                        "theileria_sp_strain_msd_af078816_tb"       ,                                           
                       "theileria_parva_l02366_tb"                 ,                                           
    #                  "theileria_taurotragi_l19082_tb"            ,                                           
                     "theileria_velifera_af097993_tb" ,
  
  
#pathogen_cols <- c(
"anaplasma_bovis_u03775_ae"    ,                                                        
            "anaplasma_bovis_ab983439_ae",                                                       
   #     "anaplasma_marginale_cp000030_ae",                                                      
       "anaplasma_platys_like_ku585990_ae",                                                    
   # "anaplasma_phagocytophilum_u02521_ae",                                                  
    #"candidatus_anaplasma_boleense_ku586025_ae"        ,                                    
        "uncultured_anaplasma_sp_clone_saso_ky924885_ae"     )                                  
    #  "uncultured_anaplasma_sp_jn862825_ae",
  #  "anaplasma_platys_ef139459_ae")
  
  #pathogens_columns <- c( "ehrlichia_sp_tibet_ehrlichia_canis_ehrlichia_minasensis_af414399_ay394465_mt163430_ae"  ,
  #     "ehrlichia_ruminantium_x61659_ae")
  
  #pathogens_columns <- c( "babesia_bigemina_ay603402_tb"  ,                                                       
   #                       "babesia_bigemina_lk391709_tb"  ,                                                       
    #                      "babesia_bigemina_ku206291_tb"  ,
     #                     "babesia_bovis_kf928959_tb"     ,                                                      
      #                   "babesia_bovis_aaxt01000002_tb" ,                                                   
        #                  "babesia_bovis_ay603398_tb"     ,                                                  
         #                 "babesia_bovis_jq437260_tb" )

# Plot with intersection size bars colored by SampleWeek
upset(
  UpSet_graph_data,
  intersect = pathogen_cols,
  min_size = 50,                    # only show combinations with ≥10 samples
  width_ratio = 0.3,
  base_annotations = list(
    'Intersection size' = intersection_size(
      mapping = aes(fill = SampleWeek)
    )
  )
) +
  scale_fill_viridis_d(option = "plasma", name = "Sample Week") +
  theme_minimal()

########################## Now by Calf #######################

UpSet_by_calf <- UpSet_graph_data %>%
  group_by(calf_id) %>%
  summarise(across(all_of(pathogen_cols), ~ as.integer(any(. > 0))))

upset(
  UpSet_by_calf,
  intersect = pathogen_cols,
  min_size = 5,  # optional: hide rare combinations
  width_ratio = 0.4,
  set_sizes = upset_set_size(),
  base_annotations = list(
    'Intersection size' = intersection_size()
  )
)

# Now add in aetiological cause
cause <- UpSet_graph_data %>%
  select(calf_id, definitive_aetiological_cause) %>%
  distinct()

UpSet_by_calf_annotated <- UpSet_by_calf %>%
  left_join(cause, by = "calf_id")

upset(
  UpSet_by_calf_annotated,
  intersect = pathogen_cols,
  min_size = 5,
  width_ratio = 0.4,
  set_sizes = upset_set_size(),
  base_annotations = list(
    'Intersection size' = intersection_size(
      mapping = aes(fill = definitive_aetiological_cause)
    ) + 
      scale_fill_viridis_d(name = "Aetiological Cause", option = "magma")
  )
)

# Calves that died
UpSet_dead <- UpSet_by_calf_annotated %>%
  filter(definitive_aetiological_cause == "Dead")

# Calves that survived
UpSet_alive <- UpSet_by_calf_annotated %>%
  filter(definitive_aetiological_cause == "Alive")

upset(
  UpSet_dead,
  intersect = pathogen_cols,
  width_ratio = 0.4,
  set_sizes = upset_set_size(),
  base_annotations = list(
    'Intersection size' = intersection_size()
  )
)

upset(
  UpSet_alive,
  intersect = pathogen_cols,
  width_ratio = 0.4,
  set_sizes = upset_set_size(),
  base_annotations = list(
    'Intersection size' = intersection_size()
  )
)
#####################################################
# Choose the pathogen columns
pathogen_cols <- c( "theileria_mutans_af078815_tb"   ,                                                      
                    "theileria_sp_strain_msd_af078816_tb"       ,                                           
                    "theileria_parva_l02366_tb"                 ,                                           
                    "theileria_taurotragi_l19082_tb"            ,                                           
                    "theileria_velifera_af097993_tb"  )  

# Reshape and summarise
prop_infected_long <- UpSet_graph_data %>%
  group_by(sample_week, calf_id) %>%
  summarise(across(all_of(pathogen_cols), ~ as.integer(any(. > 0))), .groups = "drop") %>%
  pivot_longer(cols = all_of(pathogen_cols), names_to = "pathogen", values_to = "infected") %>%
  group_by(sample_week, pathogen) %>%
  summarise(n_infected = sum(infected), n_calves = n_distinct(calf_id), prop = n_infected / n_calves, .groups = "drop")

# Calculate whether each calf was infected with ANY Theileria at each week
prop_theileria <- UpSet_graph_data %>%
  group_by(sample_week, calf_id) %>%
  summarise(
    pathogen_cols = as.integer(any(across(all_of(pathogen_cols), ~ . > 0))),
    .groups = "drop"
  ) %>%
  group_by(sample_week) %>%
  summarise(
    n_infected = sum(pathogen_cols),
    n_calves = n_distinct(calf_id),
    prop = n_infected / n_calves,
    .groups = "drop"
  )

####
write.csv(prop_theileria, "prop_infected_by_week_Theileria.csv", row.names = FALSE)




