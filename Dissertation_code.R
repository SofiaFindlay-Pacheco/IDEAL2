

########## Table 1 ##########

# survival data select desired columns
survival_data <- final_miseq_data_clean %>%
  distinct(calf_id, .keep_all = TRUE) %>%
  select(calf_id, event, time_to_event, definitive_aetiological_cause)

# Ensure time_to_event is numeric
survival_data$time_to_event <- as.numeric(survival_data$time_to_event)

# Count totals
summary_stats <- survival_data %>%
  summarise(
    total_calves = n(),
    total_deaths = sum(definitive_aetiological_cause == "Dead"),
    total_alive = sum(definitive_aetiological_cause == "Alive"),
    total_censored = sum(definitive_aetiological_cause == "Censored"),
    median_time_to_event = median(time_to_event),
    mean_time_to_event = mean(time_to_event),
    sd_time_to_event = sd(time_to_event)
  )

########## Figure 4 #####################

weekly_summary_stats <- survival_data %>%
  group_by(time_to_event) %>%
  summarise(
    total_calves = n(),
    total_deaths = sum(definitive_aetiological_cause == "Dead"),
    total_alive = sum(definitive_aetiological_cause == "Alive"),
    total_censored = sum(definitive_aetiological_cause == "Censored"),
    .groups = "drop"
  )

# Ensure definitive_aetiological_cause has a fixed factor order (to control consistent colours)
survival_data$definitive_aetiological_cause <- factor(survival_data$definitive_aetiological_cause, 
                                      levels = c("Dead", "Censored", "Alive"))

survival_data <- survival_data %>%
  mutate(
    event_group = ifelse(time_to_event == 51, "Week 51", "Before Week 51")
  )

ggplot(survival_data, aes(x = time_to_event, fill = definitive_aetiological_cause)) +
  geom_histogram(binwidth = 2, color = "black") +
  facet_wrap(~event_group, scales = "free_y") +
  scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.9, direction = -1, name = "Outcome") +
  labs(
    title = "Calf Time to Event Distribution by Outcome (Excluding Weeks 2 & 3)",
    x = "Weeks of Life",
    y = "Number of Calves"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

########## Figure 5 ########## 

# Group haemopathogen markers by genus
theileria_cols <- c(
  "theileria_parva_l02366_tb",
  "theileria_mutans_af078815_tb",
  "theileria_sp_strain_msd_af078816_tb",
  "theileria_taurotragi_l19082_tb",
  "theileria_velifera_af097993_tb"
)


anaplasma_cols <- c(
  "anaplasma_bovis_u03775_ae"    ,                                                        
  "anaplasma_bovis_ab983439_ae",                                                       
  "anaplasma_marginale_cp000030_ae",                                                      
  "anaplasma_platys_like_ku585990_ae",                                                    
  "anaplasma_phagocytophilum_u02521_ae",                                                 
  "candidatus_anaplasma_boleense_ku586025_ae"        ,                                    
  "uncultured_anaplasma_sp_clone_saso_ky924885_ae",                                       
  "uncultured_anaplasma_sp_jn862825_ae",
  "anaplasma_platys_ef139459_ae"
)

ehrlichia_cols <- c(
  "ehrlichia_sp_tibet_ehrlichia_canis_ehrlichia_minasensis_af414399_ay394465_mt163430_ae"  ,
  "ehrlichia_ruminantium_x61659_ae"
)

babesia_cols <- c(
  "babesia_bigemina_ay603402_tb"  ,                                                       
  "babesia_bigemina_lk391709_tb"  ,                                                       
  "babesia_bigemina_ku206291_tb"  ,
  "babesia_bovis_kf928959_tb"     ,                                                      
  "babesia_bovis_aaxt01000002_tb" ,                                                   
  "babesia_bovis_ay603398_tb"     ,                                                  
  "babesia_bovis_jq437260_tb")

# Define the haemopathogen columns
haemopathogen_cols <- c( 
  "theileria_mutans_af078815_tb",
  "theileria_sp_strain_msd_af078816_tb",
  "theileria_parva_l02366_tb",
  "theileria_velifera_af097993_tb",
  "anaplasma_bovis_ab983439_ae",
  "anaplasma_phagocytophilum_u02521_ae",
  "anaplasma_platys_like_ku585990_ae",
  "uncultured_anaplasma_sp_clone_saso_ky924885_ae"
)

pathogen_cols <- c("anaplasma_bovis_u03775_ae",                                                            
                   "anaplasma_bovis_ab983439_ae",                                                          
                   "anaplasma_marginale_cp000030_ae",                                                      
                   "anaplasma_platys_like_ku585990_ae",                                                    
                   "anaplasma_phagocytophilum_u02521_ae",                                                  
                   "candidatus_anaplasma_boleense_ku586025_ae",                                            
                   "uncultured_anaplasma_sp_clone_saso_ky924885_ae",                                       
                   "uncultured_anaplasma_sp_jn862825_ae",                                                  
                   "ehrlichia_sp_tibet_ehrlichia_canis_ehrlichia_minasensis_af414399_ay394465_mt163430_ae",
                   "ehrlichia_ruminantium_x61659_ae",                                                      
                   "anaplasma_platys_ef139459_ae",                                                         
                   "babesia_bigemina_ay603402_tb",                                                         
                   "babesia_bigemina_lk391709_tb",                                                         
                   "babesia_bigemina_ku206291_tb",                                                         
                   "theileria_mutans_af078815_tb",                                                         
                   "theileria_sp_strain_msd_af078816_tb",                                                  
                   "theileria_parva_l02366_tb",                                                            
                   "theileria_taurotragi_l19082_tb",                                                       
                   "theileria_velifera_af097993_tb",                                                       
                   "babesia_bovis_kf928959_tb",                                                            
                   "babesia_bovis_aaxt01000002_tb",                                                        
                   "babesia_bovis_ay603398_tb",                                                           
                   "babesia_bigemina_lk391709_2_tb",                                                       
                   "babesia_bovis_jq437260_tb")

#Some upset graph coding
binary_pathogen_data <- final_miseq_data_clean #%>%

# in columns 3-26 change all numbers above 0 to 1  
binary_pathogen_data <- binary_pathogen_data %>%
  mutate(across(11:34, ~ ifelse(. > 0, 1, 0)))

binary_infection_data <- final_miseq_data_clean %>%
  mutate(across(all_of(pathogen_cols), ~ ifelse(. > 0, 1, 0)))

# Sample week based on first two numbers of VisitID
binary_pathogen_data$SampleWeek <- substr(binary_pathogen_data$visit_id, 4, 5)

# Ensure sample_week is a factor (important for grouping colors)
binary_pathogen_data$sample_week <- as.factor(binary_pathogen_data$SampleWeek)


# Function to calculate weekly proportions for any haemopathogen group
calc_prop_by_week <- function(data, cols, group_name) {
  data %>%
    group_by(sample_week, calf_id) %>%
    summarise(infected = as.integer(any(across(all_of(cols), ~ . > 0))), .groups = "drop") %>%
    group_by(sample_week) %>%
    summarise(
      group = group_name,
      n_infected = sum(infected),
      n_calves = n_distinct(calf_id),
      prop = n_infected / n_calves,
      .groups = "drop"
    )
}

# Compute proportions for each haemopathogen genus
prop_theileria <- calc_prop_by_week(binary_pathogen_data, theileria_cols, "Theileria") 
prop_theileria$sample_week = as.numeric(as.character(prop_theileria$sample_week))
prop_theileria <- prop_theileria %>%
  filter(!sample_week %in% c(2, 3))  

prop_anaplasma <- calc_prop_by_week(binary_pathogen_data, anaplasma_cols, "Anaplasma")
prop_anaplasma$sample_week = as.numeric(as.character(prop_anaplasma$sample_week))
prop_anaplasma <- prop_anaplasma %>%
  filter(!sample_week %in% c(2, 3))  

prop_ehrlichia <- calc_prop_by_week(binary_pathogen_data, ehrlichia_cols, "Ehrlichia")
prop_ehrlichia$sample_week = as.numeric(as.character(prop_ehrlichia$sample_week))
prop_ehrlichia <- prop_ehrlichia %>%
  filter(!sample_week %in% c(2, 3))  

prop_babesia   <- calc_prop_by_week(binary_pathogen_data, babesia_cols, "Babesia")
prop_babesia$sample_week = as.numeric(as.character(prop_babesia$sample_week))
prop_babesia <- prop_babesia %>%
  filter(!sample_week %in% c(2, 3))  

# Combine all genus-level summaries
prop_all <- bind_rows(prop_theileria, prop_anaplasma, prop_ehrlichia, prop_babesia)
prop_all$sample_week <- as.numeric(as.character(prop_all$sample_week))  # Or as.Date() if it's a date

# Set 'group' as a factor in the order of legend
prop_all$group <- factor(prop_all$group, levels = c( "Anaplasma","Theileria","Ehrlichia","Babesia"))

# Then plot
ggplot(prop_all, aes(x = sample_week, y = prop, color = group)) +
  geom_line(size = 1) +
  geom_point(size = 2, shape = 16) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_viridis_d(option = "D") + 
  labs(
    title = "Weekly Proportion of Calves Infected with Haemopathogen Genera",
    x = "Weeks of Life",
    y = "Proportion Infected (%)",
    color = "Pathogen Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )

# Function to compute chi-squared test for each genus
chi_squared_by_age <- function(data, cols, genus_name) {
  data %>%
    mutate(infected = as.integer(rowSums(across(all_of(cols))) > 0)) %>%
    group_by(sample_week) %>%  # replace with derived age bins if needed
    summarise(
      infected = sum(infected),
      uninfected = n() - infected,
      .groups = "drop"
    ) -> tab
  
  # Convert to matrix for chisq.test
  chisq_input <- as.matrix(tab[, c("infected", "uninfected")])
  rownames(chisq_input) <- tab$sample_week
  
  test_result <- chisq.test(chisq_input)
  
  list(
    genus = genus_name,
    chisq_statistic = test_result$statistic,
    p_value = test_result$p.value,
    expected = test_result$expected,
    observed = chisq_input
  )
}

# Run tests for each genus
result_theileria <- chi_squared_by_age(binary_pathogen_data, theileria_cols, "Theileria")
result_anaplasma <- chi_squared_by_age(binary_pathogen_data, anaplasma_cols, "Anaplasma")
result_ehrlichia <- chi_squared_by_age(binary_pathogen_data, ehrlichia_cols, "Ehrlichia")
result_babesia   <- chi_squared_by_age(binary_pathogen_data, babesia_cols, "Babesia")

# Combine summary results
chi_summary <- data.frame(
  Genus = c("Theileria", "Anaplasma", "Ehrlichia", "Babesia"),
  ChiSq_Statistic = c(result_theileria$chisq_statistic,
                      result_anaplasma$chisq_statistic,
                      result_ehrlichia$chisq_statistic,
                      result_babesia$chisq_statistic),
  P_Value = c(result_theileria$p_value,
              result_anaplasma$p_value,
              result_ehrlichia$p_value,
              result_babesia$p_value)
)

print(chi_summary)

########## Figure 6 ########## 

# Reshape and summarise
prop_infected_long <- binary_pathogen_data %>%
  group_by(sample_week, calf_id) %>%
  summarise(across(all_of(haemopathogen_cols), ~ as.integer(any(. > 0))), .groups = "drop") %>%
  pivot_longer(cols = all_of(haemopathogen_cols), names_to = "pathogen", values_to = "infected") %>%
  group_by(sample_week, pathogen) %>%
  summarise(n_infected = sum(infected), n_calves = n_distinct(calf_id), prop = n_infected / n_calves, .groups = "drop")

#Ensure sample week is numeric
prop_infected_long$sample_week = as.numeric(as.character(prop_infected_long$sample_week))

#Remove week 2 and 3 from data
prop_infected_long <- prop_infected_long %>%
  filter(!sample_week %in% c(2, 3))  

pathogen_rename <- c(
  "theileria_parva_l02366_tb" = "T. parva",
  "theileria_mutans_af078815_tb" = "T. mutans",
  "theileria_velifera_af097993_tb" = "T. velifera",
  "anaplasma_bovis_ab983439_ae"  = "A. bovis"                       ,                                 
  "anaplasma_platys_like_ku585990_ae"= "A. platys like"                     ,                               
  "anaplasma_phagocytophilum_u02521_ae" = "A. phagocytophilum"                  ,                              
  "uncultured_anaplasma_sp_clone_saso_ky924885_ae" = "uncultured Anaplasma sp. clone saso"          ,                            
  "theileria_sp_strain_msd_af078816_tb"  = "Theileria sp. strain MSD")

prop_infected_long <- prop_infected_long %>%
  mutate(pathogen = recode(pathogen, !!!pathogen_rename))

# Order legend
prop_infected_long$pathogen <- factor(prop_infected_long$pathogen, 
                                      levels = c("A. platys like", "T. mutans", "Theileria sp. strain MSD",
                                                 "T. velifera", "uncultured Anaplasma sp. clone saso", 
                                                 "A. bovis", "T. parva", "A. phagocytophilum"))

ggplot(prop_infected_long, aes(x = sample_week, y = prop, color = pathogen, group = pathogen)) +
  geom_line(size = 1, alpha = 0.3) +
  geom_point(size = 2) +
  theme_minimal() +
  scale_color_viridis_d(option = "D") + 
  labs(
    title = "Proportion of Calves Infected per Sample Week",
    x = "Weeks of Life",
    y = "Proportion Infected"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.title = element_blank())

kruskal.test(prop ~ pathogen, data = prop_infected_long)

########## Figure 7 ########## 

diversity_data <- binary_pathogen_data %>%
  rowwise() %>%
  mutate(
    richness = sum(c_across(all_of(pathogen_cols)) > 0, na.rm = TRUE),
    simpson = if (richness == 0) NA_real_ else 
      diversity(c_across(all_of(pathogen_cols)), index = "simpson")
  ) %>%
  ungroup() %>%
  mutate(sample_week = as.numeric(as.character(sample_week))) %>%   # ensure true numbers
  filter(!sample_week %in% c(2, 3, 5))

# Average across calves at each week to get the "population-level" trend
simpson_trend <- diversity_data %>%
  group_by(sample_week) %>%
  summarise(
    mean_simpson = mean(simpson, na.rm = TRUE),
    se = sd(simpson, na.rm = TRUE)/sqrt(n())
  )

simpson_trend <- simpson_trend %>%
  mutate(sample_week = as.numeric(as.character(sample_week)))

# Plot mean Simpson index over calf age
ggplot(simpson_trend, aes(x = as.numeric(sample_week), y = mean_simpson)) +
  geom_line(color = "darkblue", size = 1.2) +
  geom_point(color = "darkblue", size = 2) +
  geom_ribbon(aes(ymin = mean_simpson - se, ymax = mean_simpson + se),
              alpha = 0.2, fill = "blue") +
  labs(title = "Simpson Diversity of Haemopathogen Infections with Age",
       x = "Weeks of Life", y = "Mean Simpson Index (±SE)") +
  theme_minimal()

########## Figure 8 & 9 ########## 

# Summarise total burden per calf 
burden_df <- final_miseq_data_clean %>%
  group_by(calf_id) %>%
  summarise(
    tparva_load = sum(theileria_parva_l02366_tb, na.rm = TRUE),
    theileria_load = sum(across(all_of(theileria_cols)), na.rm = TRUE),
    anaplasma_load = sum(across(all_of(anaplasma_cols)), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(final_miseq_data_clean %>% 
              select(calf_id, calf_sex, sample_week, agro_ecological_zones, definitive_aetiological_cause) %>% 
              distinct(), 
            by = "calf_id") %>%
  mutate(
    sex = as.factor(calf_sex),
    sublocation_zone = as.factor(agro_ecological_zones)
  )%>%
  filter(!definitive_aetiological_cause %in% c("Censored"))  

#Plot burden against survival for each risk factor
plot_burden <- function(data, burden_col, group_var, title, y_label, label_map = NULL) {
  p <- ggplot(data, aes(x = {{ group_var }}, y = {{ burden_col }}, fill = {{ group_var }})) +
    geom_violin(trim = FALSE, alpha = 0.8, color = NA) +
    geom_boxplot(width = 0.1, outlier.shape = NA, color = "white")+
    labs(title = title, x = NULL, y = y_label) +
    scale_fill_viridis_d(option = "D") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none") +
    scale_y_continuous(trans = "log1p")
  
  if (!is.null(label_map)) {
    p <- p + scale_x_discrete(labels = label_map)
  }
  
  return(p)
}

p1_breaks <- c(0, 100, 1000, 10000, 50000, 100000, 250000, 500000, 1000000, 2000000, 5000000) # Added more granular breaks at lower end
p1 <- plot_burden(burden_df, tparva_load, definitive_aetiological_cause, bquote("A) " * italic(" T. parva") * " Load by Survival Outcome"),   # italicised T. parva
                  bquote(italic("T. parva") * " Load (log scale)")                    # italicised T. parva in y-axis
) + 
  scale_y_continuous(trans = "log1p",  breaks = p1_breaks, labels = scales::comma_format()) +
  theme(
    axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

p2_breaks <- c(0, 5000, 10000, 50000, 100000, 250000, 500000, 1000000, 3000000) # Added more granular breaks at lower end
p2 <- plot_burden(burden_df, theileria_load, definitive_aetiological_cause, bquote("B) Total" * italic(" Theileria ") * " Load by Survival Outcome"),   # italicised T. parva
                  bquote(italic("Theileria") * " Load (log scale)")                    # italicised T. parva in y-axis
) +  
  scale_y_continuous(trans = "log1p", breaks = p2_breaks, labels = scales::comma_format()) +
  theme(
    axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

p3_breaks <- c(0, 5000, 10000, 50000, 100000, 250000, 500000, 1000000, 2000000, 4000000) # Added more granular breaks at lower end
p3 <- plot_burden(burden_df, anaplasma_load, definitive_aetiological_cause, bquote("C) Total" * italic(" Anaplasma ") * " Load by Survival Outcome"),   # italicised T. parva
                  bquote(italic("Anaplasma") * " Load (log scale)")                    # italicised T. parva in y-axis
) +   
  scale_y_continuous(trans = "log1p", breaks = p3_breaks, labels = scales::comma_format()) +
  theme(
    axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

p4_breaks <- c(0, 100, 1000, 5000, 10000, 50000, 100000, 250000, 1000000) # Added more granular breaks at lower end
p4 <- plot_burden(burden_df, tparva_load, sublocation_zone, bquote("A) Total" * italic(" T. parva ") * " Load by sublocations"),   # italicised T. parva
                  bquote(italic("T. parva Load") * " Load (log scale)") )+ 
  scale_y_continuous(trans = "log1p", breaks = p4_breaks, labels = scales::comma_format()) +
  theme(
    axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

p5_breaks <- c(0,100, 1000, 5000, 10000, 50000, 100000, 250000, 1000000) # Added more granular breaks at lower end
p5 <- plot_burden(burden_df, tparva_load, sex, bquote("B) Total" * italic(" T. parva ") * " Load by Sex"),   # italicised T. parva
                  bquote(italic("T. parva Load") * " Load (log scale)") )+ 
  scale_y_continuous(trans = "log1p", breaks = p5_breaks, labels = scales::comma_format()) +
  theme(
    axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# Plot 6: T. parva load by week of life 
p6_breaks <- c(0, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 2000000) # Added more granular breaks at lower end
p6 <- ggplot(burden_df, aes(x = as.numeric(sample_week), y = tparva_load)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "C) T. parva Load Across Weeks of Life", x = "Sample Week", y = "T. parva Load") +
  theme_minimal(base_size = 13) +
  scale_y_continuous(trans = "log1p", breaks = p6_breaks, labels = scales::comma_format()) +
  theme(
    axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

wilcox.test(tparva_load ~ definitive_aetiological_cause, data = burden_df)
# Get medians and IQRs
burden_df %>%
  group_by(definitive_aetiological_cause) %>%
  summarise(
    median_load = median(tparva_load, na.rm = TRUE),
    IQR_low = quantile(tparva_load, 0.25, na.rm = TRUE),
    IQR_high = quantile(tparva_load, 0.75, na.rm = TRUE),
    n = n()
  )

wilcox.test(theileria_load ~ definitive_aetiological_cause, data = burden_df)
burden_df %>%
  group_by(definitive_aetiological_cause) %>%
  summarise(
    median_load = median(theileria_load, na.rm = TRUE),
    IQR_low = quantile(theileria_load, 0.25, na.rm = TRUE),
    IQR_high = quantile(theileria_load, 0.75, na.rm = TRUE),
    n = n()
  )
wilcox.test(anaplasma_load ~ definitive_aetiological_cause, data = burden_df)
burden_df %>%
  group_by(definitive_aetiological_cause) %>%
  summarise(
    median_load = median(anaplasma_load, na.rm = TRUE),
    IQR_low = quantile(anaplasma_load, 0.25, na.rm = TRUE),
    IQR_high = quantile(anaplasma_load, 0.75, na.rm = TRUE),
    n = n()
  )
kruskal.test(tparva_load ~ agro_ecological_zones, data = burden_df)
burden_df %>%
  group_by(agro_ecological_zones) %>%
  summarise(
    median_load = median(tparva_load, na.rm = TRUE),
    IQR_low = quantile(tparva_load, 0.25, na.rm = TRUE),
    IQR_high = quantile(tparva_load, 0.75, na.rm = TRUE),
    n = n()
  )
wilcox.test(tparva_load ~ calf_sex, data = burden_df)
burden_df %>%
  group_by(calf_sex) %>%
  summarise(
    median_load = median(tparva_load, na.rm = TRUE),
    IQR_low = quantile(tparva_load, 0.25, na.rm = TRUE),
    IQR_high = quantile(tparva_load, 0.75, na.rm = TRUE),
    n = n()
  )

# Arrange and print plots 
ggarrange(p1, p2, p3, ncol = 2, nrow = 2)
ggarrange(p4, p5, ncol = 2, nrow = 2)

########## Figure 10 ########## 


# Summarize survival data grouped by calf_id
windowed_data_sex <- final_miseq_data_clean %>%
  group_by(calf_id) %>%
  summarize(
    calf_sex = first(calf_sex),
    time_to_event = max(time_to_event, na.rm = TRUE),
    event = max(event, na.rm = TRUE)
  ) %>%
  ungroup()

# Ensure time_to_event is numeric
windowed_data_sex$time_to_event <- as.numeric(windowed_data_sex$time_to_event)

# Kaplan-Meier fit by sex
km_fit_sex <- survfit(Surv(time_to_event, event) ~ calf_sex, data = windowed_data_sex)

# Set viridis colours (2 categories)
viridis_palette <- viridis(2, option = "D")

# Plot
ggsurvplot(
  km_fit_sex,
  data = windowed_data_sex,
  conf.int = TRUE,
  pval = TRUE,
  pval.method = TRUE,               # show test used (log-rank)
  pval.size = 5,                    # increase font size
  pval.coord = c(2, 0.82),         # position: x = 10 weeks, y = 0.78
  risk.table = TRUE,
  censor = TRUE,
  palette = viridis_palette,
  ggtheme = theme_minimal(base_size = 14),
  title = "Kaplan-Meier Survival Curve: Impact of Calf Sex on Mortality",
  ylab = "Survival Probability",
  xlab = "Time to Death (Weeks)",
  ylim = c(0.75, 1)
)

# cox model
cox_model_sex <- coxph(Surv(time_to_event, event) ~ calf_sex, data = windowed_data_sex)

# Display summary of the Cox model
summary(cox_model_sex)

########## Figure 11 ########## 

surv_sub <- final_miseq_data_clean %>%
  select(calf_id, sample_week, time_to_event, definitive_aetiological_cause, event, agro_ecological_zones)

surv_subs <- surv_sub %>%
  group_by(calf_id) %>%
  summarise(
    time_to_event = first(time_to_event),       # use existing column
    event = first(event),                       # keep event indicator
    agro_ecological_zones = first(agro_ecological_zones),  
    .groups = "drop"
  )

survdiff(Surv(time_to_event, event) ~ agro_ecological_zones, data = surv_subs)

# Summarise death rates by sublocation
death_summary <- surv_subs %>%
  group_by(agro_ecological_zones) %>%
  summarise(
    n = n(),
    deaths = sum(event),
    death_rate = deaths / n
  ) %>%
  arrange(desc(death_rate))

ggplot(death_summary, aes(x = reorder(agro_ecological_zones, death_rate), y = death_rate, fill = agro_ecological_zones)) +
  geom_col(color = "black") +
  scale_fill_viridis_d(option = "D") +
  coord_flip() +
  labs(
    title = "Mortality Percentage by Sublocation Zone",
    x = "Sublocation Zone",
    y = "Mortality Percentage"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

########## Figure 12 ########## 

# Identify FIRST T. parva infection per calf
first_tparva_week <- final_miseq_data_clean %>%
  filter(theileria_parva_l02366_tb > 0) %>%
  group_by(calf_id) %>%
  summarise(first_week = min(sample_week, na.rm = TRUE), .groups = "drop")

first_tparva_week <- first_tparva_week %>%
  filter(!first_week %in% c(2, 3)) 

# Merge with survival info (grouped by calf)
surv_info <- final_miseq_data_clean %>%
  group_by(calf_id) %>%
  summarise(
    time_to_event = max(time_to_event, na.rm = TRUE),
    event = max(event, na.rm = TRUE),
    .groups = "drop"
  )

# Join survival + infection timing, exclude calves never infected
km_df <- first_tparva_week %>%
  left_join(surv_info, by = "calf_id") %>%
  filter(!is.na(first_week)) %>%  # only those infected
  mutate(
    infection_timing = ifelse(first_week <= 25, "Early Infection", "Late Infection"),
    infection_timing = factor(infection_timing, levels = c("Early Infection", "Late Infection"))
  )

# Fit Kaplan-Meier survival model
km_fit <- survfit(Surv(time_to_event, event) ~ infection_timing, data = km_df)

viridis_palette <- viridis(3, option = "D")

# Plot
ggsurvplot(
  km_fit,
  data = km_df,
  conf.int = TRUE,
  risk.table = TRUE,
  pval = TRUE,
  #pval.method = TRUE,               # show test used (log-rank)
  pval.size = 5,                    # increase font size
  pval.coord = c(2, 0.25), 
  censor = TRUE,
  palette = viridis_palette,
  ggtheme = theme_minimal(base_size = 14),
  title = "Survival by Timing of T. parva Infection",
  xlab = "Time to Death (Weeks)",
  ylab = "Survival Probability",
  legend.title = "Infection Timing",
  legend.labs = c("Early infection", "Late infection")
)

########## Figure 13 ########## 

# Your rename mapping
column_rename <- c(
  "T. parva" =   "theileria_parva_l02366_tb",
  "T. mutans" = "theileria_mutans_af078815_tb",        
  "T. velifera" = "theileria_velifera_af097993_tb",      
  "A. bovis ab983439" = "anaplasma_bovis_ab983439_ae",
  "A. platys like" = "anaplasma_platys_like_ku585990_ae",
  "A. phagocytophilum" = "anaplasma_phagocytophilum_u02521_ae",
  "uncultured A. sp clone" = "uncultured_anaplasma_sp_clone_saso_ky924885_ae",
  "T. sp strain msd" = "theileria_sp_strain_msd_af078816_tb"
)

# Apply renaming to your dataframe
binary_pathogen_data <- binary_pathogen_data %>%
  rename(!!!column_rename)

# Update haemopathogen_cols to match the new names
haemopathogen_cols <- names(column_rename)

# By sample week
upset(
  binary_pathogen_data,
  intersect = haemopathogen_cols,
  min_size = 50,  
  width_ratio = 0.3,
  base_annotations = list(
    'Intersection size' = intersection_size(
      mapping = aes(fill = SampleWeek)
    )
  )
) +
  scale_fill_viridis_d(option = "D", name = "Sample Week") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),   # Remove bottom axis text
    axis.ticks.x = element_blank(),  # Remove tick marks
    plot.margin = margin(10, 10, 10, 10)  # Optional: add space
  )

# By survival outcome
binary_pathogen_data_per_calf <- binary_pathogen_data %>%
  group_by(calf_id) %>%
  summarise(across(all_of(haemopathogen_cols), ~ as.integer(any(. > 0))))

# Now add in aetiological cause
cause <- binary_pathogen_data %>%
  select(calf_id, definitive_aetiological_cause) %>%
  distinct()

binary_pathogen_data_per_calf_annotated <- binary_pathogen_data_per_calf %>%
  left_join(cause, by = "calf_id")
# Apply renaming to your dataframe

binary_pathogen_data_per_calf_annotated <- binary_pathogen_data_per_calf_annotated %>%
  rename(!!!column_rename)

# Update haemopathogen_cols to match the new names
haemopathogen_cols <- names(column_rename)

upset(
  binary_pathogen_data_per_calf_annotated,
  intersect = haemopathogen_cols,
  min_size = 5,
  width_ratio = 0.4,
  set_sizes = upset_set_size(),
  base_annotations = list(
    'Intersection size' = intersection_size(
      mapping = aes(fill = definitive_aetiological_cause)
    ) + 
      scale_fill_viridis_d(name = "Survival outcome", option = "D")
  )
)

########## Figure 14 ########## 

# Identify earliest infection for each pathogens
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
      (earliest_mutans == earliest_velifera) & (earliest_mutans < earliest_parva) ~ "T. Mutans/T. Velifera First",
      earliest_parva < earliest_mutans & earliest_parva < earliest_velifera ~ "T. Parva First",
      TRUE ~ "No exposure (3 species)"
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
    infection_order %in% c("Mutans First", "Velifera First", "T. Mutans/T. Velifera First") ~ "T. Mutans/T. Velifera First",
    TRUE ~ infection_order  # Keep other values unchanged
  ))

# Convert to factor for plotting
windowed_data$infection_order <- factor(windowed_data$infection_order, 
                                        levels = c("No exposure (3 species)", "T. Mutans/T. Velifera First", "T. Parva First"))

windowed_data_detailed <- final_miseq_data_clean %>%
  group_by(calf_id) %>%
  summarise(
    tparva_load = sum(theileria_parva_l02366_tb, na.rm = TRUE),
    outcome = first(definitive_aetiological_cause),
    .groups = "drop"
  ) %>%
  left_join(windowed_data, by = "calf_id") %>%
  filter(outcome %in% c("Alive", "Dead"))

windowed_data_detailed <- windowed_data_detailed %>%
  filter(infection_order %in% c("T. Mutans/T. Velifera First", "T. Parva First"))

ggplot(windowed_data_detailed, aes(x = infection_order, y = tparva_load, fill = outcome)) +
  geom_violin(position = position_dodge(width = 0.9), trim = FALSE, alpha = 0.8) +
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.1, 
               outlier.shape = NA, color = "black") +
  scale_y_continuous(
    trans = "log1p",
    breaks = c(1e2, 1e3, 1e4, 1e5, 1e6, 1e7),
    labels = label_scientific(digits = 1)
  ) +
  scale_fill_viridis_d(option = "D", name = "Outcome") +
  labs(
    title = "T. parva Load by Infection Order and Survival Outcome",
    x = "Infection Order",
    y = "Total T. parva Read Count (log scale)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

########## Figure 15 ########## 

# Kaplan-Meier survival fit
km_fit <- survfit(Surv(time_to_event, event) ~ infection_order, data = windowed_data)

# Plot Kaplan-Meier curve
ggsurvplot(km_fit, data = windowed_data,
           conf.int = TRUE, 
           pval = TRUE, 
           pval.method = TRUE,               # show test used (log-rank)
           pval.size = 5,                    # increase font size
           pval.coord = c(2, 0.65), 
           risk.table = TRUE, 
           censor = TRUE,  
           ggtheme = theme_minimal(),
           title = "Kaplan-Meier Survival by infection Order",
           ylim = c(0.6, 1),
           xlab = "Time to Death (Weeks)",   
           palette = viridis_palette
)

cox_model <- coxph(Surv(time_to_event, event) ~ infection_order, data = windowed_data)

# View model summary
summary(cox_model)
# Test proportional hazards assumption
cox.zph(cox_model)

########## Figure 16 ########## 

Genomics <- here("Edited original data", "Genomics.xlsx")
Genomics <- read_excel(Genomics)
Genomics_clean <- Genomics %>% clean_names()

# Merge the genomics and calf ID dataframe 

# Merge two data frames by 'calf_id', keeping all columns from both
merged_genomics <- full_join(final_miseq_data_clean, Genomics_clean, by = "calf_id")
merged_genomics <- merged_genomics %>%
  group_by(calf_id) %>%
  slice(1) %>%  # Select the first row of each group
  ungroup()
merged_genomics <- select(merged_genomics, calf_id, event,died, time_to_event, genotype, definitive_aetiological_cause, event, time_to_event)

# Manually remove errors
merged_genomics <- merged_genomics %>%
  filter(!calf_id %in% c("CA020610172", "CA051910553"))

# clean up data
merged_genomics <- merged_genomics %>%
  mutate(
    definitive_aetiological_cause = as.character(definitive_aetiological_cause),  # Convert factor to character
    definitive_aetiological_cause = ifelse(died == "No", "Alive", definitive_aetiological_cause),
    event = ifelse(died == "No", 0, event),
    time_to_event = ifelse(is.na(time_to_event) & died == "No", 51, time_to_event)  # Update only if NA
  ) %>%
  mutate(definitive_aetiological_cause = as.factor(definitive_aetiological_cause))  # Convert back to factor if needed

# Convert 'genotype' to a factor
merged_genomics$genotype <- as.factor(merged_genomics$genotype)

# Summarize survival data grouped by calf_id
merged_genomics <- merged_genomics %>%
  group_by(calf_id) %>%
  summarize(
    genotype = first(genotype),  # Get sex for each calf
    time_to_event = max(time_to_event, na.rm = TRUE),
    event = max(event, na.rm = TRUE)  # Retains event=0 for alive & censored
  ) %>%
  ungroup()

# Fit Kaplan-Meier model by genotype
km_fit_genomics <- survfit(Surv(time_to_event, event) ~ genotype, data = merged_genomics)

# Plot Kaplan-Meier curve
ggsurvplot(
  km_fit_genomics, 
  data = merged_genomics,
  conf.int = TRUE, 
  pval = TRUE, 
  pval.method = TRUE,               # show test used (log-rank)
  pval.size = 5,                    # increase font size
  pval.coord = c(5, 0.87),   
  risk.table = TRUE, 
  censor = TRUE,  
  censor.shape = "|",  # Show censored calves as vertical ticks
  censor.size = 3,  
  palette = viridis_palette,
  ggtheme = theme_minimal(base_size = 14),  
  title = "Kaplan-Meier Survival Curve: Impact of Genotype on mortality",
  ylim = c(0.85, 1),
  xlab = "Time to Death (Weeks)"
)

########## Table 2 ##########

merged_genomics$genotype <- relevel(as.factor(merged_genomics$genotype), ref = "CC")

firth_model <- coxphf(Surv(time_to_event, event) ~ genotype, data = merged_genomics)
summary(firth_model)

########## Figure 17 ########## 

# Create burden summary (one row per calf_id)
burden_summary <- final_miseq_data_clean %>%
  group_by(calf_id) %>%
  summarise(
    theileria_load = sum(across(all_of(theileria_cols)), na.rm = TRUE),
    anaplasma_load = sum(across(all_of(anaplasma_cols)), na.rm = TRUE),
    .groups = "drop"
  )

# Count unique pathogens detected per calf
co_infection_count <- final_miseq_data_clean %>%
  group_by(calf_id) %>%
  summarise(
    co_infection_n = sum(sapply(across(all_of(pathogen_cols)), function(x) any(x > 0, na.rm = TRUE))),
    .groups = "drop"
  )

# T. parva load
tparva_load_df <- final_miseq_data_clean %>%
  group_by(calf_id) %>%
  summarise(
    tparva_load = sum(theileria_parva_l02366_tb, na.rm = TRUE),
    .groups = "drop"
  )

# Merge everything into combined_data
combined_data <- windowed_data %>%
  select(calf_id, time_to_event, event, infection_order) %>%
  left_join(Genomics_clean %>% select(calf_id, genotype), by = "calf_id") %>%
  left_join(tparva_load_df, by = "calf_id") %>%
  left_join(burden_summary, by = "calf_id") %>%   # << added here
  left_join(co_infection_count, by = "calf_id") %>%
  filter(!is.na(event), !is.na(time_to_event), !is.na(genotype)) %>%
  mutate(
    genotype = factor(genotype),
    infection_order = factor(infection_order),
    genotype = relevel(genotype, ref = "CC"),
    infection_order = relevel(infection_order, ref = "No infection")
  )

combined_data <- combined_data %>%
  mutate(
    log_tparva    = as.numeric(scale(log1p(tparva_load))),
    log_anaplasma = as.numeric(scale(log1p(anaplasma_load))),
    log_theileria = as.numeric(scale(log1p(theileria_load)))
  )

cox_firth <- coxphf(
  Surv(time_to_event, event) ~ genotype + infection_order + co_infection_n
  + log_tparva + log_anaplasma + log_theileria,
  data = combined_data,
  maxit = 200,
  maxstep = 0.5
)

summary(cox_firth)

# Plot it
hr_data <- data.frame(
  variable = c("Genotype: CT", "Genotype: TT", 
               "Infection order: Mutans/Velifera First", 
               "Infection order: Parva First", 
               "Co-infection count", 
               "log(T. parva load)", 
               "log(Anaplasma load)", 
               "log(Theileria load)"),
  HR = c(0.6391, 0.2946, 0.9193, 6.3578, 0.6625, 2.1904, 0.4743, 0.8087),
  lower_CI = c(0.2985, 0.0023, 0.2314, 1.9241, 0.4845, 1.1719, 0.3221, 0.3389),
  upper_CI = c(1.3419, 2.2669, 4.1193, 27.3617, 0.8779, 4.1294, 0.6899, 1.7924))


hr_data <- hr_data %>%
  mutate(
    p_value = case_when(
      is.na(lower_CI) ~ NA_real_,
      lower_CI > 1 | upper_CI < 1 ~ 0.01,  
      TRUE ~ 0.2  # otherwise not significant
    ),
    sig = case_when(
      is.na(p_value) ~ "",
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

# Add a baseline reference (HR = 1) and label significance
ggplot(hr_data, aes(x = variable, y = HR, ymin = lower_CI, ymax = upper_CI)) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  # baseline
  geom_text(aes(label = sig, y = upper_CI * 1.1),  # place stars slightly above CI
            size = 4, color = "black") +
  coord_flip() +
  scale_y_log10() +
  labs(
    x = "",
    y = "Hazard Ratio (log scale)",
    title = "Cox–Firth Regression: Hazard Ratios with 95% CI"
  ) +
  theme_minimal()


########## Table 3 ########## 

model_full <- coxphf(
  Surv(time_to_event, event) ~ genotype + infection_order + co_infection_n + log_tparva + log_anaplasma + log_theileria,
  data = combined_data
)

# Reduced models (drop one covariate at a time)
model_no_genotype   <- update(model_full, . ~ . - genotype)
model_no_infection  <- update(model_full, . ~ . - infection_order)
model_no_coinf      <- update(model_full, . ~ . - co_infection_n)
model_no_tparva     <- update(model_full, . ~ . - log_tparva)
model_no_anaplas    <- update(model_full, . ~ . - log_anaplasma)
model_no_theil     <- update(model_full, . ~ . - log_theileria)


extract_info <- function(model, name) {
  ll <- logLik(model)
  k  <- length(coef(model))   # number of parameters
  aic <- -2 * as.numeric(ll) + 2 * k
  data.frame(Model = name, logLik = as.numeric(ll), k = k, AIC = aic)
}

results <- rbind(
  extract_info(model_full, "Full model"),
  extract_info(model_no_genotype, "No genotype"),
  extract_info(model_no_infection, "No infection order"),
  extract_info(model_no_coinf, "No co-infection count"),
  extract_info(model_no_tparva, "No Theileria parva"),
  extract_info(model_no_anaplas, "No Anaplasma"),
  extract_info(model_no_theil, "No Theileria ")
)

results$Delta_AIC <- results$AIC - min(results$AIC)

lr_test <- function(full, reduced) {
  test <- anova(full, reduced)  # works if both are coxphf
  test$"Pr(>Chi)"[2]
}

pvals <- c(
  NA,
  lr_test(model_full, model_no_genotype),
  lr_test(model_full, model_no_infection),
  lr_test(model_full, model_no_coinf),
  lr_test(model_full, model_no_tparva), 
  lr_test(model_full, model_no_anaplas), 
  lr_test(model_full, model_no_theil) 
  
)

results$p_value <- pvals

results

########## Appendix A Figure 1 ########## 

###(For this only complete data wrangle up to #### Label definitive aetiological cause column step)

# Cause of death summary
cause_of_death_summary <- final_miseq_data_clean %>%
  filter(!is.na(definitive_aetiological_cause)) %>%  # Only rows where calf died
  distinct(calf_id, .keep_all = TRUE) %>%            # Only one entry per calf
  count(definitive_aetiological_cause, sort = TRUE) %>%
  rename(Cause = definitive_aetiological_cause, Count = n)

# Plot it
ggplot(cause_of_death_summary, aes(x = reorder(Cause, -Count), y = Count, fill = Cause)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.3, size = 4) +
  scale_fill_viridis_d(option = "D") +
  labs(title = "Number of Calf Deaths by Cause",
       x = "Cause of Death",
       y = "Number of Calves") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))

########## Appendix A Figure 2 ########## 

pathogen_rename <- c(
  "theileria_parva_l02366_tb" = "T. parva",
  "theileria_mutans_af078815_tb" = "T. mutans",
  "theileria_velifera_af097993_tb" = "T. velifera",
  "anaplasma_marginale_cp000030_ae" = "A. marginale",
  "anaplasma_bovis_u03775_ae" = "A. bovis u03775",
  "ehrlichia_ruminantium_x61659_ae" = "E. ruminantium",
  "babesia_bigemina_lk391709_2_tb" = "B. bigemina lk391709 1",
  "anaplasma_bovis_ab983439_ae"  = "A. bovis ab983439",                                 
  "anaplasma_platys_like_ku585990_ae"= "A. platys like",                               
  "anaplasma_phagocytophilum_u02521_ae" = "A. phagocytophilum",                              
  "candidatus_anaplasma_boleense_ku586025_ae" = "candidatus A. boleense",                             
  "uncultured_anaplasma_sp_clone_saso_ky924885_ae" = "uncultured A. sp clone",                            
  "uncultured_anaplasma_sp_jn862825_ae" = "uncultured A. sp",                           
  "ehrlichia_sp_tibet_ehrlichia_canis_ehrlichia_minasensis_af414399_ay394465_mt163430_ae" = "E. sp tibet/canis/minasensis",
  "anaplasma_platys_ef139459_ae" = "A. platys",               
  "babesia_bigemina_ay603402_tb" = "B. bigemina ay603402",              
  "babesia_bigemina_lk391709_tb"  = "B. bigemina lk391709",             
  "babesia_bigemina_ku206291_tb"   = "B. bigemina ku206291",
  "theileria_sp_strain_msd_af078816_tb"  = "T. sp strain msd",         
  "theileria_taurotragi_l19082_tb" = "T. taurotragi ",        
  "babesia_bovis_kf928959_tb"  = "B. bovis kf928959",      
  "babesia_bovis_aaxt01000002_tb"  = "B. bovis aaxt01000002",     
  "babesia_bovis_ay603398_tb"   = "B. bovis ay603398",    
  "babesia_bovis_jq437260_tb" = "B. bovis jq437260")

prop_infected_long <- binary_pathogen_data %>%
  group_by(sample_week, calf_id) %>%
  summarise(across(all_of(pathogen_cols), ~ as.integer(any(. > 0))), .groups = "drop") %>%
  pivot_longer(cols = all_of(pathogen_cols), names_to = "pathogen", values_to = "infected") %>%
  
  # Add n_calves per week inline
  left_join(
    binary_pathogen_data %>%
      group_by(sample_week) %>%
      summarise(n_calves = n_distinct(calf_id), .groups = "drop"),
    by = "sample_week"
  ) %>%
  
  group_by(sample_week, pathogen) %>%
  summarise(n_infected = sum(infected), n_calves = first(n_calves), .groups = "drop") %>%
  mutate(
    prop = n_infected / n_calves,
    sample_week = as.numeric(as.character(sample_week)),
    pathogen = recode(pathogen, !!!pathogen_rename)
  ) %>%
  group_by(pathogen) %>%
  filter(!sample_week %in% c(2, 3)) %>%
  filter(max(prop) >= 0.1) %>%  # remove strains that never exceed 10% prevalence
  ungroup()

# proportional stacked area chart (each layer is the share of total infections that week)
ggplot(prop_infected_long, aes(x = sample_week, y = prop, fill = pathogen)) +
  geom_area(position = "fill", alpha = 0.9, color = "white", size = 0.2) +
  scale_x_continuous(breaks = seq(1, 51, by = 5), limits = c(1, 51)) +
  scale_fill_viridis_d(option = "D") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Relative Proportions of Pathogen Strains Over Time",
    x = "Weeks of Life",
    y = "Proportion of Pathogens (stacked to 100%)",
    fill = "Pathogen"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

########## Appendix A Figure 3 ########## 

# Reshape to long format
df_long <- final_miseq_data_clean %>%
  pivot_longer(
    cols = c(
      pcr_primers_ae, pcr_primers_tb,
      total_sequencing_reads_ae, total_sequencing_reads_tb,
      reads_with_pcr_primers_ae, reads_with_pcr_primers_tb,
      filtered_reads_ae, filtered_reads_tb
    ),
    names_to = c(".value", "PrimerType"),
    names_pattern = "(.*)_(ae|tb)"
  ) %>%
  mutate(
    PrimerType = case_when(
      PrimerType == "ae" ~ "Anaplasma/Ehrlichia",
      PrimerType == "tb" ~ "Theileria/Babesia",
      TRUE ~ PrimerType
    )
  )

# Plot 1: Total Sequencing Reads
p1 <- ggplot(df_long, aes(x = PrimerType, y = total_sequencing_reads, fill = PrimerType)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "white") +
  scale_y_log10() +
  scale_fill_viridis_d(option = "D") +
  labs(
    title = "(A) Log-Scaled Total Sequencing Reads",
    x = "Primer Set",
    y = "Log10(Total Reads)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Plot 2: Reads with PCR primers
p2 <- ggplot(df_long, aes(x = PrimerType, y = reads_with_pcr_primers, fill = PrimerType)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "white") +
  scale_y_log10() +
  scale_fill_viridis_d(option = "D") +
  labs(
    title = "(B) Reads with PCR Primers",
    x = "Primer Set",
    y = "Log10(Reads with Primers)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Plot 3: Filtered Reads
p3 <- ggplot(df_long, aes(x = PrimerType, y = filtered_reads, fill = PrimerType)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "white") +
  scale_y_log10() +
  scale_fill_viridis_d(option = "D") +
  labs(
    title = "(C) Filtered Reads",
    x = "Primer Set",
    y = "Log10(Filtered Reads)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# Combine plots: p1 and p2 side-by-side, p3 underneath
final_plot <- (p1 | p2) / p3
final_plot

########## Appendix B Table 1 ########## 

binary_pathogen_data_per_calf <- binary_pathogen_data %>%
  group_by(calf_id) %>%
  
  summarise(across(all_of(pathogen_cols), ~ as.integer(any(. > 0))))

# Now add in aetiological cause
cause <- final_miseq_data_clean %>%
  select(calf_id, definitive_aetiological_cause) %>%
  distinct()

binary_pathogen_data_per_calf_annotated <- binary_pathogen_data_per_calf %>%
  left_join(cause, by = "calf_id")

# Count the number of pathogens each calf is infected with
infection_counts <- binary_pathogen_data_per_calf %>%
  mutate(n_infections = rowSums(across(all_of(pathogen_cols)))) %>%
  mutate(infection_category = case_when(
    n_infections == 0 ~ "No infections",
    n_infections == 1 ~ "Single infection",
    n_infections == 2 ~ "Dual infections",
    n_infections >= 3 ~ "Triple or more infections"
  ))

# Summarise into your required table
infection_summary <- infection_counts %>%
  group_by(infection_category) %>%
  summarise(
    n_calves = n(),
    percent_of_total = round((n() / nrow(.)) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(factor(infection_category, levels = c("No infections", "Single infection", "Dual infections", "Triple or more infections")))

########## Appendix B Figure 1 ########## 

binary_pathogen_data_per_calf <- binary_pathogen_data_per_calf %>%
  rowwise() %>%
  mutate(n_unique_pathogens = sum(c_across(all_of(pathogen_cols)))) %>%
  ungroup()

binary_pathogen_data_per_calf <- binary_pathogen_data_per_calf %>%
  left_join(select(final_miseq_data_clean, calf_id, definitive_aetiological_cause) %>% distinct(), by = "calf_id")

# Sum read counts across all visits per calf
calf_total_load <- final_miseq_data_clean %>%
  group_by(calf_id) %>%
  summarise(
    total_pathogen_load = sum(across(all_of(pathogen_cols)), na.rm = TRUE),
    .groups = "drop"
  )

binary_pathogen_data_per_calf <- binary_pathogen_data_per_calf %>%
  left_join(calf_total_load, by = "calf_id")


ggplot(binary_pathogen_data_per_calf, aes(x = n_unique_pathogens, y = total_pathogen_load)) +
  geom_boxplot()

ggplot(binary_pathogen_data_per_calf, aes(x = as.factor(n_unique_pathogens), y = total_pathogen_load, fill = as.factor(n_unique_pathogens))) +
  geom_violin(trim = TRUE, alpha = 0.7, color = NA) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "white")+
  scale_fill_viridis_d(option = "D", name = "No. of Pathogens") +
  scale_y_continuous(
    trans = "log10",
    labels = scales::scientific,  # <--- Use scientific notation
    breaks = c(1e3, 1e4, 1e5, 1e6, 1e7, 1e8)
  ) +
  labs(
    title = "Total Pathogen Load by Number of Unique Pathogens",
    x = "Number of Unique Pathogens",
    y = "Total Read Count (log10 scale)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 10)
  )

cor.test(binary_pathogen_data_per_calf$total_pathogen_load, binary_pathogen_data_per_calf$n_unique_pathogens, method = "spearman")

#CI
spearman_rho <- function(data, indices) {
  d <- data[indices, ]
  cor(d$total_pathogen_load, d$n_unique_pathogens, method = "spearman")
}

# Prepare data
df <- binary_pathogen_data_per_calf[, c("total_pathogen_load", "n_unique_pathogens")]

# Bootstrap with 2000 resamples
set.seed(123)
boot_res <- boot(df, spearman_rho, R = 2000)

# Get 95% CI
boot.ci(boot_res, type = "perc")

########## Appendix B Table 2 ########## 

# Create a label for each unique infection combination per calf
co_infection_summary <- binary_pathogen_data_per_calf_annotated %>%
  mutate(combo = apply(select(., all_of(pathogen_cols)), 1, function(x) {
    if (sum(x) == 0) {
      return("None")
    } else {
      return(paste(sort(names(x)[x == 1]), collapse = " + "))
    }
  })) %>%
  group_by(combo) %>%
  summarise(
    n_calves = n(),
    deaths = sum(definitive_aetiological_cause == "Dead"),
    survival = sum(definitive_aetiological_cause == "Alive"),
    mortality_rate = round(deaths / n_calves * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(percent_of_total = round(n_calves / sum(n_calves) * 100, 1))

# Arrange in table
co_infection_summary %>%
  arrange(desc(n_calves))  

# Store the total number of calves before grouping
total_calves <- sum(co_infection_summary$n_calves)

# Now collapse and calculate % using original total
co_infection_summary_collapsed <- co_infection_summary %>%
  mutate(combo_grouped = ifelse(n_calves < 6, "Other (rare)", combo)) %>%
  group_by(combo_grouped) %>%
  summarise(
    n_calves = sum(n_calves),
    deaths = sum(deaths),
    mortality_rate = round(deaths / n_calves * 100, 1),
    percent_of_total = round(n_calves / total_calves * 100, 1),  # use full total here!
    .groups = "drop"
  ) %>%
  arrange(desc(n_calves))

# Create a combination label for each calf
co_infection_table <- binary_pathogen_data_per_calf_annotated %>%
  mutate(
    combo = apply(select(., all_of(pathogen_cols)), 1, function(x) {
      if (sum(x) == 0) {
        return("None")
      } else {
        return(paste(sort(names(x)[x == 1]), collapse = " + "))
      }
    })
  )

# Summarise how many calves have each pattern and their outcomes
co_infection_summary <- co_infection_table %>%
  group_by(combo) %>%
  summarise(
    n_calves = n(),
    deaths = sum(definitive_aetiological_cause == "Dead"),
    survival = sum(definitive_aetiological_cause == "Alive"),
    mortality_rate = round(deaths / n_calves * 100, 1),
    percent_of_total = round(n_calves / sum(n_calves) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(n_calves))

########## Appendix B Figure 2 & 3 ########## 


upset(
  binary_pathogen_data,
  intersect = theileria_cols,
  min_size = 5,
  width_ratio = 0.4,
  set_sizes = upset_set_size(),
  base_annotations = list(
    'Intersection size' = intersection_size(
      mapping = aes(fill = definitive_aetiological_cause.x)
    ) + 
      scale_fill_viridis_d(name = "Survival outcome", option = "D")
  )
)

# Correct mapping: new_name = old_name
column_rename <- c(
  "T. taurotragi"   = "theileria_taurotragi_l19082_tb"
)

binary_pathogen_data <- binary_pathogen_data %>%
  rename(!!!column_rename) %>%
  left_join(cause, by = "calf_id")

theileria_cols <- names(column_rename)

theileria_cols<- c("T. mutans" ,                                                                           
                   "T. sp strain msd",                                                                     
                   "T. parva"  ,                                                                           
                   "T. taurotragi",                                                       
                   "T. velifera" )
# By sample week
upset(
  binary_pathogen_data,
  intersect = theileria_cols,
  min_size = 50,  
  width_ratio = 0.3,
  base_annotations = list(
    'Intersection size' = intersection_size(
      mapping = aes(fill = SampleWeek)
    )
  )
) +
  scale_fill_viridis_d(option = "D", name = "Sample Week") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),   # Remove bottom axis text
    axis.ticks.x = element_blank(),  # Remove tick marks
    plot.margin = margin(10, 10, 10, 10)  # Optional: add space
  )












# By survival outcome
binary_pathogen_data_per_calf <- binary_pathogen_data %>%
  group_by(calf_id) %>%
  summarise(across(all_of(theileria_cols), ~ as.integer(any(. > 0))))

# Now add in aetiological cause
cause <- binary_pathogen_data %>%
  select(calf_id, definitive_aetiological_cause.x) %>%
  distinct()

binary_pathogen_data_per_calf_annotated <- binary_pathogen_data_per_calf %>%
  left_join(cause, by = "calf_id")

upset(
  binary_pathogen_data_per_calf_annotated,
  intersect = theileria_cols,
  min_size = 5,
  width_ratio = 0.4,
  set_sizes = upset_set_size(),
  base_annotations = list(
    'Intersection size' = intersection_size(
      mapping = aes(fill = definitive_aetiological_cause.x)
    ) + 
      scale_fill_viridis_d(name = "Survival outcome", option = "D")
  )
)

column_re <- c(
  "A. platys ef139459"   = "anaplasma_platys_ef139459_ae",
  "A. bovis u03775"   = "anaplasma_bovis_u03775_ae",
  "cadidatus A. boleense"   = "candidatus_anaplasma_boleense_ku586025_ae"
)


binary_pathogen_data <- binary_pathogen_data %>%
  rename(!!!column_re) %>%
  left_join(cause, by = "calf_id")

anaplasma_cols <- names(column_re)

anaplasma_cols<- c("A. platys like" ,                                                                      
                   "A. bovis ab983439", 
                   "uncultured A. sp clone",
                   "A. platys ef139459",
                   "A. bovis u03775",
                   "cadidatus A. boleense" 
)
# By sample week
upset(
  binary_pathogen_data,
  intersect = anaplasma_cols,
  min_size = 50,  
  width_ratio = 0.3,
  base_annotations = list(
    'Intersection size' = intersection_size(
      mapping = aes(fill = SampleWeek)
    )
  )
) +
  scale_fill_viridis_d(option = "D", name = "Sample Week") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),   # Remove bottom axis text
    axis.ticks.x = element_blank(),  # Remove tick marks
    plot.margin = margin(10, 10, 10, 10)  # Optional: add space
  )

# By survival outcome
binary_pathogen_data_per_calf <- binary_pathogen_data %>%
  group_by(calf_id) %>%
  summarise(across(all_of(anaplasma_cols), ~ as.integer(any(. > 0))))

# Now add in aetiological cause
cause <- binary_pathogen_data %>%
  select(calf_id, definitive_aetiological_cause.x.x) %>%
  distinct()

binary_pathogen_data_per_calf_annotated <- binary_pathogen_data_per_calf %>%
  left_join(cause, by = "calf_id")

upset(
  binary_pathogen_data_per_calf_annotated,
  intersect = anaplasma_cols,
  min_size = 5,
  width_ratio = 0.4,
  set_sizes = upset_set_size(),
  base_annotations = list(
    'Intersection size' = intersection_size(
      mapping = aes(fill = definitive_aetiological_cause.x.x)
    ) + 
      scale_fill_viridis_d(name = "Survival outcome", option = "D")
  )
)

########## Appendix B Table 3 ########## 

binary_pathogen_data_per_calf <- binary_pathogen_data %>%
  group_by(calf_id) %>%
  
  summarise(across(all_of(pathogen_cols), ~ as.integer(any(. > 0))))

# Now add in aetiological cause
cause <- final_miseq_data_clean %>%
  select(calf_id, definitive_aetiological_cause) %>%
  distinct()

binary_pathogen_data_per_calf_annotated <- binary_pathogen_data_per_calf %>%
  left_join(cause, by = "calf_id")

# Create simplified classification
tparva_status <- binary_pathogen_data_per_calf_annotated %>%
  mutate(
    tparva = theileria_parva_l02366_tb,
    tmutt = theileria_mutans_af078815_tb,
    co_infection = rowSums(across(all_of(pathogen_cols))) > 1,
    group = case_when(
      tparva == 1 & co_infection == FALSE ~ "T. parva only",
      tparva == 1 & co_infection == TRUE ~ "T. parva + co-infection",
      tparva == 0 ~ "No T. parva"
    )
  ) %>%
  group_by(group) %>%
  summarise(
    n_calves = n(),
    deaths = sum(definitive_aetiological_cause == "Dead"),
    mortality_rate = round(deaths / n_calves * 100, 1),
    .groups = "drop"
  )

# Create contingency table
table_tparva <- binary_pathogen_data_per_calf_annotated %>%
  mutate(
    tparva = theileria_parva_l02366_tb,
    co_infection = rowSums(across(all_of(pathogen_cols))) > 1,
    group = case_when(
      tparva == 1 & co_infection == FALSE ~ "T. parva only",
      tparva == 1 & co_infection == TRUE ~ "T. parva + co-infection",
      tparva == 0 ~ "No T. parva"
    ),
    died = ifelse(definitive_aetiological_cause == "Dead", 1, 0)
  ) %>%
  count(group, died) %>%
  pivot_wider(names_from = died, values_from = n, values_fill = 0) %>%
  column_to_rownames("group")  # Needed for test

# Apply Fisher's Exact Test
fisher.test(as.matrix(table_tparva))
groups <- rownames(table_tparva)

pairwise_results <- lapply(combn(groups, 2, simplify = FALSE), function(g) {
  sub_tab <- table_tparva[g, ]
  test <- fisher.test(as.matrix(sub_tab))
  list(
    comparison = paste(g, collapse = " vs "),
    p.value = test$p.value,
    odds.ratio = if (!is.null(test$estimate)) unname(test$estimate) else NA,
    conf.int = if (!is.null(test$conf.int)) test$conf.int else c(NA, NA)
  )
})

pairwise_results

########### Appendix B Table 4 ########## 

# Summary table
table <- tbl_cross(data = windowed_data, row = infection_order, col = event, percent = "row")
table

########## Appendix B Figure 4 ########## 

# Identify earliest infection for each pathogens
earliest_haemo <- final_miseq_data_clean %>%
  group_by(calf_id) %>%
  summarize(
    earliest_mutans = suppressWarnings(min(sample_week[theileria_mutans_af078815_tb > 0], na.rm = TRUE)),
    earliest_velifera = suppressWarnings(min(sample_week[theileria_velifera_af097993_tb > 0], na.rm = TRUE)),
    earliest_parva = suppressWarnings(min(sample_week[theileria_parva_l02366_tb > 0], na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(across(starts_with("earliest_"), ~ ifelse(is.infinite(.), 51, .)))  # Convert Inf to NA

earliest_haemo <- earliest_haemo %>%
  pivot_longer(cols = 2:4, names_to = "pathogen_strain", values_to = "initial_infection_week")

# Assign  event status (1 = infected with pathogen, 0 = not)
earliest_haemo$event <- ifelse(earliest_haemo$initial_infection_week == "51", 0, 1)

# Create the survival object
surv_obj <- Surv(time = earliest_haemo$initial_infection_week, event = earliest_haemo$event)

# Relabel strain names
earliest_haemo <- earliest_haemo %>%
  mutate(pathogen_strain = recode(pathogen_strain,
                                  "earliest_parva" = "T. parva",
                                  "earliest_mutans" = "T. mutans",
                                  "earliest_velifera" = "T. velifera"
  ))

# Fit Kaplan-Meier curves grouped by parasite
fit <- survfit(surv_obj ~ pathogen_strain, data = earliest_haemo)

viridis_palette <- viridis(3, option = "D")

# Plot the survival curves
ggsurvplot(
  fit,
  data = earliest_haemo,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  pval.method = TRUE,
  pval.size = 5,
  pval.coord = c(12, 0.30),
  censor = TRUE,
  palette = viridis_palette,
  ggtheme = theme_minimal(base_size = 14),
  xlab = "Time to First Infection (weeks)",
  ylab = "Proportion Not Yet Infected",
  legend.title = "Parasite Group",
  legend.labs = c("T. mutans", "T. parva", "T. velifera")  # remove variable name prefix
)

########## Appendix B Figure 5 ########## 


# Filter to only calves that died
died_data <- windowed_data_detailed %>%
  filter(event == 1)

# Check how many in each group
table(died_data$infection_order)

# Compare time to event (death) between groups
wilcox.test(time_to_event ~ infection_order, data = died_data)

ggplot(died_data, aes(x = infection_order, y = time_to_event, fill = infection_order)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "black") +
  labs(
    title = "Time to Death by Infection Order (Only Calves that Died)",
    x = "Infection Order",
    y = "Time to Death (weeks)"
  ) +
  scale_fill_viridis_d(option = "D") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

########## Appendix C Table 1 ##########

# Create a contingency table for genotype and survival status (died)
contingency_table <- table(Genomics_clean$genotype, Genomics_clean$died)

table <- tbl_cross(data = Genomics_clean, row = genotype, col = died, percent = "row")
table

# Perform Fisher's Exact Test
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

########## Appendix C Table 2 ########## 

# Identify if each calf was ever infected with T. parva
tparva_infection <- final_miseq_data_clean %>%
  group_by(calf_id) %>%
  summarise(tparva_positive = any(theileria_parva_l02366_tb > 0, na.rm = TRUE))

# Merge with genotype
tparva_infection <- tparva_infection %>%
  left_join(Genomics_clean %>% select(calf_id, genotype), by = "calf_id")

infection_rate_tbl <- tparva_infection %>%
  filter(genotype %in% c("TT", "CT", "CC")) %>%
  group_by(genotype) %>%
  summarise(
    n_calves = n(),
    n_infected = sum(tparva_positive, na.rm = TRUE),
    perc_infected = round(n_infected / n_calves * 100, 1)
  )

print(infection_rate_tbl)

########## Appendix C Figure 1 ########## 

# Sum total T. parva reads per calf
tparva_load <- final_miseq_data_clean %>%
  group_by(calf_id) %>%
  summarise(tparva_load = sum(theileria_parva_l02366_tb, na.rm = TRUE)) %>%
  left_join(Genomics_clean %>% select(calf_id, genotype), by = "calf_id") %>%
  filter(genotype %in% c("TT", "CT", "CC"))

kruskal.test(tparva_load ~ genotype, data = tparva_load)
tt_cc_only <- tparva_load %>% filter(genotype %in% c("TT", "CC"))
wilcox.test(tparva_load ~ genotype, data = tt_cc_only)

ggplot(tparva_load, aes(x = genotype, y = tparva_load, fill = genotype)) +
  geom_violin(trim = FALSE, alpha = 0.8) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "white") +
  scale_y_log10(
    labels = label_number(scale_cut = cut_short_scale()),
    breaks = trans_breaks("log10", function(x) 10^x)
  ) +
  scale_fill_viridis_d(option = "D") +
  labs(
    title = "T. parva Pathogen Load by Genotype",
    x = "Genotype",
    y = "Total T. parva Read Count (log scale)"
  ) +
  theme_minimal(base_size = 14)



