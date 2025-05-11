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
"theileria_sp_strain_msd_af078816_tb", 
"theileria_velifera_af097993_tb" ,
"anaplasma_bovis_u03775_ae"    ,                                                        
"anaplasma_bovis_ab983439_ae",
"uncultured_anaplasma_sp_clone_saso_ky924885_ae"     ,
"anaplasma_platys_like_ku585990_ae"
)

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



