library(tidyr)
library(ggplot2)
library(janitor)

long_data2_clean$Average_Value <- as.numeric(long_data2_clean$Average_Value)
long_data2_clean <- long_data2_clean %>% filter(!is.na(Average_Value))

# Create the heatmap
ggplot(long_data2_clean, aes(x = sample_week, y = calf_id, fill = Average_Value)) +
  geom_tile() +   # Create tiles (cells)
  scale_fill_gradient(low = "white", high = "blue") +  # Color gradient (change colors as needed)
  labs(
    title = "Heatmap of Bacteria Over Time",
    x = "SampleWeek",
    y = "Sample ID",
    fill = "Serology Value"
  ) +
  theme_minimal() +  # Use minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


