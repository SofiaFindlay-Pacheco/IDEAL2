# Load ggplot2
library(ggplot2)

# Boxplot for Total Sequencing Reads
ggplot(combined_data, aes(x = pcr_primers, y = total_sequencing_reads)) +
  geom_boxplot(fill = c("#1f77b4", "#ff7f0e")) +
  scale_y_log10() +
  labs(
    title = "Log-Scaled Total Sequencing Reads by Primer Set",
    x = "Primer",
    y = "Log10(Total Reads)"
  ) +
  theme_minimal()

# Boxplot for Reads with PCR primers
ggplot(combined_data, aes(x = pcr_primers, y = reads_with_pcr_primers)) +
  geom_boxplot(fill = c("#2ca02c", "#d62728")) +
  scale_y_log10() +
  labs(title = "Reads with PCR Primers by Primer Set", x = "Primer", y = "Reads with Primers") +
  theme_minimal()

# Boxplot for Filtered Reads
ggplot(combined_data, aes(x = pcr_primers, y = filtered_reads)) +
  geom_boxplot(fill = c("#9467bd", "#8c564b")) +
  scale_y_log10() +
  labs(title = "Filtered Reads by Primer Set", x = "Primer", y = "Filtered Reads") +
  theme_minimal()
