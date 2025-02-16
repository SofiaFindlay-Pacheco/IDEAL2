library(ggplot2)
library(janitor)


# Rows 1-12
timeline_serology <- Brief_serology_clean[1:10, ]
#timeline_serology <- Brief_serology[13:22, ]
#timeline_serology <- Brief_serology[23:32, ]
#timeline_serology <- Brief_serology[33:42, ]

# turn numeric
timeline_serology$serology_t_parva <- as.numeric(timeline_serology$serology_t_parva)
timeline_serology$serology_t_mutans <- as.numeric(timeline_serology$serology_t_mutans)
timeline_serology$serology_b_bigemina <- as.numeric(timeline_serology$serology_b_bigemina)
timeline_serology$serology_a_marginale <- as.numeric(timeline_serology$serology_a_marginale)
#timeline_serology$`Serology BTV` <- as.numeric(timeline_serology$`Serology BTV`)
#timeline_serology$`Serology EHDV` <- as.numeric(timeline_serology$`Serology EHDV`)
#timeline_serology$`Serology BRSV INDEXX (Sample to positive control percentage)` <- 
 # as.numeric(timeline_serology$`Serology BRSV INDEXX (Sample to positive control percentage)`)

# Plot the timeline for wide data
ggplot(timeline_serology, aes(x = sample_week)) +
  geom_line(aes(y = serology_t_parva, color = "T. parva", group = 1), na.rm = TRUE) +
  geom_line(aes(y = serology_t_mutans, color = "T. mutans", group = 1), na.rm = TRUE) +
  geom_line(aes(y = serology_b_bigemina, color = "B. bigemina", group = 1), na.rm = TRUE) +
  geom_line(aes(y = serology_a_marginale, color = "A. marginale", group = 1), na.rm = TRUE) +
  #geom_line(aes(y = `Serology BTV`, color = "BTV"), na.rm = TRUE) +
  #geom_line(aes(y = `Serology EHDV`, color = "EHDV"), na.rm = TRUE) +
  #geom_line(aes(y = `Serology BRSV INDEXX (Sample to positive control percentage)`, color = "BRSV INDEXX"), na.rm = TRUE) +
  
  labs(
    title = "Bacterial Amounts Over Time",
    x = "Date",
    y = "Bacterial Amount",
    color = "Bacteria"
  ) +
  theme_minimal()


