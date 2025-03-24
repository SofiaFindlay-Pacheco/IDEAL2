Genomics <- "C:/Users/sofia/OneDrive - University of Edinburgh/master/R studio/IDEAL statistics/Edited original data/Genomics.xlsx"
Genomics <- read_excel(Genomics)
Genomics_clean <- Genomics %>% clean_names()

# Load necessary library
library(nnet)
library(janitor)
install.packages("summarytools")
library(summarytools)
install.packages("gtsummary")
library(gtsummary)

################## Logistic regression #################

# Step 1: Create a contingency table for genotype and survival status (died)
contingency_table <- table(Genomics_clean$genotype, Genomics_clean$died)

table <- tbl_cross(data = Genomics_clean, row = genotype, col = died, percent = "row")
table

# Perform Fisher's Exact Test
fisher_test <- fisher.test(contingency_table)

# Print the results
print(fisher_test)

mosaicplot(table(Genomics_clean$genotype, Genomics_clean$died), main = "Effect of Genotype on Survival status")
