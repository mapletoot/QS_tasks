# An R file to document the importing, wrangling and visualisation of National
# Progression Award Qualifications changes from 2019 - 2025.

# ==============================================================================
# libraries
library(tidyr)
library(dplyr)
library(ggplot2)

# ==============================================================================
# Read in the overview files to get a flavour of the data
totals <- read.csv("NPA_totals.csv", header=TRUE)
for (i in 3:ncol(totals)){
  totals[,i] <- as.numeric(gsub("\\s+", "", totals[,i]))
}

totals
