# An R file to document the importing, wrangling and visualisation of National
# Progression Award Qualifications changes from 2019 - 2025.

# ==============================================================================
# libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(sgplot)

# ==============================================================================
# Reading in data
format_summary <- function(file){
  #' Funcion that takes a csv file copied from QS website with commas removed
  #' and formats it appropriately so that it is ready to be converted into a
  #' tibble to be mutated and plotted
  #' 
  #' @file is a .csv file with a set of headers, commas removed so that the
  #' columns can be coerced to numeric easily
  totals <- read.csv(file, header=TRUE)
  
  # Simplify the column names
  colnames(totals)[3:ncol(totals)] <- c(2025, 2024, 2023, 2022, 2021, 2020, 2019)
  
  # Chnage the data to numeric
  for (i in 3:ncol(totals)){
    totals[,i] <- as.numeric(gsub("\\s+", "", totals[,i]))
  }
  
  # remove unnecessary column
  totals <- totals[,-2]
  
  # Add in a row with the totals for the plot
  totals <- rbind(totals, rep(0, ncol(totals)))
  totals[nrow(totals),1] <- "Total"
  totals[nrow(totals),2:ncol(totals)] <- apply(totals[,-1], 2, "sum")
  totals
}

# ==============================================================================
# Changing type to tibble for plotting
totals <- format_summary("NPA_totals.csv")
totals <- as_tibble(totals)
totals

totals_plot <- totals %>%
  pivot_longer(
    cols = -Level,
    names_to = "Year",
    values_to = "Count"
  ) %>%
  mutate(
    Year = as.numeric(Year)
  )

totals_plot_4plus <- totals_plot %>%
  filter(!(Level %in% c("Total", "SCQF2", "SCQF3")))

# ==============================================================================
# Create the plot
theme_set(ggplot2::theme_grey())
ggplot(data = totals_plot_4plus, aes(Year, Count, colour = Level, group = Level)) +
  geom_line()
  +
  geom_point()

