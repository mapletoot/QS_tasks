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
    totals[,i] <- gsub("\\[c\\]", "5", totals[,i])
    totals[,i] <- gsub("\\[z\\]", "0", totals[,i])
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
  geom_line() +
  geom_point()


# ==============================================================================
# Reading in Males and females to compare
males_summary <- format_summary("NPA_male.csv")
males_summary
females_summary <- format_summary("NPA_female.csv")
females_summary

males_summary <- males_summary %>%
  mutate(Sex = "Male")

females_summary <- females_summary %>%
  mutate(Sex = "Female")

mf_summary <- bind_rows(males_summary, females_summary)
mf_summary

mf_plot <- mf_summary %>%
  pivot_longer(
    cols = -c(Level, Sex),
    names_to = "Year",
    values_to = "Count"
  ) %>%
  mutate(
    Year = as.numeric(Year)
  )

mf_plot

mf_plot_totals <- mf_plot %>%
  filter(Level == "Total")
mf_plot_totals
# ==============================================================================
# Create the plot

ggplot(data = mf_plot_totals, aes(Year, Count, colour = Sex, group = Sex)) +
  geom_line() +
  geom_point()

# ==============================================================================
# Separating by institution

# read in data
ea_totals <- format_summary("NPA_education_authority.csv")
ea_totals <- ea_totals %>%
  mutate(Institution = "Education Authority")
independent_totals <- format_summary("NPA_independent.csv")
independent_totals <- independent_totals %>%
  mutate(Institution = "Independent")
fe_totals <- format_summary("NPA_FE.csv")
fe_totals <- fe_totals %>%
  mutate(Institution = "FE")
other_totals <- format_summary("NPA_other.csv")
other_totals <- other_totals %>%
  mutate(Institution = "Other")

# filter out totals only
institution_totals <- bind_rows(
  ea_totals,
  independent_totals,
  fe_totals,
  other_totals
) %>%
  filter(Level == "Total") %>%
  pivot_longer(
    cols = `2025`:`2019`,
    names_to = "Year",
    values_to = "Count"
  ) %>%
  mutate(
    Year = as.numeric(Year)
  )

# add in factors
institution_totals$Institution <- factor(
  institution_totals$Institution,
  levels = c(
    "Independent",
    "Other",
    "FE",
    "Education Authority"
  )
)

# Normalised percentage plot
ggplot(institution_totals,
       aes(x = factor(Year),
           y = Count,
           fill = Institution)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Year",
    y = "Percentage of qualifications",
    fill = "Institution",
    title = "Share of NPA by institution type"
  )

# count plot
ggplot(institution_totals,
       aes(x = factor(Year),
           y = Count,
           fill = Institution)) +
  geom_col(position = "stack") +
  labs(
    x = "Year",
    y = "Percentage of qualifications",
    fill = "Institution",
    title = "Count of NPA by institution type"
  )
# ==============================================================================
# is the pattern the same across all qualification levels?
# ===================================
# SCQF4

scqf4_totals <- bind_rows(
  ea_totals,
  independent_totals,
  fe_totals,
  other_totals
) %>%
  filter(Level == "SCQF4") %>%
  pivot_longer(
    cols = `2025`:`2019`,
    names_to = "Year",
    values_to = "Count"
  ) %>%
  mutate(
    Year = as.numeric(Year)
  )

# add in factors
scqf4_totals$Institution <- factor(
  institution_totals$Institution,
  levels = c(
    "Independent",
    "Other",
    "FE",
    "Education Authority"
  )
)

# Normalised percentage plot
ggplot(scqf4_totals,
       aes(x = factor(Year),
           y = Count,
           fill = Institution)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Year",
    y = "Percentage of qualifications",
    fill = "Institution",
    title = "Share of SCQF4 by institution type"
  )

# ===================================
# SCQF5

scqf5_totals <- bind_rows(
  ea_totals,
  independent_totals,
  fe_totals,
  other_totals
) %>%
  filter(Level == "SCQF5") %>%
  pivot_longer(
    cols = `2025`:`2019`,
    names_to = "Year",
    values_to = "Count"
  ) %>%
  mutate(
    Year = as.numeric(Year)
  )

# add in factors
scqf5_totals$Institution <- factor(
  institution_totals$Institution,
  levels = c(
    "Independent",
    "Other",
    "FE",
    "Education Authority"
  )
)

# Normalised percentage plot
ggplot(scqf5_totals,
       aes(x = factor(Year),
           y = Count,
           fill = Institution)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Year",
    y = "Percentage of qualifications",
    fill = "Institution",
    title = "Share of SCQF5 by institution type"
  )

# ===================================
# SCQF6

scqf6_totals <- bind_rows(
  ea_totals,
  independent_totals,
  fe_totals,
  other_totals
) %>%
  filter(Level == "SCQF6") %>%
  pivot_longer(
    cols = `2025`:`2019`,
    names_to = "Year",
    values_to = "Count"
  ) %>%
  mutate(
    Year = as.numeric(Year)
  )

# add in factors
scqf6_totals$Institution <- factor(
  institution_totals$Institution,
  levels = c(
    "Independent",
    "Other",
    "FE",
    "Education Authority"
  )
)

# Normalised percentage plot
ggplot(scqf6_totals,
       aes(x = factor(Year),
           y = Count,
           fill = Institution)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Year",
    y = "Percentage of qualifications",
    fill = "Institution",
    title = "Share of SCQF6 by institution type"
  )