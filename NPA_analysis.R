# An R file to document the importing, wrangling and visualisation of National
# Progression Award Qualifications changes from 2019 - 2025.

# ==============================================================================
# libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(sgplot)
library(RColorBrewer)
library(wordcloud2)
library(ggrepel)

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

# colour scheme
qualification_colours = c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854')
# plot
ggplot(data = totals_plot_4plus, aes(Year, Count, colour = Level, group = Level)) +
  geom_line(size=1.3) +
  geom_point(shape = 16, size = 3) +
  scale_colour_manual(values = qualification_colours) +
  scale_y_continuous(limits = c(0,NA))
# display.brewer.all(colorblindFriendly = TRUE)

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
  geom_line(size=1.3) +
  geom_point(shape = 16, size = 3) +
  scale_colour_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(0,NA))

# display.brewer.all(colorblindFriendly = TRUE)

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
# colour scheme:
institution_colours <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c')
# Normalised percentage plot
ggplot(institution_totals,
       aes(x = factor(Year),
           y = Count,
           fill = Institution)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = institution_colours) +
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
  geom_col(position = "dodge") +
  scale_fill_manual(values = institution_colours) +
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
  scale_fill_manual(values = institution_colours) +
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
  scale_fill_manual(values = institution_colours) +
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
  scale_fill_manual(values = institution_colours) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Year",
    y = "Percentage of qualifications",
    fill = "Institution",
    title = "Share of SCQF6 by institution type"
  )

# ==============================================================================
# Looking at types of qualifications:

# ====================================
# Wordclouds

create_wordcloud <- function(file, filter = NULL, threshold = NULL, top = 50, year = "Awarded.Count.2025"){
  #' A function that takes an NPA file from QS, cleans the punctuation and unhelpful
  #' words aggregates the words that appear across different courses (eg sport coaching
  #' and sport leadership woudl both contribute to the word sport) to return a wordcloud 
  #' using the wordcloud2 library.
  #' 
  #' @file is an excel sheet from the Qualifications Scotland website turned into
  #' a csv.
  #' @filter allows values of "SCQF4", "SCQF5", "SCQF6" to filter the wordcloud by
  #' only that level of qualification.
  #' @threshold is the minimum frequency of a word for it to appear in the wordcloud.
  #' Should not be used in conjunction with @top.
  #' @top will put the top x words into the cloud. 50 is a good choice. Should not be
  #' used in conjunction with @threshold. 
  #' @year is the year used for the frequencies. Must match column name exactly.
  #' 
  #' @return a dataframe ready to pass to wordcloud2 function

  
  # Read in csv
  df <- read.csv(file, header=TRUE)
  
  # Only keep relevant columns and give appropriate names
  df <- df[,c(1,2, which(colnames(df) == year))]
  colnames(df) <- c("level","name", "freq")

  # Remove unhelpful words or punctuation and replace z and c with 0 and 5 respectively
  df$name <- gsub("\\(Level |\\)", "", df$name)
  df$name <- gsub("SCQF Level 5", "", df$name)
  df$name <- gsub("[:;()\\-]", "", df$name)
  df$freq <- gsub("\\[z\\]", "0", df$freq)
  df$freq <- gsub("\\[c\\]", "5", df$freq)
  df$freq <- gsub(",", "", df$freq)
  

  # appropriate column names and convert to numeric by removing space
  colnames(df) <- c("level","name", "freq")
  df$freq <- as.numeric(gsub("\\s+", "",df$freq))
  df <- df[df$freq > 0,]
  df
  
  # Filter out the relavent qualification level
  if (!is.null(filter)){
    df <- df[df$level == filter,]
  }
  
  # Only want to retain the word and frequency information
  df <- df[,c(2,3)]
  
  # split courses into separate words to be able to pick out sport, construction etc 
  # in different qualification names.
  course_words <- strsplit(
    tolower(df$name),
    split = " "
  )
  
  # Now have a list and I need to assign the frequencies to each word as per the 
  # original data by matching frequencies to length of course name list
  course_freqs <- rep(df$freq, lengths(course_words))
  
  singular_words <- data.frame(words = unlist(course_words),
                             freq = course_freqs)
  
  # remove blanks
  singular_words <- singular_words[singular_words$words != "",]
  
  # aggregate each word:
  wordcloud_df <- aggregate(freq ~ words,data = singular_words,FUN = sum)
  
  # Get rid of some unhelpful words
  wordcloud_df <- wordcloud_df[!(wordcloud_df$words %in% c("and", "in", 1, 2, "a", "-","an", "at", "for", "of", "the", "to", "with")),]
  
  # order it for purpose of filtering appropriately
  wordcloud_df <- wordcloud_df[order(wordcloud_df$freq, decreasing = TRUE), ]
  
  if (!is.null(threshold)){
    wordcloud_df <- wordcloud_df[wordcloud_df$freq >= threshold,]
  }
  
  if (!is.null(top)){
    wordcloud_df <- wordcloud_df[1:top,]
  }
  
  wordcloud_df
}

wordcloud_all <- create_wordcloud(file = "wordcloud_all.csv", top = 50)
wordcloud_4 <- create_wordcloud(file = "wordcloud_all.csv", top = 50, filter = "SCQF4")
wordcloud_5 <- create_wordcloud(file = "wordcloud_all.csv", top = 30, filter = "SCQF5")
wordcloud_6 <- create_wordcloud(file = "wordcloud_all.csv", top = 30, filter = "SCQF6")
wordcloud_male <- create_wordcloud(file = "wordcloud_male.csv", top = 30)
wordcloud_female <- create_wordcloud(file = "wordcloud_female.csv", top = 30)

wordcloud2(wordcloud_all)
wordcloud_all
wordcloud2(wordcloud_6)
wordcloud_male
wordcloud2(wordcloud_female)
wordcloud_female

comparison_sex <- merge(
  wordcloud_male,
  wordcloud_female,
  by = "words",
  all = TRUE,
  suffixes = c("_male", "_female")
)
comparison_sex[is.na(comparison_sex)] <- 0
x <- comparison_sex$freq_male
y <- comparison_sex$freq_female

cosine_similarity <- sum(x * y) /
  (sqrt(sum(x^2)) * sqrt(sum(y^2)))
cosine_similarity

comparison_level <- merge(
  wordcloud_5,
  wordcloud_6,
  by = "words",
  all = TRUE,
  suffixes = c("_5", "_6")
)
comparison_level
comparison_level[is.na(comparison_level)] <- 0
x <- comparison_level$freq_5
y <- comparison_level$freq_6

cosine_similarity <- sum(x * y) /
  (sqrt(sum(x^2)) * sqrt(sum(y^2)))
cosine_similarity

ggplot(comparison_sex,
       aes(x = freq_male,
           y = freq_female,
           label = words)) +
  geom_point() +
  geom_text_repel() +
  labs(
    x = "Male frequency",
    y = "Female frequency",
    title = "Qualification word frequencies by gender"
  )

ggplot(comparison_level,
       aes(x = freq_5,
           y = freq_6,
           label = words)) +
  geom_point() +
  geom_text_repel() +
  labs(
    x = "Level 5 frequency",
    y = "Level 6 frequency",
    title = "Qualification word frequencies by gender"
  )

# ====================================
# Qualification rankings
rankings_df = read.csv("wordcloud_all.csv")
for (i in 3:ncol(rankings_df)){
  rankings_df[,i] <- gsub("\\[z\\]", "0", rankings_df[,i])
  rankings_df[,i] <- gsub("\\[c\\]", "5", rankings_df[,i])
  rankings_df[,i] <- gsub(",", "", rankings_df[,i])
  rankings_df[,i] <- as.numeric(gsub("\\s+", "",rankings_df[,i]))
}


rankings_df
year_cols <- c("2025","2024","2023","2022","2021","2020","2019")
colnames(rankings_df)[3:ncol(rankings_df)] <- year_cols

low_uptake <- rankings_df[rowSums(rankings_df[year_cols]) <= 5,]
low_uptake

for (year in year_cols) {
  
  rankings_df[[paste0("Rank_", year)]] <-
    rank(-rankings_df[[year]],
         ties.method = "min")
}

top10_all <- data.frame(Rank = 1:10)

for (year in year_cols) {
  
  ordered_rows <- rankings_df[order(-rankings_df[[year]]), ]
  
  top10_all[[year]] <- paste0(
    ordered_rows$Subject[1:10],
    " (", ordered_rows[[year]][1:10], ")"
  )
}
top10_all

all_zero_subjects <- rankings_df[rowSums(rankings_df[year_cols]) == 0,]
all_zero_subjects


rankings_df$change <- rankings_df$Rank_2019 - rankings_df$Rank_2025
rankings_df <- rankings_df[order(rankings_df$change),]
rankings_df
