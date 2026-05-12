# An R file to document the importing, wrangling and visualisation of National
# Progression Award Qualifications changes from 2019 - 2025.

# ==============================================================================
# libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(htmlwidgets)
library(webshot2)
library(RColorBrewer)
library(wordcloud2)
library(ggrepel)
library(readxl)
library(sf)

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
plot_overall <- ggplot(data = totals_plot_4plus, aes(Year, Count, colour = Level, group = Level)) +
  geom_line(size=1.3) +
  geom_point(shape = 16, size = 3) +
  scale_colour_manual(values = qualification_colours) +
  # horizontal grid lines every 1000
  scale_y_continuous(
    expand = expansion(add = c(0, 1000)),
    limits = c(0, NA),
    minor_breaks = seq(0, 20000, 1000),
    breaks = seq(0, 20000, 5000)
  ) +
  
  scale_x_continuous(
    breaks = unique(totals_plot_4plus$Year)
  ) +
  
  labs(
    title = "NPA Counts Over Time by Level",
    x = "Year",
    y = "Count",
    colour = "Level"
  ) +
  
  theme_minimal() +
  
  theme(
    
    # larger fonts
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    
    # bring back axis lines
    axis.line = element_line(
      colour = "black",
      linewidth = 1
    ),
    
    # axis ticks
    axis.ticks = element_line(
      colour = "black",
      linewidth = 0.8
    ),
    
    # remove vertical grid lines
    #panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    # thick horizontal lines every 5000
    panel.grid.major.y = element_line(
      colour = "grey60",
      linewidth = 1
    ),
    
    # thinner horizontal lines every 1000
    panel.grid.minor.y = element_line(
      colour = "grey85",
      linewidth = 0.4
    )
  )
plot_overall
ggsave(
  "plot_overall.png",
  plot = plot_overall,
  width = 7.5,
  height = 7.5,
  dpi = 200
)

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

plot_gender <- ggplot(data = mf_plot_totals, aes(Year, Count, colour = Sex, group = Sex)) +
  geom_line(size=1.3) +
  geom_point(shape = 16, size = 3) +
  scale_colour_brewer(palette = "Paired") +
  # horizontal grid lines every 1000
  scale_y_continuous(
    expand = expansion(add = c(0, 1000)),
    limits = c(0, NA),
    minor_breaks = seq(0, 25000, 1000),
    breaks = seq(0, 25000, 5000)
  ) +
  
  scale_x_continuous(
    breaks = unique(totals_plot_4plus$Year)
  ) +
  
  labs(
    title = "NPA Counts Over Time by Sex",
    x = "Year",
    y = "Count",
    colour = "Level"
  ) +
  
  theme_minimal() +
  
  theme(
    
    # larger fonts
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    
    # bring back axis lines
    axis.line = element_line(
      colour = "black",
      linewidth = 1
    ),
    
    # axis ticks
    axis.ticks = element_line(
      colour = "black",
      linewidth = 0.8
    ),
    
    # remove vertical grid lines
    #panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    # thick horizontal lines every 5000
    panel.grid.major.y = element_line(
      colour = "grey60",
      linewidth = 1
    ),
    
    # thinner horizontal lines every 1000
    panel.grid.minor.y = element_line(
      colour = "grey85",
      linewidth = 0.4
    )
  )

ggsave(
  "plot_gender.png",
  plot = plot_gender,
  width = 7.5,
  height = 7.5,
  dpi = 200
)

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
plot_institution_pct <- ggplot(institution_totals,
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
    title = "Share of NPA by Institution Type"
  ) +
  
  scale_y_continuous(
    expand = expansion(add = c(0, 0)),
    limits = c(0, NA),
    minor_breaks = seq(0, 25000, 1000),
    breaks = seq(0, 25000, 5000)
  ) +
  
  geom_text(
    data = institution_totals %>%
      group_by(Year) %>%
      mutate(Percentage = Count / sum(Count)) %>%
      filter(Institution != "Independent"),
    aes(
      label = scales::percent(Percentage, accuracy = 1)
    ),
    position = position_fill(vjust = 0.5),
    size = 5,
    colour = "black"
  ) +
  theme(
    
    # larger fonts
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
    
  )

ggsave(
  "plot_institution_pct.png",
  plot = plot_institution_pct,
  width = 7.5,
  height = 7.5,
  dpi = 200
)

# count plot
plot_institution_count <- ggplot(institution_totals,
       aes(x = factor(Year),
           y = Count,
           fill = Institution)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = institution_colours) +
  labs(
    x = "Year",
    y = "Number of Awards",
    fill = "Institution",
    title = "Count of NPA by institution type"
  ) +
  
  theme_minimal() +
  
  theme(
    
    # larger fonts
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
    
  )

ggsave(
  "plot_institution_count.png",
  plot = plot_institution_count,
  width = 7.5,
  height = 7.5,
  dpi = 200
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
plot_scqf4 <- ggplot(scqf4_totals,
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
    title = "SCQF4 Share"
  ) +
  
  scale_y_continuous(
    expand = expansion(add = c(0, 0)),
    limits = c(0, NA),
    minor_breaks = seq(0, 25000, 1000),
    breaks = seq(0, 25000, 5000)
  ) +

  theme(
    
    # larger fonts
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
    
  )

ggsave(
  "plot_scqf4.png",
  plot = plot_scqf4,
  width = 7,
  height = 7.5,
  dpi = 200
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
plot_scqf5 <- ggplot(scqf5_totals,
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
    title = "SCQF4 Share"
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Year",
    y = "Percentage of qualifications",
    fill = "Institution",
    title = "SCQF5 Share"
  ) +
  
  scale_y_continuous(
    expand = expansion(add = c(0, 0)),
    limits = c(0, NA),
    minor_breaks = seq(0, 25000, 1000),
    breaks = seq(0, 25000, 5000)
  ) +
  
  theme(
    
    # larger fonts
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
    
  )

ggsave(
  "plot_scqf5.png",
  plot = plot_scqf5,
  width = 7,
  height = 7.5,
  dpi = 200
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
plot_scqf6 <- ggplot(scqf6_totals,
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
    title = "SCQF6 Share"
  ) +
  
  scale_y_continuous(
    expand = expansion(add = c(0, 0)),
    limits = c(0, NA),
    minor_breaks = seq(0, 25000, 1000),
    breaks = seq(0, 25000, 5000)
  ) +
  
  theme(
    
    # larger fonts
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
    
  )

ggsave(
  "plot_scqf6.png",
  plot = plot_scqf6,
  width = 7,
  height = 7.5,
  dpi = 200
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

# Need to expand display window in order for this to plot properly
wordcloud2(wordcloud_all)

# ===============================
# Cosine similarity of owrd frequencies between sexes
comparison_sex <- merge(
  wordcloud_male,
  wordcloud_female,
  by = "words",
  all = TRUE,
  suffixes = c("_male", "_female")
)

# Calculate cosine similarity by assigning NAs to 0s
comparison_sex[is.na(comparison_sex)] <- 0
x <- comparison_sex$freq_male
y <- comparison_sex$freq_female
cosine_similarity <- sum(x * y) /
  (sqrt(sum(x^2)) * sqrt(sum(y^2)))
cosine_similarity


# Plot the values on a cartesian grid for better comparison
plot_word_coords <- ggplot(comparison_sex,
       aes(x = freq_male,
           y = freq_female,
           label = words)) +
  
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    colour = "grey60"
  ) +
  geom_abline(
    slope = 0,
    intercept = 0,
    linetype = "dashed",
    colour = "grey60"
  ) +
  geom_abline(
    slope = 999999999,
    intercept = 0,
    linetype = "dashed",
    colour = "grey60"
  ) +
  
  geom_point(
    size = 3,
    alpha = 0.7,
    colour = "#1f78b4"
  ) +
  
  geom_text_repel(
    size = 4,
    max.overlaps = Inf,
    box.padding = 0.4,
    point.padding = 0.3
  ) +
  
  labs(
    x = "Male frequency",
    y = "Female frequency",
    title = "NPA Word Frequencies by Sex",
    subtitle = paste0("Cosine similarity = ", round(cosine_similarity, 3))
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
      panel.background = element_rect(fill = "#f2fff2", colour = NA),
      plot.background = element_rect(fill = "#f2fff2", colour = NA)
  )

plot_word_coords
ggsave(
  "plot_word_coords.png",
  plot = plot_word_coords,
  width = 10,
  height = 10,
  dpi = 200
)

# ==============================================================================
# Now look at the actual qualifications in a bit more detail, rather than just
# the words

# Create Qualification annual rankings by number of awards
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

zero_entries <- colSums(rankings_df[year_cols] == 0)
names(zero_entries) <- year_cols
zero_entries <- round(zero_entries / nrow(rankings_df), 2)
zero_entries

# reshape to long format
plot_df <- rankings_df %>%
  
  # keep only SCQF4-6
  filter(Level %in% c("SCQF4", "SCQF5", "SCQF6")) %>%
  
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "Year",
    values_to = "Awards"
  ) %>%
  
  group_by(Level, Year) %>%
  
  summarise(
    
    # number of qualifications with 0 recipients
    zero_subjects = sum(Awards == 0),
    
    # total number of qualifications at that level
    total_subjects = n(),
    
    # percentage with 0 recipients
    zero_percentage = zero_subjects / total_subjects,
    
    .groups = "drop"
  )

plot_df


# make Year numeric for plotting
plot_df <- plot_df %>%
  mutate(Year = as.numeric(Year))

plot_zero_percent <- ggplot(
  data = plot_df,
  aes(
    x = Year,
    y = zero_percentage,
    colour = Level,
    group = Level
  )
) +
  geom_line(linewidth = 1.3) +
  geom_point(shape = 16, size = 3) +
  
  scale_colour_manual(values = qualification_colours) +
  
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    minor_breaks = seq(0, 1, 0.1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  scale_x_continuous(
    breaks = sort(unique(plot_df$Year))
  ) +
  
  labs(
    title = "Percentage of NPA with Zero Recipients by Level",
    x = "Year",
    y = "Percentage with zero recipients",
    colour = "Level"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    
    axis.line = element_line(
      colour = "black",
      linewidth = 1
    ),
    
    axis.ticks = element_line(
      colour = "black",
      linewidth = 0.8
    ),
    
    panel.grid.minor.x = element_blank(),
    
    panel.grid.major.y = element_line(
      colour = "grey60",
      linewidth = 1
    ),
    
    panel.grid.minor.y = element_line(
      colour = "grey85",
      linewidth = 0.4
    )
  )

plot_zero_percent

ggsave(
  "plot_zero_percent.png",
  plot = plot_zero_percent,
  width = 7.5,
  height = 7.5,
  dpi = 200
)


low_uptake <- rankings_df[rowSums(rankings_df[year_cols][1:3]) <= 0,]

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


rankings_df$change <- rankings_df$'2025' - rankings_df$'2022'
rankings_df3 <- rankings_df[order(-rankings_df$change),]
rankings_df3

# ==================================
# Measuring the consistency:
rank_cols <- c(
  "Rank_2025","Rank_2024","Rank_2023",
  "Rank_2022","Rank_2021","Rank_2020","Rank_2019"
)

consistency_df <- rankings_df %>%
  
  rowwise() %>%
  
  mutate(
    
    # average rank over years
    mean_rank = mean(c_across(all_of(rank_cols))),
    
    # standard deviation of rank
    rank_sd = sd(c_across(all_of(rank_cols))),
    
    # total spread of ranks
    rank_range = max(c_across(all_of(rank_cols))) -
      min(c_across(all_of(rank_cols))),
    
    # average year-to-year movement
    mean_yearly_change = mean(
      abs(diff(c_across(all_of(rank_cols))))
    )
    
  ) %>%
  
  ungroup() %>%
  
  arrange(rank_sd)

ranks_sd <- consistency_df %>%
  select(
    Level,
    Subject,
    mean_rank,
    rank_sd,
    rank_range,
    mean_yearly_change
  ) %>%
  head(30)

print(ranks_sd, n = 30)
# ==================================
# By education authority

# get names of sheets for looping and naming columns
sheets <- excel_sheets("subjects_by_ea.xlsx")

# Now load in first sheet. Had a play around and 3 seemed to work nicely
df_ea <- read_excel("subjects_by_ea.xlsx", sheet = sheets[2], skip = 3)
# only retain qualifications - not totals
df_ea <- as.data.frame(df_ea)
df_ea <- df_ea[-(1:5),]
df_ea <- df_ea[,1:3]
df_ea[,3] <- gsub("\\[z\\]", "0", df_ea[,3])
df_ea[,3] <- gsub("\\[c\\]", "5", df_ea[,3])
df_ea[,3] <- gsub(",", "", df_ea[,3])
df_ea[,3] <- as.numeric(gsub("\\s+", "",df_ea[,3]))
colnames(df_ea)[3] <- sheets[2]
df_ea
# Loop through from sheet EA2 to EA32 and append each 2025 row to the dataframe
for (i in 3:33){
  new_sheet <- read_excel("subjects_by_ea.xlsx", sheet = sheets[i], skip = 3)
  # only retain qualifications - not totals
  new_sheet <- as.data.frame(new_sheet)
  new_sheet <- new_sheet[-(1:5),]
  new_sheet <- new_sheet[,1:3]
  new_sheet[,3] <- gsub("\\[z\\]", "0", new_sheet[,3])
  new_sheet[,3] <- gsub("\\[c\\]", "5", new_sheet[,3])
  new_sheet[,3] <- gsub(",", "", new_sheet[,3])
  new_sheet[,3] <- as.numeric(gsub("\\s+", "",new_sheet[,3]))
  df_ea[[sheets[i]]] <- new_sheet[,3]
}
df_ea
authorities = sheets[2:33]
# Now make a ranking for the qualifications in each authority
for (ea in authorities) {
  
  df_ea[[paste0("Rank_", ea)]] <-
    rank(-df_ea[[ea]],
         ties.method = "min")
}
df_ea$Rank_overall <- rankings_df$Rank_2025
df_ea


# Now create scores for each authority by using the top 5 rank of subjects in each authority,
# comparing to the national rank of that subject, adding
scores <- numeric(32)
names(scores) <- authorities

for(i in 1:32){
  
  rank_col <- paste0("Rank_", authorities[i])
  
  # rows corresponding to top 5 ranked subjects
  top5_rows <- order(df_ea[[rank_col]])[1:5]
  
  # national ranks of those subjects
  national_ranks <- df_ea$Rank_overall[top5_rows]
  
  # similarity score
  scores[i] <- sum(national_ranks)
}

# look at totals for each authority to see if the reason they are most similar 
# is because that is where most qualifications are done

total_NPA <- as.numeric(32)
total_NPA <- colSums(df_ea[,3:34])
total_NPA

# =====================================
# Now we have the scores, plot them on the map. Too do this we need to import some
# geoboundaries files

# Import locations
countries <- st_read("geoBoundaries-GBR-ADM1.geojson")
councils <- st_read("geoBoundaries-GBR-ADM3.geojson")

# Filter the scotland boundary from countries
scotland <- countries[countries$shapeName == "Scotland", ]

# Choose all councils that intersects the scotland boundary
scottish_councils <- councils[st_intersects(councils, scotland, sparse = FALSE),]

# Check what councils have been included
scottish_councils$shapeName

# Remove Cumbria and Northumberland
scottish_councils <- scottish_councils[!(scottish_councils$shapeName %in% c("Cumbria", "Northumberland")),]

# Change names so that the order matches Qualifications scotland order
scottish_councils$shapeName[scottish_councils$shapeName == "Glasgow City"] <- "City of Glasgow"
scottish_councils$shapeName[scottish_councils$shapeName == "City of Edinburgh"] <- "The City of Edinburgh"
scottish_councils$shapeName[scottish_councils$shapeName == "Na h-Eileanan Siar"] <- "Comhairle Nan Eilean Siar"
scottish_councils$shapeName[scottish_councils$shapeName == "Moray"] <- "The Moray"

# The datframes are in alphabetical order of council name, so put these names in
# the scores and totals vectors in order to plot them on the map.
ea_names <- sort(scottish_councils$shapeName)
names(scores) <- ea_names
names(total_NPA) <- ea_names
scores
scores_capped <- pmin(scores, 100)
scores
scottish_councils$similarity_score <- scores_capped[scottish_councils$shapeName]
scottish_councils$total_NPA <- total_NPA[scottish_councils$shapeName]



# Scores plot
plot_map_scores <- ggplot(scottish_councils) +
  geom_sf(aes(fill = similarity_score), colour = "white", linewidth = 0.2) +
  scale_fill_gradient(
    low = "darkred",
    high = "lightblue",
    name = "Distinctiveness\nscore"
  ) +
  labs(
    title = "Distinctiveness of each EA vs national NPA subject profile (2025)",
    subtitle = "Lower scores indicate subject choices closer to the Scottish national ranking",
  ) +
  theme_minimal() +
  theme(
    # larger fonts
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
plot_map_scores
ggsave(
  "plot_map_scores.png",
  plot = plot_map_scores,
  width = 7.5,
  height = 7.5,
  dpi = 200
)


# Totals plot
plot_map_totals <- ggplot(scottish_councils) +
  geom_sf(aes(fill = total_NPA), colour = "white", linewidth = 0.2) +
  scale_fill_gradient(
    low = "lightblue",
    high = "darkred",
    name = "Total NPAs\n2025"
  ) +
  labs(
    title = "Total NPA by EA in 2025",
    subtitle = "Only education authority schools contribute to this plot"
  ) +
  theme_minimal() +
  theme(
    # larger fonts
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
plot_map_totals

ggsave(
  "plot_map_totals.png",
  plot = plot_map_totals,
  width = 7.5,
  height = 7.5,
  dpi = 200
)
