library(ggplot2)

# Load the dataset
df <- read.csv("cleaned_movies.csv.csv")

# Create a summary table
movies_per_year <- as.data.frame(table(df$YEAR))
colnames(movies_per_year) <- c("Year", "Count")
movies_per_year$Year <- as.numeric(as.character(movies_per_year$Year))

# Plot using ggplot2
ggplot(movies_per_year, aes(x = Year, y = Count)) +
  geom_line(color = "green") +
  geom_point(color = "green") +
  ggtitle("Number of Movies Released by Year") +
  xlab("Year") +
  ylab("Number of Movies") +
  theme_minimal()

# Question 2 

library(tidyverse)


df <- read.csv("cleaned_movies.csv.csv")

#  Remove rows with missing genre
df_cleaned <- df %>% filter(!is.na(GENRE))

# Split multi-genre strings and unnest into individual rows
genre_counts <- df_cleaned %>%
  mutate(GENRE = strsplit(as.character(GENRE), ",\\s*")) %>%  # split by comma + space
  unnest(GENRE) %>%                                            # explode into separate rows
  count(GENRE, sort = TRUE)                                    # count each genre


ggplot(genre_counts, aes(x = reorder(GENRE, -n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Number of Movies by Genre",
       x = "Genre",
       y = "Number of Movies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Question 3 
library(ggplot2)

# 
df <- read.csv("cleaned_movies.csv.csv")

#  Remove rows with missing ratings
df_ratings <- df[!is.na(df$RATING), ]


ggplot(df_ratings, aes(x = RATING)) +
  geom_histogram(binwidth = 0.25, fill = "green", color = "black") +
  labs(title = "Distribution of Movie Ratings",
       x = "Rating",
       y = "Number of Movies") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey90"))


#Question4

library(dplyr)
library(ggplot2)


df <- read.csv("cleaned_movies.csv.csv")

# Remove rows with missing 'RATING' or 'MOVIES'
df_valid_ratings <- df %>%
  filter(!is.na(RATING) & !is.na(MOVIES))

#  Get the top 10 movies by rating
top_10_movies <- df_valid_ratings %>%
  arrange(desc(RATING)) %>%
  slice_head(n = 10) %>%
  select(MOVIES, RATING, YEAR, GENRE)


print(top_10_movies)

#  Plot horizontal bar chart
ggplot(top_10_movies, aes(x = RATING, y = reorder(MOVIES, RATING))) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  labs(title = "Top 10 Rated Movies",
       x = "Rating",
       y = "Movie") +
  theme_minimal()

#Question 5
library(tidyverse)

df <- read.csv("cleaned_movies.csv.csv")

#  Drop rows with missing genres
df_genres <- df %>% filter(!is.na(GENRE))

# Split multiple genres and unnest into individual rows
genre_counts <- df_genres %>%
  mutate(GENRE = strsplit(as.character(GENRE), ",\\s*")) %>%  # split on comma + space
  unnest(GENRE) %>%
  count(GENRE, sort = TRUE)  # count each genre

#  Plot the genre frequencies
ggplot(genre_counts, aes(x = reorder(GENRE, -n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Most Common Movie Genres",
       x = "Genre",
       y = "Number of Movies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Question 6:
  library(tidyverse)

df <- read.csv("cleaned_movies.csv.csv")

# Drop rows with missing RATING or GENRE
df_clean <- df %>%
  filter(!is.na(RATING) & !is.na(GENRE))

# Split multi-genre entries and unnest into individual rows
df_exploded <- df_clean %>%
  mutate(GENRE = strsplit(as.character(GENRE), ",\\s*")) %>%
  unnest(GENRE)

# roup by genre and compute average rating
avg_ratings <- df_exploded %>%
  group_by(GENRE) %>%
  summarise(avg_rating = mean(RATING, na.rm = TRUE)) %>%
  arrange(GENRE)  # Sort alphabetically

#  Plot the average ratings per genre
ggplot(avg_ratings, aes(x = GENRE, y = avg_rating)) +
  geom_bar(stat = "identity", fill = "cornflowerblue", color = "black") +
  labs(title = "Average Movie Rating per Genre (Alphabetical)",
       x = "Genre",
       y = "Average Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Question 7
library(ggplot2)
library(dplyr)


df <- read.csv("cleaned_movies.csv.csv")

#  Filter rows with no RunTime or RATING
df_clean <- df %>%
  filter(!is.na(RunTime) & !is.na(RATING))

#Calculate the correlation coefficient
correlation <- cor(df_clean$RunTime, df_clean$RATING)

#  Create the scatter plot with regression line
ggplot(df_clean, aes(x = RunTime, y = RATING)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = paste0("Correlation Between Runtime and Rating (r = ", round(correlation, 2), ")"),
    x = "Runtime (minutes)",
    y = "Rating"
  ) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey90"))


#Question8

library(tidyverse)


df <- read.csv("cleaned_movies.csv.csv")

# Remove rows with missing RATING or GENRE
df_clean <- df %>%
  filter(!is.na(RATING) & !is.na(GENRE))

# split multi-genre strings and unnest into individual rows
df_exploded <- df_clean %>%
  mutate(GENRE = strsplit(as.character(GENRE), ",\\s*")) %>%
  unnest(GENRE)

# Group by genre to calculate average rating and movie count
genre_stats <- df_exploded %>%
  group_by(GENRE) %>%
  summarise(
    avg_rating = mean(RATING, na.rm = TRUE),
    count = n()
  ) %>%
  filter(count >= 5) %>%                      #Filter out genres with < 5 movies
  arrange(desc(avg_rating))                  # Sort by average rating

# Plot the average ratings
ggplot(genre_stats, aes(x = reorder(GENRE, -avg_rating), y = avg_rating)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  labs(title = "Average Movie Rating by Genre (Min. 5 Movies)",
       x = "Genre",
       y = "Average Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Question 9
library(tidyverse)


df <- read.csv("cleaned_movies.csv.csv")

#  Clean data — ensure YEAR and RATING are numeric and not missing
df_clean <- df %>%
  filter(!is.na(RATING) & !is.na(YEAR)) %>%
  mutate(YEAR = as.numeric(as.character(YEAR)))

#  Calculate average rating per year
ratings_by_year <- df_clean %>%
  group_by(YEAR) %>%
  summarise(avg_rating = mean(RATING, na.rm = TRUE))

#Plot average rating per year with regression line
ggplot(ratings_by_year, aes(x = YEAR, y = avg_rating)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Do Newer Movies Get Higher Ratings?",
    x = "Release Year",
    y = "Average Rating"
  ) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey90"))

#Question 10 

library(tidyverse)


df <- read.csv("cleaned_movies.csv.csv")

#  Filter rows with missing Gross or GENRE
df_clean <- df %>%
  filter(!is.na(Gross) & !is.na(GENRE))

#Split multi-genre strings and unnest into individual rows
df_exploded <- df_clean %>%
  mutate(GENRE = strsplit(as.character(GENRE), ",\\s*")) %>%
  unnest(GENRE)

# Group by genre and sum gross earnings
genre_gross <- df_exploded %>%
  group_by(GENRE) %>%
  summarise(total_gross = sum(Gross, na.rm = TRUE)) %>%
  arrange(desc(total_gross))

# Plot total gross per genre
ggplot(genre_gross, aes(x = reorder(GENRE, -total_gross), y = total_gross)) +
  geom_bar(stat = "identity", fill = "goldenrod", color = "black") +
  labs(
    title = "Total Gross Earnings by Genre",
    x = "Genre",
    y = "Total Gross ($)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
