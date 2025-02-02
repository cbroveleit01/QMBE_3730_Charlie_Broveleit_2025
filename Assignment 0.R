install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")

# Load libraries
library(dplyr)
library(ggplot2)
library(readr)

setwd("/Users/charliebroveleit/Downloads/Data Mining")

# Load the dataset
netflix_data <- read.csv("netflix_titles.csv")

# View the first few rows
head(netflix_data)

# 1. How big is the dataset?
dim(netflix_data)  # Returns number of rows and columns

# 2. What are the column names?
names(netflix_data)

# 3. What are the data types?
sapply(netflix_data, class)

# 4. Are there any N/A variables?
colSums(is.na(netflix_data))

# 5. Make a graph (Example: Number of shows per year)
ggplot(netflix_data, aes(x = release_year)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Number of Netflix Titles by Release Year", x = "Release Year", y = "Count")

# 6. Provide Summary Statistics
summary(netflix_data)
