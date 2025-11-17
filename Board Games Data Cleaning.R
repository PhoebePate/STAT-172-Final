# STAT 172 FINAL PROJECT DATA CLEANING

rm(list = ls())

# load packages
library(ggplot2)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(pROC)
library(tidymodels)
library(randomForest)
library(RColorBrewer)
library(glmnet)
library(lubridate)
library(reshape2)
library(stringr)

# import data
games <- read.csv("class_data/boardgames.csv", stringsAsFactors = TRUE)
# our client is the Rook Room, a new small business in Des Moines who needs help in categorizing their board game collection. 
# Their collection of games requires a difficulty indicator (whether it is a complex game or not) based on a number of factors. 

# CLEANING STEPS

# remove unwanted columns
games <- games %>% select(-c(1, 4, 10:12, 15, 17:18, 20:22, 25, 30, 32, 34, 37:45, 47:50, 52))

# -- description column --
# turn description into word count
games$description <- as.character(games$description)
games$desc_word_count <- lengths(strsplit(games$description, "\\s+"))
games <- games %>% select(-description) # remove original column

# -- yearpublished column --
# replace years <= 0 with median
yearmed <- median(games$yearpublished)
games$yearpublished[games$yearpublished <= 0] <- yearmed

# -- minplayers column --
# replace minplayers == 0 with median
minplaymed <- median(games$minplayers)
games$minplayers[games$minplayers == 0] <- minplaymed

# -- maxplayers column --
# replace maxplayers == 0 with median
maxplaymed <- median(games$maxplayers)
games$maxplayers[games$maxplayers == 0] <- maxplaymed

# -- minplaytime column --
# replace minplaytime == 0 with median
mintimemed <- median(games$minplaytime)
games$minplaytime[games$minplaytime == 0] <- mintimemed

# -- maxplaytime column --
# replace maxplaytime == 0 with median
maxtimemed <- median(games$maxplaytime)
games$maxplaytime[games$maxplaytime == 0] <- maxtimemed

# -- create new column --
# difference between minplayers and maxplayers
games$dif_players <- (games$maxplayers - games$minplayers)

# -- boardgamecategory column --
# find count of how many times each unique category appears
cleaned_lists <- str_replace_all(games$boardgamecategory, "\\[|\\]|'", "")
split_lists <- str_split(cleaned_lists, ",\\s*")

unique_items <- unique(unlist(split_lists))
unique_items

category_counts <- table(unlist(split_lists))
view(category_counts)

# assign one of the top categories to each row (otherwise "other" category):
top_cats <- c(
  "Card Game",
  "Wargame",
  "Fantasy",
  "Party Game",
  "Dice",
  "Fighting",
  "Abstract Strategy",
  "Childrens Game",
  "Science Fiction"
)

# assign category to each row
assigned_category <- sapply(split_lists, function(x) { # go through each row’s list of categories one by one, call it x
  match_cat <- intersect(x, top_cats) # find which of the row’s categories are in your list of top categories.
  ifelse(length(match_cat) > 0, match_cat[1], "Other") # if it found any top categories, use the first one; if not, label it "Other".
})
})

# create new column for main_category
games$main_category <- assigned_category
games <- games %>% select(-boardgamecategory) # remove original column



# exploratory analysis!
# histograms
ggplot(data = games) + geom_bar(aes(x = yearpublished, fill = avgweight), position = "fill")
ggplot(data = games) + geom_bar(aes(x = numplays_month, fill = avgweight), position = "fill")
ggplot(data = games) + geom_bar(aes(x = numwanting, fill = avgweight), position = "fill")

# better as scatterplots
# chat helped make look pretty
# see relationship between avgweight and numwanting
ggplot(games, aes(x = numwanting, y = avgweight, color = avgweight)) +
  geom_point(alpha = 0.6) +
  theme_minimal()

# see relationship between avgweight and numplays
ggplot(games, aes(x = numplays, y = avgweight, color = avgweight)) +
  geom_point(alpha = 0.6) +
  theme_minimal()

ggplot(games, aes(x = numplays_month, y = avgweight, color = avgweight)) +
  geom_point(alpha = 0.6) +
  theme_minimal()

# year published
ggplot(games, aes(x = yearpublished, y = avgweight, color = avgweight)) +
  geom_point(alpha = 0.6) +
  theme_minimal()

# dif players
ggplot(games, aes(x = dif_players, y = avgweight, color = avgweight)) +
  geom_point(alpha = 0.6) +
  theme_minimal()

# Category (Factor) --> bar chart
ggplot(data = games) + geom_bar(aes(x = category))
table(games$category)

# -- Multivariate plots of x var and y var ---
ggplot(data = games) + geom_histogram(aes(x= minplaytime, fill = avgweight), position = "fill")


# avgweight over number of plays
ggplot(data = games) + geom_histogram(aes(x = numplays, fill = avgweight), position = "fill", binwidth = 1) +
  ggtitle("Avg rating over time") + 
  labs(x = "Year", y = "Proportion") + scale_fill_grey("Board Game's\nOutcome") + theme_bw() +
  theme(plot.title = element_text(hjust = .5)) 


# more multivariate plots
# what if we instead have 2 numeric x variables?? 
ggplot(data = games) + 
  geom_point(aes(x = numplays, y = news, colour = avgweight)) +scale_colour_grey() + theme_bw()

# end of exploratory analysis. 
