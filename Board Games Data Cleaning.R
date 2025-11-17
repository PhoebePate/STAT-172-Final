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
games <- games %>% select(-c(1, 4, 10:13, 15, 17:18, 20:22, 25, 30, 32, 34, 37:45, 47:50, 52))

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
category_counts <- table(unlist(split_lists))

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

<<<<<<< HEAD
# -- average (user rating) column --
# not enough people own the game, so rating shows up as 0
# not many that are 0, so we are just going to remove these few rows
games <- games %>% filter(average != 0)

# -- avgweight (complexity) column --
sum(games$avgweight == 0)
# 892 "missing" values (0) in avgweight column

# we are going to remove these rows, since we still have many other board games (rows)
games <- games %>%filter(avgweight != 0)
=======
>>>>>>> cd5b78aea2bc6cbd8727c380ecc66b9469850837
