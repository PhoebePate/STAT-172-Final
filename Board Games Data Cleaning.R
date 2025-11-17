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

# turn description into word count
games$description <- as.character(games$description)
games$desc_word_count <- lengths(strsplit(games$description, "\\s+"))
games <- games %>% select(-description)

# replace years <= 0 with median
yearmed <- median(games$yearpublished)
games$yearpublished[games$yearpublished <= 0] <- yearmed

# replace minplayers == 0 with median
minplaymed <- median(games$minplayers)
games$minplayers[games$minplayers == 0] <- minplaymed

# replace maxplayers == 0 with median
maxplaymed <- median(games$maxplayers)
games$maxplayers[games$maxplayers == 0] <- maxplaymed

# replace minplaytime == 0 with median
mintimemed <- median(games$minplaytime)
games$minplaytime[games$minplaytime == 0] <- mintimemed

# replace maxplaytime == 0 with median
maxtimemed <- median(games$maxplaytime)
games$maxplaytime[games$maxplaytime == 0] <- maxtimemed

# create new column:
# difference between minplayers and maxplayers
games$dif_players <- (games$maxplayers - games$minplayers)
