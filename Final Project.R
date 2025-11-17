# STAT 172 FINAL PROJECT

rm(list = ls())
library(tidyverse)
library(reshape2)
library(ggplot2)
library(pROC)
library(glmnet)
library(forcats)

games <- read.csv("class_data/boardgames.csv", stringsAsFactors = TRUE)
# our client is the Rook Room, a new small business in Des Moines who needs help in categorizing their board game collection. 
# Their collection of games requires a difficulty indicator (whether it is a complex game or not) based on a number of factors. 

games <- games %>% select(-c(1, 4, 10:12, 15, 17:18, 20:22, 25, 30, 32, 34, 37:50, 52))

# turn description into word count
games$desc_word_count <- lengths(strsplit(games$description, "\\s+"))

# replace minplayers == 0 with median
# replace maxplayers == 0 with median
# replace minplaytime == 0 with median
# replace maxplaytime == 0 with median
# replace years <= 0 with median

