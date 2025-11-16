# STAT 172 FINAL PROJECT

rm(list = ls())
library(tidyverse)
library(reshape2)
library(ggplot2)
library(pROC)
library(glmnet)
library(forcats)

games <- read.csv("class_data/boardgames.csv", stringsAsFactors = TRUE)

games <- games %>% select(-c(1, 4, 10:12, 15, 17:22, 25, 30, 32, 34, 37:45, 47, 49:50, 52))

# replace minplayers == 0 with median
# replace maxplayers == 0 with median
# replace minplaytime == 0 with median
# replace maxplaytime == 0 with median
# replace years <= 0 with median

