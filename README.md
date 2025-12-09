# STAT-172-Final
title: "Reproducing Results from descriptive and predictive modeling approaches to board game complexity classification"
author: "Erika Roehrs and Kaitlyn Staut"
date: "12/09/2025"
output: .md file
---

## Introduction
This repository contains the code and data required to reproduce the results found in https://github.com/PhoebePate/STAT-172-Final/tree/1a74b6306b3e10820018d29556e3ed9b68d2fc39/src . 
Specifically, to run predictions for the outcome of board game complexity on new data and also analyze/investigate relationships between board game attributes and complexity.  

## Our specific goal: our client is the Rook Room, a new small business in Des Moines who needs help in categorizing their board game collection. 
# Their collection of games requires a difficulty indicator (whether it is a complex game or not) based on a number of factors. 
# This project could be applied in the forecasting and understanding of similair board game carrier needs. 


## Requirements
To install the required R packages, run the following code in R:

```{r, eval=FALSE}

install.packages(c("ggplot2", "tidyverse", "rpart", "rpart.plot", "pROC",
                   "tidymodels", "randomForest", "RColorBrewer", "glmnet", "lubridate", "reshape2", "stringr"))
```

## Data
We used one final, cleaned source of data containing historical data from boardgamegeek.com. This data can be found in the following sub-directories:

```{r }
https://github.com/PhoebePate/STAT-172-Final/blob/1a74b6306b3e10820018d29556e3ed9b68d2fc39/src/Board%20Games%20Data%20Cleaning.R
list.files("Stat-172-Final/data/cleanboardgames.csv")
```

The data file that will be called is "cleanboardgames.csv".


## Reproduce
1. Run `Board Games Data Cleaning.R` to reproduce data cleaning steps taken. 
These steps included: column reduction, text cleaning & restructuring, fixing invalid values, additional columns, cleaning categorical columns, ensuring logical values, and creating our target variable.
  *  data/boardgames.csv #Original data source
  *  data/cleanboardgames.csv 
  
  
Link below can be used to view the plots found in the manuscript:
https://github.com/PhoebePate/STAT-172-Final/tree/1a74b6306b3e10820018d29556e3ed9b68d2fc39/output
  
2. Run `Board Games Data Exploratory Analysis.R` to reproduce histograms, boxplots, and scatterplots of explanatory variables and our target. 
  *  Stat-172-Final/output/News Difficulty.pdf
  *  Stat-172-Final/output/Num Plays Difficulty.pdf
  *  Stat-172-Final/output/Num Plays per Month Difficulty.pdf
  *  Stat-172-Final/output/Num Wanting Difficulty Boxplot.pdf
  *  Stat-172-Final/output/Podcast Difficulty.pdf
  *  Stat-172-Final/output/Word Count Difficulty.pdf
  *  Stat-172-Final/output/Year Publish Difficulty.pdf
  *  Stat-172-Final/output/ComplexXSimple.pdf
  *  Stat-172-Final/output/hist of minplaytime.pdf
  *  Stat-172-Final/output/Min Playtime by Difficulty.pdf
  *  Stat-172-Final/output/Num Plays per Month Difficulty Hist.pdf
  *  Stat-172-Final/output/Num Wanting Difficulty Hist.pdf
  *  Stat-172-Final/output/Numeric Variable Distributions.pdf
  *  Stat-172-Final/output/Year Published Difficulty.pdf

3. Run `Board Games Data Forest.R` to reproduce predictive model results (Random Forest). 
  *  Stat-172-Final/src/"Board Games Data Forest.R"

4. Run `Board Games Data Forest.R` to reproduce descriptive model (logistic regression). 
  *  Stat-172-Final/src/"Board Games Data Forest.R"

5. Run `LassoRidge.R` to reproduce penalized regression results (lasso & ridge). 
  *  Stat-172-Final/src/"LassoRidge.R"
  
6. Run `Board Games Clustering.R` to reproduce unsupervised learning clustering results. 
  *  Stat-172-Final/src/"Board Games Clustering.R"
  *  Stat-172-Final/output/ComplexitybyCluster.pdf
