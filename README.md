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

One for cleaning, one for descriptive, one for predictive, etc... your README
should walk a reader through your process.)

## Reproduce
1. Run `Board Games Data Cleaning.R` to reproduce data cleaning steps taken. 
These steps included: column reduction, text cleaning & restructuring, fixing invalid values, additional columns, cleaning categorical columns, ensuring logical values, and creating our target variable.
  *  data/boardgames.csv #Original data source
  *  data/cleanboardgames.csv 
  
  
Link below can be used to view the plots found in the manuscript:
https://github.com/PhoebePate/STAT-172-Final/tree/1a74b6306b3e10820018d29556e3ed9b68d2fc39/output
  
2. Run `Board Games Data Exploratory Analysis.R` to reproduce histograms, boxplots, and scatterplots of single variables and our target. 
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
3. Run `Board Games Data Forest.R` to reproduce error rate plots and coefficient plots for the Indonesia data. 
  *  Indonesia Analysis/coef_score_EC_hillebrecht.pdf (Figure 5)
  *  Indonesia Analysis/coef_score_hillebrecht.pdf (Figure 2)
  *  Indonesia Analysis/ER_hybrid_AI.pdf (Figure 7 b)
  *  Indonesia Analysis/ER_hybrid_EC.pdf (Figure 6)
  *  Indonesia Analysis/ER_hybrid.pdf (Figure 3 b)
4. Run `Burkina Faso Analysis/run_mcmc_weights.R` to reproduce heterogeneous ranker results. 
  *  Burkina Faso Analysis/heter_weights_omega.pdf (Figure 4 a)
  *  Burkina Faso Analysis/heter_weights_corr.pdf (Figure 4 b)


## References

Alatas,   V.,   Banerjee,   A.,   Hanna,   R.,   Olken,   B.,   and  Tobias,   J.  (2013).Targeting  the  poor:   Evidence  from  a  field  experiment  in  Indonesia.Harvard  Dataverse,https://doi.org/10.7910/DVN/M7SKQZ, V5.

Hillebrecht,  M.,  Klonner,  S.,  Pacere,  N.  A.,  and  Souares,  A.  (2020b).   Community-basedversus statistical targeting of anti-poverty programs: Evidence from Burkina Faso.Journalof African Economies, 29(3):271–305
