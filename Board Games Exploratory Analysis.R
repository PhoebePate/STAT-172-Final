# exploratory analysis!
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

# look at target 
ggplot(games, aes(difficulty)) +
  geom_bar() +
  labs(title = "Count of Complex vs Simple Games")

games %>% 
  select(minplayers, maxplayers, minplaytime, maxplaytime, yearpublished, desc_word_count) %>% 
  gather() %>% 
  ggplot(aes(value)) + 
  geom_histogram(bins = 30) +
  facet_wrap(~ key, scales = "free") +
  labs(title = "Numeric Variable Distributions")

ggplot(games, aes(difficulty, avgweight)) + 
  geom_boxplot()

# histograms
games <- read.csv("class_data/boardgames.csv", stringsAsFactors = TRUE)

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

# end of exploratory analysis. 
