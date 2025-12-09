# MODELING

rm(list = ls())

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

games <- read.csv("data/cleanboardgames.csv", stringsAsFactors = TRUE)

# K-MEANS CLUSTERING ------
# remove columns name, year, category, and difficulty
games_X <- games %>% select(-c(1, 2, 22, 23))
summary(games_X)

# standardize the numeric features
games_stand <- apply(games_X, 2, function(x)(x - mean(x))/sd(x))
summary(games_stand)

# make an elbow plot to determine the optimal number of clusters

wss <- (nrow(games_stand) - 1)*sum(apply(games_stand, 2, var))

for (i in 2:15){
  wss[i] <- sum(kmeans(games_stand, centers = i)$withinss)
}

ggplot() + geom_point(aes(x=1:15, y = wss)) +
  geom_line(aes(x = 1:15, y = wss)) +
  scale_x_continuous(breaks = c(1:15))

# choose the number of clusters at which point the slope/derivative starts 
# to get smaller, we think 4

set.seed(39265)
games_kmeans <- kmeans(games_stand, centers = 4)
str(games_kmeans)

# add cluster assignments back to the X matrix data
games_X_clustered <- games_X
games_X_clustered$km_cluster <- as.factor(games_kmeans$cluster)

# melt the data for visualization
games_long <- melt(games_X_clustered, id.vars = "km_cluster")

ggplot(data = games_long) +
  geom_boxplot(aes(x = km_cluster, y = value, fill = km_cluster)) +
  facet_wrap(~variable, scales = "free") +
  scale_fill_brewer("Grouping", palette = "Dark2") +
  labs(x = "K-means Clsuters of Board Games",
       x = "Cluster", y = "Unstandardized value")

# cluster characteristics
# 1 - low min/max players, low play time, low engagement (news, blogs, etc.)
# 2 - shorter descriptions, low language dependence, low engagement (news, blogs, etc)
# 3 - average metrics all around
# 4 - high number of plays, high engagement (news, blogs, etc.)

# add clusters back to original data set to tie clusters
# to things like number of plays

games$km_clusters <- as.factor(games_kmeans$cluster)

# how prevalent are these song types?
games %>% 
  group_by(km_clusters) %>% 
  count()

# get a sense of songs that are included in these clusters
games %>% 
  filter(km_clusters == 4) %>% 
  arrange(desc(numplays)) %>% 
  head(10)

# how does cluster membership relate to number of plays
# calculate average numplays per years since released (streams)/(2024 - released_year)
games %>% 
  ggplot() +
  geom_boxplot(aes(x = km_clusters, y = (numplays)/(2024 - yearpublished)), fill = "red", colour = "blue") +
  labs(x = "Cluster", y = "Avg. Number of Plays per Year") +
  scale_y_continuous(labels = comma) +
  theme_bw()


# rename the clusters with meaningful names
games <- games %>% 
  mutate(km_clusters_f = factor(km_clusters, levels= c(1:4),
                                labels = c("Casual Family Games", "Kid Friendly Games",
                                           "Hobby Games", "Popular Strategy Games"
                                           )))
games %>% 
  ggplot() +
  geom_boxplot(aes(x = km_clusters_f, y = numplays/(2024 - yearpublished)), fill = "lightblue")+
  labs(x = "Cluster", y = "Avg. Number of Plays per Year") +
  scale_y_continuous(labels = comma) +
  theme_bw() +
  coord_flip()

# what we've essentially done is reduced the 19 numeric variables
# into a single, simple, factor with 4 levels
# --> this is another way to create a simple/parsimonious regression model
games_kmc <- lm(numplays/(2024 - yearpublished) ~ km_clusters_f, data = games)

