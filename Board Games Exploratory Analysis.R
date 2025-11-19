# exploratory analysis!
# look at target 
rm(list = ls())
library(ggplot2)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(pROC)
library(tidymodels)
library(randomForest)
library(RColorBrewer)
library(glmnet)
library(lubridate)
library(reshape2)
library(stringr)
games %>% glimpse() %>% summary()

ggplot(games, aes(difficulty)) +
  geom_bar() +
  labs(title = "Count of Complex vs Simple Games")
# this shows the proportion of simple vs. complex which helps us think about sampling strategies
# and get a better idea of our target variable

games %>% 
  select(minplayers, maxplayers, minplaytime, maxplaytime, yearpublished, desc_word_count) %>% 
  gather() %>% 
  ggplot(aes(value)) + 
  geom_histogram(bins = 30) +
  facet_wrap(~ key, scales = "free") +
  labs(title = "Numeric Variable Distributions")
# just wanted to get a feel for the distributions of some potentially significant variables 

ggplot(games, aes(difficulty, minplaytime)) + 
  geom_boxplot() +
  labs(title = "Min Playtime by Difficulty")
# this allows us to see the minimum # of minutes for playtime broken out by our target variable
# as expected, it looks like simple games have a much lower min playtime than complex

# histograms
ggplot(data = games) + geom_bar(aes(x = yearpublished, fill = difficulty), position = "fill")
# this one is kinda cool to look at especially looking at years after 1950 and the visual of the graph
# i don't think it really tells us a whole lot but just gives another way to understand the data 
ggplot(data = games) + geom_histogram(aes(x = numplays_month, fill = difficulty), position = "fill", binwidth = 150)
# this also gives us a glimpse into the data and the numplays per month and it's interesting to see 
# the bands
ggplot(data = games) + geom_histogram(aes(x = numwanting, fill = difficulty), position = "fill", binwidth = 75)
# this is also an interesting visual, with the graph showing there's more numwanting for complex games


# more plots!
# come back to make colorblindness friendly
# chat helped to make pretty for now
# see relationship between difficulty and numwanting
ggplot(games, aes(x = difficulty, y = numwanting, color = difficulty)) +
  geom_boxplot(alpha = 0.6) +
  theme_minimal() + scale_y_log10()
# now on a log scale
# honestly I prefer the visual from the histogram but this also is giving us the same message of
# more numwants for complex games which makes sense, you can obviously see the complex is higher

# see relationship between difficulty and numplays
ggplot(games, aes(x = numplays, y = difficulty, color = difficulty)) +
  geom_point(alpha = 0.6) +
  theme_minimal()
# same as above, the histogram gives us a better visual to this, more plays for simple games

ggplot(games, aes(x = numplays_month, y = difficulty, color = difficulty)) +
  geom_point(alpha = 0.6) +
  theme_minimal()
# on a smaller scale, months, this is confirming what we saw before, more plays for simple games and this
# shows us better than the overall numplays plot

# year published
ggplot(games, aes(x = yearpublished, y = difficulty, color = difficulty)) +
  geom_point(alpha = 0.6) +
  theme_minimal()
# not much I want to say about this, just wanted to look and see

# curious about scatterplot with target and podcast
ggplot(games, aes(x = podcast, y = difficulty, color = difficulty)) +
  geom_point(alpha = 0.6) +
  theme_minimal()
# it looks like there's more podcast occurrences for complex games rather than simple which also makes sense to me

# curious about scatterplot with target and news
ggplot(games, aes(x = news, y = difficulty, color = difficulty)) +
  geom_point(alpha = 0.6) +
  theme_minimal()
# looks to be more news for simple games over complex which wasn't was I expected after looking at the podcast plot

# -- Multivariate plots of x var and y var ---
ggplot(data = games) + geom_histogram(aes(x= minplaytime, fill = difficulty), position = "fill")

# end of exploratory analysis. 

