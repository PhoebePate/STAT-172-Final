# exploratory analysis!
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

games_cleaned <- read.csv("data/cleanboardgames.csv", stringsAsFactors = TRUE)

# exploratory analysis!
# look at target 
games_cleaned %>% glimpse() %>% summary()
ggplot(games_cleaned, aes(difficulty, fill = difficulty)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Count of Complex vs Simple Games",
       fill = "Difficulty")
ggsave("output/ComplexXSimple.pdf")
# this shows the proportion of simple vs. complex which helps us think about sampling strategies
# and get a better idea of our target variable


games_cleaned %>% 
  select(minplayers, maxplayers, minplaytime, maxplaytime, yearpublished, desc_word_count) %>% 
  gather() %>% 
  ggplot(aes(value)) + 
  geom_histogram(bins = 30, fill = "grey40") +
  facet_wrap(~ key, scales = "free") +
  labs(title = "Numeric Variable Distributions")
ggsave("output/Numeric Variable Distributions.pdf")
# just wanted to get a feel for the distributions of some potentially significant variables 

ggplot(games_cleaned, aes(difficulty, minplaytime, fill = difficulty)) + 
  geom_boxplot(alpha = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Min Playtime by Difficulty", fill = "Difficulty")
ggsave("output/Min Playtime by Difficulty.pdf")
# this allows us to see the minimum # of minutes for playtime broken out by our target variable
# as expected, it looks like simple games have a much lower min playtime than complex

# histograms
ggplot(games_cleaned, aes(yearpublished, fill = difficulty)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Difficulty Proportions Over Years",
       fill = "Difficulty")
ggsave("output/Year Published Difficulty.pdf")
# this one is kinda cool to look at especially looking at years after 1950 and the visual of the graph
# i don't think it really tells us a whole lot but just gives another way to understand the data 
ggplot(games_cleaned, aes(numplays_month, fill = difficulty)) +
  geom_histogram(position = "fill", binwidth = 150) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Monthly Plays by Difficulty",
       fill = "Difficulty")
ggsave("output/Num Plays per Month Difficulty Hist.pdf")
# this also gives us a glimpse into the data and the numplays per month and it's interesting to see 
# the bands
ggplot(games_cleaned, aes(numwanting, fill = difficulty)) +
  geom_histogram(position = "fill", binwidth = 75) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Number Wanting by Difficulty",
       fill = "Difficulty")
ggsave("output/Num Wanting Difficulty Hist.pdf")
# this is also an interesting visual, with the graph showing there's more numwanting for complex games

# more plots!
# come back to make colorblindness friendly
# chat helped to make pretty for now
# see relationship between difficulty and numwanting
ggplot(games_cleaned, aes(difficulty, numwanting, colour = difficulty)) +
  geom_boxplot(alpha = 0.7) +
  scale_colour_brewer(palette = "Set2") +
  scale_y_log10() +
  labs(title = "Log NumWanting by Difficulty",
       colour = "Difficulty")
ggsave("output/ Num Wanting Difficulty Boxplot.pdf")
# now on a log scale
# honestly I prefer the visual from the histogram but this also is giving us the same message of
# more numwants for complex games which makes sense, you can obviously see the complex is higher

# see relationship between difficulty and numplays
ggplot(games_cleaned, aes(numplays, difficulty, colour = difficulty)) +
  geom_point(alpha = 0.6) +
  scale_colour_brewer(palette = "Set2") +
  labs(title = "Num Plays vs Difficulty", colour = "Difficulty")
ggsave("output/ Num Plays Difficulty.pdf")
# same as above, the histogram gives us a better visual to this, more plays for simple games

ggplot(games_cleaned, aes(numplays_month, difficulty, colour = difficulty)) +
  geom_point(alpha = 0.6) +
  scale_colour_brewer(palette = "Set2") +
  labs(title = "Num Plays per Month vs Difficulty",
       colour = "Difficulty")
ggsave("output/ Num Plays per Month Difficulty.pdf")
# on a smaller scale, months, this is confirming what we saw before, more plays for simple games and this
# shows us better than the overall numplays plot

# year published
ggplot(games_cleaned, aes(yearpublished, difficulty, colour = difficulty)) +
  geom_point(alpha = 0.6) +
  scale_colour_brewer(palette = "Set2") +
  labs(title = "Year Published vs Difficulty", colour = "Difficulty")
ggsave("output/ Year Publish Difficulty.pdf")
# not much I want to say about this, just wanted to look and see

# curious about scatterplot with target and podcast
ggplot(games_cleaned, aes(podcast, difficulty, colour = difficulty)) +
  geom_point(alpha = 0.6) +
  scale_colour_brewer(palette = "Set2") +
  labs(title = "Podcasts vs Difficulty", colour = "Difficulty")
ggsave("output/ Podcast Difficulty.pdf")
# it looks like there's more podcast occurrences for complex games rather than simple which also makes sense to me

# curious about scatterplot with target and news
ggplot(games_cleaned, aes(news, difficulty, colour = difficulty)) +
  geom_point(alpha = 0.6) +
  scale_colour_brewer(palette = "Set2") +
  labs(title = "News vs Difficulty", colour = "Difficulty")
ggsave("output/ News Difficulty.pdf")
# looks to be more news for simple games over complex which wasn't was I expected after looking at the podcast plot

# -- Multivariate plots of x var and y var ---
ggplot(games_cleaned, aes(minplaytime, fill = difficulty)) +
  geom_histogram(position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Min Playtime Distribution by Difficulty",
       fill = "Difficulty")
ggsave("output/hist of minplaytime.pdf")
# see extreme outliers, should we clean this out, we discussed but we will just leave it in and note there's some extreme values

ggplot(games_cleaned, aes(desc_word_count, numwanting, colour = difficulty)) +
  geom_point(alpha = 0.6) +
  scale_colour_brewer(palette = "Set2") +
  theme_bw() +
  labs(title = "Word Count vs Number Wanting",
       colour = "Difficulty")
ggsave("output/ Word Count Difficulty.pdf")
# most descriptions are short, you can start to see how the complex games have a higher word count, there's no strong correlation
# complexity does not strongly affect description length or number of people wanting it looks like 
# end of exploratory analysis. 