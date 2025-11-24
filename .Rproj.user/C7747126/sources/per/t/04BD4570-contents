# STAT 172 Final Project
# erika, phoebe, kaitlyn
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

games <- read.csv("boardgames1.csv", stringsAsFactors = TRUE)
games %>% str()

# CLEANING STEPS
# remove unwanted columns
games <- games %>% select(-c(1, 4, 10:12, 15, 17:18, 20:22, 25, 30, 32, 34, 37:45, 47:50, 52))

# turn description into word count
games$description <- as.character(games$description)
games$desc_word_count <- lengths(strsplit(games$description, "\\s+"))
games <- games %>% select(-description)

# replace minplayers == 0 with median
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

# -- boardgamecategory column --
# find count of how many times each unique category appears
cleaned_lists <- str_replace_all(games$boardgamecategory, "\\[|\\]|'", "")
split_lists <- str_split(cleaned_lists, ",\\s*")
unique_items <- unique(unlist(split_lists))
category_counts <- table(unlist(split_lists))
sort(category_counts,decreasing=TRUE)[1:20]

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

# create new column for main_category
games$main_category <- assigned_category
games <- games %>% select(-boardgamecategory) # remove original column

# -- average (user rating) column --
# not enough people own the game, so rating shows up as 0
# not many that are 0, so we are just going to remove these few rows
games <- games %>% filter(average != 0)

# -- avgweight (complexity) column --
sum(games$avgweight == 0)
# 892 "missing" values (0) in avgweight column

# we are going to remove these rows, since we still have many other board games (rows)
games <- games %>%filter(avgweight != 0)

# final checks for factor categorical, logical values, and no NAs
colSums(is.na(games))
str(games)
games <- games %>% filter(maxplayers >= minplayers)
games <- games %>% filter(maxplaytime >= minplaytime)
games <- games %>% filter(yearpublished >= 1900 & yearpublished <= year(Sys.Date()))

# remove duplicates
games <- games %>% distinct()

# defining target variable
games$difficulty <- ifelse(games$avgweight > 2.5, "Complex", "Simple")
games$difficulty <- as.factor(games$difficulty)


# exploratory analysis!
# look at target 
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
ggsave("output/plotname.pdf")

ggplot(data = games) + 
  geom_bar(aes(x = numplays_month, fill = difficulty), position = "fill") + 
  facet_wrap(~average) + ggtitle("Effect of Plays, Difficulty") +
  labs( x = "Plays per Month", y = "Proportion") + scale_fill_grey("Board Game\nOoutcome") + theme_bw()

# what if we instead have 2 numeric x variables?? 

ggplot(data = games) + 
  geom_point(aes(x = desc_word_count, y = numwanting, colour = difficulty)) +scale_colour_grey() + theme_bw()

# end of exploratory analysis. 














# MODELING TIME

games %>% glimpse() %>% summary()


# Goal: predict whether a board game (row in the data) is difficult based on rating of complexity (categorical response)
# this will help us to find a simple model :)
# for lass and ridge we need binary RV
games$difficulty <- ifelse(games$difficulty == "Complex", 1, 0)
games_lr <- games

# variable selection with lasso & ridge!

# ---> split into test/train
RNGkind(sample.kind = "default")
set.seed(44446)
train.idx <- sample(x= 1:nrow(games_lr), size = .7*nrow(games_lr))
train.df <- games_lr[train.idx,]
test.df <- games_lr[-train.idx,]

# start w/traditional logistic regression model fit with MLE
lr_mle <- glm(difficulty ~ .,
              data = train.df,
              family = binomial(link = "logit"))
lr_mle_coefs <- coef(lr_mle)


# Lasso & Ridge time!

# build X matrices for lasso, ridge regression 
x.train <- model.matrix(difficulty ~ ., data = train.df)[,-1]
x.test <- model.matrix(difficulty ~ ., data = test.df)[,-1]

# "vectorized" y vectors
y.train <- as.vector(train.df$difficulty)
y.test <- as.vector(test.df$difficulty)  

# Use cross validation to quickly fit & assess many many lasso & rid"ge regression models
lr_lasso_cv <- cv.glmnet(x.train, y.train, family = binomial(link = "logit"), alpha = 1)
lr_ridge_cv <- cv.glmnet(x.train, y.train, family = binomial(link = "logit"), alpha = 0)

plot(lr_lasso_cv, sign.lambda = 1)
plot(lr_ridge_cv, sign.lambda = 1)

# right side of plot = big lambda = intercept-only model (hard shrinkage)
# left side of plot = tiny baby lambda = MLE (no shrinkage)

best_lasso_lambda <- lr_lasso_cv$lambda.min # this is the lambda that minimizes out of sample error (based on cross validation)
best_ridge_lambda <- lr_ridge_cv$lambda.min 

# these would be the lambdas that would be used to fit "the predictive model"
# we can also look at the coeffs from the best models
lr_ridge_coefs <- coef(lr_ridge_cv, s = best_ridge_lambda)
lr_lasso_coefs <- coef(lr_lasso_cv, s = best_lasso_lambda)


# just to understand, let's plot the coeffs against each other
ggplot() + 
  geom_point(aes(as.vector(lr_mle_coefs), as.vector(lr_ridge_coefs))) + 
  geom_abline(aes(slope = 1, intercept = 0)) + 
  xlim(c(-10,10)) + ylim(c(-10,10))

ggplot() + 
  geom_point(aes(as.vector(lr_mle_coefs), as.vector(lr_lasso_coefs))) + 
  geom_abline(aes(slope = 1, intercept = 0)) +
  xlim(c(-10,10)) + ylim(c(-10,10))


# fit those final models 
final_lasso <- glmnet(x.train, y.train, family = binomial(link = "logit"), alpha = 1,
                      lambda = best_lasso_lambda)
final_ridge <- glmnet(x.train, y.train, family = binomial(link = "logit"), alpha = 0,
                      lambda = best_ridge_lambda)

# these are our final models. now it's time to quatify model performance. 
# plan: make ROC chart for all models & choose best model from there. 

test.df.preds <- test.df %>% 
  mutate(mle_pred = predict(lr_mle, test.df, type = "response"), # gives probability of Y = 1
         lasso_pred = predict(final_lasso, x.test, type = "response")[,1],
         ridge_pred = predict(final_ridge, x.test, type = "response")[,1])

# can see after a quick look that these probabilities are not the same
# even though the model is the same

cor(test.df.preds$mle_pred, test.df.preds$lasso_pred)
plot(test.df.preds$mle_pred, test.df.preds$lasso_pred)

# there are considerable differences between lasso predictions & MLE predictions

# make ROC curves for each of the three models
mle_rocCurve <- roc(response = as.factor(test.df.preds$popular_bin),
                    predictor = test.df.preds$mle_pred,
                    levels = c("0", "1"))
ridge_rocCurve <- roc(response = as.factor(test.df.preds$popular_bin),
                      predictor = test.df.preds$ridge_pred,
                      levels = c("0", "1"))
lasso_rocCurve <- roc(response = as.factor(test.df.preds$popular_bin),
                      predictor = test.df.preds$lasso_pred,
                      levels = c("0", "1"))

#make data frame of MLE ROC info
mle_data <- data.frame(
  Model = "MLE",
  Specificity = mle_rocCurve$specificities,
  Sensitivity = mle_rocCurve$sensitivities,
  AUC = as.numeric(mle_rocCurve$auc)
)
#make data frame of lasso ROC info
lasso_data <- data.frame(
  Model = "Lasso",
  Specificity = lasso_rocCurve$specificities,
  Sensitivity = lasso_rocCurve$sensitivities,
  AUC = lasso_rocCurve$auc %>% as.numeric
)
#make data frame of ridge ROC info
ridge_data <- data.frame(
  Model = "Ridge",
  Specificity = ridge_rocCurve$specificities,
  Sensitivity = ridge_rocCurve$sensitivities,
  AUC = ridge_rocCurve$auc%>% as.numeric
)

# Combine all the data frames
roc_data <- rbind(mle_data, lasso_data, ridge_data)


# Plot the data
ggplot() +
  geom_line(aes(x = 1 - Specificity, y = Sensitivity, color = Model),data = roc_data) +
  geom_text(data = roc_data %>% group_by(Model) %>% slice(1), 
            aes(x = 0.75, y = c(0.75, 0.65, 0.55), colour = Model,
                label = paste0(Model, " AUC = ", round(AUC, 3)))) +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "1 - Specificity", y = "Sensitivity", color = "Model") +
  theme_minimal()






