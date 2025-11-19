# RANDOM FOREST

library(rpart) # classification trees
library(rpart.plot) # makes pretty trees
library(tidyverse) 
library(tidymodels)
library(pROC)
library(randomForest)
library(ggplot2)
library(dplyr)

summary(games)
games_forest <- games %>% select(-c(1,2,10,23))
summary(games_forest)
RNGkind(sample.kind = "default")
set.seed(2291352)
train.idx <- sample(x = 1:nrow(games_forest), size = .7*nrow(games_forest))
train.df <- games_forest[train.idx,]
test.df <- games_forest[-train.idx,]

tempforest <- randomForest(difficulty ~.,
                           data = train.df,
                           ntree = 500, 
                           mtry = 4)

rf_model <- rand_forest(mtry = tune(),
                        trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("randomForest")

rf_rec <- recipe(difficulty ~., data = train.df)

rf_wf <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(rf_rec)

set.seed(172172)
folds <- vfold_cv(train.df, v = 5)

rf_tuned <- tune_grid(rf_wf,
                      resamples = folds,
                      grid = tibble(mtry = c(1:19)),
                      metrics = metric_set(roc_auc)
)
rf_results <- rf_tuned %>% collect_metrics()

ggplot(data = rf_results) +
  geom_line(aes(x = mtry, y = mean)) +
  geom_point(aes(x = mtry, y = mean)) +
  labs(x = "m (mtry) value", y = "Area under the curve (AUC)")+
  theme_bw()
scale_x_continuous(breaks = c(1:12))


