# RANDOM FOREST

rm(list = ls())

library(rpart) # classification trees
library(rpart.plot) # makes pretty trees
library(tidyverse) 
library(tidymodels)
library(pROC)
library(randomForest)
library(ggplot2)
library(dplyr)

games <- read.csv("data/cleanboardgames.csv", stringsAsFactors = TRUE)
games <- games %>% select(-c(1))

# DATA PREPERATION ------

# set the seed
RNGkind(sample.kind = "default")
set.seed(2291352)
# train.idx will contain a random sample of row indices
train.idx <- sample(x = 1:nrow(games), size = floor(.7*nrow(games)))
# make training data
train.df <- games[train.idx,]
# the rest will be for testing
test.df <- games[-train.idx,]

# BASELINE FOREST -----
myforest <- randomForest(difficulty ~ ., # recall notes on this syntax
                         data = train.df, # TRAINING DATA
                         ntree = 1000, # fit B = 1000 seperate classification trees
                         mtry = 5, # choose m: sqrt(22) = 4.690416 (rounded up to 5)
                         importance = TRUE) # importance can help us identify important predictors

myforest

# TUNING FOREST -----

# (Step 1) Define the model (with mtry as a tunable parameter)
rf_model <- rand_forest(mtry = tune(),
                        trees = 500) %>% 
  set_mode("classification") %>%
  set_engine("randomForest")

# (Step 2) Create a recipe
rf_rec <- recipe(difficulty ~ ., data = train.df)

# (Step 3) Create the workflow
rf_wf <- workflow() %>%
  add_model(rf_model) %>% #from step 1
  add_recipe(rf_rec) #from step 2

# (Step 4) Create folds for cross validation (see previous illustration)
set.seed(2024)
folds <- vfold_cv(train.df, v = 3)

# (Step 5) Tune random forest
rf_tuned <- tune_grid(
  rf_wf, #workflow from step 3
  resamples = folds, #folds created in step 4
  grid = tibble(mtry = c(2:7)),
  metrics = metric_set(roc_auc)
)

# (Step 6) Extract AUC and/or OOb error estimates
rf_results <- rf_tuned %>%
  collect_metrics()

ggplot(data = rf_results) +
  geom_line(aes(x = mtry, y = mean)) +
  geom_point(aes(x = mtry, y = mean)) +
  labs(x = "m (mtry) value", y = "Area Under the Curve (AUC)") +
  theme_bw() +
  scale_x_continuous(breaks = c(1:8))
ggsave("output/Maxmtryplot.pdf")

#mtry of 3 maximizes area under the curve

best_params <- select_best(rf_tuned, metric = "roc_auc")

final_forest <- final_forest <- randomForest(difficulty ~ .,
                                             data = train.df,
                                             ntree = 500,
                                             mtry = best_params %>% pull(mtry),
                                             importance = TRUE)

# this final forest is based on our tuning exercise

# RESULTS ------
# in most problems our goal is going to emphasize
# (1) prediction
# (2) interpretation


## GOAL IS PREDICTION ------

# start by creating ROC curve
# assuming 'positive event' is W!
pi_hat <- predict(final_forest, test.df, type = "prob")[, "Complex"]
rocCurve <- roc(response = test.df$difficulty,
                predictor = pi_hat,
                levels = c("Simple", "Complex"))

plot(rocCurve, print.thres = TRUE, print.auc = TRUE)
ggsave("output/FinalForestROCcurve.pdf")


# Make a column of predicted values in our test data
pi_star <- coords(rocCurve, "best", ret = "threshold")$threshold[1]
# make a new column of predictions
test.df$forest_pred <- ifelse(pi_hat > pi_star, "Complex", "Simple") %>% as.factor()

varImpPlot(final_forest, type = 1)
ggsave("output/ForestVariableImportancePlot.pdf")


# LOGISTIC REGRESSION

# create a bernoulli RV
games$diff_bin <- ifelse(games$difficulty == "Complex", 1, 0)

m1 <- glm(diff_bin ~ average, data = games,
          family = binomial(link = "logit"))
AIC(m1) # 18231.36

m2 <- glm(diff_bin ~ average + maxplaytime, data = games,
          family = binomial(link = "logit"))
AIC(m2) # 13789.48

m3 <- glm(diff_bin ~ average + maxplaytime + main_category, data = games,
          family = binomial(link = "logit"))
AIC(m3) # 13040

m4 <- glm(diff_bin ~ average + maxplaytime + main_category + numwanting, data = games,
          family = binomial(link = "logit"))
AIC(m4) # 12997.11

m5 <- glm(diff_bin ~ average + maxplaytime + main_category + numwanting +
            minage, data = games,
          family = binomial(link = "logit"))
AIC(m5) # 12833.29

m6 <- glm(diff_bin ~ average + maxplaytime + main_category + numwanting +
            minage + minplaytime, data = games,
          family = binomial(link = "logit"))
AIC(m6) # 12777

m7 <- glm(diff_bin ~ average + maxplaytime + main_category + numwanting +
            minage + minplaytime + yearpublished, data = games,
          family = binomial(link = "logit"))
AIC(m7) # 12777.43

#m87 would be our "final" descriptive model that we will use to
# supplement the random forest
# use the random forest for predictions
# use m7 for descriptive statements

summary(m7)

coef(m7)
exp(coef(m7))

confint(m7)
