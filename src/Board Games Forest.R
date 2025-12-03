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

summary(games)

# BASELINE FOREST -----
myforest <- randomForest(difficulty ~ ., # recall notes on this syntax
                         data = train.df, # TRAINING DATA
                         ntree = 1000, # fit B = 1000 seperate classification trees
                         mtry = 4, # choose m - sqrt(20) = 4.472136 (rounded down to 4)
                         importance = TRUE) # importance can help us identify important predictors

myforest

# TUNING FOREST -----

# (Step 1) Define the model (with mtry as a tunable parameter)
rf_model <- rand_forest(mtry = tune(), # tune() tells it to tune mtry parameter
                        trees = 1000) %>% # fix B (as large as you can afford) - doesn't tune 
  set_mode("classification") %>% # not "regression", which is for a numeric Y
  set_engine("randomForest") # note: there are multiple packages that run RFs

# (Step 2) Create a recipe
rf_rec <- recipe(difficulty ~ ., data = train.df)

# (Step 3) Create the workflow
rf_wf <- workflow() %>%
  add_model(rf_model) %>% #from step 1
  add_recipe(rf_rec) #from step 2

# (Step 4) Create folds for cross validation (see previous illustration)
set.seed(2024)
folds <- vfold_cv(train.df, v = 5)

# (Step 5) Tune random forest
rf_tuned <- tune_grid(
  rf_wf, #workflow from step 3
  resamples = folds, #folds created in step 4
  grid = tibble(mtry = c(1:19)), #think: what is possible here? (12 possible predictor variables)
  metrics = metric_set(roc_auc) # could add "accuracy here if an oob approach is desired"
)

# balanced data: either AUC or OOB works well
# imbalanced data: AUC more enlightening

# (Step 6) Extract AUC and/or OOb error estimates
rf_results <- rf_tuned %>%
  collect_metrics()

ggplot(data = rf_results) + # not training, its rf_results that has the auc information
  geom_line(aes(x = mtry, y = mean)) + # mean is the mean AUC from cross validation
  geom_point(aes(x = mtry, y = mean)) +
  labs(x = "m (mtry) value", y = "Area Under the Curve (AUC)") +
  theme_bw() +
  scale_x_continuous(breaks = c(1:12))

#mtry of 3 maximizes area under the curve

best_params <- select_best(rf_tuned, metric = "roc_auc")

final_forest <- final_forest <- randomForest(Result ~ .,
                                             data = train.df,
                                             ntree = 1000,
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
pi_hat <- predict(final_forest, test.df, type = "prob")[, "W"] # gets vector of win probabilities
rocCurve <- roc(response = test.df$Result,
                predictor = pi_hat, #probabilities of W, our positive event
                levels = c("L", "W")) #first negative event, then positive

plot(rocCurve, print.thres = TRUE, print.auc = TRUE)

# if we set pi* = 0.813, we can achieve a specificity of 1.00
# and a sensitivity of 0.810.
# That is, we'll predict a loss 100% of the time when Serena
# actually loses.
# and we'll predict a win 81% of the time when Serena actually wins.
# the area under the curve is 0.944.

# AUC can be compared across models - note that our forest
# has a higher AUC than our tree did. Thus, we prefer the forest.

# Make a column of predicted values in our test data
pi_star <- coords(rocCurve, "best", ret = "threshold")$threshold[1]
# make a new column of predictions
test.df$forest_pred <- ifelse(pi_hat > pi_star, "W", "L") %>% as.factor()

# Note: in production we do not use code that looks like:
#new_data$predicted_classes <- predict(final_forest, new_data, type = "class")



## GOAL IS INTERPRTATION ------
# If we want to interpret or understand relationships between Xs and Y...
# we can look at the relative importance if each of our variables
# in predicting wins
# Note: we can only do the following if we set importance = TRUE
varImpPlot(final_forest, type = 1)
# it looks like serve points won (2nd and 1st server) are most important
# for predicting a win/loss.
# these are the variables for which the model would suffer if
# they were removed

# so random forests are great at ranking variables, but they provide
# little insight into DIRECTIONAL EFFECTS
# that is - when Xend increases, does P(W) increase or decrease?

# random forest strength: automatic variable selection
# random forest weakness: no directional effects for interpretation
# logistic regression strength: we get directional effects
# logistic regression weakness: no automatic variable selection

# ---> pair random forests and logistic regressions up!

# let's fit a logistic regression using only the most important variables as
# explanatory variables

# first create a bernoulli RV
cdata$Result_bin <- ifelse(cdata$Result == "W", 1, 0)

m1 <- glm(Result_bin ~ X2nd., data = cdata,
          family = binomial(link = "logit"))
AIC(m1) # 367.5994

m2 <- glm(Result_bin ~ X2nd. + X1st., data = cdata,
          family = binomial(link = "logit"))
AIC(m2) # 288.9497

m3 <- glm(Result_bin ~ X2nd. + X1st. + vRk, data = cdata,
          family = binomial(link = "logit"))
AIC(m3) # 278.1877

m4 <- glm(Result_bin ~ X2nd. + X1st. + vRk + A., data = cdata,
          family = binomial(link = "logit"))
AIC(m4) # 276.5052

m5 <- glm(Result_bin ~ X2nd. + X1st. + vRk + A. + Rd, data = cdata,
          family = binomial(link = "logit"))
AIC(m5) # 281.4107

#m4 would be our "final" descriptive model that we will use to
# supplement the random forest
# i.e. - our plan is to use the random forest for predictions
# but we will use m4 for descriptive statements

summary(m4)

coef(m4)
exp(coef(m4))
# not all default odds ratios are meaningful...
# holding other game characteristics constant, for every 10% (0.1)
# increase in 2nd serve point %, Serena's odds of winning
# increase by a factor of exp(10.882655*0.1) = 2.9.
# That is, the odds of winning almost triple

confint(m4)
