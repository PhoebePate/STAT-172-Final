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

games_cleaned <- read.csv("data/cleanboardgames.csv", stringsAsFactors = TRUE)

# MODELING TIME
games_cleaned %>% glimpse() %>% summary()


# Goal: predict whether a board game (row in the data) is difficult based on rating of complexity (categorical response)
# this will help us to find a simple model :)
# for lasso and ridge we need binary RV
games_cleaned$difficulty <- ifelse(games_cleaned$difficulty == "Complex", 1, 0)
games_cleaned_lr <- games_cleaned

# variable selection with lasso & ridge!

# ---> split into test/train
RNGkind(sample.kind = "default")
set.seed(44446)
train.idx <- sample(x= 1:nrow(games_cleaned_lr), size = .7*nrow(games_cleaned_lr))
train.df <- games_cleaned_lr[train.idx,]
test.df <- games_cleaned_lr[-train.idx,]

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






