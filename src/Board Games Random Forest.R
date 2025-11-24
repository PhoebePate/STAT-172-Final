# RANDOM FOREST

library(rpart) # classification trees
library(rpart.plot) # makes pretty trees
library(tidyverse) 
library(tidymodels)
library(pROC)
library(randomForest)
library(ggplot2)
library(dplyr)

source("STAT-172-Final/Board Games Data Cleaning.R")

games_forest <- read.csv("class_data/cleanboardgames.csv")
summary(games_forest)
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
                        trees = 500) %>%
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

best_params <- select_best(rf_tuned, metric = "roc_auc")


final_forest <- randomForest(Result ~.,
                             data = train.df,
                             ntree = 1000,
                             mtry = best_params %>% pull(mtry),
                             importance = TRUE)

# use below model when tidymodels does not work
final_forest <- randomForest(Result ~.,
                             data = train.df,
                             ntree = 1000,
                             mtry = 2,
                             importance = TRUE)

#Results
# in most problems our goal is going to emphasize
# (1) prediction
# (2) interpretation

# GOAL IS PREDICTION

# Start by creating ROC Curve
# Assumming 'positive event' is W!

pi_hat <- predict(final_forest, test.df, type = "prob")[,"W"] # gets vector of win probabilities
rocCurve <- roc(response = test.df$Result,
                predictor = pi_hat, #probabilities of W, our positive event
                levels = c("L","W"))

plot(rocCurve, print.thres = TRUE, print.auc = TRUE)

# if we set pi* = 0.841, we can achieve a specificity of 1.00
# and a sensitivity of 0.771.
# That is, we'll predict a loss 100% of the time when Serena
# actually loses.
# And we'll predict a win 77.1% of the time when Serena actually wins.
# The area under the curve is 0.946

#AUC can be compared across models - note that our forest
# has a higher AUC than our tree did. Thus, we prefer the forest.

#Make a column of predicted values in our test data
pi_star <- coords(rocCurve, "best", ret = "threshold")$threshold[1]

#make a new column of predictions
test.df$forest_pred <- ifelse(pi_hat > pi_star, "W","L")%>% as.factor()


# Note: in production we do not use code that looks like
#newdata$predicted_classes <- predict(final_forest,new_data,type = "class") #type = class assumes pi* of 0.5, different L and W, not correct specificity/sensitivity


#GOAL IS INTERPRETATION
# if we want to interpret or understand relationships betweeen Xs and Y...
#we can look at the relative importance of each of our variables
#in predicting wins
#note;we can only do the following if we set importance = TRUE
varImpPlot(final_forest,type = 1)
# It looks like we serve points won (2nd and 1st serve) are the most 
# important for predicting win/loss
#These are the variables for which the model would suffer if they were removed

#So random forests are great at ranking variables, but they provide
# little insight into DIRECTIONAL EFFECTS
# that is - when X2nd increases, does P(W) increase or decrease?

#Random forest strength: automatic variable selection
#Random Forest weakness: no directional effects for interpretation
#Logistic regression strength: we get directional effects
#Logistic regression weakness: no automatic variable selection

#-----> pair random forests and logistic regressions up!

# Let's fit a logistic regression using only th emost important variables as
# explanatory variables.



# First create a bernoulli RV
cdata$Result_bin <- ifelse(cdata$Result == "W",1,0)

m1 <- glm(Result_bin ~ X2nd., data = cdata,
          family = binomial(link = "logit"))
AIC(m1) #367.5994

m2 <- glm(Result_bin ~ X2nd. + X1st., data = cdata,
          family = binomial(link = "logit"))
AIC(m2) #288.9497 good! want AIC to be small

m3 <- glm(Result_bin ~ X2nd. + X1st. + vRk, data = cdata,
          family = binomial(link = "logit"))
AIC(m3) #278.1877 good! want AIC to be small

m4 <- glm(Result_bin ~ X2nd. + X1st. + vRk + A., data = cdata,
          family = binomial(link = "logit"))
AIC(m4) #278.1877 good! want AIC to be small

m5 <- glm(Result_bin ~ X2nd. + X1st. + vRk + A. + Rd, data = cdata,
          family = binomial(link = "logit"))
AIC(m5) #281.4107 bad! want AIC to be small

# m4 would be our "final" descriptive model that we will use to 
#supplement the random forest
#i.e., - our plan is to use the random forest for predictions
# but we will use m4 for descriptive statements

summary(m4) # gives z value
# the coefficients are large, and need to be exponentiated
summary(cdata$X2d.)
# this is where the odds increasing by 53,000 comes from, 0->1 instead of incremental

exp(10.882655*.1) # if increases X2nd. by 10%, odds triple

coef(m4)

exp(coef(m4))
# not all default odd rations are meaningfull...
# Holding other game characteristics constant, for every 10% (0.1)
# increase in 2nd serve point %, serena's odds of winning
# increase by a factor of exp(10.882655*.1) = 2.9
# That is, the odds of winning almost triple.

confint(m4)










