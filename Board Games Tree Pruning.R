# Board Games Decision Tree

source("STAT-172-Final/Board Games Data Cleaning.R") # sourcing the dataframe from 

games_clean <- read.csv("class_data/cleanboardgames.csv")
games_clean <- games_clean %>% select(-c(1,2,10,23))


RNGkind(sample.kind = "default")
set.seed(2291352)

train.idx <- sample(x = 1:nrow(games_clean), size = 0.8*nrow(games_clean))


# create training data
train.df <- games_clean[train.idx,]
# create testing data
test.df <- games_clean[-train.idx,]

###----- TREE FITTING --------

set.seed(172172172)
games_tree <- rpart(difficulty ~ .,
               data = train.df, 
               method = 'class')


# plot our tree
rpart.plot(games_tree)
levels(train.df$difficulty)

### ------- TUNE OUR TREE ---------

printcp(games_tree)


# DON'T look at rel error: this is in sample error


# Grow a super big tree (too big for prediction!), then prune it down.

set.seed(172172172)
games_tree <- rpart(difficulty ~ ., # assumes you want ALL OTHER COLUMNS AS PREDICTORS
               data = train.df, 
               method = 'class',
               control = rpart.control(cp = 0.0001, minsplit = 1))
rpart.plot(games_tree)
# prune it
printcp(games_tree)


optimalcp <- games_tree$cptable[which.min(games_tree$cptable[,"xerror"]),"CP"]
games_tree2 <- prune(games_tree,cp = optimalcp)

#games_tree2 is the (tuned) tree

rpart.plot(games_tree2)

### --- MODEL VALIDATION  + PREDICTION----
# make the column of predictions based on pruned tree
#do this in the test dataset so we can see how well we do
# on truly new data

test.df$result_pred <- predict(ctree2,test.df,type = "class")
table(test.df$result_pred,test.df$Result)

#accuracy: (9+95)/(9+10+95+5)


